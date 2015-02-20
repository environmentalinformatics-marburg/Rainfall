#' Calculate predictors from MSG cloud masked data
#' @param scenerasters A raster stack of cloud masked MSG scenes with non 
#' clouded areas were set to NA
#' @param sunzenith A raster of the sun zenith values
#' @param spectral A character vector indicating the msg channels to be included.
#' Possible values: "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
#' "IR9.7","IR10.8","IR12.0","IR13.4","T0.6_1.6","T6.2_10.8","T7.3_12.0",
#' "T8.7_10.8","T10.8_12.0", "T3.9_7.3","T3.9_10.8"
#' @param texture data frame of all spectral and texture combinations which are
#' to be calculated. (Tip: Use expand.grid to create this data.frame)
#' @param pptext data frame of all spectral and texture combinations which are
#' to be calculated for teh overall cloud entity.
#'  (Tip: Use expand.grid to create this data.frame)
#' @param zonstat data frame of all spectral and zonal stat combinations
#'  (min,max,mean or sd) which are to be calculated for the overall cloud entity.
#'  (Tip: Use expand.grid to create this data.frame)
#' @param shape geoemtry variables which should be included. Possible values
#' are "Ar",SI","CA","Ur","CAI","PAR","distEdges","Re","Ru","OIC",
#' CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
#' "TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3"
#' @param further a character vector including Currently "jday" and/or 
#' "sunzenith" which will also be used as variables.
#' see \code{\link{geometryVariables}} and \code{\link{borgIndices}} 
#' for description of the variables.

#' @param date Date of the msg scene in format yyyymmddhhmm. Only imprtant if 
#' the day of the year (jday) is calculated (see param "further").
#' @examples
#' #list msg raster in the example folder:
#' scenes<-list.files(system.file("msg",package="Rainfall"),
#' pattern=".rst$")
#' 
#' # name the variables (according file name convention) 
#' #which are to be used:
#' x=c("ca02p0001","ca02p0002","ca02p0003","ct01dk004","ct01dk005",
#' "ct01dk006","ct01dk007","ct01dk008","ct01dk009","ct01dk010","ct01dk011")
#' 
#' # raster the sunzenith raster which is defined as "ma11" according to 
#' #file name conventions:
#' sunzenith<-raster(system.file("msg",
#' scenes[substr(scenes,20,23) =="ma11"],package="Rainfall"))
#'  
#' # stack the msg scenes:
#' msg_example <-  stack(system.file("msg",
#' scenes[substr(scenes,20,28)%in%x],package="Rainfall"))
#' 
#' date <- substr(scenes[1],1,12)
#' 
#' # set non clouded areas to NA:
#' msg_example=reclassify(msg_example, cbind(-99,NA))
#' 
#' # name the msg channels:
#' names(msg_example)<-c("VIS0.6","VIS0.8","NIR1.6","IR3.9",
#' "WV6.2","WV7.3","IR8.7","IR9.7","IR10.8","IR12.0","IR13.4")
#' 
#' #calculate variables (takes some time...)
#' pred <- calculatePredictors(msg_example,
#' sunzenith,
#' spectral=c("VIS0.6","VIS0.8","NIR1.6","T0.6_1.6","T6.2_10.8"),
#' texture=expand.grid(c("NIR1.6","T6.2_10.8"),
#' c("variance", "contrast")),
#' pptext=expand.grid("T3.9_10.8",c("variance","mean")),
#' shape=c("Ar","CAI","SI","CI1"),
#' zonstat=data.frame("spec"=c("VIS0.8","VIS0.8","T6.2_10.8"),
#' "var"=c("mins","sds","maxs")),
#' date=date)
#' print(pred)
#' 


calculatePredictors<-function (scenerasters,
                               spectral,
                               sunzenith=NULL,
                               texture=NULL,
                               pptext=NULL,
                               zonstat=NULL,
                               shape=NULL,
                               further=c("sunzenith","jday"),
                               date){
  require(raster)
  if (class(texture[,1])=="factor") texture[,1] <- as.character(texture[,1])
  if (class(texture[,2])=="factor") texture[,2]<- as.character(texture[,2])
  if (class(pptext[,1])=="factor") pptext[,1]<- as.character(pptext[,1])
  if (class(pptext[,2])=="factor") pptext[,2]<- as.character(pptext[,2])
  if (class(zonstat[,1])=="factor") zonstat[,1]<- as.character(zonstat[,1])
  if (class(zonstat[,2])=="factor") zonstat[,2]<- as.character(zonstat[,2])
  
  names<-unique(c(spectral,texture[,1],pptext[,1]))
  spectralvars <- spectralDerivate (scenerasters, names)
  if (nlayers(spectralvars)>0){
    spectralvars<-stack(scenerasters,spectralvars)
  } else {
    spectralvars<-scenerasters
  }
  ### Texture parameters #######################################################
  if (!is.null(texture)){
    glcm_varunique<-unique(texture[,1]) #first column=spectral var,second =texture
    glcm_input <-lapply(glcm_varunique,FUN=function(x)texture[,2][texture[,1]==x])
    names(glcm_input)<-glcm_varunique
    
    glcm_filter<-list()
    for (i in 1: length (glcm_input)){
      glcm_filter[[i]] <- textureVariables (x=spectralvars[[names(glcm_input)[i]]],
                                            n_grey = 32,    
                                            var=as.character(glcm_input[[i]]),filter=3)
      names(glcm_filter[[i]]$size_3)<-paste0("f3_",names(glcm_input)[i],"_",
                                             names(glcm_filter[[i]]$size_3))
    }
  }
  
  ### Geometry parameters ######################################################
  if (!is.null(shape)||!is.null(pptext)||!is.null(zonstat)){
    shape<-c("cloudPatches",shape)
    cloud_geometry <- geometryVariables (x=scenerasters[[4]],var=shape)
  }
  ### Texture per Patch ########################################################
  if (!is.null(pptext)){
    pp_glcm_varunique<-unique(pptext[,1]) 
    pp_glcm_input <-lapply(pp_glcm_varunique,
                           FUN=function(x)pptext[,2][pptext[,1]==x])
    names(  pp_glcm_input)<-pp_glcm_varunique
    
    pp_glcmPatches<-list()
    glcmPerPatchRaster<-list()
    for (i in 1: length (pp_glcm_input)){
      pp_glcmPatches[[i]]<-glcmPerPatch(x=spectralvars[[names(pp_glcm_input)[i]]],
                                        patches=cloud_geometry$cloudPatches, 
                                        var=as.character(pp_glcm_input[[i]]))
      
    }
  }
  ### zonal stat: Mean,sd,min,max per Pacth ####################################
  if (!is.null(zonstat)){
    zstat_varunique<-unique(zonstat[,1])
    zstat_input <-lapply(zstat_varunique,
                         FUN=function(x)zonstat[,2][zonstat[,1]==x])
    names(zstat_input)<-zstat_varunique
    zstatPatches<-list()
    zonalstat<-list()
    for (i in 1: length (zstat_input)){
      zonalstat[[i]] <-ppStat( spectralvars[[names(zstat_input)[i]]],
                               cloud_geometry$cloudPatches, 
                               var=zstat_input[[i]]) 
      names(zonalstat)[[i]]<-names(spectralvars[[names(zstat_input)[i]]])
    }
  }
  ### further vars ######################################
  if (!is.null(further)){
    namesF<-c()
    furtherVar<-stack()
    if ("jday" %in% further){
      dayOfYear<-scenerasters[[1]]
      values(dayOfYear)<-rep(strptime(date, "%Y%m%d")$yday+1,ncell(dayOfYear))
      furtherVar <- stack(furtherVar, dayOfYear)
      namesF<-c(namesF,"jday")
    }
    if ("sunzenith" %in% further){
      furtherVar<-stack(furtherVar,sunzenith)
      namesF<-c(namesF,"sunzenith")
    }
    names(furtherVar)<-namesF
  }
  
  
  ##############################################################################
  ###             Compile data table
  ##############################################################################
  
  
  result<-stack( spectralvars [[spectral]])
  if (!is.null(texture)) result <- stack (result,stack(unlist(glcm_filter)))
  if(!is.null(shape)||!is.null(pptext)||!is.null(zonstat)){
    if(nlayers(cloud_geometry>1)) result <- stack (result,cloud_geometry[[-1]])
  }
  if (!is.null(pptext)) {result <- stack (result,stack(unlist(
    pp_glcmPatches)))
  }
  if (!is.null(zonstat)) result <- stack (result,stack(unlist(zonalstat)))
  if(!is.null(further)) result <- stack (result,furtherVar)
  
  return(result)
  
}