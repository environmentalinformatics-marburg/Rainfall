#' Calculate predictors from MSG cloud masked data
#' @param scenerasters A raster stack of cloud masked MSG scenes with non 
#' clouded areas were set to NA
#' @param model A rfe object Optional. Can be used instad of the spectral,texture,
#' pptext,zonstat,shape parameters. Variables included in the optimal model are the
#' calculated.
#' @param useOptimal if is.null(model): Logical. Use the optimal variables from rfe or those
#' less variables which lead to a model performance within one sd of the 
#' optimal model?
#' @param sunzenith A raster of the sun zenith values
#' @param spectral A character vector indicating the msg channels to be included.
#' Possible values: "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
#' "IR9.7","IR10.8","IR12.0","IR13.4","T0.6_1.6","T6.2_10.8","T7.3_12.0",
#' "T8.7_10.8","T10.8_12.0", "T3.9_7.3","T3.9_10.8"
#' @param texture data frame of all spectral and texture combinations ("mean", "variance", "homogeneity", 
#' "contrast", "dissimilarity", "entropy","second_moment") and filter sizes 
#' which are to be calculated. (Tip: Use expand.grid to create this data.frame)
#' @param pptext data frame of all spectral and texture combinations which are
#' to be calculated for teh overall cloud entity.
#'  (Tip: Use expand.grid to create this data.frame)
#' @param zonstat data frame of all spectral and zonal stat 
#' ("mean","min","max","sd") combinations
#'  (min,max,mean or sd) which are to be calculated for the overall cloud entity.
#'  (Tip: Use expand.grid to create this data.frame)
#' @param shape geoemtry variables which should be included. Possible values
#' are "Ar",SI","CA","Ur","CAI","PAR","distEdges","Re","Ru","OIC",
#' CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
#' "TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3"
#' @param filterstat data.frame of all spectral and "mean","min","max","sd" and
#' filter size combinations
#' @param further a character vector including Currently "jday" and/or 
#' "sunzenith" which will also be used as variables.
#' see \code{\link{geometryVariables}} and \code{\link{borgIndices}} 
#' for description of the variables.
#' @param date Date of the msg scene in format yyyymmddhhmm. Only imprtant if 
#' the day of the year (jday) is calculated (see param "further").
#' @param x_min see \code{\link{textureVariables}} 
#' @param x_max see \code{\link{textureVariables}} 
#' @author Hanna Meyer
#' @export calculatePredictors
#' @examples
#' ############################################################################
#' #Example 1: Predictors from predictor list
#' ############################################################################ 
#' # stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' # raster the sunzenith 
#' sunzenith<-getSunzenith(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' #get Date
#' date <- getDate(system.file("extdata/msg",package="Rainfall"))
#' 
#' #calculate variables (takes some time...)
#' pred <- calculatePredictors(msg_example,
#' sunzenith=sunzenith,
#' spectral=c("VIS0.6","VIS0.8","NIR1.6","T0.6_1.6","T6.2_10.8"),
#' texture=expand.grid(c("NIR1.6","T6.2_10.8"),
#' c("variance", "contrast"),c(3,9)),
#' pptext=expand.grid("T3.9_10.8",c("variance","mean")),
#' shape=c("Ar","CAI","SI","CI1"),
#' filterstat=expand.grid(c("VIS0.6","T6.2_10.8"),
#' c("min", "max"),c(3,9)),
#' zonstat=data.frame("spec"=c("VIS0.8","VIS0.8","T6.2_10.8"),
#' "var"=c("min","sd","max")),
#' date=date)
#' print(pred)
#' ############################################################################
#' #Example 2:calculate predictors from an rfe model
#' ############################################################################
#' #'  # stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' data(rfeModel)
#' pred<-calculatePredictors(msg_example,model=rfeModel,date=NULL,sunzenith=NULL)
#' 


calculatePredictors<-function (scenerasters,
                               model=NULL,
                               useOptimal=TRUE,
                               spectral=NULL,
                               sunzenith=NULL,
                               texture=NULL,
                               pptext=NULL,
                               zonstat=NULL,
                               filterstat=NULL,
                               shape=NULL,
                               x_min=NULL,
                               y_min=NULL,
                               further=c("sunzenith","jday"),
                               date){
  
  require(raster)
  require(doParallel)
  registerDoParallel(detectCores())
  
  if(!is.null(model)){
    vars<-varFromRfe(model, useOptimal=useOptimal)
    spectral<-vars$spectral
    texture<-vars$texture
    filterstat<-vars$filterstat
    pptext<-vars$pptext
    zonstat<-vars$zonstat
    shape<-vars$shape
    further<-vars$further
    if ("sunzenith"%in%further&is.null(sunzenith)){
      stop ("please provide sunzenith raster")
    }
    if ("jday"%in%further&is.null(date)){
      stop ("please provide date information")
    }
    
  }
  
  if (ncol(texture)==2) texture=cbind(texture,rep(3,nrow(texture)))
  if (class(texture[,1])=="factor") texture[,1] <- as.character(texture[,1])
  if (class(texture[,2])=="factor") texture[,2]<- as.character(texture[,2])
  if (class(texture[,3])=="factor") texture[,3]<- as.character(texture[,3])
  if (class(filterstat[,1])=="factor") filterstat[,1] <- as.character(filterstat[,1])
  if (class(filterstat[,2])=="factor") filterstat[,2]<- as.character(filterstat[,2])
  if (class(filterstat[,3])=="factor") filterstat[,3]<- as.character(filterstat[,3])
  if (class(pptext[,1])=="factor") pptext[,1]<- as.character(pptext[,1])
  if (class(pptext[,2])=="factor") pptext[,2]<- as.character(pptext[,2])
  if (class(zonstat[,1])=="factor") zonstat[,1]<- as.character(zonstat[,1])
  if (class(zonstat[,2])=="factor") zonstat[,2]<- as.character(zonstat[,2])
  
  names<-unique(c(spectral,texture[,1],pptext[,1],zonstat[,1],filterstat[,1]))
  spectralvars <- spectralDerivate (scenerasters, names)
  if (nlayers(spectralvars)>0){
    spectralvars<-stack(scenerasters,spectralvars)
  } else {
    spectralvars<-scenerasters
  }
  ### Texture parameters #######################################################
  if (!is.null(texture)){
    glcm_filterunique<-unique(texture[,3])
    
    glcm_input <-lapply(glcm_filterunique,FUN=function(x)texture[,1][texture[,3]==x])
    names(glcm_input)<-paste0("glcm", glcm_filterunique)
    
    for (k in 1:length(glcm_input)){
      glcm_varunique<-unique(texture[,1][texture[,3]==glcm_filterunique[k]]) #first column=spectral var,second =texture
      
      glcm_input[[k]] <-lapply(glcm_varunique,FUN=function(x)texture[,2][texture[,1]==x&texture[,3]==glcm_filterunique[k]])
      names(glcm_input[[k]])<-glcm_varunique
    }
    
    glcm_filter<-list()
    
    for (k in 1:length(glcm_input)){
      glcm_filter[[k]]<-foreach(i=1:length (glcm_input[[k]]),
                                .packages= c("glcm","raster","doParallel","Rainfall"))%dopar%{
                                  tmp<-textureVariables (x=spectralvars[[names(glcm_input[[k]])[i]]],   
                                                         var=as.character(glcm_input[[k]][[i]]),
                                                         filter=as.numeric(glcm_filterunique[k]),
                                                         min_x=min_x[names(glcm_input[[k]])[i]],
                                                         max_x=max_x[names(glcm_input[[k]])[i]])
                                  eval(parse(text=paste0("names(tmp$size_",glcm_filterunique[k],")=c(",
                                                         paste0("'",names(glcm_input)[[k]],"_", 
                                                                names(spectralvars[[names(glcm_input[[k]])[i]]]),
                                                                "_",as.character(glcm_input[[k]][[i]]),collapse=",",sep="'"),")")))
                                  return(tmp)
                                }  
    }
  }
  
  
  ### Filter ###################################################################
  
  if (!is.null(filterstat)){
    filterunique<-unique(filterstat[,3])
    
    filter_input <-lapply(filterunique,FUN=function(x)filterstat[,1][filterstat[,3]==x])
    names(filter_input)<-paste0("filter", filterunique)
    
    for (k in 1:length(filter_input)){
      filter_varunique<-unique(filterstat[,1][filterstat[,3]==filterunique[k]]) #first column=spectral var,second =filterstat
      
      filter_input[[k]] <-lapply(filter_varunique,FUN=function(x)filterstat[,2][filterstat[,1]==x&filterstat[,3]==filterunique[k]])
      names(filter_input[[k]])<-filter_varunique
    }
    
    spatial_filter<-list()
    
    for (k in 1:length(filter_input)){
      spatial_filter[[k]]<-foreach(i=1:length (filter_input[[k]]),
                                   .packages= c("raster","doParallel","Rainfall"))%dopar%{
                                     tmp<-filterStats (scenerasters=spectralvars[[names(filter_input[[k]])[i]]],
                                                       var=as.character(filter_input[[k]][[i]]),size=as.numeric(filterunique[k]))
                                     
                                     return(tmp)
                                   }  
    }
  }
  
  
  ### Geometry parameters ######################################################
  if (!is.null(shape)||!is.null(pptext)||!is.null(zonstat)){
    shape<-c("cloudPatches",shape)
    cloud_geometry <- geometryVariables (sceneraster=scenerasters[[1]],var=shape)
  }
  ### Texture per Patch ########################################################
  if (!is.null(pptext)){
    pp_glcm_varunique<-unique(pptext[,1]) 
    pp_glcm_input <-lapply(pp_glcm_varunique,
                           FUN=function(x)pptext[,2][pptext[,1]==x])
    names(  pp_glcm_input)<-pp_glcm_varunique
    
    pp_glcmPatches<-list()
    glcmPerPatchRaster<-list()
    
    pp_glcmPatches<-foreach(i=1:length(pp_glcm_input),
                            .packages= c("glcm","raster","doParallel",
                                         "Rainfall"))%dopar%{
                                           glcmPerPatch(x=spectralvars[[names(
                                             pp_glcm_input)[i]]],
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
    
    zonalstat<-foreach(i=1: length (zstat_input),
                       .packages= c("raster","doParallel",
                                    "Rainfall"))%dopar%{
                                      ppStat( spectralvars[[names(zstat_input)[i]]],
                                              cloud_geometry$cloudPatches, 
                                              var=zstat_input[[i]]) 
                                    }
    names(zonalstat)<-names(spectralvars[[names(zstat_input)]])
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
  
  if(!is.null(spectral)){
    result<-stack( spectralvars [[spectral]])
  } else result <- stack()
  if (!is.null(texture)) {
    if(!is.null(model)){
      tmp2 <- unlist(lapply(unlist(glcm_filter),names))
      glcm_filter<-stack(unlist(glcm_filter))
      names(glcm_filter)<-tmp2
    }
    result <- stack (result,stack(unlist(glcm_filter)))
  }
  if(!is.null(shape)||!is.null(pptext)||!is.null(zonstat)){
    if(nlayers(cloud_geometry>1)) result <- stack (result,cloud_geometry[[-1]])
  }
  if (!is.null(pptext)) {result <- stack (result,stack(unlist(
    pp_glcmPatches)))
  }
  if (!is.null(filterstat)) {result <- stack (result,stack(unlist(
    spatial_filter)))
  }
  if (!is.null(zonstat)) result <- stack (result,stack(unlist(zonalstat)))
  if(!is.null(further)) result <- stack (result,furtherVar)
  
  
  return(result)
  
}