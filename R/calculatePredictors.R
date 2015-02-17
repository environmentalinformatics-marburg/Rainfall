#' Calculate predictors from MSG cloud masked data
#' @param scenerasters A raster stack of cloud masked MSG scenes with non 
#' clouded areas were set to NA
#' @param sunzenith A raster of the sun zenith values
#' @param variables A character vector indicating the msg channels to be included.
#' Possible values: "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
#' "IR9.7","IR10.8","IR12.0","IR13.4".
#' @param derivVariables A character vector of derivated variables
#'  to be calculated. possible values are
#' "T0.6_1.6","T6.2_10.8","T7.3_12.0","T8.7_10.8","T10.8_12.0",
#' "T3.9_7.3","T3.9_10.8","sunzenith")
#' @param xderivTexture MSG channels from which texture is to be calculated
#' @param date Date of the msg scene in format yyyymmddhhmm
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
#' pred <- calculatePredictors(msg_example,sunzenith,variables=c("VIS0.6","VIS0.8","NIR1.6"),
#' derivVariables=c("T0.6_1.6","T6.2_10.8"),
#' xderivTexture=c("VIS0.6","T0.6_1.6"),date=date)
#' print(pred)
#' 


calculatePredictors<-function (scenerasters,sunzenith,variables,derivVariables,xderivTexture,date){
  T0.6_1.6 <- scenerasters$VIS0.6-scenerasters$NIR1.6
  T6.2_10.8 <- scenerasters$WV6.2-scenerasters$IR10.8
  T7.3_12.0 <- scenerasters$WV7.3-scenerasters$IR12.0
  T8.7_10.8 <- scenerasters$IR8.7-scenerasters$IR10.8
  T10.8_12.0 <- scenerasters$IR10.8-scenerasters$IR12.0
  T3.9_7.3 <- scenerasters$IR3.9-scenerasters$WV7.3
  T3.9_10.8 <- scenerasters$IR3.9-scenerasters$IR10.8
  scenerasters<-stack(scenerasters,T0.6_1.6,T6.2_10.8,T7.3_12.0,T8.7_10.8,
                      T10.8_12.0,T3.9_7.3,T3.9_10.8,sunzenith)
  names(scenerasters)<-c("VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3",
                        "IR8.7","IR9.7","IR10.8","IR12.0","IR13.4",
                        "T0.6_1.6","T6.2_10.8","T7.3_12.0","T8.7_10.8",
                        "T10.8_12.0","T3.9_7.3","T3.9_10.8","sunzenith")
  scenerasters<-scenerasters[[c(variables,derivVariables)]]
  names(scenerasters)<- c(variables,derivVariables)
  
  ### Texture parameters #########################################################
  glcm_filter <- textureVariables (x=scenerasters[[xderivTexture]],
                                    n_grey = 32,filter=c(3),    
                                    var=c("mean", "variance", "homogeneity", 
                                          "contrast", "dissimilarity", 
                                          "entropy","second_moment"))
  names(glcm_filter$size_3)<-paste0("f3_",names(glcm_filter$size_3))
  
  ### Geometry parameters #########################################################
  cloud_geometry <- geometryVariables (x=scenerasters[[4]])
  
  ### Texture per Patch ##########################################################
  glcmPatches<-glcmPerPatch(x=scenerasters[[xderivTexture]],cloud_geometry$cloudPatches)
  glcmPerPatchRaster<-foreach(i=2:ncol(glcmPatches),.combine=stack,
                              .packages=c("raster","doParallel"))%dopar%{
                                reclassify(cloud_geometry$cloudPatches,matrix(c(
                                  glcmPatches[,1],glcmPatches[,i]),ncol=2))}
  reclasstable<-cbind(1:max(values(cloud_geometry$cloudPatches),na.rm=TRUE),
                      1:max(values(cloud_geometry$cloudPatches),
                            na.rm=TRUE)%in%glcmPatches[,1])
  reclasstable[reclasstable[,2]==0,2]=NA
  reclasstable[!is.na(reclasstable[,2]),2]<-reclasstable[!is.na(reclasstable[,2]),1]
  reclasstable<-reclasstable[is.na(reclasstable[,2]),]
  if (nrow(glcmPatches)==1){
    glcmPerPatchRaster<-reclassify(glcmPerPatchRaster,matrix(reclasstable,ncol=2))
  } else{
    glcmPerPatchRaster<-reclassify(glcmPerPatchRaster,reclasstable)
  }
  names(glcmPerPatchRaster)<-colnames(glcmPatches)[-1]
  names(glcmPerPatchRaster)<-paste0("pp_",names(glcmPerPatchRaster))
  ### zonal stat: Mean,sd,min,max per Pacth ######################################
  ZonalStats<-cloud_geometry$cloudPatches
  ###mean
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloud_geometry$cloudPatches,fun="mean")
  MeanPerPatch<-foreach(i=2:ncol(tmpStats),.combine=stack,
                        .packages=c("raster","doParallel"))%dopar%{
                          reclassify(ZonalStats,matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(MeanPerPatch)<-paste0("mean_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  ###sd
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloud_geometry$cloudPatches,fun="sd")
  SdPerPatch<-foreach(i=2:ncol(tmpStats),.combine=stack,
                      .packages=c("raster","doParallel"))%dopar%{
                        reclassify(ZonalStats,matrix(tmpStats[,c(1,i)],ncol=2))} 
  
  names(SdPerPatch)<-paste0("sd_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  ###min
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloud_geometry$cloudPatches,fun="min")
  MinPerPatch<-foreach(i=2:ncol(tmpStats),.combine=stack,
                       .packages=c("raster","doParallel"))%dopar%{
                         reclassify(ZonalStats,matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(MinPerPatch)<-paste0("min_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  ###max
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloud_geometry$cloudPatches,fun="max")
  MaxPerPatch<-foreach(i=2:ncol(tmpStats),.combine=stack,
                       .packages=c("raster","doParallel"))%dopar%{
                         reclassify(ZonalStats,matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(MaxPerPatch)<-paste0("max_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  
  ################################################################################
  ###             Compile data table
  ################################################################################
  
  dayOfYear<-scenerasters[[1]]
  values(dayOfYear)<-rep(strptime(date, "%Y%m%d")$yday+1,ncell(dayOfYear))
  
  
  result<-list(dayOfYear,scenerasters,MeanPerPatch,SdPerPatch,MinPerPatch,MaxPerPatch,
               glcmPerPatchRaster,cloud_geometry,glcm_filter)
  names(result)<-c("dayOfYear","scenerasters","MeanPerPatch","SdPerPatch","MinPerPatch","MaxPerPatch",
                   "glcmPerPatchRaster","cloud_geometry","glcm_filter")
  return(result)
  
}