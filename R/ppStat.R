#' Calculate zonal statistics for cloud patches
#' @param scenerasters RasterLayer or rasterStack of spectral channels
#' @param cloudPatches A Raster of cloud entities calculated 
#' by \code{\link{geometryVariables}}
#' @param var vector containing values of "mean","sd","min","max"
#' @examples 
#' msg_example <-getChannels(inpath=system.file("msg",package="Rainfall"),
#' channels="VIS0.8")
#' 
#' #calculate geometry Variables
#' geometry <- geometryVariables(msg_example,var=c("cloudPatches"))
#' 
#' ppStat(msg_example,geometry$cloudPatches,var=c("mean","sd"))

ppStat <- function (scenerasters, cloudPatches, var=c("mean","sd","min","max")){
  require(raster)
  require(doParallel)
  ZonalStats <- cloudPatches
  ###mean
  if (any(var=="mean")){
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloudPatches,fun="mean")
  means<-foreach(i=2:ncol(tmpStats),.combine=stack,
                        .packages=c("raster","doParallel"))%dopar%{
                          reclassify(ZonalStats,
                                     matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(means)<-paste0("mean_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  }
  ###sd
  if (any(var=="sd")){
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloudPatches,fun="sd")
  sds<-foreach(i=2:ncol(tmpStats),.combine=stack,
                      .packages=c("raster","doParallel"))%dopar%{
                        reclassify(ZonalStats,
                                   matrix(tmpStats[,c(1,i)],ncol=2))} 
  
  names(sds)<-paste0("sd_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  }
  ###min
  if (any(var=="min")){
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloudPatches,fun="min")
  mins<-foreach(i=2:ncol(tmpStats),.combine=stack,
                       .packages=c("raster","doParallel"))%dopar%{
                         reclassify(ZonalStats,
                                    matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(mins)<-paste0("min_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  }
  ###max
  if (any(var=="max")){
  tmpStats<-zonal(scenerasters[[1:(nlayers(scenerasters)-1)]],
                  cloudPatches,fun="max")
  maxs<-foreach(i=2:ncol(tmpStats),.combine=stack,
                       .packages=c("raster","doParallel"))%dopar%{
                         reclassify(ZonalStats,
                                    matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(maxs)<-paste0("max_",names(scenerasters)[
    1:(nlayers(scenerasters)-1)])
  }
  result <-eval(parse(text=paste0("stack(",
                                  paste0(var,"s",collapse=","),collapse="",")")))
  names (result) <- paste0(names(scenerasters),"_",var)
  return(result)
}