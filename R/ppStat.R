#' Calculate zonal statistics for cloud patches
#' @param x RasterLayer or rasterStack

ppStat <- function (x, cloudPatches, var){
  require(raster)
  require(doParallel)
  ZonalStats <- cloudPatches
  ###mean
  if (any(var=="means")){
  tmpStats<-zonal(x[[1:(nlayers(x)-1)]],
                  cloudPatches,fun="mean")
  means<-foreach(i=2:ncol(tmpStats),.combine=stack,
                        .packages=c("raster","doParallel"))%dopar%{
                          reclassify(ZonalStats,
                                     matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(means)<-paste0("mean_",names(x)[
    1:(nlayers(x)-1)])
  }
  ###sd
  if (any(var=="sds")){
  tmpStats<-zonal(x[[1:(nlayers(x)-1)]],
                  cloudPatches,fun="sd")
  sds<-foreach(i=2:ncol(tmpStats),.combine=stack,
                      .packages=c("raster","doParallel"))%dopar%{
                        reclassify(ZonalStats,
                                   matrix(tmpStats[,c(1,i)],ncol=2))} 
  
  names(sds)<-paste0("sd_",names(x)[
    1:(nlayers(x)-1)])
  }
  ###min
  if (any(var=="mins")){
  tmpStats<-zonal(x[[1:(nlayers(x)-1)]],
                  cloudPatches,fun="min")
  mins<-foreach(i=2:ncol(tmpStats),.combine=stack,
                       .packages=c("raster","doParallel"))%dopar%{
                         reclassify(ZonalStats,
                                    matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(mins)<-paste0("min_",names(x)[
    1:(nlayers(x)-1)])
  }
  ###max
  if (any(var=="maxs")){
  tmpStats<-zonal(x[[1:(nlayers(x)-1)]],
                  cloudPatches,fun="max")
  maxs<-foreach(i=2:ncol(tmpStats),.combine=stack,
                       .packages=c("raster","doParallel"))%dopar%{
                         reclassify(ZonalStats,
                                    matrix(tmpStats[,c(1,i)],ncol=2))} 
  names(maxs)<-paste0("max_",names(x)[
    1:(nlayers(x)-1)])
  }
  result <-eval(parse(text=paste0("stack(",
                                  paste0(var,collapse=","),collapse="",")")))
  names (result) <- paste0(names(x),"_",var)
  return(result)
}