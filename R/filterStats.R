#' Calculate statistics of spatial filters of MSG bands
#' @param scenerasters A raster stack of cloud masked MSG scenes with non 
#' clouded areas were set to NA

filterStats<-function(scenerasters,
                      size=3,
                      var=c("min","mean","max","sd"),
                      na.rm=TRUE){
  require(raster)
  require(doParallel)
  registerDoParallel(detectCores())
  result=list()
  for (i in 1:length(var)){
    result[[i]]<-foreach(k=1:nlayers(scenerasters),
                    .packages= c("raster"))%dopar%{
                      tmp<-focal(scenerasters[[k]], 
                                 w=matrix(1,nrow=size,ncol=size), 
                                 fun=eval(parse(text=var[i])),na.rm=na.rm)
                      names(tmp)<-paste0("f",size,"_",names(scenerasters[[k]]),
                                         "_",var[i])
                      return(tmp)
                    } 
  }
  result<-stack(unlist(result))
  if(na.rm){
    result <- mask(result,scenerasters)
  }
  return(result)
}