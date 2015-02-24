#' Calculate selected Texture parameters for overall cloud entities
#'  based on spectral properties
#'  @param x 
#'  @return a raster stack of the texture parameters for each entity
#'  @seealso \code{\link{calculateTexture}}, \code{\link{glcm}}
#'  @examples
#'  # stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' #calculate cloud patches
#' geometry <- geometryVariables(msg_example[[1]],var="cloudPatches")
#' 
#' #calculate glcm per patch
#' glcmPerPatch(msg_example[[1]],geometry$cloudPatches,var=c("mean"))
#'  
glcmPerPatch <- function (x,patches,nrasters=1:nlayers(x),
                          var=c("mean", "variance", "homogeneity", 
                                "contrast", "dissimilarity", 
                                "entropy","second_moment"),n_grey=32){

  require(doParallel)
  registerDoParallel(detectCores())
  results<-c()
  library(glcm)
  if (dim(x)[[1]]%%2==0){
    dimx<-dim(x)[[1]]-1
  }
  if (dim(x)[[2]]%%2==0){
    dimy<-dim(x)[[2]]-1
  }
  for (i in 1:max(values(patches),na.rm=TRUE)){
    xperPatch<-x
    values(xperPatch)[values(patches)!=i]<-NA
    if  (sum(!is.na(values(xperPatch[[1]])))<9) {next}
    if (class(x)=="RasterStack"||class(x)=="RasterBrick"){
      xperPatch[170/2,250/2]<-apply(values(xperPatch),2,"mean",na.rm=TRUE)
    }
    if (class(x)=="RasterLayer"){
      xperPatch[170/2,250/2]<-mean(values(xperPatch),na.rm=TRUE)
    }
    glcm_filter<-foreach(k=nrasters,.packages= c("glcm","raster"))%dopar%{
      glcm(xperPatch[[k]], window = c(dimx, dimy),
           shift=list(c(0,1), c(1,1), c(1,0)),statistics=var,n_grey=n_grey,
           na_opt="ignore")
    }
    resultsNew<-as.data.frame(cbind(i,as.data.frame(lapply(
      glcm_filter,function(x){values(x)[!is.na(values(x))]}))))
    names(resultsNew)<-c("PatchID",names(x))
    resultsNew<-unlist(resultsNew)
    names(resultsNew)<-c(rep("ID",length(var)),
                         paste0(expand.grid(var,names(x))[,1],
                                "_",expand.grid(var,names(x))[,2]))
    if (length(var)>1){
      resultsNew<-resultsNew[-c(2:length(var))]
    }
    results<-rbind(results,resultsNew)
    
  }
  
  glcmPerPatchRaster<-foreach(k=2:ncol(results),.combine=stack,
                              .packages=c("raster","doParallel"))%dopar%{
                                reclassify(patches,matrix(c(
                                  results[,1],results[,k]),ncol=2))}
  reclasstable<-cbind(1:max(values(patches),na.rm=TRUE),
                      1:max(values(patches),
                            na.rm=TRUE)%in%results
                      [,1])
  reclasstable[reclasstable[,2]==0,2]<-NA
  reclasstable[!is.na(reclasstable[,2]),2]<-reclasstable[!is.na(
    reclasstable[,2]),1]
  reclasstable<-reclasstable[is.na(reclasstable[,2]),]
  if (nrow(results)==1){
    glcmPerPatchRaster<-reclassify(glcmPerPatchRaster,matrix(reclasstable,
                                                             ncol=2))
  } else{
    glcmPerPatchRaster<-reclassify(glcmPerPatchRaster,reclasstable)
  }
  names(glcmPerPatchRaster)<-colnames(results)[-1]
  names(glcmPerPatchRaster)<-paste0("pp_",names(glcmPerPatchRaster))
  
  return(glcmPerPatchRaster)
}


