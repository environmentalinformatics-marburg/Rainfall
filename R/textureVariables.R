#' Calculate selected Texture parameters from clouds based on spectral properties
#' 
#' @param x A rasterLayer or a rasterStack containing different channels
#' where clouds are already masked
#' @param nrasters A vector of channels to use from x. Default =nlayers(x)
#' @param filter A vector of numbers indicating the environment sizes for which 
#' the textures are calculated
#' @param var A string vector of parameters to be calculated. see \code{\link{glcm}}
#' @param n_grey Number of grey values. see \code{\link{glcm}}
#' @param parallel A logical value indicating whether parameters are calculated 
#' parallely or not
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @details This functions fills the glcm function with standard settings used 
#' for the rainfall retrieval
#' @author Hanna Meyer
#' @examples
#' ## example on how to calculate texture from one channel
#' 
#' msg_example <-  raster(system.file("msg",
#' "201010081250_mt09s_ct01dk009_m1hct_1000_rg01de_003000.rst",
#' package="Rainfall"))
#' 
#' # set non clouded areas to NA:
#' msg_example=reclassify(msg_example, cbind(-99,NA))
#' 
#' #calculate texture
#' result <- textureVariables(msg_example)
#' plot(result$size_3)
#' 
#' 
#' ## example on how to calculate texture from a list of msg channels
#' 
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
#' # set non clouded areas to NA:
#' msg_example=reclassify(msg_example, cbind(-99,NA))
#' 
#' # name the msg channels:
#' names(msg_example)<-c("VIS0.6","VIS0.8","NIR1.6","IR3.9",
#' "WV6.2","WV7.3","IR8.7","IR9.7","IR10.8","IR12.0","IR13.4")
#' 
#' #calculate texture
#' result <- textureVariables(msg_example,nrasters=1:3,
#' var=c("mean", "variance", "homogeneity"))
#' 
#' #plot the results from VIS0.6 channel:
#' plot(result$size_3$VIS0.6)

#' @seealso \code{\link{glcm}}

textureVariables <- function(x,nrasters=1:nlayers(x),filter=3,
                             var=c("mean", "variance", "homogeneity", 
                                   "contrast", "dissimilarity", 
                                   "entropy","second_moment")
                             ,parallel=TRUE,n_grey = 32){
  require(glcm) 
  if (parallel){
    require(doParallel)
    registerDoParallel(detectCores())
  }
  glcm_filter<-list()
  for (j in 1:length(filter)){
    if (class (x)=="RasterStack"||class (x)=="RasterBrick"){
      if (parallel){
        glcm_filter[[j]]<-foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%dopar%{
                                    glcm(x[[i]], window = c(filter[j], filter[j]), 
                                         shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                                         statistics=var,n_grey=n_grey,
                                         na_opt="ignore") 
                                  } 
      } else {
        glcm_filter[[j]]<-foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%do%{
                                    glcm(x[[i]], window = c(filter[j], filter[j]), 
                                         shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                                         statistics=var,n_grey=n_grey,
                                         na_opt="ignore") 
                                  }
      }
      names(glcm_filter[[j]])<-names(x)[nrasters]
    } else {
      glcm_filter[[j]]<-glcm(x, window = c(filter[j], filter[j]), 
                             shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                             statistics=var,n_grey=n_grey,
                             na_opt="ignore"
      ) 
    }   
  }
  names(glcm_filter)<-paste0("size_",filter)
  return(glcm_filter)
}




