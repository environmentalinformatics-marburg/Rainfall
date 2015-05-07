#' Calculate selected Texture parameters from clouds based on spectral properties
#' 
#' @param x A rasterLayer or a rasterStack containing different channels
#' where clouds are already masked
#' @param nrasters A vector of channels to use from x. Default =nlayers(x)
#' @param filter A vector of numbers indicating the environment sizes for which 
#' the textures are calculated
#' @param var A string vector of parameters to be calculated.
#'  see \code{\link{glcm}}
#' @param n_grey Number of grey values. see \code{\link{glcm}}
#' @param parallel A logical value indicating whether parameters are calculated 
#' parallely or not
#' @param min_x for each channel the minimum value which can occur. If NULL then
#' the minimum value from the rasterLayer is used.
#' @param max_x for each channel the maximum value which can occur. If NULL then
#' the maximum value from the rasterLayer is used.
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @details This functions fills the glcm function with standard settings used 
#' for the rainfall retrieval
#' @author Hanna Meyer
#' @export textureVariables
#' @examples 
#' ## example on how to calculate texture from a list of msg channels
#' 
#' #'# stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#'  
#' #calculate texture
#' result <- textureVariables(msg_example,nrasters=1:3,
#' var=c("mean", "variance", "homogeneity"))
#' 
#' #plot the results from VIS0.6 channel:
#' plot(result$size_3$VIS0.6)

#' @seealso \code{\link{glcm}}

textureVariables <- function(x,
                             nrasters=1:nlayers(x),
                             filter=c(3),
                             var=c("mean", "variance", "homogeneity", 
                                   "contrast", "dissimilarity", 
                                   "entropy","second_moment"),
                             parallel=TRUE,
                             n_grey = 128,
                             min_x=NULL,
                             max_x=NULL){
  require(glcm) 
  require(raster)
  if (parallel){
    require(doParallel)
    registerDoParallel(detectCores())
  }
  
  
  #set values larger than the max/min value to the max/minvalue. 
  #Otherwise NA would be used
  if(!is.null(min_x)){
    if (length(nrasters)>1){
    for (i in nrasters){
      x[[i]]=reclassify(x[[i]], c(max_x[i],Inf,max_x[i]))
      x[[i]]=reclassify(x[[i]], c(-Inf,min_x[i],min_x[i]))
    }
    } else
    {
      #x=reclassify(x, c(max_x[i],Inf,max_x[i]))
      #x=reclassify(x, c(-Inf,min_x[i],min_x[i])) 
      x=reclassify(x, c(max_x,Inf,max_x))
      x=reclassify(x, c(-Inf,min_x,min_x)) 
    }
  }
  
  
  glcm_filter<-list()
  for (j in 1:length(filter)){
    if (class (x)=="RasterStack"||class (x)=="RasterBrick"){  
      if (parallel){
        glcm_filter[[j]]<-foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%dopar%{
                                    mask(glcm(x[[i]], 
                                              window = c(filter[j], filter[j]), 
                                              shift=list(c(0,1), c(1,1), c(1,0), 
                                                         c(1,-1)),
                                              statistics=var,n_grey=n_grey,
                                              min_x=min_x[i],max_x=max_x[i],
                                              na_opt="center"), x[[i]])
                                  } 
      } else {
        glcm_filter[[j]]<-foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%do%{
                                    mask(glcm(x[[i]], 
                                              window = c(filter[j], filter[j]), 
                                              shift=list(c(0,1), c(1,1), c(1,0), 
                                                         c(1,-1)),
                                              statistics=var,n_grey=n_grey,
                                              min_x=min_x[i],max_x=max_x[i],
                                              na_opt="center"), x[[i]])
                                  }
      }
      names(glcm_filter[[j]])<-names(x)[nrasters]
    } else {
      glcm_filter[[j]]<-mask(glcm(x, window = c(filter[j], filter[j]), 
                                  shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                                  statistics=var,n_grey=n_grey,
                                  min_x=min_x,max_x=max_x,
                                  na_opt="center"), x)
    }   
  }
  names(glcm_filter)<-paste0("size_",filter)
  return(glcm_filter)
}




