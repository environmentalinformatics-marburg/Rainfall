#' Get channels from a msg scene's inputpath
#' @param inpath The path to the msg channels of one scene
#' @param type A character string indicating the file type
#' @param channels A character vector of the channels to return
#' @param mask A numeric number which is to bet set to NA 
#' because there are no clouds. 
#' @return A Raster stack of the MSG channels
#' @author Hanna Meyer
#' @export getChannels
#' @examples
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"),
#' channels=c("VIS0.6","IR12.0"))

getChannels <- function (inpath, type="rst",channels= c(
  "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
  "IR9.7","IR10.8","IR12.0","IR13.4"), mask=-99){
  require(raster)
  lut<-cbind(c("ca02p0001","ca02p0002","ca02p0003","ct01dk004","ct01dk005",
               "ct01dk006","ct01dk007","ct01dk008","ct01dk009","ct01dk010",
               "ct01dk011"),c(
             "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
             "IR9.7","IR10.8","IR12.0","IR13.4"))
  if (any(!channels%in%lut[,2])){
    stop ("channel is not supported")
  }
  
  x<-lut[,1][lut[,2]%in%channels]
  
  scenes<-list.files(inpath, pattern=paste0(".",type,"$"))

  scenes <- scenes[substr(scenes, 30,34)=="na001"]
  
  if (sum(substr(scenes,20,28)%in%x)<length(channels)){
    stop ("Warning: not all channels could be loaded. 
           check if they exist in the inpath")
  }
  result <-  stack(paste0(inpath,"/",scenes[substr(scenes,20,28)%in%x]))
  # set non clouded areas to NA:
  if (!is.null(mask)){
    result <- reclassify(result, cbind(mask,NA))
  }
  names(result) <- lut[,2][lut[,1]%in%substr(scenes[substr(scenes,20,28)%in%x],20,28)]

  return(result)
}