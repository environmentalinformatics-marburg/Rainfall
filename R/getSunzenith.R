#' Get sunzenith raster from a msg scene's inputpath
#' @param inpath The path to the msg channels including sunzenith of one scene
#' @param type A character string indicating the file type
#' @return The sunzenith raster
#' @author Hanna Meyer
#' @export getSunzenith
#' @examples
#' sunzenith <-getSunzenith(inpath=system.file("extdata/msg",package="Rainfall"))


getSunzenith <- function(inpath,type="rst"){
  scenes<-list.files(inpath, pattern=paste0(".",type,"$"))
  result<-raster(paste0(inpath,"/",scenes[substr(scenes,20,23) =="ma11"]))
  names(result)<-"sunzenith"
  return(result)
}