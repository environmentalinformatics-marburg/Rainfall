#' Get the date from a msg scene's inputpath
#' @param inpath The path to the msg channels including sunzenith of one scene
#' @param type A character string indicating the file type. 
#' Can be ignored if fromFolder==TRUE
#' @param fromFolder If FALSE (default) then the date is read from a single scene.
#' If TRUE then the date is read from a list of folders named according the output
#' of the MSG processing. eg "MT9P201001011515_mt09s"
#' @return The date as character
#' @author Hanna Meyer
#' @examples
#' date <-getDate(inpath=system.file("extdata/msg",package="Rainfall"))


getDate <- function (inpath,type="rst", fromFolder=FALSE){
  if (!fromFolder){
    scenes<-list.files(inpath, pattern=paste0(".",type,"$"))
    date <- substr(scenes[1],1,12)
  }
  if (fromFolder){
    scenes <- list.files(inpath)
    date <- substr(scenes,5,16)
  }
  return(date)
}