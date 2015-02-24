#' Get the date from a msg scene's inputpath
#' @param inpath The path to the msg channels including sunzenith of one scene
#' @param type A character string indicating the file type
#' @return The date as character
#' @author Hanna Meyer
#' @examples
#' date <-getDate(inpath=system.file("extdata/msg",package="Rainfall"))


getDate <- function (inpath,type="rst"){
  scenes<-list.files(inpath, pattern=paste0(".",type,"$"))
  date <- substr(scenes[1],1,12)
  return(date)
}