#' Classify a scene into day, twilight, night according to its sunzenith 
#' @param sunzenith RasterLayer of sunzenith
#' @return Character with value either "day", "twilight" or "night
#' @author Hanna Meyer
#' @export getDaytime
#' @examples
#' sunzenith <- getSunzenith(inpath=system.file("extdata/msg",package="Rainfall"))
#' getDaytime(sunzenith)
getDaytime<-function(sunzenith){
  meanzenith <- mean(values(sunzenith),na.rm=TRUE)
  if ((meanzenith)==-99) stop ("sunzenith raster contains unvalid data") 
  if (meanzenith<70) time <- "day"
  if (meanzenith>=70&meanzenith<=108) time <- "twilight" 
  if (meanzenith>108) time <- "night"
  return(time)
}