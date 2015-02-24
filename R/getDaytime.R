#' Classify a scene into day, twilight, night according to its sunzenith 
#' @param sunzenith RasterLayer of sunzenith
#' @return Character with value either "day", "twilight" or "night
#' @examples
#' sunzenith <-getSunzenith(inpath=system.file("extdata/msg",package="Rainfall"))
#' GetDaytime(sunzenith)
getDaytime<-function(sunzenith){
  meanzenith=mean(values(sunzenith))
  if (meanzenith<70) time = "day"
  if (meanzenith>=70&meanzenith<=108) time="twilight" 
  if (meanzenith>108) time="night"
  return(time)
}