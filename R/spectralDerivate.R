
#' Calculation of derivated spectral channels
#' @param scenerasters A RasterStack of the MSG channels
#' @param names Variables to be calculated. Currently 
#' "T0.6_1.6","T6.2_10.8","T7.3_12.0","T8.7_10.8","T10.8_12.0",
#' "T3.9_7.3","T3.9_10.8" are supported.
#' @author Hanna Meyer
#' @examples 
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' spectralDerivate(msg_example)
#' 
spectralDerivate <- function (scenerasters, names=c("T0.6_1.6","T6.2_10.8","T7.3_12.0",
                                         "T8.7_10.8","T10.8_12.0","T3.9_7.3",
                                         "T3.9_10.8")){
  require (raster)
  spectralvars<-stack()
  varnames=c()
  if ("T0.6_1.6" %in% names){
    spectralvars <- stack (spectralvars, scenerasters$VIS0.6-scenerasters$NIR1.6)
    varnames <- c(varnames,"T0.6_1.6")
  }
  if ("T6.2_10.8"  %in% names){
    spectralvars <- stack(spectralvars,scenerasters$WV6.2-scenerasters$IR10.8)
    varnames <- c(varnames,"T6.2_10.8")
  }
  if ("T7.3_12.0"  %in% names){
    spectralvars <- stack(spectralvars,scenerasters$WV7.3-scenerasters$IR12.0)
    varnames <- c(varnames,"T7.3_12.0")
  }
  if ("T8.7_10.8" %in% names){
    spectralvars <- stack(spectralvars,scenerasters$IR8.7-scenerasters$IR10.8)
    varnames <- c(varnames,"T8.7_10.8")
  }
  if ("T10.8_12.0"  %in% names){
    spectralvars <- stack(spectralvars,scenerasters$IR10.8-scenerasters$IR12.0)
    varnames <- c(varnames,"T10.8_12.0")
  }
  if ("T3.9_7.3" %in% names){
    spectralvars <- stack(spectralvars,scenerasters$IR3.9-scenerasters$WV7.3)
    varnames <- c(varnames,"T3.9_7.3")
  }
  if ("T3.9_10.8" %in% names){
    spectralvars <- stack(spectralvars,scenerasters$IR3.9-scenerasters$IR10.8)
    varnames <- c(varnames,"T3.9_10.8")
  }
  if (nlayers(spectralvars)>0){
    names(spectralvars) <- varnames
  }
  return (spectralvars)
}
  