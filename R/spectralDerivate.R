spectralDerivate <- function (x, names){
  require (raster)
  spectralvars<-stack()
  varnames=c()
  if ("T0.6_1.6" %in% names){
    spectralvars <- stack (spectralvars, x$VIS0.6-x$NIR1.6)
    varnames <- c(varnames,"T0.6_1.6")
  }
  if ("T6.2_10.8"  %in% names){
    spectralvars <- stack(spectralvars,x$WV6.2-x$IR10.8)
    varnames <- c(varnames,"T6.2_10.8")
  }
  if ("T7.3_12.0"  %in% names){
    spectralvars <- stack(spectralvars,x$WV7.3-x$IR12.0)
    varnames <- c(varnames,"T7.3_12.0")
  }
  if ("T8.7_10.8" %in% names){
    spectralvars <- stack(spectralvars,x$IR8.7-x$IR10.8)
    varnames <- c(varnames,"T8.7_10.8")
  }
  if ("T10.8_12.0"  %in% names){
    spectralvars <- stack(spectralvars,x$IR10.8-x$IR12.0)
    varnames <- c(varnames,"T10.8_12.0")
  }
  if ("T3.9_7.3" %in% names){
    spectralvars <- stack(spectralvars,x$IR3.9-x$WV7.3)
    varnames <- c(varnames,"T3.9_7.3")
  }
  if ("T3.9_10.8" %in% names){
    spectralvars <- stack(spectralvars,x$IR3.9-x$IR10.8)
    varnames <- c(varnames,"T3.9_10.8")
  }
  names(spectralvars) <- varnames
  return (spectralvars)
}
  