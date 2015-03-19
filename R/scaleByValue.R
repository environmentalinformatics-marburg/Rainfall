#' Scales Predictors using mean and sd from lookup table
#' 
#' @param scenerasters RasterStack of the scene which should be scaled
#' @return scaleTable Data.frame created with \code{\link{calcScalingStats}}
#' @author Hanna Meyer


scaleByValue <- function (scenerasters, scaleTable){
  for (i in 1:nlayers(scenerasters)){
    rowID <- which(row.names(tmp)==names(scenerasters)[i])
    scenerasters[[i]] <-  (scenerasters[[i]]-tmp$means[rowID])/tmp$sd[rowID]
  }
  return(scenerasters)
}