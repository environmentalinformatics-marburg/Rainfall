#' Scales Predictors using mean and sd from lookup table
#' 
#' @param scenerasters RasterStack of the scene which should be scaled
#' @return scaleTable Data.frame created with \code{\link{calcScalingStats}}
#' @author Hanna Meyer
#' @export scaleByValue


scaleByValue <- function (scenerasters, scaleTable){
  for (i in 1:nlayers(scenerasters)){
    rowID <- which(row.names(tmp)==names(scenerasters)[i])
    scenerasters[[i]] <-  (scenerasters[[i]]-tmp$mean[rowID])/tmp$sd[rowID]
  }
  return(scenerasters)
}