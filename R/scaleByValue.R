#' Scales Predictors using mean and sd from lookup table
#' 
#' @param x RasterStack of the scene which should be scaled or data.frame
#' @return scaleTable Data.frame created with \code{\link{calcScalingStats}}
#' @author Hanna Meyer
#' @export scaleByValue


scaleByValue <- function (x, scaleTable){
  if(class(x)=="RasterStack"||class(x)=="RasterLayer"||class(x)=="RasterBrick"){
    
    for (i in 1:nlayers(x)){
      rowID <- which(row.names(scaleTable)==names(x)[i])
      if(nlayers(x)==1){
        x <-  (x-scaleTable$mean[rowID])/scaleTable$sd[rowID]
      }else{
      x[[i]] <-  (x[[i]]-scaleTable$mean[rowID])/scaleTable$sd[rowID]
      }
    }
  } else{
    for (i in 1:ncol(x)){
      rowID <- which(row.names(scaleTable)==colnames(x)[i])
      x[,i] <-  (x[,i]-scaleTable$mean[rowID])/scaleTable$sd[rowID]
    }
  }
  
  
  return(x)
}