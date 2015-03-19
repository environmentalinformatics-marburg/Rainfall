#' Returns mean and sd values of the predictors which can be used for scaling 
#' predictors used for training
#' 
#' @param x Data.frame or rasterStack of the predictors
#' @return Data.frame with mean and sd values
#' @author Hanna Meyer
#' @examples
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' scaleparam <- calcScalingStats(msg_example)


calcScalingStats <- function (x){
  
  if (class(x)=="RasterStack"||class(x)=="RasterBrick"){
    x <- as.data.frame(x)
  }
  
  means <- colMeans(x,na.rm = TRUE)
  sds <- apply(x,2,sd,na.rm=TRUE)
  
  result <- data.frame(means,sds)
  
  if ("jday"%in%row.names(result)){
    result[which(row.names(result)=="jday"),] <- c(mean(1:365),sd(1:365)) 
  }
  if ("sunzenith"%in%row.names(result)){
    result[which(row.names(result)=="sunzenith"),] <- c(mean(0:360),sd(0:360)) 
  }
  return(result)
  
}