#' Create a training subset from data
#' @param predictors Either a data.frame with each column is one predictor and each
#' row represents one pixel. Or (if only one scene is used for training) 
#' a RasterStack with one Raster is one Predictor Variable.
#' @param response A vector of either Rainfall area or rainfall rates for
#' the corresponding pixels in predictors. If only one scene is used for model 
#' training, "response" may also be a RasterLayer of the response variable.
#' @param sampsize Numeric value between 0 and 1 indicating the percentage of
#' data points to be returned
#' @param seed see \code{\link{set.seed}}
#' @return A list with two entries: predictors and response
#' @author Hanna Meyer
#' @seealso \code{\link{createDataPartition}}
#' @description Bases on createDataPartition from the caret package.

createSubset <- function (predictors, response, sampsize=0.01, seed=20){
  
  if(class(predictors)=="RasterStack"||class(predictors)=="RasterBrick"){
    predictors <- raster::as.data.frame(predictors)
  }
  if(class(response)=="RasterLayer") {
    response <- values(response)
  }
  
  keep <- complete.cases(predictors)
  predictors <- predictors[keep,]
  response <- response[keep] 
  if (class(response)=="numeric"){
    keep <- response>threshold
    response<-response[keep]
    predictors <- predictors[keep,]
  }
  set.seed(seed)
  samples<-createDataPartition(response,
                               p = sampsize,list=FALSE)
  response<-response[samples]
  if (class(response)=="character"||class(response)=="factor"){
    response<-as.factor(response)
    response<-factor(response,levels=c("Rain","NoRain"))
  }
  predictors <- predictors[samples,]  
  result<-list(predictors,response)
  names(result)=c("predictors","response")
  return(result)
  
}

