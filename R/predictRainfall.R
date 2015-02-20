#' Predicts the rainfall rate based on a trained model and MSG data
#' 
#' @param model The final model from either caret's train or rfe. 
#' Use model$fit$finalModel or model$finalModel to get it
#' @param rainmask A raster indicates areas which are not raining with NA values
#' @return A Raster Layer containing predicted rainfall  
#' @author Hanna Meyer


predictRainfall <- function (model, predictors){
  require(caret)
  library(raster)
  predVars<-calculatePredictors(msg,sunzenith,variables,xderivTexture,date )
  predVars<-predVars[[model$coefnames]]
  values(predVars)[is.na(values(rainmask))] <- NA
  prediction<-predict(predVars,model)
  
}