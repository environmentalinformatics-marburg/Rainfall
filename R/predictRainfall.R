#' Predicts the rainfall rate based on a trained model and MSG data
#' 
#' @param model The final model from either caret's train or rfe. 
#' Use model$fit$finalModel or model$finalModel to get it
#' @param sceneraster The Meteosat data from which rainfall should be predicted.
#' Load them with getChannels
#' @param rainmask A raster indicates areas which are not raining with NA values
#' @param sunzenith optional. Only needed if included in the predictor variables
#' @param date optional. only needed if jday is included in the predictor variables
#' @return A Raster Layer containing predicted rainfall  
#' @description Functions calculates predictors which are required by the model
#' and uses them for prediction
#' @author Hanna Meyer
#' @export predictRainfall
#' @examples
#' # stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' reference <- raster(system.file("extdata/radar",
#' "201007121650_radolan_SGrid.rst",package="Rainfall"))
#' values(reference)[values(reference<0.06)]=NA
#' 
#' data(rfeModel)
#' 
#' #predict on the new scene (don't expect good results from the small model!)
#' pred<-predictRainfall(model=rfeModel, sceneraster=msg_example, rainmask=reference)
#' validate(obs=reference,pred=pred)


predictRainfall <- function (model, sceneraster, rainmask=NULL, sunzenith=NULL,
                             date=NULL){
  require(caret)
  library(raster)
  predVars<-calculatePredictors(sceneraster,model=model,sunzenith=sunzenith,
                                date=date)
  if(!is.null(reference)){
   predVars<- mask(predVars,reference)
  }
  prediction<-predict(predVars,model)
}