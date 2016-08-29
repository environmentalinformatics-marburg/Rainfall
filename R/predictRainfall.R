#' Predicts the rainfall rate based on a trained model and MSG data
#' 
#' @param model The final model from either caret's train or rfe. 
#' Use model$fit$finalModel or model$finalModel to get it
#' @param inpath Path to the MSG data
#' @param sceneraster If no inpath is specified: 
#' The Meteosat data from which rainfall should be predicted.
#' Load them with getChannels
#' @param rainmask A raster indicates areas which are not raining with NA values
#' @param sunzenith If no inpath is specified: sunzenith optional. 
#' Only needed if included in the predictor variables
#' @param date If no inpath is specified: date optional. 
#' only needed if jday is included in the predictor variables
#' @param useOptimal if model is a rfe object: Logical. Use the optimal variables 
#' from rfe or those less variables which lead to a model performance within one 
#' sd of the optimal model?
#' @param scaleparam A data.frame created with \code{\link{calcScalingStats}}
#' @param type If inpath!=NULL then the data type of the MSG raster
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
#' data(rfemodel)
#' 
#' #predict on the new scene (don't expect good results from the small model!)
#' pred<-predictRainfall(model=rfeModel, sceneraster=msg_example, rainmask=reference)
#' validate(obs=reference,pred=pred)


predictRainfall <- function (model, 
                             inpath=NULL, 
                             sceneraster=NULL, 
                             rainmask=NULL, 
                             sunzenith=NULL,
                             date=NULL, 
                             useOptimal=TRUE, 
                             scaleparam=model$scaleParam,
                             min_x=NULL,
                             max_x=NULL,
                             type="tif"){
  require(caret)
  library(raster)
  if (!is.null(inpath)){
    sceneraster <- getChannels(inpath,type=type)
    date <- getDate(inpath)
    sunzenith <- getSunzenith(inpath,type=type)
  }
  predVars<-calculatePredictors(sceneraster,model=model,sunzenith=sunzenith,
                                date=date,useOptimal=useOptimal,min_x=min_x,
                                max_x=max_x)
  
  
  if (!is.null(scaleparam)){
    predVars <- scaleByValue(predVars,scaleparam)
  }
  if(!is.null(rainmask)){
   predVars<- mask(predVars,rainmask)
  }
  if (class(model)=="rfe"){
  prediction <- predict(predVars,model$fit)
  } else{
    prediction <- predict(predVars,model)
  }
  prediction<-mask(prediction,predVars[[4]])
  values(prediction)[values(prediction)<0]=0
  return(prediction)
}