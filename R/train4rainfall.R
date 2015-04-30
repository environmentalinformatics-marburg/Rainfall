#' Performs model training
#' @param predictors Either a data.frame with each column is one predictor and each
#' row represents one pixel. Or (if only one scene is used for training) 
#' a RasterStack with one Raster is one Predictor Variable.
#' @param response A vector of either Rainfall area or rainfall rates for
#' the corresponding pixels in predictors. If only one scene is used for model 
#' training, "response" may also be a RasterLayer of the response variable.
#' @param scaleVars Center and scale variables?
#' @param threshold if response is Rainfall rate: pixels larger than
#' the threshold are used for rainfall rate training
#' @param seed Any integer number. Used to produce reproducable results
#' @param nnetSize Number of hidden units in nnet
#' @param nnetDecay. Decay value(s) used in nnet training
#' @param thresholdTune optional threshold tuning. Only if response ="RInfo"
#' @return A train object. If keepScaling=TRUE a list with the first object is
#'  the train object and the second object is a data.frame including mean and sd 
#'  values for all predictors which can be used for ensuring same scaling 
#'  with new unknown values.
#' @author Hanna Meyer
#' @export train4rainfall
#' @seealso \code{\link{train}}
#' @references train Function in the caret package
#' @examples
#' #' # stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' # raster the sunzenith 
#' sunzenith<-getSunzenith(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' #get Date
#' date <- getDate(system.file("extdata/msg",package="Rainfall"))
#' 
#' response <- raster(system.file("extdata/radar",
#' "201007121650_radolan_SGrid.rst",package="Rainfall"))
#' 
#' #get optimal variables from rfe model
#' data(rfeModel)
#' pred<-calculatePredictors(msg_example,model=rfeModel,date=date)
#' 
#' train4rainfall(pred,response,sampsize=0.1,out="Rain")


train4rainfall <- function (predictors,
                            response,
                            out="Rain",
                            scaleVars=FALSE,
                            sampsize=0.25,
                            threshold=0.06,
                            nnetSize=2:5,
                            nnetDecay = c(0.05,0.07),
                            thresholdTune=c(seq(0,0.1,0.05),seq(0.12,0.30,0.02),seq(0.35,1,0.05)),
                            seed=20){
  require(caret)
  require(raster)
  require(doParallel)
  ### Preprocessing ############################################################
  if(class(predictors)=="RasterStack"||class(predictors)=="RasterBrick"){
    predictors <- raster::as.data.frame(predictors)
  }
  if(class(response)=="RasterLayer") {
    response <- values(response)
  }
  
#  keep <- complete.cases(predictors)
#  predictors <- predictors[keep,]
#  response <- response[keep] 
#  if (class(response)=="numeric"){
#    keep <- response>threshold
#    response<-response[keep]
#    predictors <- predictors[keep,]
#  }
  traindata <- createSubset(predictors,response,threshold=threshold,out=out,sampsize=sampsize,seed=seed) 
  
  if(scaleVars){
      calcScaling<-data.frame("mean"=apply(traindata$predictors,2,mean),"sd"=apply(predictors,2,sd))
      traindata$predictors <- scaleByValue(traindata$predictors,calcScaling)
#    if ("jday" %in% names(predictors)){
#      jday <- (predictors$jday-mean(1:365))/sd(1:365) 
#      predictors<-data.frame(apply(predictors[,-which(names(predictors)=="jday")],2,scale),jday)
#    } else {
#      predictors<-data.frame(apply(predictors,2,scale))
#    }
  }
  
#  samples<-createDataPartition(traindata$response,
#                               p = sampsize,list=FALSE)
#  traindata$response=traindata$response[samples]
#  if (class(traindata$response)=="character"||class(response)=="factor"){
#    traindata$response=as.factor(traindata$response)
#    traindata$response=factor(traindata$response,levels=c("Rain","NoRain"))
#  }
#  traindata$predictors <- traindata$predictors[samples,]
  
  set.seed(seed)
  cvSplits <- createFolds(traindata$response, k = 10,returnTrain=TRUE)
  ### train Settings #############################################################  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  
  if (class(traindata$response)=="factor"){
    metric="Dist" #wenn nicht _thres dann "ROC
    maximize = FALSE #when dist is used, then min value is important
    classProbs =TRUE
    #metric="ROC"
    linout=FALSE
    summaryFunction = "fourStats"
    method=nnet_thres
    if (!is.na(thresholdTune)){
      tuneGrid <- expand.grid(.size = nnetSize,
                              .decay = nnetDecay,
                              .threshold = thresholdTune)
    } else{
      tuneGrid <- expand.grid(.size = nnetSize,
                              .decay = nnetDecay)
    }
  }
  else{
    metric="RMSE"
    maximize=FALSE
    classProbs =FALSE
    linout=TRUE
    summaryFunction ="defaultSummary"
    method="nnet"
    tuneGrid <- expand.grid(.size = nnetSize,
                            .decay = nnetDecay)
  }
  ### Model  ###############################################################    
  
  ctrl <- trainControl(index=cvSplits,
                       method="cv",
                       summaryFunction = eval(parse(text=summaryFunction)),
                       classProbs = classProbs,
                       returnResamp = "all")
  
  
  model <- train(traindata$predictors,
                 traindata$response,
                 linout = linout, 
                 trace = FALSE,
                 method = method,
                 trControl=ctrl,
                 tuneGrid=tuneGrid,
                 metric=metric,
                 maximize=maximize,
                 verbose=TRUE)
  
  stopCluster(cl)
  if (scaleVars){
    model$scalingparam=calcScaling
  }
  return(model)
}