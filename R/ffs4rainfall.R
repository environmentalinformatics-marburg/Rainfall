#' Performs forward feature selection
#' @param predictors Either a data.frame with each column is one predictor and each
#' row represents one pixel. Or (if only one scene is used for training) 
#' a RasterStack with one Raster is one Predictor Variable.
#' @param response A vector of either Rainfall area or rainfall rates for
#' the corresponding pixels in predictors. If only one scene is used for model 
#' training, "response" may also be a RasterLayer of the response variable.
#' @param sampsize. Number of data points used for training
#' @param threshold if response is Rainfall rate: pixels larger than
#' the threshold are used for rainfall rate training
#' @param out Either Rain or RInfo indicating weather rainfall rates
#' or rainfall areas should be used.
#' @param seed Any integer number. Used to produce reproducable results
#' @param method ML algorithm to be applied. default is nnet
#' @param tuneGrid list of tuning parameters to be supplied to model training. 
#' See https://topepo.github.io/caret/modelList.html for tuning values
#' @details Predictors are centered and scaled according to mean and sd values.
#' If the day of the year is used as predictor, this variable is 
#' scaled considering max=365 and min=1
#' @return A train object
#' @author Hanna Meyer
#' @export ffs4rainfall
#' @seealso \code{\link{ffe}}
#' @references ffe Function in the Rsenal package
#' @examples

#' # stack the msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' # raster the sunzenith 
#' sunzenith<-getSunzenith(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' #get Date
#' date <- getDate(inpath=system.file("extdata/msg",package="Rainfall"))
#' 
#' #calculate variables (takes some time...)
#' pred <- calculatePredictors(msg_example,
#' sunzenith=sunzenith,
#' spectral=c("VIS0.6","NIR1.6","T0.6_1.6"),
#' texture=expand.grid(c("NIR1.6","T6.2_10.8"),
#' c("variance", "contrast"),c(3,5,9)),
#' filterstat=expand.grid(c("NIR1.6","T6.2_10.8"),
#' c("sd","min"),c(3,5,9)),
#'  further=NULL,
#'  date=date)
#'  

#'response <- raster(system.file("extdata/radar",
#' "201007121650_radolan_SGrid.rst",package="Rainfall"))
#' 
#' #Train small ffs model with 0.1% of the pixels (takes around 1 minute...)
#' ffsModel <- ffs4rainfall(predictors=pred,
#' response,
#' out="Rain",
#' sampsize=0.01)
#' 

ffs4rainfall <- function (predictors,
                          response,
                          sampsize=1,
                          threshold=0.06,
                          out="Rain",
                          tuneGrid=list(.size = 2:5,
                                        .decay = c(0.05,0.07)),
                          method="nnet",
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
  
  traindata <- createSubset(predictors,response,threshold=threshold,out=out,
                            sampsize=sampsize,seed=seed)
  rm(predictors)
  rm(response)
  gc()
  
  calcScaling<- calcScalingStats(traindata$predictors)
  
  # if ("jday" %in% names(traindata$predictors)){
  traindata$predictors <- scaleByValue(traindata$predictors,calcScaling)
  #jday <- (traindata$predictors$jday-mean(1:365))/sd(1:365) 
  #traindata$predictors<-data.frame(apply(traindata$predictors[,-which(names(traindata$predictors)=="jday")],2,scale),jday)
  #  } else {
  #   traindata$predictors<-data.frame(apply(traindata$predictors,2,scale))
  #  }
  set.seed(seed)
  cvSplits <- createFolds(traindata$response, k = 10,returnTrain=TRUE)
  ### FFS Settings #############################################################  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  trainFuncs <- caretFuncs #Default caret functions
  if (out=="RInfo"){
    trainFuncs$summary <- twoClassSummary
    tctrl <- trainControl(
      method="cv",
      classProbs =TRUE) 
    metric<-"ROC"
    maximize<-TRUE
    linout<-FALSE
  } else{
    tctrl <- trainControl(method="cv")
    linout<-TRUE
    metric<-"RMSE"
    maximize<-FALSE
  }
  ### FSS Model  ###############################################################    
  ffsModel <- ffs(traindata$predictors,
                  traindata$response,
                  linout = linout, 
                  method = method,
                  trControl=tctrl,
                  tuneGrid=expand.grid(tuneGrid),
                  metric=metric,
                  maximize=maximize)
  
  stopCluster(cl)
  
  ffsModel$scaleParam <- calcScaling
  
  return(ffsModel)
}