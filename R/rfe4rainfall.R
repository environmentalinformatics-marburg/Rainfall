#' Performs recursive feature selction
#' @param predictors Either a data.frame with each column is one predictor and each
#' row represents one pixel. Or (if only one scene is used for training) 
#' a RasterStack with one Raster is one Predictor Variable.
#' @param response A vector of either Rainfall area or rainfall rates for
#' the corresponding pixels in predictors. If only one scene is used for model 
#' training, "response" may also be a RasterLayer of the response variable.
#' @param threshold if response is Rainfall rate: pixels larger than
#' the threshold are used for rainfall rate training
#' @param seed Any integer number. Used to produce reproducable results
#' @param varSize integer vector indicating the numbers of 
#' variables to consider in rfe.
#' @param nnetSize Number of hidden units in nnet
#' @param nnetDecay. Decay value(s) used in nnet training
#' @details Predictors are centered and scaled according to mean and sd values.
#' If the day of the year is used as predictor, this variable is 
#' scaled considering max=365 and min=1
#' @return A rfe model
#' @author Hanna Meyer
#' @seealso \code{\link{rfe}}
#' @references rfe Function in the caret package
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
#' #Train small rfe model with 0.1% of the pixels (takes around 1 minute...)
#' rfeModel <- rfe4rainfall(predictors=pred,
#' response,
#' sampsize=0.001)
#' 
#' # Show results:
#' print(rfeModel)
#' plotRfeCV(rfeModel)
#' predictors(rfeModel)
#' plot(varImp(rfeModel$fit,scale=TRUE))

rfe4rainfall <- function (predictors,
                          response,
                          sampsize=1,
                          threshold=0.06,
                          varSize=c(1:5,seq(10,ncol(predictors),10)),
                          nnetSize=c(seq(2,10,2),seq(20,ncol(predictors),10)),
                          nnetDecay = 0.05,
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
  keep <- complete.cases(predictors)
  predictors <- predictors[keep,]
  response <- response[keep] 
  if (class(response)=="numeric"){
    keep <- response>threshold
    response<-response[keep]
    predictors <- predictors[keep,]
  }
  samples<-createDataPartition(response,
                               p = sampsize,list=FALSE)
  response=response[samples]
  if (class(response)=="character"||class(response)=="factor"){
    response=as.factor(response)
    response=factor(response,levels=c("Rain","NoRain"))
  }
  predictors <- predictors[samples,]
  
  if ("jday" %in% names(predictors)){
    jday <- (predictors$jday-mean(1:365))/sd(1:365) 
    predictors<-data.frame(apply(predictors[,-which(names(predictors)=="jday")],2,scale),jday)
  } else {
    predictors<-data.frame(apply(predictors,2,scale))
  }
  set.seed(seed)
  cvSplits <- createFolds(response, k = 10,returnTrain=TRUE)
  ### RFE Settings #############################################################  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  nnetFuncs <- caretFuncs #Default caret functions
  if (class(response)=="factor"){
    nnetFuncs$summary <- twoClassSummary
    tctrl <- trainControl(
      method="cv",
      classProbs =TRUE)
    rctrl <- rfeControl(index=cvSplits,
                        functions = nnetFuncs,
                        method="cv",
                        returnResamp = "all")
    metric="ROC"
    maximize=TRUE
    linout=FALSE
  } else{
    tctrl <- trainControl(method="cv")
    rctrl <- rfeControl(index=cvSplits,
                        functions = nnetFuncs,
                        method="cv",
                        returnResamp = "all")
    linout=TRUE
    metric="RMSE"
    maximize=FALSE
  }
  ### RFE Model  ###############################################################    
  rfeModel <- rfe(predictors,
                  response,
                  linout = linout, 
                  trace = FALSE,
                  sizes = varSize,
                  method = "nnet",
                  rfeControl = rctrl,
                  trControl=tctrl,
                  tuneGrid=expand.grid(.size = nnetSize,
                                       .decay = nnetDecay),
                  metric=metric,
                  maximize=maximize)
  
  stopCluster(cl)
  return(rfeModel)
}