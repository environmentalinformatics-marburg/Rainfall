#' Performs recursive feature selction
#' @param predictors A data.frame with each column is one predictor and each
#' row represents one pixel
#' @param response Vector of either Rainfall area or rainfall rates for
#' the corresponding pixels in predictors
#' @param threshold if response is Rainfall rate: pixels larger than
#' the threshold are used for rainfall rate training
#' @param seed Any integer number. Used to produce reproducable results
#' @param varSize integer vector indicating the numbers of 
#' variables to consider in rfe.
#' @param nnetsize Number of hidden units in nnet
#' @param nnetDecay. Decay value(s) used in nnet training
#' @details Predictors are centered and scaled according to mean and sd values.
#' If the day of the year is used as predictor, this variable is 
#' scaled considering max=365 and min=1
#' @return A rfe model
#' @author Hanna Meyer
#' @seealso \code{\link{rfe}}
#' @references rfe Function in the caret package
#' @examples

#' #list msg raster in the example folder:
#' scenes<-list.files(system.file("msg",package="Rainfall"),
#' pattern=".rst$")
#'
#' # name the variables (according file name convention)
#' #which are to be used:
#' x=c("ca02p0001","ca02p0002","ca02p0003","ct01dk004","ct01dk005",
#' "ct01dk006","ct01dk007","ct01dk008","ct01dk009","ct01dk010","ct01dk011")
#' # raster the sunzenith raster which is defined as "ma11" according to
#'
#' #file name conventions:
#' sunzenith<-raster(system.file("msg",
#' scenes[substr(scenes,20,23) =="ma11"],package="Rainfall"))
#'
#' # stack the msg scenes:
#' msg_example <-  stack(system.file("msg",
#' scenes[substr(scenes,20,28)%in%x],package="Rainfall"))
#' date <- substr(scenes[1],1,12)
#' 
#' # set non clouded areas to NA:
#' msg_example=reclassify(msg_example, cbind(-99,NA))
#' 
#' # name the msg channels:
#' names(msg_example)<-c("VIS0.6","VIS0.8","NIR1.6","IR3.9",
#' "WV6.2","WV7.3","IR8.7","IR9.7","IR10.8","IR12.0","IR13.4")
#' 
#' #calculate variables (takes some time...)
#' pred <- calculatePredictors(msg_example,
#' sunzenith,
#' spectral=c("VIS0.6","NIR1.6","T0.6_1.6"),
#' texture=expand.grid(c("NIR1.6","T6.2_10.8"),
#' c("variance", "contrast")),
#'  pptext=expand.grid("T3.9_10.8",c("variance","mean")),
#'  shape=c("Ar","CAI","SI","CI1"),
#'  zonstat=data.frame("spec"=c("VIS0.8","VIS0.8","T6.2_10.8"),
#'  var=c("mins","sds","maxs")),
#'  further=NULL,
#'  date=date)
#'  
#'#create data.frame from predictors
#'predictors <- as.data.frame(pred)
#'response <- raster(system.file("radar",
#' "201010081250_radolan_SGrid.rst",package="Rainfall"))
#' response <- values(response)
#' 
#' #Train small rfe model with 0.1% of the pixels (takes around 1 minute...)
#' rfeModel <- rfe4rainfall(predictors,response,sampsize=0.001)
#' 
#' # Show results:
#' print(rfeModel)
#' plotRfeCV(rfeModel)
#' predictors(rfeModel)
#' plot(varImp(rfeModel$fit,scale=TRUE))

rfe4rainfall <- function (predictors,response,
                          sampsize=0.25,
                          threshold=0.06,
                          varSize=c(1:5,seq(10,ncol(predictors),10)),
                          nnetSize=seq(2,ncol(predictors),8),
                          nnetDecay = 0.05,
                          seed=20){
  require(caret)
  ### Preprocessing ############################################################
  keep <- complete.cases(predictors)
  predictors <- predictors[keep,]
  response <- response[keep] 
  if (response=="numeric"){
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