
#' Plot correlation of predictors
#' @param predictors Either A RasterStack of predictor Variables or a data.frame
#' @param subset Indicates the predictors to use
#' @author Hanna Meyer
#' @description bases on the corrplot package. 
#' @references Taiyun Wei (2013). corrplot: Visualization of a correlation matrix. 
#' R package version 0.73. http://CRAN.R-project.org/package=corrplot
#' @examples 
#'  # stack  msg scenes:
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"))
#' data(rfeModel)
#' pred<-calculatePredictors(msg_example,model=rfeModel)
#' plotPredCorr(pred)

plotPredCorr <- function(predictors,subset=1:20){
  require(corrplot)
  if (class(predictors)=="RasterStack"||class(predictors)=="RasterBrick"){
    predictors<-as.data.frame(predictors) 
  }
  predictors<-predictors[complete.cases(predictors),]
  if ("jday" %in% names(predictors)){
    if (length(unique(predictors$jday))==1)
      predictors <- predictors[-which(names(predictors)=="jday")]
  }
  if (ncol(predictors)<max(subset)) subset=subset[subset<ncol(predictors)]
  correlation<-cor(predictors[,subset], use ="complete.obs")
  corrplot(correlation,type="lower",tl.cex=1)
  
}