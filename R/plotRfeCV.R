
#' Plot rfe model results based on cross validation
#' @description 
#' function takes an rfe model and plots the mean metric with standard deviations
#' to visualize the effect of the numbers of variables on the model performance.
#' @param rfeModel A rfe object
#' @param metric The metric to visualize. must be stored in the rfe object
#' @seealso \code{\link{rfe}}
#' @note returnResamp = "all" must be set in rfe training
#' @examples
#' data(BloodBrain)
#' x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
#' x <- x[, -findCorrelation(cor(x), .8)]
#' x <- as.data.frame(x)
#' set.seed(1)
#' rfeModel <- rfe(x, logBBB,
#' sizes = c(2, 10,30, 35, 40, 60, 65),
#' rfeControl = rfeControl(functions = lmFuncs, 
#' method="cv",returnResamp = "all"),method="nnet")
#' plotRfeCV(rfeModel)

plotRfeCV <- function (rfeModel,metric="RMSE"){
  data <- as.data.frame(rfeModel$resample)
  sdv<-c()
  means<-c()
  for (i in unique(data$Variables)){
    sdv<-c(sdv,sd(eval(parse(text=paste("data$",metric)))[data$Variables==i]))
    means<-c(means,mean(eval(parse(text=paste("data$",metric)))
                        [data$Variables==i]))
  }
  #  input_list <- list(...)
  xyplot(means~unique(data$Variables),
         ylim=c(min(means-sdv),max(means+sdv)),
         xlim=c(min(data$Variables),max(data$Variables)),
         xlab="Number of Variables",
         ylab=paste0(metric," (Cross-Validation)"),
         panel = function(x, y, ...){
           panel.polygon(c(unique(data$Variables),rev(unique(data$Variables))),
                         c(means+sdv, rev(means-sdv)), col="grey80", 
                         border=FALSE)
           panel.xyplot(x,y,type=c("b","g"),col="black",pch=16)
         }
  )
}

