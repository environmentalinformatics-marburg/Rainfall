
#' Plot rfe model results based on cross validation
#' @description 
#' function takes an rfe model and plots the mean metric with standard deviations
#' to visualize the effect of the numbers of variables on the model performance.
#' @param rfeModel A rfe object
#' @param metric The metric to visualize. must be stored in the rfe object
#' @param xlim Numeric vector defining x axis limits. See \code{\link{plot}}.
#' If xlim == "minmax" these limits are set according to the range of data values.
#' @param ylim Numeric vector defining x axis limits. See \code{\link{plot}}.
#' If ylim == "minmax" these limits are set according to the range of data values.
#' @author Hanna Meyer
#' @export plotRfeCV
#' @seealso \code{\link{rfe}}
#' @note returnResamp = "all" must be set in rfe training
#' @examples
#' data(rfeModel)
#' plotRfeCV(rfeModel)

plotRfeCV <- function (rfeModel,metric=rfeModel$metric,xlim="minmax",ylim="minmax"){
  data <- as.data.frame(rfeModel$resample)
  sdv<-c()
  means<-c()
  for (i in unique(data$Variables)){
    sdv<-c(sdv,sd(eval(parse(text=paste("data$",metric)))[data$Variables==i]))
    means<-c(means,mean(eval(parse(text=paste("data$",metric)))
                        [data$Variables==i]))
  }
  #  input_list <- list(...)
  if (xlim=="minmax"){
    xlim <- c(min(data$Variables),max(data$Variables))
  }
  if (ylim=="minmax"){
  ylim <- c(min(means-sdv),max(means+sdv))
  }
  xyplot(means~unique(data$Variables),
         ylim=ylim,
         xlim=xlim,
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

