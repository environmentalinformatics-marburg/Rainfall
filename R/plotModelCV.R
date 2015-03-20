
#' Plot rfe model or train results based on cross validation
#' @description 
#' function takes an rfe model or a train object and plots the mean metric with 
#' standard deviations to visualize the effect of a tuning parameter (
#' for rfe model this is the numbers of variables) on the model performance.
#' @param model A rfe or train object
#' @param metric The metric to visualize. must be stored in the rfe object
#' @param tuningValue The tuning value which is depicted on the x axis.
#' For rfe models default is "Variables", the number of variables.
#' @param xlim Numeric vector defining x axis limits. See \code{\link{plot}}.
#' If xlim == "minmax" these limits are set according to the range of data values.
#' @param ylim Numeric vector defining x axis limits. See \code{\link{plot}}.
#' If ylim == "minmax" these limits are set according to the range of data values.
#' @author Hanna Meyer
#' @export plotModelCV
#' @seealso \code{\link{rfe}}
#' @note returnResamp = "all" must be set in rfe training
#' @examples
#' data(rfeModel)
#' plotModelCV(rfeModel)

plotModelCV <- function (model,metric=model$metric,tuningValue="Variables",
                         xlim="minmax",ylim="minmax"){
  
  data <- as.data.frame(model$resample)
  
  if (!tuningValue %in% names(data)){
    stop ("tuningValue is not available in model results")
  }
  
  names(data)[which(names(data)==tuningValue)]="tuningValue"
  
  sdv<-c()
  means<-c()
  for (i in unique(data$tuningValue)){
    sdv<-c(sdv,sd(eval(parse(text=paste("data$",metric)))[data$tuningValue==i]))
    means<-c(means,mean(eval(parse(text=paste("data$",metric)))
                        [data$tuningValue==i]))
  }
  #  input_list <- list(...)
  if (xlim=="minmax"){
    xlim <- c(min(data$tuningValue),max(data$tuningValue))
  }
  if (ylim=="minmax"){
  ylim <- c(min(means-sdv),max(means+sdv))
  }
  xyplot(means~unique(data$tuningValue),
         ylim=ylim,
         xlim=xlim,
         xlab=tuningValue,
         ylab=paste0(metric," (Cross-Validation)"),
         panel = function(x, y, ...){
           panel.polygon(c(unique(data$tuningValue),rev(unique(data$tuningValue))),
                         c(means+sdv, rev(means-sdv)), col="grey80", 
                         border=FALSE)
           panel.xyplot(x,y,type=c("b","g"),col="black",pch=16)
         }
  )
}

