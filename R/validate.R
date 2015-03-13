#' Validation of model results
#' @param obs A RasterLayer of observed rainfall areas or rainfall rates
#' @param pred A RasterLayer of predicted rainfall areas or rainfall rates
#' @param type A character string indicating the type of the model output.
#' Either "classification" or "regressione"
#' @return A data.frame containing the validation statistics
#' @description The calculations base on the regressionStats or 
#' classificationStats of the Rsenal package
#' @author Hanna Meyer
#' @export validate
#' @seealso \code{\link{classificationStats}}, \code{\link{regressionStats}}

validate <- function (obs, pred, type="regression",...){
  require(Rsenal)
  
  if (sum(!is.na(values(pred)))==0||sum(!is.na(values(obs)))==0){
    print ("error: one of the input rasters is empty")
    stop
  }
  
  if (type=="regression"){
    stats <- regressionStats(values(pred), values(obs),adj.rsq=FALSE)
  }
  if (type=="classification"){
    stats <- classificationStats(values(pred), values(obs))
  }
  return(stats)
}