validate <- function (obs, pred, type="regression",...){
  require(Rsenal)
  if (type=="regression"){
    regressionStats(values(pred), values(obs),adj.rsq=FALSE)
  }
  if (type=="classification"){
    classificationStats(values(pred), values(obs))
  }
  
}