#' Extract variables from rfe model
#' @param rfeModel An object of class rfe
#' @param useOptimal Logical. Use the optimal variables from rfe or those
#' less variables which lead to a model performance within one sd of the 
#' optimal model?
#' @return A list of variables from the rfe object which can be directly used
#' in calculatePredictors
#' @description This is an internal function used by calculatePredictors
#' @author Hanna Meyer
#' @export varFromRfe
#' @examples
#' data(rfeModel)
#' varFromRfe(rfeModel) 

varFromRfe <- function (rfeModel,useOptimal=TRUE){
  if (class(rfeModel)=="rfe"){
  if (useOptimal){
  vars<-rfeModel$optVariables
  } else {
    vars <- names(Rsenal::varsRfeCV(rfeModel))
  }
  } else {
    vars <- names(rfeModel$trainingData)
  }
  
  textures<-vars[substr(vars,1,4)=="glcm"]
  texture <-c()
  if (length(textures)>0){
    textures<-strsplit(textures,"_")
    for (i in 1:length(textures)){
      if (length(textures[[i]])==3){
        texture <-rbind(texture,cbind(textures[[i]][2],textures[[i]][3],substr(textures[[i]][1],5,5)))
      }
      if (length(textures[[i]])==4){
        texture <- rbind(texture,cbind(paste0(textures[[i]][2],"_",textures[[i]][3]),textures[[i]][4],substr(textures[[i]][1],5,5)))
      }
    }
  }
  texture<-data.frame(texture)
  
  filterstats<-vars[substr(vars,1,1)=="f"]
  filterstat <-c()
  if (length(filterstats)>0){
    filterstats<-strsplit(filterstats,"_")
    for (i in 1:length(filterstats)){
      if (length(filterstats[[i]])==3){
        filterstat <-rbind(filterstat,cbind(filterstats[[i]][2],filterstats[[i]][3],substr(filterstats[[i]][1],2,2)))
      }
      if (length(filterstats[[i]])==4){
        filterstat <- rbind(filterstat,cbind(paste0(filterstats[[i]][2],"_",filterstats[[i]][3]),filterstats[[i]][4],substr(filterstats[[i]][1],2,2)))
      }
    }
  }
  filterstat<-data.frame(filterstat)
  
  
  
  
  spectral<-vars[substr(vars,1,2)=="VI"|substr(vars,1,2)=="NI"|substr(vars,1,2)=="IR"|substr(vars,1,1)=="T"|substr(vars,1,2)=="WV"]
  
  if (length(spectral) >0){
    spectral <- spectral[substr(spectral,nchar(spectral)-1,nchar(spectral))!="ax"&substr(spectral,nchar(spectral)-1,nchar(spectral))!="sd"&
                           substr(spectral,nchar(spectral)-1,nchar(spectral))!="in"&substr(spectral,nchar(spectral)-1,nchar(spectral))!="an"]
  }
  pptextTmp <- vars[substr(vars,1,2)=="pp"]
  pptext <- c()
  if (length(pptextTmp)>0){
    pptextTmp <- strsplit(pptextTmp,"_")
    for (i in 1:length(pptextTmp)){
      if (length(pptextTmp[[i]])==3){
        pptext <- rbind(pptext,cbind(pptextTmp[[i]][3],(pptextTmp[[i]][2])))
      }
      if (length(pptextTmp[[i]])==4){
        pptext <- rbind(pptext,cbind(paste0(pptextTmp[[i]][3],"_",pptextTmp[[i]][4]),pptextTmp[[i]][2]))
      }
    }
  }
  
  zonstatTmp <- vars[substr(vars,nchar(vars)-1,nchar(vars))=="ax"|substr(vars,nchar(vars)-1,nchar(vars))=="sd"|
                       substr(vars,nchar(vars)-1,nchar(vars))=="in"|substr(vars,nchar(vars)-1,nchar(vars))=="an"]
  zonstatTmp <- zonstatTmp[!substr(zonstatTmp,1,1)=="f"&&!substr(zonstatTmp,1,4)=="glcm"]
  zonstat=c()
  if (length(zonstatTmp)>0){
    zonstatTmp <- strsplit(zonstatTmp,"_")
    
    
    for (i in 1:length(zonstatTmp)){
      if (length(zonstatTmp[[i]])==2){
        zonstat <- rbind(zonstat,cbind(zonstatTmp[[i]][1],(zonstatTmp[[i]][2])))
      }
      if (length(zonstatTmp[[i]])==3){
        zonstat <-rbind(zonstat,cbind(paste0(zonstatTmp[[i]][1],"_",zonstatTmp[[i]][2]),zonstatTmp[[i]][3]))
      }
    }
  }
  
  possibleshapes<-c("Ar","SI","CA","Ur","CAI","PAR","distEdges","Re","Ru","OIC", 
"CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E", "TR","CR","C2","FR",
"EI","SF1","GSI","SF2","C3","SF3")
  shape<-vars[vars%in%possibleshapes]
  
  
  further<-vars[vars=="sunzenith"|vars=="jday"]
  
  result <- list("spectral"=spectral,"texture"=texture,"shape"=shape,
                 "pptext"=pptext,"zonstat"=zonstat,"filterstat"=filterstat,"further"=further)
  result <- result[lapply(result,length)!=0]
  return(result)
}