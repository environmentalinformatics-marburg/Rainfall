#' Aggregates MSG images and sunzenith to one hour (mean calculation)
#' @param inpath Character string of the path to the MSG scenes. 
#' Only one subfolder and the "/cal" is allowed 
#' (as returned from the msg processing). E.g use this function for one day.
#' e.g. inpath= /meteosat-images/2010/01/01/ including one folder for every scene 
#' and for every scene the folder "cal"
#' @param outpath Character string of the output path
#' @param type A character string indicating the output file type. 
#' Can be ignored if fromFolder==TRUE
#' @param returnResult if TRUE, data are returned to teh R environment
#' @details Bases on the following name conventions:
#' Single scenes are stored in a folder MT9PDate_mt09s with Date is in the format
#' yyymmddhhmm in the subfolder "cal".
#' Scenes must be named according to the described file name conventions.
#' @return If no outpath is speciefied, the results are returned to the R 
#' environment as a list of rasterStacks. If outpath is specified, 
#' files are written to disk
#' @author Hanna Meyer
#' @export tempAggregate


tempAggregate <- function (inpath, outpath=NULL, returnResult=FALSE, type="tif"){
  require(doParallel)
  registerDoParallel(detectCores())
  files <-list.files(inpath)
  date <- getDate(inpath,fromFolder=TRUE)
  if (date=="") stop ("inpath contains no MSG scenes")
  outdates <- paste0(unique(substr(date,1,10)),"00")
  cfiles <- list()
  for (i in 1:length(outdates)){
    cfiles[[i]] <- paste0(inpath,"/",files[substr(date,1,10)==
                                             substr(outdates[i],1,10)])
  }
  tmp=list.files(paste0(cfiles[[1]][1],"/cal"))[1]
  meta=c(substr(tmp,30,34),substr(tmp,41,46))
  out <- foreach(i=1:length (cfiles),
                 .packages= c("raster","doParallel","Rainfall"))%dopar%{ 
                   ch=list()
                   for (k in 1:length(cfiles[[i]])){
                     ch[[k]] <- stack(
                       getChannels(paste0(cfiles[[i]][k],"/cal")),
                       getSunzenith(paste0(cfiles[[i]][k],"/meta"))
                     )
                   }
                   means <- stack()
                   for (k in 1: nlayers(ch[[1]])){
                     means=stack(means,calc(stack(lapply(ch,function(x)x[[k]])),mean))
                   }
                   names(means) <- names(ch[[1]])
                   if (!is.null(outpath)){
                   writeToFile(scenerasters=means,date=outdates[i],outpath=outpath, type=type, meta=meta)
                   }
                   return(means)
                 }
  if(is.null(outpath)||returnResult){
    return(out)
  }
}
