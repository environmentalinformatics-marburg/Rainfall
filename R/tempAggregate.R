#' Aggregates MSG images to one hour (mean calculation)
#' @param inpath Character string of the path to the MSG scenes. 
#' Only one subfolder and the "/cal" is allowed 
#' (as returned from the msg processing). E.g use this function for one day.
#' e.g. inpath= /meteosat-images/2010/01/01/ including one folder for every scene 
#' and for every scene the folder "cal"
#' @param outpath Character string of the output path
#' @param type A character string indicating the file type. 
#' Can be ignored if fromFolder==TRUE
#' @details Bases on the following name conventions:
#' Single scenes are stored in a folder MT9PDate_mt09s with Date is in the format
#' yyymmddhhmm in the subfolder "cal".
#' Scenes must be named according to the described file name conventions.
#' @return If no outpath is speciefied, the results are returned to the R 
#' environment as a list of rasterStacks. If outpath is specified, 
#' files are written to disk
#' @author Hanna Meyer


tempAggregate <- function (inpath, outpath=NULL, type="rst"){
  require(doParallel)
  registerDoParallel(detectCores())
  files <-list.files(inpath)
  date <- getDate(inpath,fromFolder=TRUE)
  outdates <- paste0(unique(substr(date,1,10)),"00")
  cfiles <- list()
  for (i in 1:length(outdates)){
    cfiles[[i]] <- paste0(inpath,"/",files[substr(date,1,10)==
                                             substr(outdates[i],1,10)])
  }
  out <- foreach(i=1:length (cfiles),
                 .packages= c("raster","doParallel","Rainfall"))%dopar%{ 
                   ch=list()
                   for (k in 1:length(cfiles[[i]])){
                     ch[[k]] <- getChannels(paste0(cfiles[[i]][k],"/cal"))  #hier eventuell noch paste mit /cal
                   }
                   means <- stack()
                   for (k in 1: nlayers(ch[[1]])){
                     means=stack(means,calc(stack(lapply(ch,function(x)x[[k]])),mean))
                   }
                   names(means) <- names(ch[[1]])
                   writeToFile(scenerasters=means,date=outdates[i],outpath=outpath)
                 }
  if(is.null(outpath)){
    return(out)
  }
}



#was ist mit sunzenith?