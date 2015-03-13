#' Writes a stack of MSG channels to a new file with respect to file
#' name conventions
#' @param scenerasters Raster stack of MSG scenes originally retrieved by 
#' \code{\link{getCahnnels}}
#' @param date Date of the scene. (see \code{\link{getDate}})
#' @param outpath Path to where files are to be written
#' @return Nothing is returned to the R environment. New files are stored on disk.
#' @details maimly used as internal function of \code{\link{tempAggregate}}
#' @author Hanna Meyer
#' @export writeToFile

writeToFile <- function (scenerasters, date, outpath, meta=NA){
  cnames<-names(scenerasters)
  outpathname<-paste0(outpath,"/",substr(date,1,4),"/",substr(date,5,6),"/",
                      substr(date,7,8),"/",substr(date,9,10))
  dir.create(outpathname,recursive=TRUE)
  lut<-cbind(c("ca02p0001","ca02p0002","ca02p0003","ct01dk004","ct01dk005",
               "ct01dk006","ct01dk007","ct01dk008","ct01dk009","ct01dk010",
               "ct01dk011","ma11danb1"),c(
                 "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
                 "IR9.7","IR10.8","IR12.0","IR13.4","sunzenith"))
  if (length(meta)==2){
    outnames <- paste0(date,"_mt09s_",lut[cnames==lut[,2],1],"_",meta[1],"_1000_",meta[2],"_003000.rst")
  }else{
    outnames <- paste0(date,"_mt09s_",lut[cnames==lut[,2],1],"_m1hct_1000_rg01xx_003000.rst")
  }
  for (i in 1:nlayers(scenerasters)){
    writeRaster(scenerasters[[i]],filename=paste0(outpathname,"/",outnames[i]),overwrite=TRUE)
  }
}
