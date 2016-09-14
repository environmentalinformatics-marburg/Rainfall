
#' Match temporally aggregated (1h) MSG data with station data
#' @param stationpath path to the csv files of the 1h weather station data
#' @param MSG_extract result from extractMSG. The first column is the date, the 
#' second column is the station name. The MSG data of the corresponding time and
#' location are organized in the subsequent columns.
#' @author Hanna Meyer
#' @export msgstatmatch


msgstatmatch <- function (stationpath,MSG_extract){
  matchfun <- function (i,MSG_extract,stationpath,files,filenames){
    
    rainfall <- data.frame()
    statdatsub <- MSG_extract[MSG_extract$Station==unique(MSG_extract$Station)[i],]
    statdatsub$date <- as.character(statdatsub$date)
    statdat <- tryCatch(read.csv(paste0(stationpath,"/",
                                        files[which(filenames==unique(MSG_extract$Station)[i])])),
                        error = function(e)e)
    if(inherits(statdat, "error")) {
      print(paste0(unique(MSG_extract$Station)[i]," could not be processed"))
      next
    }
    statdat$datetime <- as.character(statdat$datetime)
    statdat$datetime <- gsub("-", "",statdat$datetime)
    statdat$datetime <- gsub("T", "",statdat$datetime)
    statdat$datetime <- gsub(":", "",statdat$datetime)
    
    for (k in 1:nrow(statdatsub)){
      if (any(statdatsub$date[k]==statdat$datetime)==FALSE){next}
      if(sum(is.na(data.frame(statdatsub[k,],
                              statdat[statdatsub$date[k]==statdat$datetime,4])[3:13]))==11){next}
      rainfall <- rbind(rainfall,data.frame(statdatsub[k,],
                                            statdat[statdatsub$date[k]==statdat$datetime,4]))
    }
    print(paste0(i, "von ", length(unique(MSG_extract$Station))))
    return(rainfall)
  }
  
  
  
  lib <- c("raster","doParallel","foreach")
  sapply(lib, function(x) require(x, character.only = TRUE))
  crs <- detectCores()-1
  files <- list.files(path=stationpath,pattern=".csv")
  filenames <- substr(files,1,nchar(files)-4)
  names(MSG_extract)[2] <- "Station"
  
  MSG_extract <- MSG_extract[order(MSG_extract$Station, MSG_extract$date),]
  MSG_extract$Station <- as.character(MSG_extract$Station)
  MSG_extract$date <- paste0(MSG_extract$date,"00")
  

  
  cl <- makeCluster(crs, outfile = "debug.txt")
  registerDoParallel(cl)
  
  rslt <- foreach(i=1:length(unique(MSG_extract$Station)),.errorhandling = "remove",
                  .packages=lib,.combine = rbind)%dopar%{
                    matchfun(i,MSG_extract,stationpath,files,filenames)}
  stopCluster(cl)
  
  names(rslt)[ncol(rslt)]<-"rainfall"
  return(rslt)
}


