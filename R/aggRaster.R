#' Temporal aggregation of raster data
#' @param data List of raster files with each list entry containing
#' all hourly raster files of one single day.
#' @param agg.level Aggregation level in hours (numeric).
#' @param write.file logical: Should aggregated raster files be locally stored
#' (TRUE) or kept in memory (FALSE).
#' @param days character: Days with available hourly raster files.
#' @param path.out character: Path to output directory.
#' @param  process.incomplete logical: Should incomplete raster data sets (e.g. two raster 
#' files at agg.level = 3) be processed?
#' @param ... Further arguments to be passed.
#' @return A raster Layer
#' @author Meike Kuehnlein
#' @export aggRaster




#author:Meike Kühnlein
aggRaster <- function(data, 
                      agg.level,
                      write.file = TRUE,
                      days, 
                      path.out,
                      process.incomplete = FALSE,
                      ...) {
  

  # Required packages
  library(rgdal)
  
  # Diurnal aggregation
  if (agg.level == 24) {
    
    data.agg <- lapply(seq(data), function(i) {
      if(length(rasters[[i]]) != 1){
        tmp <- stack(data[[i]])
        calc(tmp, fun = sum, na.rm = TRUE, 
             filename = ifelse(write.file, 
                               paste(path.out, days[i], "_24h.rst", sep = ""), 
                               ''), overwrite = TRUE)
      } else if(length(rasters[[i]]) == 1) {
        tmp <- data[[i]]
        #writeRaster(tmp, file=paste(path.out, days[i], "_24h.rst", sep = ""), overwrite = TRUE)
      }
      
      
    })
    
    # Aggregation of several hours per day  
  } else {
    
    data.agg <- lapply(seq(data), function(i) {
      
      lapply(seq(1, length(data[[i]]), agg.level), function(j) {
        # Number of rasters per day divisible by agg.level
        if ((j + agg.level - 1) <= length(data[[i]])) {
          tmp <- stack(data[[i]][seq(j, j + agg.level - 1)])
          calc(tmp, fun = sum, na.rm = TRUE, 
               filename = ifelse(write.file, 
                                 paste(path.out, days[i], "_", agg.level, "h_", j, ".rst", sep = ""),
                                 ''), overwrite = TRUE)
          # Number of rasters per day indivisible by agg.level
        } else {
          
          if (process.incomplete) {
            tmp <- stack(data[[i]][seq(j, length(data[[i]]))])
            
            if (nlayers(tmp) > 1) {
              calc(tmp, fun = sum, na.rm = TRUE, 
                   filename = ifelse(write.file, 
                                     paste(path.out, days[i], "_", agg.level, "h_", j, ".rst", sep = ""), 
                                     ''), overwrite = TRUE)
            } else {
              tmp
            }
          }
        }
      })
    })
    
  }
  
  # Return output
  return(data.agg)
  
}