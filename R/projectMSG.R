#' Quick&Dirty Method to project MSG data
#' 
#' @param x A RasterLayer in MSG projection
#' @param xRaster A Raster with the same spatial extent as x containing the Latlon
#' x coordinates as values. Output from the processing routine.
#' @param yRaster A Raster with the same spatial extent as x containing the Latlon
#' y coordinates as values. Output from the processing routine.
#' @param res resolution in degree of the projected raster
#' @return x projected to Latlon
#' @author Hanna Meyer
#' @export projectMSG
#' @examples
#' #not run
#' #yRaster=raster(paste(datapath,"/000000000000_00000_ml01danb1_na001_1000_rg01de_003000.rst",sep=""), 
#'  #native = T, crs = "+proj=longlat +datum=WGS84")xRaster=raster(paste(datapath,"/000000000000_00000_ml02danb1_na001_1000_rg01de_003000.rst", sep=""),
#'  #native = T, crs = "+proj=longlat +datum=WGS84")
#'  #x=msg_example[[1]]
#'  #projectMSG(x,xRaster, yRaster)

projectMSG <- function (x,xRaster, yRaster, res=0.07){

  e <- extent(data.frame("x"=values(xRaster),"y"=values(yRaster)))
  base <- raster(e, res)
  result <- rasterize(data.frame(values(xRaster),values(yRaster)), base, values(x), fun=mean)
}


