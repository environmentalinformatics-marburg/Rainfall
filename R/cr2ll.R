#' Get geocoordinates from MSG column and rows
#' @param column Integer
#' @param row Integer
#' @param ccoff Integer coefficient of the scalling function (see page 28, Ref [1])              
#' @param lloff Integer coefficient of the scalling function  (see page 28, Ref [1]) 
#' @return Data frame including latitude and longitude of the MSG image
#' @author Hanna Meyer based on the FORTRAN Routine of EUMETSAT 2005, 2009.
#' @references
#' [1] LRIT/HRIT Global Specification                     
#' (CGMS 03, Issue 2.6, 12.08.1999)                  
#'  for the parameters used in the program.
#'  
#'  [2] MSG Ground Segment LRIT/HRIT Mission Specific 
#'  Implementation, EUMETSAT Document, 
#'  (EUM/MSG/SPE/057, Issue 6, 21. June 2006).
#' @export cr2ll
#' @useDynLib Rainfall
#' @examples
#' cr2ll(data.frame(c(2899,3435),c(1200,1340)))

cr2ll=function(colrow,ccoff=1856,lloff=1856){
  cr2llOut <- apply(colrow,1,function(x){
    col <- 3712-colrow[,1]
    row <- 3712-colrow[,2]
    tmp <- .Fortran("pixcoord2geocoord",
                    column = as.integer(col), 
                    row = as.integer(row),
                    ccoff = as.integer(ccoff), 
                    lloff = as.integer(lloff),
                    latitude = as.double(0),
                    longitude = as.double(0),
                    PACKAGE="Rainfall")
    return(data.frame("Lat"=tmp$latitude,"Lon"=tmp$longitude))
  })
  do.call(rbind,cr2llOut)
  return(do.call(rbind,cr2llOut))
}