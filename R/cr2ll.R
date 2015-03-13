#' Get geocoordinates from MSG column and rows
#' @param column Integer
#' @param row Integer
#' @param ccoff Integer coefficient of the scalling function (see page 28, Ref [1])              
#' @param lloff Integer coefficient of the scalling function  (see page 28, Ref [1])     
#' @return Data frame including latitude and longitude of the MSG image
#' @author EUMETSAT 2005, 2009. (R implementation: Hanna Meyer)
#' @references
#' [1] LRIT/HRIT Global Specification                     
#' (CGMS 03, Issue 2.6, 12.08.1999)                  
#'  for the parameters used in the program.
#'  
#'  [2] MSG Ground Segment LRIT/HRIT Mission Specific 
#'  Implementation, EUMETSAT Document, 
#'  (EUM/MSG/SPE/057, Issue 6, 21. June 2006).
#' @useDynLib Rainfall
#' @export cr2ll
#' @examples
#' cr2ll(1669,3401)

cr2ll=function(col,row,ccoff=1856,lloff=1856){
  
  cr2llOut <- .Fortran("pixcoord2geocoord",
                       column = as.integer(col), 
                       row = as.integer(row),
                       ccoff = as.integer(ccoff), 
                       lloff = as.integer(lloff),
                       latitude = as.double(0),
                       longitude = as.double(0),
                       PACKAGE="Rainfall")
  return(data.frame("Lat"=cr2llOut$latitude,"Lon"=cr2llOut$longitude))
  
}