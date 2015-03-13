#' Get MSG pixel coordinates (column/rows) from geocordiantes
#' @param latitude Numeric
#' @param longitude Numeric
#' @param ccoff Integer coefficient of the scalling function (see page 28, Ref [1])              
#' @param lloff Integer coefficient of the scalling function  (see page 28, Ref [1])     
#' @return Data frame including column and row of the MSG image
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
#' @export ll2cr
#' @examples
#' ll2cr(50.7,8.7)

ll2cr=function(lat,lon,ccoff=1856,lloff=1856){

ll2crOut <- .Fortran("geocoord2pixcoord",
                     latitude = as.double(lat),
                     longitude = as.double(lon),
                     ccoff = as.integer(ccoff), 
                     lloff = as.integer(lloff),
                     column = as.integer(0), 
                     row = as.integer(0),
                     PACKAGE="Rainfall")
return(data.frame("Column"=ll2crOut$column,"Row"=ll2crOut$row))

}