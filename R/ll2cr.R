#' Get MSG pixel coordinates (column/rows) from geocordiantes
#' @param latlon data.frame with first column =latitude and second column is
#' longitude specified as numeric.
#' @param ccoff Integer coefficient of the scalling function (see page 28, Ref [1])              
#' @param lloff Integer coefficient of the scalling function  (see page 28, Ref [1])
#' @param type either rst or NA. If rst col/rows are modified since idrisi 
#' starts with col/row 0 instead of 1     
#' @return Data frame including column and row of the MSG image
#' @author Hanna Meyer based on the FORTRAN Routine of EUMETSAT 2005, 2009.
#' @references
#' [1] LRIT/HRIT Global Specification                     
#' (CGMS 03, Issue 2.6, 12.08.1999)                  
#'  for the parameters used in the program.
#'  
#'  [2] MSG Ground Segment LRIT/HRIT Mission Specific 
#'  Implementation, EUMETSAT Document, 
#'  (EUM/MSG/SPE/057, Issue 6, 21. June 2006).
#' @export ll2cr
#' @examples
#' #for a single coordinate
#' ll2cr(data.frame(50.7,8.7))
#'
#' #for more than one coordinate
#' latlon=data.frame("lat"=c(50.7, 53.8),"lon"=c(8.7, 11.2))
#' ll2cr(latlon)


ll2cr=function(latlon,ccoff=1856,lloff=1856,type="rst"){ 
  ll2crOut <-  apply(latlon,1,function(x){
    lat <- x[1]
    lon <- x[2]
    tmp= .Fortran("geocoord2pixcoord",
                  latitude = as.double(lat),
                  longitude = as.double(lon),
                  ccoff = as.integer(ccoff), 
                  lloff = as.integer(lloff),
                  column = as.integer(0), 
                  row = as.integer(0),
                  PACKAGE="Rainfall")
    tmp$column=3712-tmp$column #-1 --> Idrisi (beginnt mit 0 zu zählen)
    tmp$row=3712-tmp$row #-1 --> Idrisi (beginnt mit 0 zu zählen)
    if(type!="rst"){
      tmp$column <- tmp$column+1
      tmp$row <- tmp$row+1
    }
    return(data.frame("Column"=tmp$column,"Row"=tmp$row))
  })
  do.call(rbind,ll2crOut)
  return(do.call(rbind,ll2crOut))
}