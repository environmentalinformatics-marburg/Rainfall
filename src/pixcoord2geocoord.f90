SUBROUTINE pixcoord2geocoord( column,  row,  ccoff,  lloff, latitude, longitude)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
! subroutine pixcoord2geocoord 
!  
!  PURPOSE: 
!  return the geograhic latitude and longitude of an MSG image   
!    for a given pair of latitude/longitude.             
!    (based on the formulas given in Ref. [1])                
!                                                        
!                                                        
!  DEPENDENCIES:                                         
!    none                                                 
!                                                        
!                                                        
!  REFERENCE:                                            
!  [1] LRIT/HRIT Global Specification                     
!      (CGMS 03, Issue 2.6, 12.08.1999)                  
!      for the parameters used in the program           
!  [2] MSG Ground Segment LRIT/HRIT Mission Specific 
!      Implementation, EUMETSAT Document, 
!      (EUM/MSG/SPE/057, Issue 6, 21. June 2006).
!                                                        
!  MODIFICATION HISTORY:                                 
!    Version 1.01
!  08.08.2008 removed a bug in longi = atan(s2/s1) statement
!    Copyright(c) EUMETSAT 2005, 2009
!                                                        
!                                                        
!  INPUT:                                                
!    row   (int) row-value of the pixel
!    colum (int) columb-value of the pixel
!    ccoff (int) coefficient of the scalling function    
!                (see page 28, Ref [1])                  
!                NOTE: since Fortran cannot distinguish between 
!                      upper case and lower case letters the name
!                      "ccoff" is used rather than "COFF" to distinguish
!                      between them.
!    lloff (int) coefficient of the scalling function    
!                (see page 28, Ref [1])                  
!                NOTE: since Fortran cannot distinguish between 
!                      upper case and lower case letters the name
!                      "ccoff" is used rather than "COFF" to distinguish
!                      between them.
!                                                        
!  OUTPUT:                                               
!    latitude (double) geographic Latitude of the wanted pixel [Degrees]
!    longitude (double) geographic Longitude of the wanted pixel [Degrees]
!                                                        
!                                                        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  USE defMSGProject

  IMPLICIT NONE

  INTEGER, INTENT (IN) :: column, row, ccoff, lloff
  REAL(KIND(0.0d0)) , INTENT (OUT) :: latitude, longitude

  
  REAL(KIND(0.0d0)) :: s1, s2, s3, sn, sd, sxy
  REAL(KIND(0.0d0)) :: x, y, xx, yy
  REAL(KIND(0.0d0)) :: longi, lati
  REAL(KIND(0.0d0)) :: lat
  REAL(KIND(0.0d0)) :: lon


  REAL(KIND(0.0d0)) :: a, b, sa

  INTEGER ::  c, l


  c=column
  l=row

  !  calculate viewing angle of the satellite by use of the equation 
  !  on page 28, Ref [1]. 

  x = (2.0d0**16.0d0 * ( DBLE(c) - DBLE(ccoff) )) / DBLE(CFAC)
  y = (2.0d0**16.0d0 * ( DBLE(l) - DBLE(lloff) )) / DBLE(LFAC)


  !  now calculate the inverse projection using equations on page 25, Ref. [1]  

  !  first check for visibility, whether the pixel is located on the Earth 
  !  surface or in space. 
  !  To do this calculate the argument to sqrt of "sd", which is named "sa". 
  !  If it is negative then the sqrt will return NaN and the pixel will be 
  !  located in space, otherwise all is fine and the pixel is located on the 
  !  Earth surface.

  sa =  (SAT_HEIGHT * cos(x) * cos(y) )**2 - (cos(y)*cos(y) + 1.006803d0 * sin(y)*sin(y)) * 1737121856.0d0

  ! take care if the pixel is in space, that an error code will be returned
  if ( sa .LE. 0.0 ) then
     latitude = -999.999
     longitude = -999.999
     return 
  end if

  ! now calculate the rest of the formulas using eq. on page 25 Ref [1]

  sd = sqrt( (SAT_HEIGHT * cos(x) * cos(y) )**2	- (cos(y)*cos(y) + 1.006803d0 * sin(y)*sin(y)) * 1737121856.0d0 )
  sn = (SAT_HEIGHT * cos(x) * cos(y) - sd) / ( cos(y)*cos(y) + 1.006803d0 * sin(y)*sin(y) ) 
  
  s1 = SAT_HEIGHT - sn * cos(x) * cos(y)
  s2 = sn * sin(x) * cos(y)
  s3 = -sn * sin(y)

  sxy = sqrt( s1*s1 + s2*s2 )

  ! using the previous calculations now the inverse projection can be
  ! calculated, which means calculating the lat./long. from the pixel
  ! row and column by equations on page 25, Ref [1].

  longi = atan(s2/s1) + SUB_LON
  lati  = atan((1.006803d0*s3)/sxy)
  ! convert from radians into degrees
  latitude = lati*180.0d0/PI
  longitude = longi*180.0d0/PI


END SUBROUTINE pixcoord2geocoord
