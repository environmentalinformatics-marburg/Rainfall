
SUBROUTINE geocoord2pixcoord( latitude,  longitude,  ccoff,  lloff,  column, row)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! subroutine geocoord2pixcoord                                    
!                                                       
! PURPOSE:                                              
!   return the pixel column and line of an MSG image 
!   for a given pair of geographic latitude/longitude.                   
!   (based on the formulas given in Ref. [1])                
!                                                       
!                                                       
! DEPENDENCIES:                                         
!   none                                       
!                                                       
!                                                       
! REFERENCE:                                            
! [1] LRIT/HRIT Global Specification                     
!     (CGMS 03, Issue 2.6, 12.08.1999)                  
!     for the parameters used in the program.
! [2] MSG Ground Segment LRIT/HRIT Mission Specific 
!     Implementation, EUMETSAT Document, 
!     (EUM/MSG/SPE/057, Issue 6, 21. June 2006).
!                                                       
!                                                       
! MODIFICATION HISTORY:
!   Version 1.01
!    Copyright(c) EUMETSAT 2005, 2009
!                                                       
!                                                       
! INPUT:                                                
!   latitude  (double) geographic Latitude of a point [Degrees] 
!   longitude (double) geographic Longitude of a point [Degrees]
!   ccoff (int)        coefficient of the scalling function   
!                      (see page 28, Ref [1])                 
!                      NOTE: since Fortran cannot distinguish between 
!                            upper case and lower case letters the name
!                            "ccoff" is used rather than "COFF" 
!                            to distinguish between them.
!                   
!   lloff (int)        coefficient of the scalling function   
!                      (see page 28, Ref [1])                 
!                      NOTE: since Fortran cannot distinguish between 
!                            upper case and lower case letters the name
!                            "lloff" is used rather than "LOFF" 
!                            to distinguish between them.
!                                                       
!                                                       
! OUTPUT:                                               
!   row    (int)       row-value of the pixel
!   column (int)       column-value of the pixel
!                                                       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  USE defMSGProject

  IMPLICIT NONE


  REAL(KIND(0.0d0)), INTENT (IN) :: latitude, longitude
  INTEGER, INTENT (IN)  :: ccoff, lloff
  INTEGER, INTENT (OUT) :: column, row
  

  INTEGER :: c=0, l=0
  INTEGER :: ccc=0, lll=0
  INTEGER :: x=0, y=0

  REAL(KIND(0.0d0)) :: lati, longi
  REAL(KIND(0.0d0)) :: c_lat
  REAL(KIND(0.0d0)) :: lat
  REAL(KIND(0.0d0)) :: lon
  REAL(KIND(0.0d0)) :: r1, r2, r3, rn, re, rl
  REAL(KIND(0.0d0)) :: xx, yy, sa
  REAL(KIND(0.0d0)) :: cc, ll
  REAL(KIND(0.0d0)) :: dotprod


  lati= latitude
  longi= longitude

  ! check if the values are sane, otherwise return error value
  if (lati .LT. -90.0d0 .OR. lati .GT. 90.0d0 .OR. longi .LT. -180.0d0 .OR. longi .GT. 180.0d0 ) then
     row = -999
     column = -999
     return
  end if

  ! convert them to radians 
  lat = lati*PI / 180.0d0
  lon = longi *PI / 180.0d0


  ! calculate the geocentric latitude from the       
  ! geographic one using equations on page 24, Ref. [1] 

  c_lat = atan ( (0.993243d0*(sin(lat)/cos(lat)) ))
      

  ! using c_lat calculate the length from the Earth 
  ! centre to the surface of the Earth ellipsoid    
  ! equations on page 23, Ref [1]                      
  
  re = R_POL / sqrt( (1.0d0 - 0.00675701d0 * cos(c_lat) * cos(c_lat) ) )


  ! calculate the forward projection using equations on page 24, Ref. [1]

  rl = re
  r1 = SAT_HEIGHT - rl * cos(c_lat) * cos(lon - SUB_LON)
  r2 = - rl *  cos(c_lat) * sin(lon - SUB_LON)
  r3 = rl * sin(c_lat)
  rn = sqrt( r1*r1 + r2*r2 +r3*r3 )

  ! check for visibility, whether the point on the Earth given by the
  ! latitude/longitude pair is visible from the satellte or not. This 
  ! is given by the dot product between the vectors of:
  ! 1) the point to the spacecraft,
  ! 2) the point to the centre of the Earth.
  ! If the dot product is positive the point is visible otherwise it
  ! is invisible.

  dotprod = r1*(rl * cos(c_lat) * cos(lon - SUB_LON)) - r2*r2 - r3*r3*((r_EQ/R_POL)**2)

  if (dotprod .LE. 0.0d0 ) then
     column = -999
     row = -999
     return
  end if

  ! the forward projection is x and y 

  xx = atan( (-r2/r1) )
  yy = asin( (-r3/rn) )


  ! convert to pixel column and row using the scaling functions on 
  ! page 28, Ref. [1]. And finding nearest integer value for them. 

  cc = DBLE(ccoff) + xx *  2.0d0**(-16.0d0) * DBLE(CFAC)
  ll = DBLE(lloff) + yy *  2.0d0**(-16.0d0) * DBLE(LFAC)


  ccc=nint(cc)
  lll=nint(ll)		

  column = ccc
  row = lll

      

END SUBROUTINE geocoord2pixcoord
