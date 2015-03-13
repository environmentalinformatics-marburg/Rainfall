MODULE defMSGProject
  ! define global parameters used in the routines as given in Ref. [1].
  ! to distinguish between them and other variables they are written
  ! in CAPITAL LETTERS.

  REAL(KIND(0.0d0)), PARAMETER :: PI=3.14159265359d0

  REAL(KIND(0.0d0)), PARAMETER ::  SAT_HEIGHT= 42164.0d0  ! distance from Earth centre to satellite    
  REAL(KIND(0.0d0)), PARAMETER ::  R_EQ = 6378.169d0      ! radius from Earth centre to equator
  REAL(KIND(0.0d0)), PARAMETER ::  R_POL= 6356.5838d0     !
  REAL(KIND(0.0d0)), PARAMETER ::  SUB_LON = 0.0d0        ! Longitude of Sub-Satellite Point in radiant

  INTEGER, PARAMETER ::  CFAC = -781648343 
  INTEGER, PARAMETER ::  LFAC = -781648343

  INTEGER, PARAMETER ::  COFF = 1856
  INTEGER, PARAMETER ::  LOFF = 1856

END MODULE defMSGProject
