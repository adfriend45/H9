!======================================================================!
MODULE CONSTANTS
!----------------------------------------------------------------------!
! All constants in H9: physical, biological, and computational.
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
INTEGER, PARAMETER :: EDPERY =   365 ! Earth days per year     (days/yr)
INTEGER, PARAMETER :: HPERED =    24 ! Hours per Earth day          (hr)
INTEGER, PARAMETER :: SPERED = 86400 ! Seconds per Earth day         (s)
INTEGER, PARAMETER :: JMPERED =   12 ! Julian months per ED     (months)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Last Julian day in each month.
!----------------------------------------------------------------------!
INTEGER, PARAMETER :: JDENDOFM (0:JMPERED) = &
& (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! N.B. Epsilon needs verification.
REAL, PARAMETER :: EPS       = 1.0E-8 ! Epsilon for FPops            (x)
REAL, PARAMETER :: PI        = 3.1416 ! Ratio of circ' to diam'  (ratio)
REAL, PARAMETER :: KGCPERMOL =  0.012 ! Kg C per mol C          (kg/mol)
REAL, PARAMETER :: FORMF     =    1.0 ! Tree form factor      (unitless)
REAL, PARAMETER :: SIGC      =  350.0 ! Wood density           (kgC/m^3)
REAL, PARAMETER :: SIGAF     =   0.28 ! Foliage area density   (m^2/m^3)
!----------------------------------------------------------------------!
! Height allometry from Bartelink (1997).
!----------------------------------------------------------------------!
REAL, PARAMETER :: alpha     =  101.9 ! Height allometery     (unitless)
REAL, PARAMETER :: beta      =  0.769 ! Height allometery     (unitless)
!----------------------------------------------------------------------!
! Mean value from Bartelink (1997).
!----------------------------------------------------------------------!
REAL, PARAMETER :: SLA       =   17.2 ! Specific leaf area  (m^2/kg[DM])
!----------------------------------------------------------------------!
! Crown/stem diameter ratio fitted to Bartelink (1997), using his
! la = f (Acrown) relationship, inverted, and fitted by eye.
!----------------------------------------------------------------------!
REAL, PARAMETER :: b_cd      =   29.0 ! Crown area allometry     (m^2/m)
!----------------------------------------------------------------------!
! Foliage/sapwood area ratio from Bartlink (1997)              (m^2/m^2)
!----------------------------------------------------------------------!
REAL, PARAMETER :: FASA      = 3310.0
!----------------------------------------------------------------------!
REAL, PARAMETER :: Aplot     =  200.0 ! Plot area                  (m^2)
REAL, PARAMETER :: BG_MAX    =    0.1 ! Max. rate branch growth   (m/yr)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END MODULE CONSTANTS
!======================================================================!
