!======================================================================!
MODULE CONSTANTS
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
INTEGER, PARAMETER :: EDPERY =   365 ! Earth days per year     (days/yr)
INTEGER, PARAMETER :: HPERED =    24 ! Hours per Earth day          (hr)
INTEGER, PARAMETER :: SPERED = 86400 ! Seconds per Earth day         (s)
INTEGER, PARAMETER :: JMPERED =   12 ! Julian months per ED     (months)

!----------------------------------------------------------------------!
! Last Julian day in each month.
!----------------------------------------------------------------------!
INTEGER, PARAMETER :: JDENDOFM (0:JMPERED) = &
& (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
REAL, PARAMETER :: EPS       = 1.0E-8 ! Epsilon for FPops            (x)
REAL, PARAMETER :: PI        = 3.1416 ! Ratio of circ to diam'   (ratio)
REAL, PARAMETER :: KGCPERMOL =  0.012 ! Kg C per mol C          (kg/mol)
REAL, PARAMETER :: FORMF     =    1.0 ! Stem form factor      (unitless)
REAL, PARAMETER :: SIGC      =  350.0 ! Stem wood density      (kgC/m^3)
REAL, PARAMETER :: alpha     =   59.8 ! Height allometery     (unitless)
REAL, PARAMETER :: beta      =  0.769 ! Height allometery     (unitless)
REAL, PARAMETER :: a_cd      =   1.13 ! Crown area allometry       (m^2)
REAL, PARAMETER :: b_cd      =   15.2 ! Crown area allometry     (m^2/m)
REAL, PARAMETER :: FASA      = 1000.0 ! Foliage/sapwood area   (m^2/m^2)
REAL, PARAMETER :: Aplot     =  200.0 ! Plot area                  (m^2)
!----------------------------------------------------------------------!
END MODULE CONSTANTS
!======================================================================!
