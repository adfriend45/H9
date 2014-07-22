!======================================================================!
MODULE CONSTANTS
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
INTEGER, PARAMETER :: EDPERY =   365 ! Earth days per year     (days/yr)
INTEGER, PARAMETER :: HPERED =    24 ! Hours per Earth day          (hr)
INTEGER, PARAMETER :: SPERED = 86400 ! Seconds per Earth day         (s)
REAL, PARAMETER :: PI        = 3.1416 ! Ratio of circ to diam'   (ratio)
REAL, PARAMETER :: KGCPERMOL = 0.012  ! Kg C per mol C          (kg/mol)
REAL, PARAMETER :: FORMF     =   1.0  ! Stem form factor      (unitless)
REAL, PARAMETER :: SIGC      = 350.0  ! Stem wood density      (kgC/m^3)
REAL, PARAMETER :: alpha     = 59.8   ! Height allometery     (unitless)
REAL, PARAMETER :: beta      = 0.769  ! Height allometery     (unitless)
!----------------------------------------------------------------------!
END MODULE CONSTANTS
!======================================================================!
