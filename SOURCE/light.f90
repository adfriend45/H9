!======================================================================!
SUBROUTINE light
!----------------------------------------------------------------------!
! Compute fractional absorbed light incident and absorbed by each tree
! crown (i.e. iPAR and fPAR).
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
INTEGER :: KJ
REAL :: LAI_above,space !TTR deleted shade as it is now an array with a value for each tree
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Go through individuals to calculate fPAR.
!----------------------------------------------------------------------!
Acrowns_above (:) = 0.0
Afoliage_above (:) = 0.0
!----------------------------------------------------------------------!
INDIVIDUALS_areas_above: DO KI = 1, NIND_alive
  INDIVIDUALS_above: DO KJ = 1, NIND_alive
    DIFFERENT: IF (KJ .NE. KI) THEN
      TALLER: IF (ih (KJ) > ih(KI)) THEN
        Acrowns_above  (KI) = Acrowns_above  (KI) + Acrown   (KJ)
        Afoliage_above (KI) = Afoliage_above (KI) + Afoliage (KJ)
      END IF TALLER
    END IF DIFFERENT
  END DO INDIVIDUALS_above
  !--------------------------------------------------------------------!
  ! Open horizontal area above available for individual KI.
  !--------------------------------------------------------------------!
  space = MAX (0.0,Aplot-Acrowns_above(KI))
  !--------------------------------------------------------------------!
  ! Fraction of crown shaded (fraction).
  !--------------------------------------------------------------------!
  SHADE_fraction: IF (Acrown (KI) <= space) THEN
    shade (KI) = 0.0
  ELSE
    shade (KI) = (Acrown (KI) - space) / Acrown (KI)
  END IF SHADE_fraction
  !--------------------------------------------------------------------!
  ! Mean LAI of canopies above tree                            (m^2/m^2)
  !--------------------------------------------------------------------!
  LAI_above = Afoliage_above (KI) / Aplot
  !--------------------------------------------------------------------!
  ! Mean iPAR at top of tree                                  (fraction)
  !--------------------------------------------------------------------!
  iPAR (KI) = (1.0 - shade (KI)) + shade (KI) * EXP (-0.5 * LAI_above)
  !--------------------------------------------------------------------!
  ! fPAR of tree                                              (fraction)
  !--------------------------------------------------------------------!
  fPAR (KI) = iPAR (KI) * (1.0 - EXP (-0.5 * LAIcrown (KI)))
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_areas_above ! KI = 1, NIND_alive

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
