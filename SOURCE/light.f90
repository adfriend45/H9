!======================================================================!
SUBROUTINE light
!----------------------------------------------------------------------!
! Compute fractional absorbed light of each tree.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
INTEGER :: KJ
REAL :: shade,LAI_above,space,LAIc
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Put crown and leaf areas of each tree in each layer.
!----------------------------------------------------------------------!
Acrown_layer   (:,:) = 0.0
Afoliage_layer (:,:) = 0.0
!----------------------------------------------------------------------!
INDIVIDUALS_areas: DO KI = 1, NIND
  !--------------------------------------------------------------------!
  ! Tree height (cm).
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (100.0 * H (KI))
  !--------------------------------------------------------------------!
  Acrown_layer   (ib(KI)+1:ih(KI),KI) = Acrown   (KI)
  Afoliage_layer (ib(KI)+1:ih(KI),KI) = Afoliage (KI)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_Areas ! KI = 1, NIND
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Go through individuals to calculate fPAR.
!----------------------------------------------------------------------!
Acrowns_above (:) = 0.0
Afoliage_above (:) = 0.0
!----------------------------------------------------------------------!
INDIVIDUALS_areas_above: DO KI = 1, NIND
  INDIVIDUALS_above: DO KJ = 1, NIND
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
    shade = 0.0
  ELSE
    shade = (Acrown (KI) - space) / Acrown (KI)
  END IF SHADE_fraction
  !--------------------------------------------------------------------!
  ! Mean LAI of canopies above tree                            (m^2/m^2)
  !--------------------------------------------------------------------!
  LAI_above = Afoliage_above (KI) / (Acrowns_above (KI) + EPS)
  !--------------------------------------------------------------------!
  ! Mean iPAR at top of tree                                  (fraction)
  !--------------------------------------------------------------------!
  iPAR (KI) = (1.0 - shade) + shade * EXP (-0.5 * LAI_above)
  !--------------------------------------------------------------------!
  ! fPAR of tree                                              (fraction)
  !--------------------------------------------------------------------!
  fPAR (KI) = iPAR (KI) * (1.0 - EXP (-0.5 * LAIcrown (KI)))
  !--------------------------------------------------------------------!
  !write (*,*) KI,iPAR(KI),fPAR(KI),shade,LAIcrown(KI),H(KI)
END DO INDIVIDUALS_areas_above ! KI = 1, NIND

! Fraction of foliage area below compensation point.
DO KI = 1, NIND
  LAIc = LOG (0.03 / (iPAR (KI) + EPS)) / (-0.5)
  floss (KI) = 1.0 - LAIc / LAIcrown (KI)
  floss (KI) = MAX (floss (KI), 0.0)
  floss (KI) = MIN (floss (KI), 1.0)
  write (*,'(i3,5f10.2)') KI,LAIcrown(KI),floss(KI),H(KI),Acrown(KI),Acrowns_above(KI)
END DO

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
