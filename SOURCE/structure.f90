!======================================================================!
SUBROUTINE structure
!----------------------------------------------------------------------!
! Compute tree and canopy structures.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
INTEGER :: KJ
REAL :: shade,LAI_above,space,LAIc,flap
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! New structure for each tree based on new Cv, giving new height,
! potential crown area, and potential foliage area.
! Actual crown area at end of year may be less because of space
! constraints, and actual foliage area at end of year may be less
! because it is too high for iPAR.
!----------------------------------------------------------------------!
DO KI = 1, NIND_alive
  V = Cv (KI) / SIGC ! Stem volume                               (m^3)
  r (KI) = (V / (( FORMF / 3.0) * PI * alpha)) ** (1.0 / (2.0 + beta))
  Asapwood = PI * r (KI) ** 2 - Aheart (KI) ! Sapwood area       (m^2)
  Asapwood = MAX (0.0,Asapwood)
  D = 2.0 * r (KI)                       ! Stem diameter           (m)
  H (KI) = alpha * r (KI) ** beta        ! Stem height             (m)
  Dcrown = a_cd + b_cd * D          ! Crown diameter               (m)
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2 ! Crown area            (m^2)
  Afoliage (KI) = FASA * Asapwood   ! Foliage area               (m^2)
END DO
    
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
! Squeeze canopy areas into plot.
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
DO KI = 1, NIND
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
END DO
!----------------------------------------------------------------------!

DO KI = 1, NIND_alive
  ! Crown too big? If look at top of crown, tallest tree does not
  ! have to shrink.
  IF (Acrowns_layers(ih(KI)) > Aplot) THEN
    flap = Acrowns_layers(ih(KI)) / Aplot
    Acrown (KI) = Acrown (KI) / flap
  END IF
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE structure
!======================================================================!
