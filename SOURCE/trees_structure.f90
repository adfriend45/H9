!======================================================================!
SUBROUTINE trees_structure
!----------------------------------------------------------------------!
! Compute tree and canopy structures taking into account control of
! radiation on foliage areas and space on crown areas.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!
REAL :: flap,LAIc
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Increase heartwood area if LAIcrown was too high for iPAR.
!----------------------------------------------------------------------!
INDIVIDUALS_LAI_constraint: DO I = 1, NIND_alive
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Allowable crown LAI based on incident PAR                  (m^2/m^2)
  !--------------------------------------------------------------------!
  LAIc = LOG (0.03 / (iPAR (KI) + EPS)) / (-0.5)
  !--------------------------------------------------------------------!
  ! Fraction of sapwood to convert to heartwood               (fraction)
  !--------------------------------------------------------------------!
  floss = 1.0 - LAIc / (LAIcrown (KI) + EPS)
  !--------------------------------------------------------------------!
  IF (floss > 0.0) THEN
    !------------------------------------------------------------------!
    floss = MIN (floss, 1.0)
    !------------------------------------------------------------------!
    ! Increase heartwood area                                      (m^2)
    !------------------------------------------------------------------!
    Aheart (KI) = Aheart (KI) + floss * Afoliage (KI) / FASA
    !------------------------------------------------------------------!
    ! Increase height to base of crown to maintain foliage area
    ! vertical density.                                             (cm)
    !------------------------------------------------------------------!
    base = FLOOR (floss * (100.0 * H (KI) - &
           FLOAT (ib (KI)))) + ib (KI)
    base = MIN (CEILING(100.0*H(KI)),base)
    ib (KI) = MAX (ib(KI),base)
    ib (KI) = MIN (ib(KI),ih(KI)-2)
    !------------------------------------------------------------------!
  END IF
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_LAI_constraint
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! New structure for each tree based on new Cv, giving new height,
! potential crown area, and actual foliage area.
!----------------------------------------------------------------------!
INDIVIDUALS_potential_crowns: DO I = 1, NIND_alive
  KI = LIVING (I)
!----------------------------------------------------------------------!
  ! Stem volume                                                    (m^3)
  !--------------------------------------------------------------------!
  V = Cv (KI) / SIGC
  !--------------------------------------------------------------------!
  ! Stem radius                                                      (m)
  !--------------------------------------------------------------------!
  r (KI) = (V / (( FORMF / 3.0) * PI * alpha)) ** (1.0 / (2.0 + beta))
  !--------------------------------------------------------------------!
  ! Sapwood area                                                   (m^2)
  !--------------------------------------------------------------------!
  Asapwood = PI * r (KI) ** 2 - Aheart (KI)
  Asapwood = MAX (0.0,Asapwood)
  !--------------------------------------------------------------------!
  ! Foliage area                                                   (m^2)
  !--------------------------------------------------------------------!
  Afoliage (KI) = FASA * Asapwood
  !--------------------------------------------------------------------!
  ! Stem diameter                                                    (m)
  !--------------------------------------------------------------------!
  D = 2.0 * r (KI)
  !--------------------------------------------------------------------!
  ! Tree height                                                      (m)
  !--------------------------------------------------------------------!
  H (KI) = alpha * r (KI) ** beta
  !--------------------------------------------------------------------!
  ! Tree height as integer                                          (cm)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (100.0 * H (KI))
  !ib (KI) = MIN (ib(KI),ih(KI)-2)
  !--------------------------------------------------------------------!
  ! Potential crown diameter                                         (m)
  !--------------------------------------------------------------------!
  Dcrown = a_cd + b_cd * D
  !--------------------------------------------------------------------!
  ! Potential crown area                                           (m^2)
  !--------------------------------------------------------------------!
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_potential_crowns
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Sum crown areas of each tree in each layer to compute space
! constraints.
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
DO I = 1, NIND_alive
  KI = LIVING (I)
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
!DO KI = 1, NIND_alive
DO I = 1, NIND_alive
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Crown too big? Look only at top of crown so tallest tree does not
  ! have to shrink.
  !--------------------------------------------------------------------!
  IF (Acrowns_layers (ih (KI)) > Aplot) THEN
    !------------------------------------------------------------------!
    ! Total overlap as a fraction of plot area.
    !------------------------------------------------------------------!
    flap = Acrowns_layers (ih (KI)) / Aplot
    !------------------------------------------------------------------!
    ! Reduce all crowns that contribute to this overlap by the fraction
    ! of overlap in the plot. Does this work OK?
    !------------------------------------------------------------------!
    Acrown (KI) = Acrown (KI) / flap
    !------------------------------------------------------------------!
  END IF
  !--------------------------------------------------------------------!
  ! Tree crown LAI                                             (m^2/m^2)
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!
