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
!INTEGER :: base ! PAR-limited height to base of crown               (cm) !TTR This is not used anymore currently
REAL    :: flap,LAIc
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
  ! Potential sapwood area                                         (m^2)
  !--------------------------------------------------------------------!
  Asapwood = PI * r (KI) ** 2 - Aheart (KI)
  Asapwood = MAX (0.0,Asapwood)
  !--------------------------------------------------------------------!
  ! Potential foliage area                                         (m^2)
  !--------------------------------------------------------------------!
  Afoliage (KI) = FASA * Asapwood
  !--------------------------------------------------------------------!
  ! Stem diameter                                                    (m)
  !--------------------------------------------------------------------!
  D = 2.0 * r (KI)
  !--------------------------------------------------------------------!
  ! Potential crown diameter                                         (m)
  !--------------------------------------------------------------------!
  Dcrown = a_cd + b_cd * D
  !--------------------------------------------------------------------!
  ! Potential crown area                                           (m^2)
  !--------------------------------------------------------------------!
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
  ! Tree height                                                      (m)
  !--------------------------------------------------------------------!
  H (KI) = alpha * r (KI) ** beta
  !--------------------------------------------------------------------!
  ! Tree height as integer                                    (DZ_CROWN)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (H (KI) / DZ_CROWN_M)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_potential_crowns
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Sum crown areas of each tree in each layer to compute space
! constraints.
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
INDIVIDUALS_layers: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! ib index starts at 0, so assume ib+1 is lowest layer.
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_layers
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
INDIVIDUALS_squeeze: DO I = 1, NIND_alive
!----------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Crown too big? Look only at top of crown so tallest tree does not
  ! have to shrink unless it is too big for plot itself.
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
  ! Height to base of crown from foliage area                       (cm)
  !--------------------------------------------------------------------!
  ib (KI) = ih (KI) - NINT (Afoliage (KI) / (SIGAF * Acrown (KI) * &
  &         DZ_CROWN_M))
  !--------------------------------------------------------------------!
  ! Reduce foliage area and increase heartwood area if insufficient
  ! canopy volume for sapwood-area controlled foliage area.
  !--------------------------------------------------------------------!
  IF (ib (KI) < 0) THEN
    ib (KI) = 0
    Afoliage (KI) = SIGAF * Acrown (KI) * FLOAT (ih (KI)) * DZ_CROWN_M
  ENDIF
  !--------------------------------------------------------------------!
  ! Tree crown LAI                                             (m^2/m^2)
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!

!----------------------------------------------------------------------!
END DO INDIVIDUALS_squeeze
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!
