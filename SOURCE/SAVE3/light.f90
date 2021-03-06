!======================================================================!
SUBROUTINE light
!----------------------------------------------------------------------!
! Compute fractional absorbed light incident and absorbed by each tree
! crown (i.e. iPAR and fPAR). fPAR is the fraction of total PAR
! incident on the plot that is absorbed by the individual. Hence:
! PAR absorbed by individual = fPAR * PAR incident on plot.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
INTEGER :: KJ
REAL :: LAI_above,space,frac,LAI_above_base
!----------------------------------------------------------------------!

! Now allowing crown overlap within crowns, maybe better to calculate
! fPAR of whole plot and then divide this into trees, somehow.
! By all layers?
! For LAI:
! Instead of iPAR, use the PAR irradiance at the bottom of the crown
! in the absence of that individual to see how much more foliage
! could be added?

!----------------------------------------------------------------------!
! Go through individuals to calculate fPAR.
!----------------------------------------------------------------------!
Acrowns_above  (:) = 0.0
Afoliage_above (:) = 0.0
!----------------------------------------------------------------------!
individuals_areas_above : DO I = 1, NIND_alive
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Include own leaf area for light at base of crown.
  !--------------------------------------------------------------------!
  Afoliage_above_base (KI) = Afoliage (KI)
  !--------------------------------------------------------------------!
  individuals_above : DO J = 1, NIND_alive
  !--------------------------------------------------------------------!
    KJ = LIVING (J)
    different : IF (KJ .NE. KI) THEN
      !----------------------------------------------------------------!
      taller : IF (ih (KJ) > ih (KI)) THEN
      !----------------------------------------------------------------!
        Acrowns_above  (KI) = Acrowns_above (KI) + Acrown   (KJ)
        IF (ib (KJ) < ih (KI)) THEN ! Overlap.
          !------------------------------------------------------------!
          ! frac is fraction of taller tree's crown that is above
          ! tree KI                                           (fraction)
          !------------------------------------------------------------!
          frac = (FLOAT (ih (KJ)) - FLOAT (ih (KI))) / &
          &      (FLOAT (ih (KJ)) - FLOAT (ib (KJ)))
          !------------------------------------------------------------!
        ELSE
          !------------------------------------------------------------!
          ! No overlap.
          !------------------------------------------------------------!
          frac = 1.0
          !------------------------------------------------------------!
        END IF
        Afoliage_above (KI) = Afoliage_above (KI) + frac * Afoliage (KJ)
      END IF taller
      !----------------------------------------------------------------!
      ! Now do foliage above base of crown.
      !----------------------------------------------------------------!
      above_base : IF (ih (KJ) > ib (KI)) THEN
        IF (ib (KJ) < ib (KI)) THEN ! Some not included.
          frac = (FLOAT (ih (KJ)) - FLOAT (ib (KI))) / &
          &      (FLOAT (ih (KJ)) - FLOAT (ib (KJ)))
        ELSE
          frac = 1.0
        END IF
        Afoliage_above_base (KI) = Afoliage_above_base (KI) + frac * &
        &                          Afoliage (KJ)
      END IF above_base
      !----------------------------------------------------------------!
      
    END IF different
  END DO individuals_above
  !--------------------------------------------------------------------!
  ! Open horizontal area above available for individual KI.
  !--------------------------------------------------------------------!
  space = MAX (0.0, Aplot - Acrowns_above (KI))
  !--------------------------------------------------------------------!
  ! Fraction of crown shaded (fraction).
  !--------------------------------------------------------------------!
  shade_fraction: IF (Acrown (KI) <= space) THEN
    !------------------------------------------------------------------!
    ! Crown fits in gap, so assume KI not shaded at all.
    !------------------------------------------------------------------!
    shade (KI) = 0.0
    !------------------------------------------------------------------!
  ELSE
    !------------------------------------------------------------------!
    ! At least some taller trees overlap KI.
    !------------------------------------------------------------------!
    shade (KI) = (Acrown (KI) - space) / Acrown (KI)
    !------------------------------------------------------------------!
  END IF shade_fraction
  !--------------------------------------------------------------------!
  ! Mean LAI of canopies above tree                            (m^2/m^2)
  !--------------------------------------------------------------------!
  LAI_above = Afoliage_above (KI) / Aplot
  !--------------------------------------------------------------------!
  ! Mean iPAR at top of tree                                  (fraction)
  !--------------------------------------------------------------------!
  iPAR (KI) = (1.0 - shade (KI)) + shade (KI) * EXP (-0.5 * LAI_above)
  !--------------------------------------------------------------------!
  ! Mean LAI of canopies above base of tree                    (m^2/m^2)
  !--------------------------------------------------------------------!
  LAI_above_base = Afoliage_above_base (KI) / Aplot
  !--------------------------------------------------------------------!
  ! Mean iPAR at the bottom of tree for hbc.
  !--------------------------------------------------------------------!
  iPAR_base (KI) = EXP (-0.5 * LAI_above_base)
  !--------------------------------------------------------------------!
  ! fPAR of tree                                              (fraction)
  !--------------------------------------------------------------------!
  fPAR (KI) = iPAR (KI) * (1.0 - EXP (-0.5 * LAIcrown (KI)))
  !--------------------------------------------------------------------!
END DO individuals_areas_above ! I = 1, NIND_alive

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
