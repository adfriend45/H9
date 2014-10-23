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
REAL :: LAI_above,space,frac
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
INDIVIDUALS_areas_above: DO I = 1, NIND_alive
  KI = LIVING (I)
  INDIVIDUALS_above: DO J = 1, NIND_alive
    KJ = LIVING (J)
    DIFFERENT: IF (KJ .NE. KI) THEN
      ! Float h(KI) to increase accuracy?
      TALLER: IF (ih (KJ) > ih (KI)) THEN
        Acrowns_above  (KI) = Acrowns_above  (KI) + Acrown   (KJ)
        IF (ib (KJ) < ih (KI)) THEN ! Overlap.
          frac = (FLOAT (ih (KJ)) - FLOAT (ih (KI))) / &
          &      (FLOAT (ih (KJ)) - FLOAT (ib (KJ)))
        ELSE ! No overlap.
          frac = 1.0
        END IF
        frac = 1.0
        Afoliage_above (KI) = Afoliage_above (KI) + frac * Afoliage (KJ)
        !Afoliage_above (KI) = Afoliage_above (KI) + Afoliage (KJ)
      END IF TALLER
    END IF DIFFERENT
  END DO INDIVIDUALS_above
  !--------------------------------------------------------------------!
  ! Open horizontal area above available for individual KI.
  !--------------------------------------------------------------------!
  space = MAX (0.0, Aplot - Acrowns_above (KI))
  !--------------------------------------------------------------------!
  ! Fraction of crown shaded (fraction).
  !--------------------------------------------------------------------!
  SHADE_fraction: IF (Acrown (KI) <= space) THEN
    ! Crown fits in gap, so assume KI not shaded at all.
    shade (KI) = 0.0
  ELSE
    ! At least some taller trees overlap KI.
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
END DO INDIVIDUALS_areas_above ! I = 1, NIND_alive

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
