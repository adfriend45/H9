!======================================================================!
SUBROUTINE light
!----------------------------------------------------------------------!
! Compute light profile in plot in 10-cm deep layers. !TTR Why 10 cm?
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
INTEGER :: KJ       ! Current individual (loop variable) for comparison 
REAL :: iPAR        ! Fraction of PAR incident at top of tree (?)
REAL :: shade       ! Fraction of individual crown area being shaded
REAL :: LAI_above   ! Summed LAI of all taller trees (m^2 m^-2)
REAL :: space       ! Open space in above canopy (m^2)
!----------------------------------------------------------------------!

! Put crown and leaf areas of each tree in each layer.
Acrown_layer   (:,:) = 0.0
Afoliage_layer (:,:) = 0.0
DO KI = 1, NIND
  ih (KI) = CEILING (100.0 * H (KI)) 
  Acrown_layer   (ib(KI)+1:ih(KI),KI) = Acrown   (KI) ! TTR Can't this cause double accounting 
  Afoliage_layer (ib(KI)+1:ih(KI),KI) = Afoliage (KI) ! TTR ibid.
END DO ! KI = 1, NIND

! Go through individuals to see if shaded.
Acrowns_above (:) = 0.0
Afoliage_above (:) = 0.0
DO KI = 1, NIND
  DO KJ = 1, NIND
    IF (KJ .NE. KI) THEN
      IF (ih (KJ) > ih(KI)) THEN
        Acrowns_above  (KI) = Acrowns_above  (KI) + Acrown   (KJ) 
        Afoliage_above (KI) = Afoliage_above (KI) + Afoliage (KJ) 
      END IF
    END IF
  END DO
  space = MAX (0.0,Aplot-Acrowns_above(KI))
  IF (Acrown (KI) <= space) THEN
    shade = 0.0
  ELSE
    ! Fraction of crown shaded.
    shade = (Acrown (KI) - space) / Acrown (KI)
  END IF
  LAI_above = Afoliage_above (KI) / (Acrowns_above (KI) + EPS)
  ! Mean iPAR at top of this tree.
  iPAR = (1.0 - shade) + shade * EXP (-0.5 * LAI_above) 
  ! fPAR of this tree.
  fPAR (KI) = iPAR * (1.0 - EXP (-0.5 * LAIcrown (KI)))
  write (*,*) KI,iPAR,fPAR(KI),shade,LAIcrown (KI),H(KI)
END DO ! KI = 1, NIND

! Top point in each crown with negative contribution.
DO KI = 1, NIND
  !rPAR_base (KI) = 0
  rPAR_base (KI) = 110000
!  DO L = ib(KI)+1, ih
!    IF (rPAR (L) < 0.03) rPAR_base (KI) = L
!  END DO
END DO

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
