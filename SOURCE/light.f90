!======================================================================!
SUBROUTINE light
!----------------------------------------------------------------------!
! Compute light profile in plot in 10-cm deep layers.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!
INTEGER :: ih,cd,L,top,bot
REAL :: iPAR,lLAI
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Plot foliage area density (m^2/m^2/cm).
!----------------------------------------------------------------------!
fad (:) = 0.0
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Height of canopy to nearest 1 cm.
!----------------------------------------------------------------------!
top = 1
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Height of base of canopy to nearest 1 cm.
!----------------------------------------------------------------------!
bot = 11000
!----------------------------------------------------------------------!

DO KI = 1, NIND

  ! Height of tree to nearest 1 cm, rounding up.
  ih = CEILING (100.0 * H (KI))
  
  ! Height of canopy to nearest 1 cm.
  top = MAX (top,ih)

  ! Height of base of canopy to nearest 1 cm.
  bot = MIN (bot,ib (KI))
      
  ! Crown depth (cm).
  cd = ih - ib (KI)
  
  ! Populate foliage area density with this tree's foliage area,
  ! assuming crown fills plot.
  ! L=1 refers to 0-1 cm layer.
  DO L = ib (KI)+1, ih
    !fad (L) = fad (L) + (Afoliage (KI) / Aplot) / FLOAT (cd)
    fad (L) = fad (L) + (Afoliage (KI) / (Acrown (KI) + EPS)) / FLOAT (cd)
  END DO

END DO ! KI = 1, NIND

! Cumulative foliage area down through plot.
! L=1 refers to 0-1 cm layer.
cad (:) = 0.0
DO L = top, 1, -1
  cad (L) = cad (L+1) + fad (L)
END DO

! Relative PAR profile (i.e. the relative PAR at the top of each layer).
rPAR (top:11000) = 1.0
DO L = top-1, 1, -1
  rPAR (L) = EXP (-0.5 * cad (L+1))
END DO

! fPAR for each tree. That is, the fraction of the incident light on the plot
! that is absorbed by each tree.
fPAR (:) = 0.0
DO KI = 1, NIND
  ih = CEILING (100.0 * H (KI))
  cd = ih - ib (KI)
  ! LAI in each layer from current tree.
  !lLAI = (Afoliage (KI) / Aplot) / FLOAT (cd)
  lLAI = (Afoliage (KI) / (Acrown (KI) + EPS)) / FLOAT (cd)
  DO L = ih, ib(KI)+1, -1
    fPAR (KI) = fPAR (KI) + rPAR (L) * (1.0 - EXP (-0.5 * lLAI))
  END DO
END DO

! Top point in crown with negative contribution.
!DO KI = 1, NIND
!  rPAR_base (KI) = 0
!  DO L = ib(KI)+1, ih
!    IF (rPAR (L) < 0.03) rPAR_base (KI) = L
!  END DO
!END DO

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
