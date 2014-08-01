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
INTEGER :: ih,cd,I,top,bot,L
REAL :: iPAR,lLAI
!----------------------------------------------------------------------!

fad (:) = 0.0
top = 0
bot = 1100
DO KI = 1, NIND

  ! Assume all canopies are 1 m deep if tall enough.
  ! Height to nearest 1 cm, rounding up.
  ih = CEILING (100.0 * H (KI))
  IF (ih > top) top = ih
  
  ! Base of crown to nearest 1 cm, rounding down.
  !ib (KI)= MAX (FLOOR (100.0 * (H (KI) - 1.0)), 0)
  !IF (ib (KI) < bot) bot = ib (KI)
  bot = ib (KI)
  
  ! Canopy depth (cm).
  cd = ih - ib (KI)
  
  ! Populate foliage area density with this tree's foliage area.
  DO I = ib (KI)+1, ih
    fad (I) = fad (I) + (Afoliage (KI) / Aplot) / FLOAT (cd)
  END DO

END DO ! KI = 1, NIND

! Cumulative foliage area down through plot.
cad (:) = 0.0
cad (top) = fad (top)
DO L = top-1, bot+1, -1
  cad (L) = cad (L+1) + fad (L)
END DO

! Relative PAR profile.
rPAR (:) = 0.0
rPAR (top) = 1.0
DO L = top-1, bot+1, -1
  rPAR (L) = EXP (-0.5 * cad (L))
END DO

! Top point in crown with negative contribution.
DO KI = 1, NIND
  rPAR_base (KI) = 0
  DO L = bot+1, top
    IF (rPAR (L) < 0.03) rPAR_base (KI) = L
  END DO
END DO

! fPAR for each tree. That is, the fraction of incident light absorbed by each tree in
! the plot.
fPAR (:) = 0.0
LAI = 0.0
DO KI = 1, NIND
  LAI = LAI + Afoliage(KI)/Aplot
  ih = CEILING (100.0 * H (KI))
  ! Canopy depth (cm).
  cd = ih - ib (KI)
  write (*,*) ib(KI),ih
  ! LAI in each layer from current tree.
  lLAI = (Afoliage (KI) / Aplot) / FLOAT (cd)
  DO L = ih, ib(KI)+1, -1
    iPAR = EXP (-0.5 * cad (L+1))
    fPAR (KI) = fPAR (KI) + iPAR * (1.0 - EXP (-0.5 * lLAI))
  END DO
  write (*,*) Afoliage(KI)/Aplot,fPAR(KI),H(KI)
END DO
write (*,*) 'total LAI  = ',LAI
write (*,*) 'total fPAR = ',1.0-EXP(-0.5*LAI)
write(*,*)
!stop

!----------------------------------------------------------------------!
!PAR_base = EXP (-0.5 * LAI)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
