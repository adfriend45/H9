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
  !WRITE (*,*) L, cad (L), rPAR (L)
END DO
!WRITE (*,*)

! Top point in crown with negative contribution.
DO KI = 1, NIND
  rPAR_base (KI) = 0
  DO L = bot+1, top
    IF (rPAR (L) < 0.03) rPAR_base (KI) = L
  END DO
  !write (*,*) ki,bot,rpar_base(KI),top
END DO

!----------------------------------------------------------------------!
!PAR_base = EXP (-0.5 * LAI)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
