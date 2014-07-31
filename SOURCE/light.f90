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
INTEGER :: ib,ih,cd,I
!----------------------------------------------------------------------!

fad (:) = 0.0

DO KI = 1, NIND

  ! Assume all canopies are 1 m deep if tall enough.
  ! Height to nearest 1 cm, rounding up.
  ih = CEILING (100.0 * H (KI))
  
  ! Base of crown to nearest 1 cm, rounding down.
  ib = MAX (FLOOR (100.0 * (H (KI) - 1.0)), 0)
  WRITE (*,*) H(KI),ib,ih
  
  ! Canopy depth (cm).
  cd = ih - ib
  
  ! Populate foliage area density with this tree's foliage area.
  DO I = ib+1, ih
    fad (I) = fad (I) + Afoliage (KI) / FLOAT (cd)
  END DO

END DO ! KI = 1, NIND

DO I = 1, 700
  write (*,*) I,fad (I)
END DO

!----------------------------------------------------------------------!
PAR_base = EXP (-0.5 * LAI)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE light
!======================================================================!
