!======================================================================!
SUBROUTINE structures

!----------------------------------------------------------------------!
! Compute new canopy structure.
! First get new heights and crown areas, using squeezing if necessary.
! Then compute new light profile.
! Then new foliage areas in each crown.
! Then new ib for each tree.
! Then new light profile and repeat until LAI of plot does not change
! more than some tolerance.
!----------------------------------------------------------------------!

USE CONSTANTS
USE CONTROL
USE TREE

!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

REAL :: iPAR_base_N,fi,Asapwood,Astem,BCR,Afoliage_sum,K,lim,LAI_save

CALL squeeze

lim = 10.0
LAI_save = 0.0
DO WHILE (lim > 0.01)

CALL light

! Compute potential crown LAIs based on iPAR_base.
DO I = 1, NIND_alive
  KI = LIVING (I)
  IF (Acrown (KI) > EPS) THEN
    fi = Acrown (KI) / Aplot
    ! iPAR_base without this individual.
    IF (fi < 1.0) THEN
      iPAR_base_N = (iPAR_base (KI) - &
      &             iPAR (KI) * EXP (kext * LAIcrown (KI)) * fi) / &
      &             (1.0 - fi)
    ELSE
      iPAR_base_N = iPAR_base (KI) - &
      &             iPAR (KI) * EXP (kext * LAIcrown (KI))
    ENDIF
    LAIcrown_want (KI) = LOG ((iPAR_base_N * (1.0 - fi) - &
    &                    0.03) / (-fi * iPAR (KI))) / kext
  END IF
END DO

! Compute ib based on iPAR_base.
DO I = 1, NIND_alive
  KI = LIVING (I)
  BCR = BRC_limit * (1.0 - iPAR_base (KI))
  ib (KI) = NINT (FLOAT (ih (KI)) * BCR)
END DO

! Finally, diagnose Aheart etc.
Afoliage_sum = 0.0
DO I = 1, NIND_alive
  KI = LIVING (I)
  IF (Acrown (KI) > EPS) THEN
    Astem = pi * r (KI) ** 2
    Aheart (KI) = MIN (Aheart (KI), Astem) ! In case of shrinkage.
    Asapwood = (LAIcrown_want (KI) * Acrown (KI)) / FASA
    IF (Asapwood < (Astem - Aheart (KI))) THEN
      Aheart (KI) = Astem - Asapwood
      Afoliage (KI) = Acrown (KI) * LAIcrown_want (KI)
      LAIcrown (KI) = LAIcrown_want (KI)
    ELSE
      Afoliage (KI) = FASA * (Astem - Aheart (KI))
      if (afoliage (KI) < 0.0) stop
      LAIcrown (KI) = Afoliage (KI) / Acrown (KI)
    ENDIF
  ELSE
    Afoliage (KI) = 0.0
    LAIcrown (KI) = 0.0
  END IF
  Afoliage_sum = Afoliage_sum + Afoliage (KI)
END DO
LAI = Afoliage_sum / (Aplot + EPS)
lim = ABS (LAI - LAI_save)
LAI_save = LAI
WRITE (*,*) JYEAR,K,'LAI = ',LAI
END DO

!----------------------------------------------------------------------!
END SUBROUTINE structures
!======================================================================!