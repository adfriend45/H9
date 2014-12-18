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

REAL :: iPAR_base_N,fi,Asapwood,Astem,BCR,Afoliage_sum,K,lim
REAL :: ib_mean_save
REAL :: LAI_above_base,ib_mean,CL,Vcrown

!----------------------------------------------------------------------!
! Compute crown heights and areas based on new Cv's.
!----------------------------------------------------------------------!
CALL squeeze
!----------------------------------------------------------------------!

lim = 10.0
ib_mean_save = 0.0
DO WHILE (lim > 0.01) ! Limit is on fractional change in mean ib.

  !-------------------------------------------------------------------!
  ! Compute fPAR, iPAR, and iPAR_base.
  !-------------------------------------------------------------------!
  CALL light
  !-------------------------------------------------------------------!

  !-------------------------------------------------------------------!
  ! Compute potential crown Afoliages based on iPAR_base and Vcrown.
  !-------------------------------------------------------------------!
  DO I = 1, NIND_alive
    KI = LIVING (I)
    IF (Acrown (KI) > EPS) THEN
      LAI_above_base = LOG (iPAR_base (KI)) / kext
      Afoliage_want (KI) = Aplot * (LOG (0.03) / kext - &
      &                    LAI_above_base) + Afoliage (KI)
      !---------------------------------------------------------------!
    END IF
  END DO

  ! Compute ib based on iPAR_base.
  ib_mean = 0.0
  DO I = 1, NIND_alive
    KI = LIVING (I)
    BCR = BRC_limit * (1.0 - iPAR_base (KI))
    ib (KI) = NINT (FLOAT (ih (KI)) * BCR)
    !-----------------------------------------------------------------!
    ! Limit foliage area density to a sensible value given crown
    ! volume. 10 m^2/m^3 would sensible (i.e. LAI of 10 in 1 m tall
    ! grass sward), but to allow for extremes, use 20 m^2/m^3.
    !-----------------------------------------------------------------!
    CL = H (KI) - FLOOR (ib (KI) * DZ_CROWN_M)
    Vcrown = CL * Acrown (KI)
    Afoliage_want (KI) = MIN (Afoliage_want(KI),20.0*Vcrown)
    !-----------------------------------------------------------------!
    ib_mean = ib_mean + ib (KI)
  END DO
  ib_mean = ib_mean / FLOAT (NIND_alive)

  ! Finally, diagnose Aheart etc.
  Afoliage_sum = 0.0
  DO I = 1, NIND_alive
    KI = LIVING (I)
    IF (Acrown (KI) > EPS) THEN
      Astem = pi * r (KI) ** 2
      ! In case of shrinkage.
      Aheart_new (KI) = MIN (Aheart_new (KI), Astem)
      Asapwood = Afoliage_want (KI) / FASA
      IF (Asapwood < (Astem - Aheart_new (KI))) THEN
        Aheart_new (KI) = Astem - Asapwood
        Afoliage (KI) = Afoliage_want (KI)
      ELSE
        Afoliage (KI) = FASA * (Astem - Aheart_new (KI))
        if (afoliage (KI) < 0.0) stop 'Afoliage < 0 in structures'
      ENDIF
      LAIcrown (KI) = Afoliage (KI) / Acrown (KI)
    ELSE
      Afoliage (KI) = 0.0
      LAIcrown (KI) = 0.0
    END IF
    Afoliage_sum = Afoliage_sum + Afoliage (KI)
  END DO
  LAI = Afoliage_sum / (Aplot + EPS)
  lim = ABS (ib_mean - ib_mean_save) / (ib_mean_save + EPS)
  ib_mean_save = ib_mean
  WRITE (*,*) JYEAR,'LAI = ',LAI,ib_mean
END DO

!----------------------------------------------------------------------!
! Diagnose foliage biomass.
!----------------------------------------------------------------------!
DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Foliage biomass                                             (kg[DM})
  !--------------------------------------------------------------------!
  Bfoliage (KI) = Afoliage (KI) / SLA
  Aheart (KI) = Aheart_new (KI)
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE structures
!======================================================================!