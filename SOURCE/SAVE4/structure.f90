!======================================================================!
SUBROUTINE structure
!----------------------------------------------------------------------!
! Re-make of combined light + trees_structure.
!----------------------------------------------------------------------!

USE CONSTANTS
USE CONTROL
USE TREE

!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

INTEGER :: short,tall
REAL    :: flap,lose,Asap,BRC,x

!----------------------------------------------------------------------!
! Sum of crown areas in each layer                                 (m^2)
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Stem volume                                                    (m^3)
  !--------------------------------------------------------------------!
  V = Cv (KI) / SIGC
  !--------------------------------------------------------------------!
  ! Stem radius                                                      (m)
  !--------------------------------------------------------------------!
  r (KI) = (V / (( FORMF / 3.0) * PI * alpha)) ** (1.0 / (2.0 + beta))
  !--------------------------------------------------------------------!
  ! Stem horizontal cross-sectional area                           (m^2)
  !--------------------------------------------------------------------!
  Astem (KI) = PI * r (KI) ** 2
  !--------------------------------------------------------------------!
  ! Stem can shrink, and so heartwood needs to be kept in bounds   (m^2)
  !--------------------------------------------------------------------!
  Aheart (KI) = MIN (Astem(KI),Aheart(KI))
  !--------------------------------------------------------------------!
  ! Stem length above ground (i.e. height)                           (m)
  !--------------------------------------------------------------------!
  H (KI) = alpha * r (KI) ** beta
  !--------------------------------------------------------------------!
  ! Height in canopy depth units                            (DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (H (KI) / DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ! Potential crown area                                             (m)
  !--------------------------------------------------------------------!
  Acrown (KI) = PI * (b_cd * r (KI)) ** 2
  !--------------------------------------------------------------------!
  ! Add crown area to the layers it covers                         (m^2)
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Heights of tallest and shortest trees in plot (DZ_CROWN).
!----------------------------------------------------------------------!
tall  = MAXVAL (ih(LIVING(1:NIND_alive)))
short = MINVAL (ih(LIVING(1:NIND_alive)))
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Now squeeze crown areas if necessary to fit in plot. Taller trees
! take priority (i.e. they are not squeezed if their tops have enough
! space).
!----------------------------------------------------------------------!

DO L = tall, short, -1
  IF (Acrowns_layers (L) > Aplot) THEN
    flap = (Acrowns_layers (L) - Aplot) / &
    &      (Acrowns_layers (L) - Acrowns_layers (L+1))
    DO I = 1, NIND_alive
      KI = LIVING (I)
      IF (L == ih (KI)) THEN
        lose = flap * Acrown (KI)
        Acrown (KI) = Acrown (KI) - lose
        Acrowns_layers (ib(KI)+1:ih(KI)) =       &
        &  Acrowns_layers (ib(KI)+1:ih(KI)) - lose
      END IF
    END DO
  END IF
END DO

! I think we have to iterate until the canopy does not change.
! Keep the light.f90 routine as this seems efficient.
! ib changing will change acrowns_layers...so maybe just keep as before
! but iterate!
DO J = 1, 10
DO I = 1, NIND_alive
  KI = LIVING (I)
  Asap = Astem (KI) - Aheart (KI)
  Afoliage (KI) = FASA * Asap
END DO
CALL light
DO I = 1, NIND_alive
  KI = LIVING (I)
  x = ((LOG(iPAR_base(KI)))-(LOG(0.03)))/((LOG(iPAR_base(KI))))
  x = MAX (0.0,x)
  IF (x > 0.0) Aheart (KI) = Aheart (KI) + x * (Astem (KI) - Aheart (KI))
  BRC = BRC_limit * (1.0 - iPAR_base (KI))
  ib (KI) = NINT (FLOAT (ih (KI)) * BRC)
END DO
END DO
!----------------------------------------------------------------------!
END SUBROUTINE structure
!======================================================================!
