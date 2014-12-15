!======================================================================!
SUBROUTINE squeeze
!----------------------------------------------------------------------!
! Compute new height and crown areas, squeezing if necessary to fit in
! plot.
!----------------------------------------------------------------------!

USE CONSTANTS
USE CONTROL
USE TREE

!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

INTEGER :: tall,short
REAL :: flap,lose

!----------------------------------------------------------------------!
! First, calculate potential crown areas based on D.
! Then pack them so they fit, with tallest ones having advantage if
! needed squeezed.
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
  ! Stem length above ground (i.e. height)                           (m)
  !--------------------------------------------------------------------!
  H (KI) = alpha * r (KI) ** beta
  !--------------------------------------------------------------------!
  ! Height in canopy depth units                            (DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (H (KI) / DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ! Stem diameter                                                    (m)
  !--------------------------------------------------------------------!
  D = 2.0 * r (KI)
  !--------------------------------------------------------------------!
  ! Crown diameter                                                   (m)
  !--------------------------------------------------------------------!
  Dcrown = b_cd * D
  !--------------------------------------------------------------------!
  ! Potential crown area (m^2)
  !--------------------------------------------------------------------!
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
  ! Add crown area to the layers it covers                         (m^2)
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Now go down through plot and reduce crown areas if necessary to fit
! horizontally. Restrict range to analyse to the range of tree heights
! as only look at top of each crown for area test.
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Heights of tallest and shortest trees in plot (DZ_CROWN).
!----------------------------------------------------------------------!
tall  = MAXVAL (ih(LIVING(1:NIND_alive)))
short = MINVAL (ih(LIVING(1:NIND_alive)))
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Loop down through plot.
!----------------------------------------------------------------------!
DO L = tall, short, -1
  !--------------------------------------------------------------------!
  ! Test if sum of crown layers at this level is too large for plot.
  !--------------------------------------------------------------------!
  over_area : IF (Acrowns_layers (L) > Aplot) THEN
    !------------------------------------------------------------------!
    ! flap is the ratio of excess crown area to additional crown area
    ! added when going from layer above to this one. Excess must be due
    ! to the addition of one or more trees as their heights extend to
    ! this level.
    !------------------------------------------------------------------!
    flap = (Acrowns_layers (L) - Aplot)                 / &
    &      (Acrowns_layers (L) - Acrowns_layers (L+1))
    !------------------------------------------------------------------!
    DO I = 1, NIND_alive
      !----------------------------------------------------------------!
      ! Index of living individual                                   (n)
      !----------------------------------------------------------------!
      KI = LIVING (I)
      !----------------------------------------------------------------!
      tree_top : IF (L == ih (KI)) THEN
        !--------------------------------------------------------------!
        ! Crown area to lose from this individual                 (m^2)
        !--------------------------------------------------------------!
        lose = flap * Acrown (KI)
        !--------------------------------------------------------------!
        ! Remove this crown area                                  (m^2)
        !--------------------------------------------------------------!
        Acrown (KI) = Acrown (KI) - lose
        !--------------------------------------------------------------!
        ! Adjust canopy profile due to change in crown area.
        !--------------------------------------------------------------!
        Acrowns_layers (ib(KI)+1:ih(KI)) = &
        &  Acrowns_layers (ib(KI)+1:ih(KI)) - lose
        !--------------------------------------------------------------!
      END IF tree_top
      !----------------------------------------------------------------!
    END DO
    !------------------------------------------------------------------!
  END IF over_area
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!

END SUBROUTINE squeeze
!======================================================================!
