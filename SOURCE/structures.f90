!======================================================================!
SUBROUTINE structures
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

!----------------------------------------------------------------------!
! Count of trees at each height.
!----------------------------------------------------------------------!
n_L_ih (:)   = 0
!----------------------------------------------------------------------!
! Index of trees at each height.
!----------------------------------------------------------------------!
L_ih   (:,:) = 0
!----------------------------------------------------------------------!
! Sum of crown areas in each layer                                 (m^2)
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
! New Cvs from grow, so calculate new dimensions.
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
  ! Vertical coordinate.
  !--------------------------------------------------------------------!
  !L = ih (KI)
  !--------------------------------------------------------------------!
  ! Increment counts of trees at its vertical coordinates.
  !--------------------------------------------------------------------!
  !n_L_ih (ib(KI)+1:L) = n_L_ih (ib(KI)+1:L) + 1
  !if (L == 3056) write (*,*) 'hell',KI,n_L_ih(ib(KI)+1),n_L_ih(L)
  !--------------------------------------------------------------------!
  ! Save index of this tree at its vertical coordinates.
  !--------------------------------------------------------------------!
  !L_ih (ib(KI)+1:L,n_L_ih (L)) = KI
  !--------------------------------------------------------------------!
  DO L = ib(KI)+1, ih (KI)
    n_L_ih (L) = n_L_ih (L) + 1
    L_ih (L,n_L_ih(L)) = KI
  END DO
  ! Stem diameter                                                    (m)
  !--------------------------------------------------------------------!
  D = 2.0 * r (KI)
  !--------------------------------------------------------------------!
  ! Crown diameter (may want to add constraint to growth rate)       (m)
  !--------------------------------------------------------------------!
  Dcrown = b_cd * D
  !--------------------------------------------------------------------!
  ! Potential crown area                                           (m^2)
  !--------------------------------------------------------------------!
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
  ! Add crown areas to the layers in which they cover              (m^2)
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) =  Acrowns_layers (ib(KI)+1:ih(KI)) &
  &                                   + Acrown (KI)
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!
! Heights of tallest and shortest trees in plot (DZ_CROWN).
!----------------------------------------------------------------------!
tall  = MAXVAL (ih(LIVING(1:NIND_alive)))
short = MINVAL (ih(LIVING(1:NIND_alive)))
!----------------------------------------------------------------------!
DO L = tall, short, -1
  !--------------------------------------------------------------------!
  ! flap is total area that is outside plot as a fraction of the area
  ! added when going down to this layer.
  !--------------------------------------------------------------------!
  IF (Acrowns_layers (L) > Aplot) THEN
    flap = (Acrowns_layers (L) - Aplot) / (Acrowns_layers (L) -        &
    &       Acrowns_layers (L+1))
  ELSE
    flap = 0.0
  END IF
  !--------------------------------------------------------------------!
  ! Because strange things can happen with ratios at low differences,
  ! bound flap. May want to confirm this is necessary at some point.
  !--------------------------------------------------------------------!
  flap = MIN (1.0,flap)
  !--------------------------------------------------------------------!
  ! Squeeze crown areas if necessary.
  !--------------------------------------------------------------------!
  IF (flap > 0.0) THEN
    !------------------------------------------------------------------!
    ! Total crown area is too large at this level. flap is the ratio
    ! of excess crown area to additional crown area added when going
    ! from layer above to this one. Excess must be due to the addition
    ! of one or more trees as their heights extend to this level.
    ! Reduce each tree crown area that has caused the overlap by flap.
    !------------------------------------------------------------------!
    DO I = 1, n_L_ih (L)
      KI = L_ih (L,I)
      lose = flap * Acrown (KI)
      Acrown (KI) = Acrown (KI) - lose
      Acrowns_layers (ib(KI)+1:ih(KI)) = &
      &  Acrowns_layers (ib(KI)+1:ih(KI)) - lose
      ! Presumably could look at light profile here. Allow for
      ! different kext of each tree.
    END DO
    !------------------------------------------------------------------!
    STOP
  END IF
!  next do a limit on leaf area density and cumulate leaf area downwards, !and cal. ipar and ipar_base values for each tree...
! could we use the actual D increment to calculate how each should
! change?
! use mean kext in each layer to calculate light profile, try and do so
! if only one tree very simple! otherwise need to step down through
! canopy, in each layer check each tree can keep its leaf area. Seems
! only way!
END DO
! Clean up.
n_L_ih (:)   = 0
L_ih   (:,:) = 0
!----------------------------------------------------------------------!
END SUBROUTINE structures
!======================================================================!