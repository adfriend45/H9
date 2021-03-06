!======================================================================!
SUBROUTINE trees_structure
!----------------------------------------------------------------------!
! Compute tree and canopy structures taking into account controls of
! sapwood areas and radiation on foliage areas. Overlap of crown
! volumes is not allowed, and total crown area at each level must not
! exceed plot area.
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
INTEGER :: tall,short
REAL :: LAIc,Astem,Asapwood
REAL :: Afol_sap,Afol_iPAR,flap,lose,dDcrown,BCR,Ah,x,Afoliage_sum
REAL :: CL     ! Crown length   (m)
REAL :: Vcrown ! Crown volume (m^3)
!----------------------------------------------------------------------!

! First, calculate potential crown areas based on D and maximum
! potential rate of growth.
! Crown depth constrained by iPAR_base.
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
  Dcrown = 2.0 * SQRT (Acrown (KI) / PI)
  dDcrown = b_cd * D - Dcrown
  ! Following can have a big effect.
  !****adfdDcrown = MIN (dDcrown, 2.0 * BG_MAX)
  !--------------------------------------------------------------------!
  ! Crown diameter                                                   (m)
  !--------------------------------------------------------------------!
  Dcrown = Dcrown + dDcrown
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  Acrown (KI) = MIN (Acrown(KI),Aplot)
  !--------------------------------------------------------------------!
  ! Add crown area to the layers it covers                         (m^2)
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
  !--------------------------------------------------------------------!
END DO

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
    ! Total crown area is too large at this level. flap is the ratio
    ! of excess crown area to additional crown area added when going
    ! from layer above to this one. Excess must be due to the addition
    ! of one or more trees as their heights extend to this level.
    !------------------------------------------------------------------!
    IF ((Acrowns_layers (L) - Acrowns_layers (L+1)) > EPS) THEN
      flap = (Acrowns_layers (L) - Aplot) / (Acrowns_layers (L) - &
      &       Acrowns_layers (L+1))
    ELSE
      flap = 0.0
    ENDIF
    !------------------------------------------------------------------!
    ! Because strange things can happen with ratios at low differences,
    ! limit flap.
    !------------------------------------------------------------------!
    flap = MIN (1.0,flap)
    flap = MAX (0.0,flap)
    !------------------------------------------------------------------!
    DO I = 1, NIND_alive
      !----------------------------------------------------------------!
      ! Index of living individual                                   (n)
      !----------------------------------------------------------------!
      KI = LIVING (I)
      !----------------------------------------------------------------!
      ! I think this works as long as each tree has a unique ih.
      ! If it does not, then may be bias depending on which one is
      ! reduced first. Can fix...
      !----------------------------------------------------------------!
      tree_top : IF ((ih (KI) == L) .AND. (flap > 0.0)) THEN
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
  END IF over_area
END DO

!----------------------------------------------------------------------!
! Calculate new individual LAI, etc. given r, Acrown, and iPAR_base.
!----------------------------------------------------------------------!
Afoliage_sum = 0.0 ! For iteration control.
INDIVIDUALS: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  ! Index of living individual                                       (n)
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Stem horizontal cross-sectional area                           (m^2)
  !--------------------------------------------------------------------!
  ! Foliage needs to be lost from crown until the 
  ! lowest foliage receives iPAR=0.03, and heartwood grows
  ! accordingly. ib varies with light a base.
  Astem = PI * r (KI) ** 2
  ! Stem can shrink, and so heartwood needs to be kept in bounds.
  Aheart_new (KI) = MIN (Astem,Aheart(KI))
  x = ((LOG(iPAR_base(KI)))-(LOG(0.03)))/((LOG(iPAR_base(KI))))
  x = MAX (0.0,x)
  ! Following not having big enough effect on overall LAI because not
  ! accounting for fact the other trees are contributing to iPAR_base.
  ! In other words, increasing heartwood area of this tree by x cannot
  ! control overall LAI enough.
  IF (x > 0.0) Aheart_new (KI) = Aheart_new (KI) + &
  &            x * (Astem - Aheart_new (KI))
  !--------------------------------------------------------------------!
  ! Stem sapwood area                                              (m^2)
  !--------------------------------------------------------------------!
  Asapwood = Astem - Aheart_new (KI)
  !--------------------------------------------------------------------!
  ! Sapwood area-limited foliage area                              (m^2)
  !--------------------------------------------------------------------!
  Afol_sap = FASA * Asapwood
  !--------------------------------------------------------------------!
  ! Foliage area                                                   (m^2)
  !--------------------------------------------------------------------!
  Afoliage (KI) = Afol_sap
  !--------------------------------------------------------------------!
  ! Crown length                                                     (m)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (H (KI) / DZ_CROWN_M)
  CL = H (KI) - FLOOR (ib (KI) * DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ! Crown volume                                                   (m^3)
  !--------------------------------------------------------------------!
  Vcrown = CL * Acrown (KI)
  !--------------------------------------------------------------------!
  ! Limit foliage area density to a sensible value given crown volume.
  ! 10 m^2/m^3 would sensible (i.e. LAI of 10 in 1 m tall grass sward),
  ! but to allow for extremes, use 20 m^2/m^3.
  !--------------------------------------------------------------------!
  Afoliage (KI) = MIN (Afoliage (KI), 20.0 * Vcrown)
  !--------------------------------------------------------------------!
  ! Foliage biomass                                             (kg[DM})
  !--------------------------------------------------------------------!
  Bfoliage (KI) = Afoliage (KI) / SLA
  !--------------------------------------------------------------------!
  ! Set height to base of crown using basal light. Slope of
  ! relationship tuned to Bartelink (1997) data using idea from
  ! O'Connell & Kelty (1994). Should perhaps add constraint on rate of
  ! change. BCR is the ratio of the height to the base of the crown
  ! to the total tree height.
  !--------------------------------------------------------------------!
  BCR = BRC_limit * (1.0 - iPAR_base (KI))
  ib (KI) = NINT (FLOAT (ih (KI)) * BCR)
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!
  Afoliage_sum = Afoliage_sum + Afoliage (KI)
END DO INDIVIDUALS
LAI = Afoliage_sum / (Aplot + EPS)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!
