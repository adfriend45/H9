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
REAL :: Afol_sap,Afol_iPAR,flap,lose,dDcrown,LCR,Ah
!----------------------------------------------------------------------!

! First, calculate potential crown areas based on D and maximum
! potential rate of growth.
! Crown depth constrained by iPAR.
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
! Calculate new individual LAI, etc. given r, Acrown, and iPAR.
!----------------------------------------------------------------------!
INDIVIDUALS: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  ! Index of living individual                                       (n)
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  ! Try just growing a bif of heartwood if too little light at base of
  ! crown.
  !--------------------------------------------------------------------!
  ! Stem horizontal cross-sectional area                           (m^2)
  !--------------------------------------------------------------------!
  Astem = PI * r (KI) ** 2
  !if (ipar_base (ki) < 0.14) aheart (ki) = aheart (ki) + 0.05 * astem
  !if (ipar_base (ki) < 0.14) ib (ki) = ib (ki) + 500
  LAIc = LOG (0.03) / (-0.5)
  Ah = Astem - Acrown (KI) * LAIc / FASA
  Aheart (KI) = MAX (Aheart (KI), Ah)
  !--------------------------------------------------------------------!
  ! Stem sapwood area                                              (m^2)
  !--------------------------------------------------------------------!
  Asapwood = Astem - Aheart (KI)
  !--------------------------------------------------------------------!
  ! Sapwood area-limited foliage area                              (m^2)
  !--------------------------------------------------------------------!
  Afol_sap = FASA * Asapwood
  !--------------------------------------------------------------------!
  ! iPAR-limited LAI                                           (m^2/m^3)
  !--------------------------------------------------------------------!
  !LAIc = LOG (0.03 / (iPAR (KI) + EPS)) / (-0.5)
  !LAIc = LOG (0.14 / (iPAR (KI) + EPS)) / (-0.5) !****adf
  !--------------------------------------------------------------------!
  ! iPAR-limited foliage area                                      (m^2)
  !--------------------------------------------------------------------!
  !Afol_iPAR = LAIc * Acrown (KI)
  !--------------------------------------------------------------------!
  ! Restrict foliage area and heartwood area if iPAR-limit exceeded.
  !--------------------------------------------------------------------!
  !iPAR_limits_test : IF (Afol_iPAR < Afol_sap) THEN
    !------------------------------------------------------------------!
    ! Foliage area must be reduced because of incident light.
    !------------------------------------------------------------------!
    ! Fraction of foliage area that must be lost              (fraction)
    !------------------------------------------------------------------!
    !floss = 1.0 - Afol_iPAR / Afol_sap
    !------------------------------------------------------------------!
    ! Increase heartwood area appropriately                        (m^2)
    !------------------------------------------------------------------!
    !Aheart (KI) = Aheart (KI) + floss * Afoliage (KI) / FASA
    !Aheart (KI) = MIN (Aheart(KI),Astem)
    !Aheart (KI) = 0.0 !****adf
    !------------------------------------------------------------------!
    ! Set foliage area to light-limited value                      (m^2)
    !------------------------------------------------------------------!
    !Afoliage (KI) = Afol_iPAR
    !------------------------------------------------------------------!
  !ELSE
    !------------------------------------------------------------------!
    ! Foliage area is OK at sapwood-limited area                   (m^2)
    !------------------------------------------------------------------!
    Afoliage (KI) = Afol_sap
    !------------------------------------------------------------------!
  !ENDIF iPAR_limits_test
  !--------------------------------------------------------------------!
  ! Foliage biomass                                             (kg[DM})
  !--------------------------------------------------------------------!
  Bfoliage (KI) = Afoliage (KI) / SLA
  !--------------------------------------------------------------------!
  ! Set height to base of crown using foliage area density  (DZ_CROWN_M)
  !--------------------------------------------------------------------!
  !ib (KI) = ih (KI) - NINT (Afoliage (KI) / (SIGAF * Acrown (KI) * &
  !&         DZ_CROWN_M))
  !ib (KI) = 0 !****adf
  !****adf
  !ib (KI) = ih (KI) - NINT (Afoliage (KI) / (1.1 * Acrown (KI) * &
  !&         DZ_CROWN_M))
  !IF (iPAR_base (KI) < 0.141) ib (KI) = ib (KI) + 500
  !write (*,*) iPAR_base (KI)
  !ib (KI) = MIN (ib (KI), ih(KI)/2)
  ! Live crown ratio from shading (after O'Connell and Kelty, 1994).
  LCR = 1.0 - (4.0 * (0.96-0.81)/(0.98-0.05)) * (1.0 - iPAR_base (KI))
  ib (KI) = NINT (FLOAT (ih (KI)) * (1.0 - LCR))
  !ib (KI) = ih (KI) / 2
  !****adf
  !--------------------------------------------------------------------!
  ! Keep height to base of crown within sensible bounds     (DZ_CROWN_M)
  !--------------------------------------------------------------------!
  !IF (ib (KI) >= ih (KI)) THEN
  !  ib (KI) = ih (KI) - 1
    !Afoliage (KI) = 0.0
    !Acrown   (KI) = 0.0
  !END IF
  !IF (ib (KI) < 0) THEN
  !  ib (KI) = 0
  !  Afoliage (KI) = SIGAF * FLOAT (ih (KI) - ib (KI)) * DZ_CROWN_M *   &
  !  &               Acrown (KI)
  !****adf
  !  Afoliage (KI) = 1.1 * FLOAT (ih (KI) - ib (KI)) * DZ_CROWN_M *   &
  !  &               Acrown (KI)
  !****adf
  !END iF
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!
