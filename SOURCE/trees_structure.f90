!======================================================================!
SUBROUTINE trees_structure
!----------------------------------------------------------------------!
! Compute tree and canopy structures taking into account controls of
! sapwood areas and radiation on foliage areas. Overlap of crown
! volumes is allowed, but no individual crown can exceed plot area.
! But, overlap within crowns not accounted for in iPAR logic or fPAR.
! Need to do this. Do explicit canopy light profile?
! No, decided to keep crown volumes separate and order trees in terms
! height.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!
INTEGER :: top,bottom
REAL :: LAIc,Astem,Asapwood
REAL :: Afol_sap,Afol_iPAR,flap,lose

! First calculate potential crown areas based on D, rate of growth.
! Crown depth constrained by iPAR.
! Then pack them so they fit, with tallest ones having advantage if
! needed squeezed.
Acrowns_layers (:) = 0.0
DO I = 1, NIND_alive
  KI = LIVING (I)
  V = Cv (KI) / SIGC
  r (KI) = (V / (( FORMF / 3.0) * PI * alpha)) ** (1.0 / (2.0 + beta))
  D = 2.0 * r (KI)
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  Acrown (KI) = MIN (Acrown(KI),Aplot)
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
END DO
! Now go down through plot.
! If total areas > Aplot and this is top of crown, gets squeezed
! proportionally. Need to find all trees as may be more than one, before
! squeezing. After each squeezing, need to adjust crown area profile.
top    = MAXVAL (ih(LIVING(1:NIND_alive)))
bottom = MINVAL (ih(LIVING(1:NIND_alive)))
write (*,*) NIND_alive,'bottom top',bottom,top
write (98,*) top,bottom
DO L = top, bottom, -1
  write (98,*) L,FLOAT(L)*DZ_CROWN_M,Acrowns_layers(L),Acrowns_layers(L+1)
  IF (Acrowns_layers (L) > Aplot) THEN
    flap = (Acrowns_layers (L) - Aplot) / (Acrowns_layers (L) - &
    &       Acrowns_layers (L+1) + EPS)
    flap = MIN (1.0,flap)
    flap = MAX (0.0,flap)
    !write (98,*) 'flap=',flap
    !flap = 0.0
    DO I = 1, NIND_alive
      KI = LIVING (I)
      ! I think this works as long as each tree has a unique ih.
      ! If it does not, then may be bias depending on which one is
      ! reduced first. Can fix...
      IF (ih (KI) == L) THEN
        !write (98,*) KI,Acrown(KI)
        lose = flap * Acrown (KI)
        Acrown (KI) = Acrown (KI) - lose
        Acrowns_layers (ib(KI)+1:ih(KI)) = &
        &  Acrowns_layers (ib(KI)+1:ih(KI)) - lose
      END IF
    END DO
  !write (98,*) '1',L,FLOAT(L)*DZ_CROWN_M,Acrowns_layers(L)
  END IF
END DO

!----------------------------------------------------------------------!
! Calculate new r, H, ih, Acrown, Aheart, Afoliage, ib, and LAIcrown
! of each individual.
!----------------------------------------------------------------------!
INDIVIDUALS: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  ! Index of living individual
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
  ! Stem horizontal cross-sectional area                           (m^2)
  !--------------------------------------------------------------------!
  Astem = PI * r (KI) ** 2
  !--------------------------------------------------------------------!
  ! Stem sapwood area                                              (m^2)
  !--------------------------------------------------------------------!
  Asapwood = Astem - Aheart (KI)
  !--------------------------------------------------------------------!
  ! Sapwood area-limited foliage area                              (m^2)
  !--------------------------------------------------------------------!
  Afol_sap = FASA * Asapwood
  !--------------------------------------------------------------------!
  ! Stem diameter                                                    (m)
  !--------------------------------------------------------------------!
  D = 2.0 * r (KI)
  !--------------------------------------------------------------------!
  ! Crown diameter                                                   (m)
  !--------------------------------------------------------------------!
  Dcrown = a_cd + b_cd * D
  !--------------------------------------------------------------------!
  ! Crown area                                                     (m^2)
  !--------------------------------------------------------------------!
  !Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
  ! Restrict crown area to plot area                               (m^2)
  !--------------------------------------------------------------------!
  !Acrown (KI) = MIN (Acrown(KI),Aplot)
  !--------------------------------------------------------------------!
  ! iPAR-limited LAI                                           (m^2/m^3)
  !--------------------------------------------------------------------!
  LAIc = LOG (0.03 / (iPAR (KI) + EPS)) / (-0.5)
  !--------------------------------------------------------------------!
  ! iPAR-limited foliage area                                      (m^2)
  !--------------------------------------------------------------------!
  Afol_iPAR = LAIc * Acrown (KI)
  !--------------------------------------------------------------------!
  ! Restrict foliage area and heartwood area if iPAR-limit exceeded.
  !--------------------------------------------------------------------!
  IF (Afol_iPAR < Afol_sap) THEN
    !------------------------------------------------------------------!
    floss = 1.0 - Afol_iPAR / Afol_sap
    Aheart (KI) = Aheart (KI) + floss * Afoliage (KI) / FASA
    Aheart (KI) = MIN (Aheart(KI),Astem)
    Afoliage (KI) = Afol_iPAR
  ELSE
    Afoliage (KI) = Afol_sap
  ENDIF
  !--------------------------------------------------------------------!
  ! Set height to base of crown using foliage area density  (DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ib (KI) = ih (KI) - NINT (Afoliage (KI) / (SIGAF * Acrown (KI) * &
  &         DZ_CROWN_M))
  !--------------------------------------------------------------------!
  ! Keep height to base of crown within sensible bounds     (DZ_CROWN_M)
  !--------------------------------------------------------------------!
  IF (ib (KI) > ih (KI)) THEN
    ib (KI) = ih (KI)
    Afoliage (KI) = 0.0
    Acrown (KI) = 0.0
  END IF
  IF (ib (KI) < 0) THEN
    ib (KI) = 0
    Afoliage (KI) = SIGAF * FLOAT (ih (KI) - ib (KI)) * DZ_CROWN_M *   &
    &               Acrown (KI)
  END iF
  write (98,*) KI,Acrown(KI),Afoliage(KI),ib(KI),ih(KI)
  !--------------------------------------------------------------------!
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!