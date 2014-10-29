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
REAL :: LAIc,Astem,Asapwood
REAL :: Afol_sap,Afol_iPAR

! First calculate potential crown areas based on D, rate of growth.
! Crown depth constrained by iPAR.
! Then pack them so they fit, with tallest ones having advantage if
! needed squeezed.

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
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
  ! Restrict crown area to plot area                               (m^2)
  !--------------------------------------------------------------------!
  Acrown (KI) = MIN (Acrown(KI),Aplot)
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
  ib (KI) = MIN (ih(KI),ib(KI))
  ib (KI) = MAX (0,ib(KI))
  !--------------------------------------------------------------------!
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!