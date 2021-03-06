!======================================================================!
MODULE TREE
!----------------------------------------------------------------------!
! Contains tree variables.
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
! State variables.
!----------------------------------------------------------------------!
INTEGER, ALLOCATABLE :: LIVING (:) ! Indices of living trees         (n)
INTEGER, ALLOCATABLE :: ib     (:) ! Height to base of crown  (DZ_CROWN)
REAL   , ALLOCATABLE :: Cv     (:) ! Tree structural carbon         (kg)
REAL   , ALLOCATABLE :: Aheart (:) ! Heartwood area                (m^2)
!----------------------------------------------------------------------!
! Derived tree variables.
!----------------------------------------------------------------------!
INTEGER, ALLOCATABLE :: ih             (:) ! Crown height     (DZ_CROWN)
REAL   , ALLOCATABLE :: rwidth       (:,:) ! Stem ring width         (m)
REAL   , ALLOCATABLE :: Acrowns_layers (:) ! Sum crown area/layer  (m^2)
REAL   , ALLOCATABLE :: rold           (:) ! Saved stem radius       (m)
REAL   , ALLOCATABLE :: H              (:) ! Stem height             (m)
REAL   , ALLOCATABLE :: Afoliage       (:) ! Foliage area          (m^2)
REAL   , ALLOCATABLE :: Bfoliage       (:) ! Foliage biomass    (kg[DM])
REAL   , ALLOCATABLE :: fPAR           (:) ! fPAR for each tree   (frac)

REAL, ALLOCATABLE :: iPAR_base (:) ! Relative PAR at bottom of crown (fraction)
REAL, ALLOCATABLE :: shade (:) ! Fraction of crown being shaded by higher trees
REAL, ALLOCATABLE :: Acrown (:)   ! Tree crown area                (m^2)
REAL, ALLOCATABLE :: LAIcrown (:) ! Tree crown LAI             (m^2/m^2)
REAL, ALLOCATABLE :: Acrowns_above (:)
REAL, ALLOCATABLE :: Afoliage_above (:)
REAL, ALLOCATABLE :: Afoliage_above_base (:)
REAL, ALLOCATABLE :: r (:)        ! Stem radius                      (m)
REAL, ALLOCATABLE :: Aheart_new (:)
REAL, ALLOCATABLE :: iPAR       (:)
REAL, ALLOCATABLE :: Afoliage_want       (:)
!TTR record the area shaded for each tree for visulisation
!----------------------------------------------------------------------!
! Internal intermediate variables.
!----------------------------------------------------------------------!
REAL :: D        ! Stem diameter                                     (m)
REAL :: dCv      ! Tree structural carbon time derivative         (kg/s)
REAL :: GPP      ! Tree crown gross photosynthesis          (umol/m^s/s)
REAL :: Resp     ! Tree maintenance respiration             (umol/m^s/s)
REAL :: Cup      ! Net tree carbon uptake                   (kgC/tree/s)
REAL :: Clit     ! Tree structural carbon litter flux       (kgC/tree/s)
REAL :: tau      ! Tree structural carbon residence time            (yr)
REAL :: V        ! Stem volume                                     (m^3)
REAL :: Dcrown   ! Crown diameter                                    (m)
REAL :: LAI      ! Plot leaf area index                        (m^2/m^2)
REAL :: NPP_ann_acc ! Accumulated annual NPP                (kgC/m^2/yr)
REAL :: floss
!----------------------------------------------------------------------!
END MODULE TREE
!======================================================================!
