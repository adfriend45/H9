!======================================================================!
MODULE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
! State variables.
!----------------------------------------------------------------------!
REAL, ALLOCATABLE :: Cv     (:) ! Tree structural carbon            (kg)
REAL, ALLOCATABLE :: Aheart (:) ! Heartwood area                   (m^2)
!----------------------------------------------------------------------!
INTEGER, ALLOCATABLE :: ib (:) ! Height to base of crown            (cm)
!----------------------------------------------------------------------!
! Derived tree variables.
!----------------------------------------------------------------------!
INTEGER, ALLOCATABLE :: ih (:) ! Crown height                       (cm)
INTEGER :: base ! PAR-limited height to base of crown               (cm)
!----------------------------------------------------------------------!
REAL, ALLOCATABLE :: rwidth   (:,:) ! Stem ring width               (mm)
REAL, ALLOCATABLE :: Acrowns_layers (:)
REAL, ALLOCATABLE :: rold     (:) ! Saved stem radius                (m)
REAL, ALLOCATABLE :: H        (:) ! Stem height                      (m)
REAL, ALLOCATABLE :: Afoliage (:) ! Foliage area                   (m^2)
REAL, ALLOCATABLE :: fPAR (:) ! fPAR for each tree (fraction)
REAL, ALLOCATABLE :: Acrown (:)   ! Tree crown area                (m^2)
REAL, ALLOCATABLE :: LAIcrown (:) ! Tree crown LAI            (m^2/m^2)
REAL, ALLOCATABLE :: Acrowns_above (:)
REAL, ALLOCATABLE :: Afoliage_above (:)
REAL, ALLOCATABLE :: r (:)        ! Stem radius                      (m)
REAL, ALLOCATABLE :: iPAR (:) ! Relative PAR at top of crown (fraction)
REAL, ALLOCATABLE :: shade (:) ! Fraction of crown being shaded by higher trees !TTR record the area shaded for each tree for visulisation 
INTEGER, ALLOCATABLE :: UID (:) !TTR Unique ID for the tree 
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
REAL :: Asapwood ! Sapwood area                                    (m^2)
REAL :: LAI      ! Plot leaf area index                        (m^2/m^2)
REAL :: NPP_ann_acc ! Accumulated annual NPP                (kgC/m^2/yr)
REAL :: floss
!----------------------------------------------------------------------!
END MODULE TREE
!======================================================================!
