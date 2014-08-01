!======================================================================!
MODULE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
REAL, ALLOCATABLE :: rwidth   (:) ! Stem ring width                 (mm)
REAL, ALLOCATABLE :: fad      (:) ! Vert' foliage area density (m^2/m^2)
REAL, ALLOCATABLE :: cad      (:) ! Cum' Vert' fol' area den'  (m^2/m^2)
REAL, ALLOCATABLE :: rPAR     (:) ! Relative PAR profile      (fraction)
REAL, ALLOCATABLE :: Cv       (:) ! Tree structural carbon          (kg)
REAL, ALLOCATABLE :: rold     (:) ! Saved stem radius                (m)
REAL, ALLOCATABLE :: H        (:) ! Stem height                      (m)
REAL, ALLOCATABLE :: Afoliage (:) ! Foliage area                   (m^2)
INTEGER, ALLOCATABLE :: rPAR_base (:) ! Base of crown for +ve        (m)
INTEGER, ALLOCATABLE :: ib (:) ! Height to base of crown            (cm)
REAL, ALLOCATABLE :: Aheart    (:) ! Heartwood area                (m^2)
REAL :: D        ! Stem diameter                                     (m)
REAL :: dCv      ! Tree structural carbon time derivative         (kg/s)
REAL :: Acrown   ! Tree crown area                                 (m^2)
REAL :: GPP      ! Tree crown gross photosynthesis          (umol/m^s/s)
REAL :: Resp     ! Tree maintenance respiration             (umol/m^s/s)
REAL :: Cup      ! Net tree carbon uptake                   (kgC/tree/s)
REAL :: Clit     ! Tree structural carbon litter flux       (kgC/tree/s)
REAL :: tau      ! Tree structural carbon residence time            (yr)
REAL :: V        ! Stem volume                                     (m^3)
REAL :: r        ! Stem radius                                       (m)
REAL :: Dcrown   ! Crown diameter                                    (m)
REAL :: Asapwood ! Sapwood area                                    (m^2)
REAL :: LAI      ! Leaf area index                             (m^2/m^2)
REAL :: fAPAR    ! Fraction of PAR absorbed by crown          (fraction)
REAL :: NPP_ann_acc ! Accumulated annual NPP                (kgC/m^2/yr)
!----------------------------------------------------------------------!
END MODULE TREE
!======================================================================!
