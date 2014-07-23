!======================================================================!
MODULE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
REAL :: Cv       ! Tree structural carbon                           (kg)
REAL :: dCv      ! Tree structural carbon time derivative         (kg/s)
REAL :: Acrown   ! Tree crown area                                 (m^2)
REAL :: GPP      ! Tree crown gross photosynthesis          (umol/m^s/s)
REAL :: Resp     ! Tree maintenance respiration             (umol/m^s/s)
REAL :: Cup      ! Net tree carbon uptake                   (kgC/tree/s)
REAL :: Clit     ! Tree structural carbon litter flux       (kgC/tree/s)
REAL :: tau      ! Tree structural carbon residence time            (yr)
REAL :: V        ! Stem volume                                     (m^3)
REAL :: r        ! Stem radius                                       (m)
REAL :: D        ! Stem diameter                                     (m)
REAL :: H        ! Stem height                                       (m)
REAL :: Dcrown   ! Crown diameter                                    (m)
REAL :: Afoliage ! Foliage area                                    (m^2)
REAL :: LAI      ! Leaf area index                             (m^2/m^2)
REAL :: fAPAR    ! Fraction of PAR absorbed by crown          (fraction)
REAL :: NPP_ann_acc ! Accumulated annual NPP                (kgC/m^2/yr)
REAL :: rold        ! Saved stem radius                              (m)
REAL :: rwidth      ! Stem ring width                               (mm)
!----------------------------------------------------------------------!
END MODULE TREE
!======================================================================!
