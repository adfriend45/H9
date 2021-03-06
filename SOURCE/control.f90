!======================================================================!
MODULE CONTROL
!----------------------------------------------------------------------!
! Definition of control parameters and variables.
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! General internal control parameters.
!----------------------------------------------------------------------!
INTEGER :: I  ! For general local use                                (n)
INTEGER :: J  ! For general local use                                (n)
INTEGER :: L  ! For general local use                                (n)
INTEGER :: NT ! GROW call loop count                                 (n)
INTEGER :: KI ! Individual tree index                                (n)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Input from driver.txt.
!----------------------------------------------------------------------!
REAL    :: DTSRC      ! Source time step (= 1 ITU)                   (s)
REAL    :: DZ_CROWN_m ! Crown depth division                         (m)
INTEGER :: NITR       ! No. tree growth time steps per ITU           (n)
INTEGER :: NYRS       ! No. years to simulate                        (y)
INTEGER :: YEARI      ! Start of model run                 (calendar yr)
INTEGER :: MONI       ! Start of model run                (Julian month)
INTEGER :: IHRI       ! Start of model run              (24-hr clock hr)
INTEGER :: NMONAV     ! No. months in a diagnostic acc period   (months)
INTEGER :: NIND_max   ! Max. no. trees to simulate                   (n)
INTEGER :: F_OUT      ! Output flag for individuals  (0 = none; 1 = txt)
INTEGER :: DZ_CROWN   ! Crown depth division                        (mm)
INTEGER :: H_MAX      ! Maximum possible tree height                 (m)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Parameters calculated internally.
!----------------------------------------------------------------------!
INTEGER :: NDAY   ! No. internal timesteps per day                 (ITU)
REAL    :: DTTR   ! Tree growth time step                            (s)
INTEGER :: ITE1   ! Length of model run                            (ITU)
INTEGER :: ITIMEI ! Start of model run              (internal time, ITU)
INTEGER :: ITIMEE ! End of model run                (internal time, ITU)
INTEGER :: N_LAYERS ! No. horizontal layers in plot                  (n)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Variables calculated internally.
!----------------------------------------------------------------------!
INTEGER :: ITIME      ! Internal time                              (ITU)
INTEGER :: JYEAR      ! Current year          (Julian calendar year, yr)
INTEGER :: JMON       ! Current month    (Julian calendar month, months)
INTEGER :: JDAY       ! Current day          (Julian calendar day, days)
INTEGER :: MONTHS     ! No. months run in year                  (months)
INTEGER :: NIND_alive ! No. living trees                             (n)
REAL    :: RANDOM     ! Random number, 0-1                           (x)
!----------------------------------------------------------------------!
END MODULE CONTROL
!======================================================================!
