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
INTEGER :: DZ_CROWN   ! Crown depth division                        (mm)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Parameters calculated internally.
!----------------------------------------------------------------------!
INTEGER :: NDAY   ! No. internal timesteps per day                 (ITU)
REAL    :: DTTR   ! Tree growth time step                            (s)
INTEGER :: ITE1   ! Length of model run                              (s)
INTEGER :: ITIMEI ! Start of model run                (internal time, s)
INTEGER :: ITIMEE ! End of model run                  (internal time, s)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Variables calculated internally.
!----------------------------------------------------------------------!
INTEGER :: ITIME      ! Internal time                                (s)
INTEGER :: JYEAR      ! Current year          (Julian calendar year, yr)
INTEGER :: JMON       ! Current month    (Julian calendar month, months)
INTEGER :: JDAY       ! Current day          (Julian calendar day, days)
INTEGER :: MONTHS     ! No. months run in year                  (months)
INTEGER :: NIND_alive ! No. living trees                             (n)
REAL    :: RANDOM     ! Random number, 0-1                           (x)
!----------------------------------------------------------------------!

END MODULE CONTROL
!======================================================================!