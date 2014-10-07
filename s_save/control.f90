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
INTEGER :: NT ! GROW call loop count                                 (n)
INTEGER :: KI ! Individual tree count                                (n)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Input from driver.txt.
!----------------------------------------------------------------------!
REAL    :: DTSRC  ! Source time step (= 1 ITU)                       (s)
INTEGER :: NITR   ! No. tree growth time steps per ITU               (n)
INTEGER :: NYRS   ! No. years to simulate                            (y)
INTEGER :: YEARI  ! Start of model run                     (calendar yr)
INTEGER :: MONI   ! Start of model run                    (Julian month)
INTEGER :: IHRI   ! Start of model run                  (24-hr clock hr)
INTEGER :: NMONAV ! No. months in a diagnostic acc period       (months)
INTEGER :: NIND   ! No. trees to simulate                            (n)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Parameters calculated internally.
!----------------------------------------------------------------------!
INTEGER :: NDAY   ! No. internal timesteps per day                 (ITU)
REAL    :: DTTR   ! Tree growth time step                            (s)
INTEGER :: ITE1   ! Length of simulation                             (s)
INTEGER :: ITIMEI ! Start of model run                (internal time, s)
INTEGER :: ITIMEE ! End of model run                  (internal time, s)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Variables calculated internally.
!----------------------------------------------------------------------!
INTEGER :: ITIME  ! Internal time                                    (s)
INTEGER :: JYEAR  ! Current year              (Julian calendar year, yr)
INTEGER :: JMON   ! Current month        (Julian calendar month, months)
INTEGER :: JDAY   ! Current day              (Julian calendar day, days)
INTEGER :: MONTHS ! No. months run in year                      (months)
REAL    :: RANDOM ! Random number, 0-1                               (x)
!----------------------------------------------------------------------!

END MODULE CONTROL
!======================================================================!