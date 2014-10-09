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
INTEGER :: I  ! General loop count                                   (n)
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
INTEGER :: NIND   ! Max. no. trees to simulate                       (n)
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
INTEGER :: NIND_alive ! No. living trees                             (n)
REAL    :: RANDOM ! Random number, 0-1                               (x)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
INTEGER :: F_IND_OUT                ! Flag for individual outputs 
INTEGER :: NCID                     ! The ID for the netcdf output file
INTEGER :: status                   ! I/O error parameter
!----------------------------------------------------------------------!

END MODULE CONTROL
!======================================================================!