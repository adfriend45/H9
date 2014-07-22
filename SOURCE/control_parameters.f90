!======================================================================!
MODULE CONTROL_PARAMETERS
!----------------------------------------------------------------------!
! Definition of control parameters.
!----------------------------------------------------------------------!
IMPLICIT NONE
SAVE
!----------------------------------------------------------------------!
! General internal control parameters.
!----------------------------------------------------------------------!
INTEGER :: NT ! GROW call loop count                                 (n)
!----------------------------------------------------------------------!
! Input from driver.txt.
!----------------------------------------------------------------------!
REAL    :: DTSRC ! Source time step (= 1 ITU)                        (s)
INTEGER :: NITR  ! No. tree growth time steps per ITU                (n)
INTEGER :: NYRS  ! No. years to simulate                             (y)
INTEGER :: IHRI  ! Start of model run                   (24-hr clock hr)
!----------------------------------------------------------------------!
! Calculated internally.
!----------------------------------------------------------------------!
INTEGER :: NDAY   ! No. internal timesteps per day                 (ITU)
REAL    :: DTTR   ! Tree growth time step                            (s)
INTEGER :: ITE1   ! Length of simulation                             (s)
INTEGER :: ITIMEI ! Start of model run                (internal time, s)
INTEGER :: ITIME  ! Internal time                                    (s)
INTEGER :: ITIMEE ! End of model run                  (internal time, s)
!----------------------------------------------------------------------!
END MODULE CONTROL_PARAMETERS
!======================================================================!