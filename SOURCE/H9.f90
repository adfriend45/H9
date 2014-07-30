!======================================================================!
PROGRAM H9
!----------------------------------------------------------------------!
! A model to look at effects of source/sink and competition dynamics on
! the response of natural forest to increasing atmospheric CO2.
!
! Put source code in /SOURCE
! Put run control file, 'driver.txt', in /EXECUTE
! Use './q' to compile and run the SOURCE code.
!----------------------------------------------------------------------!
! Author             : Andrew D. Friend
! Date started       : 18th July, 2014
! Date last modified : 30th July, 2014
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
USE NETCDF
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Open run control text file.
!----------------------------------------------------------------------!
OPEN (10,FILE='/store/H9/EXECUTE/driver.txt',STATUS='OLD')
!----------------------------------------------------------------------!
! Open run documentation text file.
!----------------------------------------------------------------------!
OPEN (20,FILE='run.txt',STATUS='UNKNOWN')
!----------------------------------------------------------------------!

READ (10,*) DTSRC  ! Source time step = 1 ITU                        (s)
READ (10,*) NITR   ! No. tree growth time steps per ITU              (n)
READ (10,*) NYRS   ! Length of model run from 1/1/year1              (y)
READ (10,*) YEARI  ! Start of model run              (calendar year, yr)
READ (10,*) MONI   ! Start of model run                   (Julian month)
READ (10,*) IHRI   ! Start of model run              (24-hour clock, hr)
READ (10,*) NMONAV ! No. months is a diagnostic acc period      (months)
READ (10,*) NIND   ! No. trees to simulate                           (n)

WRITE (20,'(A8,F10.2,A3)') 'DTSRC = ',DTSRC,'  s'
WRITE (20,'(A8,I10  ,A3)') 'NITR  = ',NITR ,'  n'
WRITE (20,'(A8,I10  ,A3)') 'NYRS  = ',NYRS ,'  y'
WRITE (20,'(A8,I10  ,A3)') 'IHRI  = ',IHRI ,' hr'

!----------------------------------------------------------------------!
ALLOCATE (rwidth (NYRS))
ALLOCATE (D  (NIND))
ALLOCATE (Cv (NIND))
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
rwidth (:) = 0.0
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Close run control text file.
!----------------------------------------------------------------------!
CLOSE (10)
!----------------------------------------------------------------------!
! Close run documentation text file.
!----------------------------------------------------------------------!
CLOSE (20)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Set internal time control parameters.
!----------------------------------------------------------------------!
NDAY = 2 * NINT (0.5 * SPERED / DTSRC) ! No. ITUs per day          (ITU)
DTTR = DTSRC / FLOAT (NITR)  ! Tree growth timestep                  (s)
ITE1 = NYRS * EDPERY * NDAY  ! Length of simulation                (ITU)
IHRI = 0                     ! Start of model run       (24-hr clock hr)
ITIMEI= IHRI * NDAY / HPERED ! Start of model run                  (ITU)
ITIME = ITIMEI               ! Initialise internal time            (ITU)
ITIMEE = ITE1                ! End of model run                    (ITU)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Initialise state variables.
!----------------------------------------------------------------------!
CALL RANDOM_SEED
DO KI = 1, NIND
  CALL RANDOM_NUMBER (RANDOM)
  DO WHILE (RANDOM == 0.0)
    CALL RANDOM_NUMBER (RANDOM)
  END DO
  D = RANDOM * 0.01      ! Stem diameter                             (m)
  r = D / 2.0            ! Stem radius                               (m)
  rold (KI) = r                        ! Saved stem radius           (m)
  H = alpha * r ** beta  ! Stem height                               (m)
  Dcrown = a_cd + b_cd * D             ! Crown diameter              (m)
  Acrown = pi * (Dcrown / 2.0) ** 2    ! Crown area                (m^2)
  Acrown = MIN (Parea,Acrown)
  LAI (KI) = Afoliage / (Acrown + EPS) ! Leaf area index       (m^2/m^2)
  Aheart (KI) = 0.0                    ! Heartwood area            (m^2)
  Asapwood = PI * r ** 2  - Aheart     ! Sapwood area                (m^2)
  Afoliage = FASA * Asapwood           ! Foliage area                (m^2)
  V = (FORMF / 3.0)  * pi * r ** 2 * H ! Stem volume                 (m^3)
  Cv (I) = SIGC * V                    ! Stem carbon                (kg)
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Initialise diagnostic variables.
!----------------------------------------------------------------------!
NPP_ann_acc = 0.0 ! Accumulated annual NPP                  (kgC/m^2/yr)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Open model run diagnostics file.
!----------------------------------------------------------------------!
OPEN (10,FILE='/store/H9/OUTPUT/output_ann.txt',STATUS='UNKNOWN')
!----------------------------------------------------------------------!
WRITE (10,*) '8'            ! No. data columns in output_ann.txt
WRITE (10,*) NYRS           ! No. data lines   in output_ann.txt
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Main loop.
!----------------------------------------------------------------------!
DO WHILE (ITIME < ITIMEE)
!----------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  JYEAR = YEARI + ITIME / (NDAY * EDPERY) ! Julian calendar year    (yr)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  JDAY = 1 + MOD (ITIME / NDAY, EDPERY) ! Julian day              (days)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Compute current month.
  !--------------------------------------------------------------------!
  JMON = 1
  DO WHILE (JDAY > JDENDOFM (JMON))
    JMON = JMON + 1
  END DO
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Compute light profile down through plot.
  !--------------------------------------------------------------------!
  CALL LIGHT
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Call GROW NITR times each ITU.
  !--------------------------------------------------------------------!
  DO NT = 1, NITR
    CALL GROW
  END DO
  !--------------------------------------------------------------------!

  ! Grow heartwood if low light at crown base, 1-cm/ITU.
  IF (PAR_base < 0.05) THEN
      Aheart = Aheart + 0.01 / FASA
  END IF

  !--------------------------------------------------------------------!
  ! Accumulated diagnostics.
  !--------------------------------------------------------------------!
  IF ((MOD (ITIME, NDAY) == 0) .AND. (JDAY == JDENDOFM (12))) THEN
    rwidth (JYEAR-YEARI+1) = (r - rold) ! Stem ring width           (mm)
    WRITE (10,'(I7,7F12.4)') JYEAR,NPP_ann_acc,Acrown,                 &
    &                        1.0e3*rwidth(JYEAR-YEARI+1),              &
    &                        LAI,Aheart,PAR_base,H
    NPP_ann_acc = 0.0
    rold = r
  ENDIF
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ITIME = ITIME + 1 ! Increment Internal Time Units.
  !--------------------------------------------------------------------!

!----------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!
! End of main loop (WHILE (ITIME < ITIMEE)).
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Close model run diagnostics files.
!----------------------------------------------------------------------!
CLOSE (10)
CLOSE (11)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END PROGRAM H9
!======================================================================!
