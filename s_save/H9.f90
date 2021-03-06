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
! Date last modified : 4th August, 2014
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
USE NETCDF
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
IMPLICIT NONE
integer :: i
real :: floss
!----------------------------------------------------------------------!
CHARACTER driver*40
CHARACTER output*40
!----------------------------------------------------------------------!
! Open run control text file.
!----------------------------------------------------------------------!
CALL getenv('DRIVER',driver)
OPEN (10,FILE=driver,STATUS='OLD')
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
ALLOCATE (rwidth    (NYRS))
ALLOCATE (fad      (11000))
ALLOCATE (cad      (11000))
ALLOCATE (rPAR     (11000))
ALLOCATE (Cv        (NIND))
ALLOCATE (rold      (NIND))
ALLOCATE (H         (NIND))
ALLOCATE (Afoliage  (NIND))
ALLOCATE (Aheart    (NIND))
ALLOCATE (rPAR_base (NIND))
ALLOCATE (ib        (NIND))
ALLOCATE (fPAR      (NIND))
ALLOCATE (Acrown    (NIND))
ALLOCATE (LAIcrown  (NIND))
ALLOCATE (Acrowns_above (NIND))
ALLOCATE (Afoliage_above (NIND))
ALLOCATE (ih (NIND))
ALLOCATE (Acrown_layer   (11000,NIND))
ALLOCATE (Afoliage_layer (11000,NIND))
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
LAI = 0.0 ! Plot LAI                                           (m^2/m^2)
DO KI = 1, NIND
  CALL RANDOM_NUMBER (RANDOM)
  DO WHILE (RANDOM == 0.0)
    CALL RANDOM_NUMBER (RANDOM)
  END DO
  D = RANDOM * 0.005 + 0.001 ! Stem diameter                         (m)
  !D = RANDOM * 0.5 + 0.001 ! Stem diameter                         (m)
  r = D / 2.0            ! Stem radius                               (m)
  rold (KI) = r                        ! Saved stem radius           (m)
  H (KI) = alpha * r ** beta  ! Stem height                          (m)
  ib (KI) = 0                 ! Height to base of crown             (cm)
  Dcrown = a_cd + b_cd * D             ! Crown diameter              (m)
  Acrown (KI) = pi * (Dcrown / 2.0) ** 2 ! Crown area              (m^2)
  Acrown (KI) = MIN (Aplot,Acrown(KI))
  Aheart (KI)= 0.0                     ! Heartwood area            (m^2)
  Asapwood = PI * r ** 2  - Aheart (KI) ! Sapwood area             (m^2)
  Afoliage (KI) = FASA * Asapwood      ! Foliage area              (m^2)
  LAI = LAI + Afoliage (KI) / (Aplot + EPS) ! Plot LAI         (m^2/m^2)
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  V = (FORMF / 3.0)  * pi * r ** 2 * H (KI) ! Stem volume          (m^3)
  Cv (KI) = SIGC * V                   ! Stem carbon                (kg)
END DO

!----------------------------------------------------------------------!
! Set up plot light profile.
!----------------------------------------------------------------------!
CALL light
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Initialise diagnostic variables.
!----------------------------------------------------------------------!
NPP_ann_acc = 0.0 ! Accumulated annual NPP                  (kgC/m^2/yr)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Open model run diagnostics file.
!----------------------------------------------------------------------!
CALL getenv('OUTPUT',output)
OPEN (10,FILE=output,STATUS='UNKNOWN')
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
  ! Call GROW NITR times each ITU.
  !--------------------------------------------------------------------!
  DO NT = 1, NITR
    CALL grow
  END DO
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Accumulated diagnostics and re-calculate plot light profile at end
  ! of each year.
  !--------------------------------------------------------------------!
  IF ((MOD (ITIME, NDAY) == 0) .AND. (JDAY == JDENDOFM (12))) THEN
    LAI = 0.0
    DO KI = 1, NIND
      LAI = LAI + Afoliage (KI) / (Aplot + EPS) ! Plot LAI     (m^2/m^2)
      LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
    END DO
    rwidth (JYEAR-YEARI+1) = (r - rold (1)) ! Stem ring width       (mm)
    WRITE (10,'(I7,5F12.4,I7,F12.4)') JYEAR,NPP_ann_acc,Acrown(1),     &
    &                        1.0e3*rwidth(JYEAR-YEARI+1),              &
    &                        LAI,Aheart(1),ib(1),H(1)
    WRITE ( *,'(I7,5F12.4,I7,2F12.4)') JYEAR,NPP_ann_acc,Acrown(1),    &
    &                        1.0e3*rwidth(JYEAR-YEARI+1),             &
    &                        LAI,Aheart(1),ib(1),H(1),H(2)
    NPP_ann_acc = 0.0
    rold (1) = r
    CALL light
    DO KI = 1, NIND
      floss = (FLOAT (rPAR_base (KI)) - FLOAT (ib (KI))) / &
      &       (        100.0 * H (KI) - FLOAT (ib (KI)))
      floss = MAX (0.0,floss)
      floss = MIN (1.0,floss)
      !Aheart = Aheart + floss * Afoliage / FASA
      !ib (KI) = MAX (ib (KI),rPAR_base (KI))
      !write (*,*) JYEAR-YEARI+1,ki,floss,ib(ki),lai,D,Acrown
    END DO ! KI = 1, NIND
    !stop
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
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END PROGRAM H9
!======================================================================!
