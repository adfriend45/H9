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
! Date last modified : 6th August, 2014
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
USE NETCDF
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
IMPLICIT NONE
integer :: i,rPAR_base
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
ALLOCATE (ib        (NIND))
ALLOCATE (fPAR      (NIND))
ALLOCATE (Acrown    (NIND))
ALLOCATE (LAIcrown  (NIND))
ALLOCATE (Acrowns_above (NIND))
ALLOCATE (Afoliage_above (NIND))
ALLOCATE (ih (NIND))
ALLOCATE (r (NIND))
ALLOCATE (iPAR (NIND))
ALLOCATE (floss (NIND))
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
  r (KI) = D / 2.0            ! Stem radius                          (m)
  rold (KI) = r (KI)                   ! Saved stem radius           (m)
  H (KI) = alpha * r (KI) ** beta  ! Stem height                     (m)
  ib (KI) = 0                 ! Height to base of crown             (cm)
  Dcrown = a_cd + b_cd * D             ! Crown diameter              (m)
  Acrown (KI) = pi * (Dcrown / 2.0) ** 2 ! Crown area              (m^2)
  Acrown (KI) = MIN (Aplot,Acrown(KI))
  Aheart (KI)= 0.0                     ! Heartwood area            (m^2)
  Asapwood = PI * r (KI) ** 2  - Aheart (KI) ! Sapwood area        (m^2)
  Afoliage (KI) = FASA * Asapwood      ! Foliage area              (m^2)
  LAI = LAI + Afoliage (KI) / (Aplot + EPS) ! Plot LAI         (m^2/m^2)
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  V = (FORMF / 3.0)  * pi * r (KI) ** 2 * H (KI) ! Stem volume     (m^3)
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
WRITE (10,'(A86)') ' JYEAR NPP_ann_acc   Acrown(1)      rwidth         LAI  &
& Aheart(1)  ib(1)    H(1)'
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
    rwidth (JYEAR-YEARI+1) = (r (1) - rold (1)) ! Stem ring width       (mm)
    WRITE (10,'(I7,5F12.4,I7,F12.4)') JYEAR,NPP_ann_acc,Acrown(1),     &
    &                        1.0e3*rwidth(JYEAR-YEARI+1),              &
    &                        LAI,Aheart(1),ib(1),H(1)
    WRITE ( *,'(I7,5F12.4,I7,2F12.4)') JYEAR,NPP_ann_acc,Acrown(1),    &
    &                        1.0e3*rwidth(JYEAR-YEARI+1),             &
    &                        LAI,Aheart(1),ib(1),H(1),H(2)
    NPP_ann_acc = 0.0
    rold (1) = r (1)
    ! New structure for each tree based on new Cv.
    DO KI = 1, NIND
      V = Cv (KI) / SIGC ! Stem volume                               (m^3)
      r (KI) = (V / (( FORMF / 3.0) * PI * alpha)) ** (1.0 / (2.0 + beta))
      D = 2.0 * r (KI)                       ! Stem diameter           (m)
      H (KI) = alpha * r (KI) ** beta        ! Stem height             (m)
      Dcrown = a_cd + b_cd * D          ! Crown diameter               (m)
      Acrown (KI) = PI * (Dcrown / 2.0) ** 2 ! Crown area            (m^2)
      Acrown (KI) = MIN (Aplot,Acrown(KI))
      Afoliage (KI) = FASA * Asapwood   ! Foliage area                (m^2)
    END DO
    ! Squeeze canopy areas into plot.
    CALL light
    DO KI = 1, NIND
      Aheart (KI) = Aheart (KI) + floss (KI) * Afoliage (KI) / FASA
      Asapwood = PI * r (KI) ** 2 - Aheart (KI) ! Sapwood area         (m^2)
      rPAR_base = FLOOR (floss (KI) * (100.0 * H (KI) - FLOAT (ib (KI)))) + &
      &           ib (KI)
      rPAR_base = MIN (CEILING(100.0*H(KI)),rPAR_base)
      !floss = (FLOAT (rPAR_base (KI)) - FLOAT (ib (KI))) / &
      !&       (        100.0 * H (KI) - FLOAT (ib (KI)))
      !floss = MAX (0.0,floss)
      !floss = MIN (1.0,floss)
      !ib (KI) = MAX (ib (KI),rPAR_base)
      !write (*,*) JYEAR-YEARI+1,ki,floss,ib(ki),lai,D,Acrown
    END DO ! KI = 1, NIND
    CALL light
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
