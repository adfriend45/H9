!======================================================================!
PROGRAM H9
!----------------------------------------------------------------------!
! A model to look at the effects of source/sink and competition
! dynamics on the response of natural forest to increasing atmospheric
! CO2.
!
! Put source code in /SOURCE
! Put run control file, 'driver.txt', in /EXECUTE
! Use './q' to compile and run the SOURCE code.
!----------------------------------------------------------------------!
! Authors            : Andrew D. Friend, Tim T. Rademacher
! Date started       : 18th July, 2014
! Date last modified : 8th October, 2014
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
CHARACTER (LEN = 100) :: driver         ! Filename for driver file.
CHARACTER (LEN = 100) :: output         ! Filename for output file.
INTEGER :: size
INTEGER, ALLOCATABLE :: seed (:)
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

READ (10, *) DTSRC  ! Source time step = 1 ITU                        (s)
READ (10, *) NITR   ! No. tree growth time steps per ITU              (n)
READ (10, *) NYRS   ! Length of model run from 1/1/year1              (y)
READ (10, *) YEARI  ! Start of model run              (calendar year, yr)
READ (10, *) MONI   ! Start of model run                   (Julian month)
READ (10, *) IHRI   ! Start of model run              (24-hour clock, hr)
READ (10, *) NMONAV ! No. months in a diagnostic acc period      (months)
READ (10, *) NIND   ! No. trees to simulate                           (n)
READ (10, *) F_OUT  ! Output flag for individual tree variable format

WRITE (20,'(A8,F10.2,A3)') 'DTSRC = ',DTSRC,'  s'
WRITE (20,'(A8,I10  ,A3)') 'NITR  = ',NITR ,'  n'
WRITE (20,'(A8,I10  ,A3)') 'NYRS  = ',NYRS ,'  y'
WRITE (20,'(A8,I10  ,A3)') 'IHRI  = ',IHRI ,' hr'

!----------------------------------------------------------------------!
ALLOCATE (LIVING    (NIND))
ALLOCATE (Cv        (NIND))
ALLOCATE (Aheart    (NIND))
ALLOCATE (ib        (NIND))
ALLOCATE (rold      (NIND))
ALLOCATE (H         (NIND))
ALLOCATE (Afoliage  (NIND))
ALLOCATE (fPAR      (NIND))
ALLOCATE (Acrown    (NIND))
ALLOCATE (LAIcrown  (NIND))
ALLOCATE (rwidth (NYRS,NIND))
ALLOCATE (Acrowns_layers (11000))
ALLOCATE (Acrowns_above  (NIND))
ALLOCATE (Afoliage_above (NIND))
ALLOCATE (ih    (NIND))
ALLOCATE (r     (NIND))
ALLOCATE (iPAR  (NIND))
ALLOCATE (shade (NIND))
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
rwidth (:,:) = 0.0
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Close run control text file.
!----------------------------------------------------------------------!
CLOSE (10)
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
CALL RANDOM_SEED (size=size)
ALLOCATE (seed(size))
CALL RANDOM_SEED (put=seed)
!CALL RANDOM_SEED
NIND_alive = 0
DO I = 1, NIND
  KI = I
  LIVING (I) = KI ! Assign index of living tree                      (n)
  CALL RANDOM_NUMBER (RANDOM)
  DO WHILE (RANDOM == 0.0)
    CALL RANDOM_NUMBER (RANDOM)
  END DO
  D = RANDOM * 0.01 + 0.001  ! Stem diameter                         (m)
  r (KI) = D / 2.0           ! Stem radius                           (m)
  rold (KI) = r (KI)         ! Saved stem radius                     (m)
  H (KI) = alpha * r (KI) ** beta  ! Stem height                     (m)
  !--------------------------------------------------------------------!
  ! Tree height as integer                                          (cm)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (100.0 * H (KI))
  !--------------------------------------------------------------------!
  ib (KI) = 0                 ! Height to base of crown             (cm)
  Dcrown = a_cd + b_cd * D             ! Crown diameter              (m)
  Acrown (KI) = pi * (Dcrown / 2.0) ** 2 ! Crown area              (m^2)
  Aheart (KI)= 0.0                     ! Heartwood area            (m^2)
  Asapwood = PI * r (KI) ** 2  - Aheart (KI) ! Sapwood area        (m^2)
  Afoliage (KI) = FASA * Asapwood      ! Foliage area              (m^2)
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  V = (FORMF / 3.0)  * pi * r (KI) ** 2 * H (KI) ! Stem volume     (m^3)
  Cv (KI) = SIGC * V                   ! Stem carbon                (kg)
  NIND_alive = NIND_alive + 1
END DO

!----------------------------------------------------------------------!
! Set up plot light profile.
!----------------------------------------------------------------------!
CALL light
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Set up trees and plot structure.
!----------------------------------------------------------------------!
CALL trees_structure
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Kill trees with no foliage area.
!----------------------------------------------------------------------!
CALL mortal
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Initialise diagnostic variables.
!----------------------------------------------------------------------!
NPP_ann_acc = 0.0 ! Accumulated annual NPP                  (kgC/m^2/yr)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Open model run diagnostics files.
!----------------------------------------------------------------------!
CALL getenv('OUTPUT',output)             
OPEN (21,FILE=output,STATUS='UNKNOWN')
IF (F_OUT == 1) THEN ! Output a txt file
  CALL getenv('OUTPUT2',output)
  OPEN (22,FILE=output,STATUS='UNKNOWN')
END IF
!----------------------------------------------------------------------!
WRITE (21,*) '8'            ! No. data columns in output_ann.txt
WRITE (21,*) NYRS           ! No. data lines   in output_ann.txt
WRITE (21,'(A86)') ' JYEAR NPP_ann_acc   Acrown(1)      rwidth         & 
&LAI   Aheart(1)  ib(1)    H(1)'
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
  JDAY = 1 + MOD (ITIME / NDAY, EDPERY)   ! Julian day            (days)
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
  ! Call GROW NITR times each ITU to grow tree stem volumes.
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
    !------------------------------------------------------------------!
    ! New canopy and tree structures based on growth, space, and light.
    !------------------------------------------------------------------!
    CALL trees_structure
    !------------------------------------------------------------------!
    ! New light distribution.
    !------------------------------------------------------------------!
    CALL light
    !------------------------------------------------------------------!
    ! Kill trees with no foliage area.
    !------------------------------------------------------------------!
    CALL mortal
    !------------------------------------------------------------------!
    LAI = 0.0
    !DO KI = 1, NIND_alive
    DO I = 1, NIND_alive
      KI = LIVING (I)
      !----------------------------------------------------------------!
      ! Canopy LAI                                             (m^2/m^2)
      !----------------------------------------------------------------!
      LAI = LAI + Afoliage (KI) / (Aplot + EPS) ! Plot LAI     (m^2/m^2)
      !----------------------------------------------------------------!
      ! Stem ring width                                             (mm)
      !----------------------------------------------------------------!
      rwidth (JYEAR-YEARI+1,KI) = (r (KI) - rold (KI))
      !----------------------------------------------------------------!
    END DO
    !------------------------------------------------------------------!
    CALL write_outputs
    write (20,*) NIND_alive 
    !------------------------------------------------------------------!
    ! Reset plot NPP diagnostic                             (kgC/m^2/yr)
    !------------------------------------------------------------------!
    NPP_ann_acc = 0.0
    !------------------------------------------------------------------!
    ! Save stem radii to compute ring widths                         (m)
    !------------------------------------------------------------------!
    rold (:) = r (:)
    !------------------------------------------------------------------!
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
! Close run documentation text file.
!----------------------------------------------------------------------!
CLOSE (20)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Close model run diagnostics files.
!----------------------------------------------------------------------!
CLOSE (21) ! Close the annual output file
IF (F_OUT == 1) CLOSE (22) ! Close the individual tree output file
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END PROGRAM H9
!======================================================================!
