!======================================================================!
PROGRAM H9
!----------------------------------------------------------------------!
! A model to look at the effects of source/sink and competition
! dynamics on the response of natural forest to increasing atmospheric
! CO2.
!
! Put source code and makefile in /SOURCE
! Put run control file, 'driver.txt', in /EXECUTE
! Create /OUTPUT
! Use './q' to compile and run the SOURCE code.
!----------------------------------------------------------------------!
! Authors            : Andrew D. Friend, Tim T. Rademacher
! Date started       : 18th July, 2014
! Date last modified : 26th November, 2014
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
!INTEGER :: size
!INTEGER, ALLOCATABLE :: seed (:)
INTEGER :: n
INTEGER, DIMENSION (1) :: seed = (/3/)
REAL :: Asapwood,Afoliage_sum
INTEGER :: tall,short
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

READ (10,*) DTSRC    ! Source time step = 1 ITU                      (s)
READ (10,*) NITR     ! No. tree growth time steps per ITU            (n)
READ (10,*) NYRS     ! Length of model run from 1/1/year1            (y)
READ (10,*) YEARI    ! Start of model run            (calendar year, yr)
READ (10,*) MONI     ! Start of model run                 (Julian month)
READ (10,*) IHRI     ! Start of model run            (24-hour clock, hr)
READ (10,*) NMONAV   ! No. months in a diagnostic acc period    (months)
READ (10,*) NIND_max ! Max No. trees to simulate                     (n)
READ (10,*) F_OUT    ! Flag for individual output file   (0=none; 1=txt)
READ (10,*) DZ_CROWN ! Crown depth division                         (mm)
READ (10,*) H_MAX    ! Maximum possible tree height                  (m)

!----------------------------------------------------------------------!
! Crown depth division                                               (m)
!----------------------------------------------------------------------!
DZ_CROWN_M = DZ_CROWN / 1000.0
!----------------------------------------------------------------------!

WRITE (20,'(A8,F10.2,A3)') 'DTSRC = ',DTSRC,'  s'
WRITE (20,'(A8,I10  ,A3)') 'NITR  = ',NITR ,'  n'
WRITE (20,'(A8,I10  ,A3)') 'NYRS  = ',NYRS ,'  y'
WRITE (20,'(A8,I10  ,A3)') 'IHRI  = ',IHRI ,' hr'

!----------------------------------------------------------------------!
N_LAYERS = (1000 * H_MAX) / DZ_CROWN
ALLOCATE (LIVING    (NIND_max))
ALLOCATE (Cv        (NIND_max))
ALLOCATE (Aheart    (NIND_max))
ALLOCATE (ib        (NIND_max))
ALLOCATE (rold      (NIND_max))
ALLOCATE (H         (NIND_max))
ALLOCATE (Afoliage  (NIND_max))
ALLOCATE (Bfoliage  (NIND_max))
ALLOCATE (fPAR      (NIND_max))
ALLOCATE (Acrown    (NIND_max))
ALLOCATE (LAIcrown  (NIND_max))
ALLOCATE (ih        (NIND_max))
ALLOCATE (r         (NIND_max))
ALLOCATE (iPAR      (NIND_max))
ALLOCATE (iPAR_base (NIND_max))
ALLOCATE (shade     (NIND_max))
ALLOCATE (rwidth    (NYRS,NIND_max))
ALLOCATE (Acrowns_layers      (N_LAYERS))
ALLOCATE (Acrowns_above       (NIND_max))
ALLOCATE (Afoliage_above      (NIND_max))
ALLOCATE (Afoliage_above_base (NIND_max))
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Initialise all annual stem ring widths in all trees and years      (m)
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
!CALL RANDOM_SEED (size=size)
!ALLOCATE (seed(size))
CALL RANDOM_SEED (size=n)
NIND_alive = 0
DO I = 1, NIND_max
  KI = I
  LIVING (I) = KI ! Assign index of living tree                      (n)
  CALL RANDOM_NUMBER (RANDOM)
  DO WHILE (RANDOM == 0.0)
    CALL RANDOM_NUMBER (RANDOM)
  END DO
  !D = RANDOM * 0.01 + 0.001  ! Stem diameter                        (m)
  D = RANDOM * 0.001 + 0.0001  ! Stem diameter                       (m)
  D = 0.001 !****adf
  r    (KI) = D / 2.0  ! Stem radius                                 (m)
  !--------------------------------------------------------------------!
  ! Save stem radius for ring width computation                      (m)
  !--------------------------------------------------------------------!
  rold (KI) = r (KI)
  !--------------------------------------------------------------------!
  ! Stem height                                                      (m)
  !--------------------------------------------------------------------!
  H (KI) = alpha * r (KI) ** beta
  !--------------------------------------------------------------------!
  ! Tree height as integer                                    (DZ_CROWN)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (H (KI) / DZ_CROWN_M)
  !--------------------------------------------------------------------!
  ib (KI) = 0                 ! Height to base of crown       (DZ_CROWN)
  Dcrown = b_cd * D           ! Crown diameter                       (m)
  Acrown (KI) = pi * (Dcrown / 2.0) ** 2 ! Crown area              (m^2)
  Aheart (KI)= 0.0            ! Heartwood area                     (m^2)
  Asapwood = PI * r (KI) ** 2  - Aheart (KI) ! Sapwood area        (m^2)
  !--------------------------------------------------------------------!
  ! Foliage area from sapwood area                                 (m^2)
  !--------------------------------------------------------------------!
  Afoliage (KI) = FASA * Asapwood
  Bfoliage (KI) = Afoliage (KI) / SLA
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)        ! (m^2/m^2)
  V = (FORMF / 3.0) * pi * r (KI) ** 2 * H (KI) ! Stem volume      (m^3)
  Cv (KI) = SIGC * V          ! Stem carbon                         (kg)
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
CALL getenv('OUTPUT',output) ! Normally 'output_ann.txt'.
OPEN (21,FILE=output,STATUS='UNKNOWN')
!----------------------------------------------------------------------!
WRITE (21,*) '8'            ! No. data columns in output_ann.txt
WRITE (21,*) NYRS           ! No. data lines   in output_ann.txt
WRITE (21,'(A100)') ' JYEAR NIND_alive NPP_ann_acc  Acrown(1)  rwidth(1&
&)       LAI      Aheart(1) ib(1)    H(1)     D(1)'
IF (F_OUT == 1) THEN ! Output a txt file
  CALL getenv('OUTPUT2',output) ! Normally 'output_trees.txt'
  OPEN (22,FILE=output,STATUS='UNKNOWN')
END IF
!----------------------------------------------------------------------!
CALL getenv('OUTPUT3',output) ! Normally 'diag.txt'.
OPEN (23,FILE=output,STATUS='UNKNOWN')
WRITE (23,*) '12' ! No. data columns in diag.txt
WRITE (23,*) NYRS ! No. data lines   in diag.txt
WRITE (23,'(A123)') '&
&JYEAR      &
&D           &
&Bfoliage    &
&Cv          &
&Aheart      &
&ib*DZ_M     &
&ih*DZ_M     &
&Acrown      &
&Afoliage    &
&rwidth      &
&LAI         &
&LAIcrown    &
&NIND_alive'
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
  ! Call GROW NITR times each ITU to update tree stem carbon mass.
  !--------------------------------------------------------------------!
  DO NT = 1, NITR
    CALL grow
  END DO
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Accumulated diagnostics and re-calculate plot light profile at end
  ! of each year.
  !--------------------------------------------------------------------!
  end_of_year : IF ((MOD (ITIME, NDAY) == 0) .AND. &
                &   (JDAY == JDENDOFM (12))) THEN
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
    ! Initialise sum of foliage areas                              (m^2)
    !------------------------------------------------------------------!
    Afoliage_sum = 0.0
    !------------------------------------------------------------------!
    DO I = 1, NIND_alive
      !----------------------------------------------------------------!
      ! Assign index of living tree                                  (n)
      !----------------------------------------------------------------!
      KI = LIVING (I)
      !----------------------------------------------------------------!
      ! Accumulate plot foliage area across individuals            (m^2)
      !----------------------------------------------------------------!
      Afoliage_sum = Afoliage_sum + Afoliage (KI)
      !----------------------------------------------------------------!
      ! Stem annual ring width                                       (m)
      !----------------------------------------------------------------!
      rwidth (JYEAR-YEARI+1,KI) = (r (KI) - rold (KI))
      !----------------------------------------------------------------!
    END DO
    !------------------------------------------------------------------!
    ! Plot LAI                                                 (m^2/m^2)
    !------------------------------------------------------------------!
    LAI = Afoliage_sum / (Aplot + EPS)
    !------------------------------------------------------------------!
    CALL write_outputs
    write (20,*) NIND_alive
    !------------------------------------------------------------------!
    ! Reset plot NPP diagnostic                             (kgC/m^2/yr)
    !------------------------------------------------------------------!
    NPP_ann_acc = 0.0
    !------------------------------------------------------------------!
    ! Save stem radii for ring width computation                     (m)
    !------------------------------------------------------------------!
    rold (:) = r (:)
    !------------------------------------------------------------------!
    WRITE (*,*) 'No. per hectare = ',10000.0*FLOAT(NIND_alive)/Aplot
    !------------------------------------------------------------------!
  ENDIF end_of_year
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ITIME = ITIME + 1 ! Increment Internal Time                      (ITU)
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
IF (F_OUT == 1) CLOSE (22) ! Close the individual tree output file.
CLOSE (23)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Diagnostic of final canopy profile.
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
INDIVIDUALS_layers: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! ib index starts at 0, so assume ib+1 is lowest layer.
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_layers
!----------------------------------------------------------------------!
! Heights where crowns are in plot (DZ_CROWN).
!----------------------------------------------------------------------!
tall  = MAXVAL (ih(LIVING(1:NIND_alive)))
short = MINVAL (ib(LIVING(1:NIND_alive)))
!----------------------------------------------------------------------!
DO L = tall, short, -1 
  WRITE (99,*) L,FLOAT(L)*DZ_CROWN_M,Acrowns_layers(L)
END DO
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END PROGRAM H9
!======================================================================!
