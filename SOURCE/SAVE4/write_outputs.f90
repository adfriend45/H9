!=====================================================================!
SUBROUTINE write_outputs
!---------------------------------------------------------------------!
! Output all necessary diagnostics at the end of the simulated year.
! This should eventually include netcdf output and options for variable
! temporal averaging. The output file units are as follows:
!     20    - log file
!     21    - annual outputs
!     22    - individual tree outputs
!---------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE

!---------------------------------------------------------------------!
IMPLICIT NONE
!---------------------------------------------------------------------!

IF (F_OUT == 1) WRITE (22,8000) JYEAR,NIND_alive,NYRS,NIND_max,Aplot
8000 FORMAT (I5,I10,I5,I10,F8.1)

individuals: DO I = 1, NIND_alive
  KI = LIVING (I)
  IF (F_OUT == 1) THEN
    ! Normally 'output_trees.txt'.
    WRITE (22, 8001) FLOAT (ib(KI))*DZ_CROWN_M,                       &
                     FLOAT (ih(KI))*DZ_CROWN_M,                       &
                     Acrown   (KI),                                   &
                     shade    (KI),                                   &
                     LAIcrown (KI),                                   &
                     r        (KI)
  END IF
END DO individuals
8001 format (6F15.7)

!KI = LIVING (1)
KI = 62
! Normally 'output_ann.txt'.
WRITE (21, 8002) JYEAR, &
&  NIND_alive                    , &
&  NPP_ann_acc                   , &
&  Acrown(KI)                    , &
&  1.0e3*rwidth(JYEAR-YEARI+1,KI), &
&  LAI                           , &
&  Aheart(KI)                    , &
&  ib(KI)                        , &
&  h(KI)                         , &
&  2.0*r(KI)
8002 format (I7,I7,5F12.4,I7,2F10.4)

!KI = LIVING (5)
! Normally 'diag.txt'.
WRITE (23,8003) JYEAR                   , & ! 0
&               2.0*r(KI)               , & ! 1
&               Bfoliage(KI)            , & ! 2
&               Cv     (KI)             , & ! 3
&               Aheart (KI)             , & ! 4
&               FLOAT(ib(KI))*DZ_CROWN_M, & ! 5
&               FLOAT(ih(KI))*DZ_CROWN_M, & ! 6
&               Acrown(KI)              , & ! 7
&               Afoliage(KI)            , & ! 8
&               1.0e3*rwidth(JYEAR-YEARI+1,KI), & !  9
&               LAI                           , & ! 10
&               LAIcrown (KI)                 , & ! 11
&               NIND_alive                    , & ! 12
&               NPP_ann_acc                       ! 13
8003 FORMAT (I5,11F12.4,I10,F12.4)

!---------------------------------------------------------------------!
END SUBROUTINE write_outputs
!=====================================================================!
