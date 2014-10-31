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

INDIVIDUALS: DO I = 1, NIND_alive
  KI = LIVING (I)
  IF (F_OUT == 1) THEN
    WRITE (22, 8001) FLOAT (ib(KI))*DZ_CROWN_M,                       &
                     FLOAT (ih(KI))*DZ_CROWN_M,                       &
                     Acrown   (KI),                                   &
                     shade    (KI),                                   &
                     LAIcrown (KI),                                   &
                     r        (KI)
  END IF
END DO INDIVIDUALS
8001 format (6F15.7)

KI = LIVING (1)
WRITE (21, 8002) JYEAR,NIND_alive,NPP_ann_acc,Acrown(KI),             &
                 1.0e3*rwidth(JYEAR-YEARI+1,KI),LAI,Aheart(KI),       &
                 ib(KI),h(KI),2.0*r(KI)
8002 format (I7,I7,5F12.4,I7,2F10.4)

KI = LIVING (1)
WRITE (23,8003) JYEAR                   , & ! 0
&               Cv     (KI)             , & ! 1
&               Aheart (KI)             , & ! 2
&               FLOAT(ib(KI))*DZ_CROWN_M, & ! 3
&               FLOAT(ih(KI))*DZ_CROWN_M, & ! 4
&               Acrown(KI)              , & ! 5
&               Afoliage(KI)            , & ! 6
&               Afoliage(KI)/(Acrown(KI)*FLOAT(ih(KI))*DZ_CROWN_M)       , & ! 7
&               Afoliage(KI)/(Acrown(KI)*FLOAT(ih(KI)-ib(KI))*DZ_CROWN_M), & ! 8
&               1.0e4*rwidth(JYEAR-YEARI+1,KI), & ! 9
&               LAI                           , & ! 10
&               NIND_alive                        ! 11
8003 FORMAT (I5,10F12.4,I10)

!---------------------------------------------------------------------!
END SUBROUTINE write_outputs
!=====================================================================!
