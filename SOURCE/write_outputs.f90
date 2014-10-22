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
8000 format (i5, i10, i5, i10, f8.1)

INDIVIDUALS: DO I = 1, NIND_alive
  KI = LIVING (I)
  IF (F_OUT == 1) THEN
    WRITE (22, 8001) JYEAR-YEARI+1, KI, ib (KI), ih (KI),            &
                     LAIcrown (KI), r (KI), Acrown (KI), shade (KI)
  END IF
END DO INDIVIDUALS
8001 format (4I5,4F15.7)

KI = LIVING (1)
WRITE (21, 8002) JYEAR,NIND_alive,NPP_ann_acc,Acrown(KI),              &
                 1.0e3*rwidth(JYEAR-YEARI+1,KI),LAI,Aheart(KI),         &
                 ib(KI),h(KI),r(KI)
8002 format (I7,I7,5F12.4,I5,2F10.4)

KI = LIVING (1)
WRITE (23,8003) JYEAR,Cv(KI),Aheart(KI),FLOAT(ib(KI))* &
&            DZ_CROWN_M,FLOAT(ih(KI))* &
&            DZ_CROWN_M,Acrown(KI), &
&            Afoliage(KI), &
&            Afoliage(KI)/(Acrown(KI)*FLOAT(ih(KI))* &
&            DZ_CROWN_M), &
&            Afoliage(KI)/(Acrown(KI)*FLOAT(ih(KI)-ib(KI))* &
&            DZ_CROWN_M),&
&            1.0e4*rwidth(JYEAR-YEARI+1,KI),LAI,NIND_alive
8003 FORMAT (I5,10F12.4,I10)

!---------------------------------------------------------------------!
END SUBROUTINE write_outputs
!=====================================================================!
