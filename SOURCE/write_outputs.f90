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

write (22, 8000) JYEAR, NIND_alive, NYRS, NIND_max, Aplot
8000 format (i5, i10, i5, i10, f8.1)

INDIVIDUALS: DO I = 1, NIND_alive
  KI = LIVING (I)
  write (22, 8001) JYEAR-YEARI+1, KI, ib (KI), ih (KI),         &
                   LAIcrown (KI), r (KI), Acrown (KI), shade (KI)
  write (*, 8001)  JYEAR-YEARI+1, KI, ib (KI), ih (KI),              &
                   LAIcrown (KI), r (KI), Acrown (KI), shade (KI)
END DO INDIVIDUALS
8001 format (4i5, 4f15.7)

write (21, 8002) JYEAR, NPP_ann_acc, Acrown (1),                      &
                 1.0e3*rwidth (JYEAR-YEARI+1, 1), LAI, Aheart (1),    &
                 ib (1), h (1)
write (*, 8002) JYEAR, NPP_ann_acc, Acrown (1),                      &
                 1.0e3*rwidth (JYEAR-YEARI+1, 1), LAI, Aheart (1),   &
                 ib (1), h (1)
8002 format (i7, 5f12.4, i7, f12.4)

WRITE (23,*) JYEAR,Cv(1),Aheart(1),ib(1),ih(1)

!---------------------------------------------------------------------!
END SUBROUTINE write_outputs
!=====================================================================!
