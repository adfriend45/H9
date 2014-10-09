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

 
WRITE (22, 8000) JYEAR, NIND_alive, NYRS, NIND, Aplot
8000 format (i5, i10, i5, i10, f8.1)

<<<<<<< HEAD
IF (F_IND_OUT == 1) THEN
  INDIVIDUALS: DO KI = 1, NIND_alive
    WRITE (22, 8001) JYEAR-YEARI+1, UID (KI), ib (KI), ih (KI),       &
                     LAIcrown (KI), r (KI), Acrown (KI), shade (KI)
  END DO INDIVIDUALS
ELSE IF (F_IND_OUT == 2) THEN
  
END IF
8001 format (2i5, 2i10, 4f15.7)
=======
INDIVIDUALS: DO I = 1, NIND_alive
  KI = LIVING (I)
  write (22, 8001) JYEAR-YEARI+1, KI, ib (KI), ih (KI),         &
                   LAIcrown (KI), r (KI), Acrown (KI), shade (KI)
  write (*, 8001)  JYEAR-YEARI+1, KI, ib (KI), ih (KI),              &
                   LAIcrown (KI), r (KI), Acrown (KI), shade (KI)
END DO INDIVIDUALS
8001 format (4i5, 4f15.7)
>>>>>>> FETCH_HEAD

INDIVIDUALS2: DO KI = 1, NIND_alive
  WRITE (*, *) 'H9', JYEAR-YEARI+1, KI, ib (KI), h (KI),              &
                     LAIcrown (KI), r (KI), Acrown (KI)
END DO INDIVIDUALS2
                     
WRITE (21, 8002) JYEAR, NPP_ann_acc, Acrown (1),                      &
                 1.0e3*rwidth (JYEAR-YEARI+1, 1), LAI, Aheart (1),    &
                 ib (1), h (1)
<<<<<<< HEAD
WRITE (*, 8002) JYEAR, NPP_ann_acc, Acrown (1),                      &
                 1.0e3*rwidth (JYEAR-YEARI+1, 1), LAI, Aheart (1),   &
                 ib (1), h (1)
=======
write (*, 8002) JYEAR, NPP_ann_acc, Acrown (211),                      &
                 1.0e3*rwidth (JYEAR-YEARI+1, 211), LAI, Aheart (211),   &
                 ib (211), h (211)
>>>>>>> FETCH_HEAD
8002 format (i7, 5f12.4, i7, f12.4)

!---------------------------------------------------------------------!
END SUBROUTINE write_outputs
!=====================================================================!
