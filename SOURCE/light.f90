!======================================================================!
SUBROUTINE LIGHT
!----------------------------------------------------------------------!
! Compute light profile in plot.
!----------------------------------------------------------------------!
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
PAR_base = EXP (-0.5 * LAI)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE LIGHT
!======================================================================!