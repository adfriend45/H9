!======================================================================!
SUBROUTINE mortal
!----------------------------------------------------------------------!
! Kill trees that do not deserve to live.
!----------------------------------------------------------------------!
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!
INTEGER :: LIVING_New (NIND_max)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! To start assume all are alive.
!----------------------------------------------------------------------!
LIVING_New (:) = 1
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
LIVING_Individuals1: DO I = 1, NIND_alive
!----------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Assign tree index.
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Score a 0 if dies (i.e. no foliage).
  !--------------------------------------------------------------------!
  IF (Afoliage (KI) <= 0.0) THEN
    LIVING_New (I) = 0
  END IF
  !--------------------------------------------------------------------!

!----------------------------------------------------------------------!
END DO LIVING_Individuals1
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Initialise count of living trees                                   (n)
!----------------------------------------------------------------------!
J = 0
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
LIVING_Individuals2: DO I = 1, NIND_alive

  !--------------------------------------------------------------------!
  ! Test if tree is living.
  !--------------------------------------------------------------------!
  IF (LIVING_New (I) == 1) THEN
  !--------------------------------------------------------------------!

    !------------------------------------------------------------------!
    ! Increment count of living trees                                (n)
    !------------------------------------------------------------------!
    J = J + 1
    !------------------------------------------------------------------!

    !------------------------------------------------------------------!
    ! Confirm or shuffle up index of living tree                     (n)
    !------------------------------------------------------------------!
    LIVING (J) = LIVING (I)
    !------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  END IF ! Test if living.
  !--------------------------------------------------------------------!

!----------------------------------------------------------------------!
END DO LIVING_Individuals2
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Save number of living trees                                        (n)
!----------------------------------------------------------------------!
NIND_alive = J
write (*,*) 'NIND_alive=',NIND_alive,LIVING(1)
if (NIND_alive == 0) stop
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! It may be sensible to tidy up remaining dead tree locations.
!----------------------------------------------------------------------!
END SUBROUTINE mortal
!======================================================================!
