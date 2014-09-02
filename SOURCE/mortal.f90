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
NIND_alive = 0
DO KI = 1, NIND
  IF (Afoliage (KI) > 0.0) THEN
    NIND_alive = NIND_alive + 1
    IF (NIND_alive .NE. KI) THEN
      CV        (NIND_alive) = Cv       (KI)
      Aheart    (NIND_alive) = Aheart   (KI)
      ib        (NIND_alive) = ib       (KI)
      ih        (NIND_alive) = ih       (KI)
      rwidth    (:,NIND_alive) = rwidth (:,KI)
      rold      (NIND_alive) = rold     (KI)
      H         (NIND_alive) = H        (KI)
      Afoliage  (NIND_alive) = Afoliage (KI)
      fPAR      (NIND_alive) = fPAR     (KI)
      Acrown    (NIND_alive) = Acrown   (KI)
      LAIcrown  (NIND_alive) = LAIcrown (KI)
      r         (NIND_alive) = r        (KI)
      iPAR      (NIND_alive) = iPAR     (KI)
    END IF
  END IF
END DO
!----------------------------------------------------------------------!
! Tidy up remaining dead tree locations.
!----------------------------------------------------------------------!
Afoliage (NIND_alive+1:NIND) = 0.0
!----------------------------------------------------------------------!
END SUBROUTINE mortal
!======================================================================!