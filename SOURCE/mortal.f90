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
      CV (NIND_alive) = Cv (KI)
      rold      (NIND_alive) = rold (KI)
      H         (NIND_alive) = H (KI)
      Afoliage  (NIND_alive) = Afoliage (KI)
      Aheart    (NIND_alive) = Aheart (KI)
      ib        (NIND_alive) = ib (KI)
      fPAR      (NIND_alive) = fPAR (KI)
      Acrown    (NIND_alive) = Acrown (KI)
      LAIcrown  (NIND_alive) = LAIcrown (KI)
      Acrowns_above (NIND_alive) = Acrowns_above (KI)
      Afoliage_above (NIND_alive) = Afoliage_above (KI)
      ih (NIND_alive) = ih (KI)
      r (NIND_alive) = r (KI)
      floss (NIND_alive) = floss (KI)
      Acrown_layer   (:,NIND_alive) = Acrown_layer (:,KI)
      Afoliage_layer (:,NIND_alive) = Afoliage_layer (:,KI)
    END IF
  END IF
END DO
Afoliage (NIND_alive+1:NIND) = 0.0
write (20,*) NIND_alive
!----------------------------------------------------------------------!
END SUBROUTINE mortal
!======================================================================!