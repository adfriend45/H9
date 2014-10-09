!======================================================================!
SUBROUTINE grow
!----------------------------------------------------------------------!
! Update an individual tree's carbon mass.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
LIVING_Individuals: DO I = 1, NIND_alive
!----------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Assign tree index.
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! fPAR is the fraction of total PAR incident on the plot that is
  ! absorbed by the individual. For an LAI of 7, a constant of
  ! 11.0e-6 will give annual NPP of 2.02 kC/m^2/yr.
  !write (*,*) 0.5*0.012*(1.0-EXP(-0.5*7.0))*11.0e-6*60.0*60.0*24.0*365.0
  !stop
  !--------------------------------------------------------------------!
  GPP   = 11.0e-6 * fPAR (KI) ! Crown gross photosynthesis  (molC/m^2/s)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Following just nudges trees so they do not all grow the same.
  !--------------------------------------------------------------------!
  CALL RANDOM_NUMBER (RANDOM)
  !--------------------------------------------------------------------!
  GPP = GPP * (0.9 + RANDOM / 5.0)
  !--------------------------------------------------------------------!
  Resp  = 0.5 * GPP ! Maintenance respiration               (molC/m^2/s)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Net tree C uptake (kgC/tree/s).
  !--------------------------------------------------------------------!
  Cup = KGCPERMOL * Acrown (KI) * (GPP - Resp)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  tau = 100.0 * FLOAT (EDPERY * SPERED) ! Struct'l C residence time  (s)
  Clit = Cv (KI) / tau  ! Structural litter flux            (kgC/tree/s)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  dCV = Cup - Clit ! Structural carbon time derivative      (kgC/tree/s)
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  Cv (KI) = Cv (KI) + DTTR * dCv
  !--------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  ! Accumulate annual diagnostics.
  !--------------------------------------------------------------------!
  NPP_ann_acc = NPP_ann_acc + DTTR * Cup / (Aplot + EPS)
  !--------------------------------------------------------------------!

!----------------------------------------------------------------------!
END DO LIVING_Individuals
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
END SUBROUTINE grow
!======================================================================!
