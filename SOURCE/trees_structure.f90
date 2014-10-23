!======================================================================!
SUBROUTINE trees_structure
!----------------------------------------------------------------------!
! Compute tree and canopy structures taking into account control of
! radiation on foliage areas and space on crown areas.
!----------------------------------------------------------------------!
USE CONSTANTS
USE CONTROL
USE TREE
!----------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------!
REAL :: flap,LAIc
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Increase heartwood area if LAIcrown was too high for iPAR.
!----------------------------------------------------------------------!
INDIVIDUALS_LAI_constraint: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Allowable crown LAI based on incident PAR                  (m^2/m^2)
  !--------------------------------------------------------------------!
  LAIc = LOG (0.03 / (iPAR (KI) + EPS)) / (-0.5)
  !--------------------------------------------------------------------!
  ! Fraction of sapwood to convert to heartwood               (fraction)
  !--------------------------------------------------------------------!
  floss = 1.0 - LAIc / (LAIcrown (KI) + EPS)
  !--------------------------------------------------------------------!
  IF (floss > 0.0) THEN
    !------------------------------------------------------------------!
    floss = MIN (floss, 1.0)
    !------------------------------------------------------------------!
    ! Increase heartwood area by appropriate fraction              (m^2)
    !------------------------------------------------------------------!
    Aheart (KI) = Aheart (KI) + floss * Afoliage (KI) / FASA
    !------------------------------------------------------------------!
    ! Reduce crown depth by appropriate fraction                     (m)
    !------------------------------------------------------------------!
    ib (KI) = ib (KI) + CEILING (floss * (H (KI) - FLOAT (ib (KI))))
    ib (KI) = MIN (ih(KI),ib(KI))
    !------------------------------------------------------------------!
  END IF
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_LAI_constraint
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! New structure for each tree based on new Cv, giving new height,
! potential crown area, and potential foliage area.
!----------------------------------------------------------------------!
INDIVIDUALS_potential_crowns: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Stem volume                                                    (m^3)
  !--------------------------------------------------------------------!
  V = Cv (KI) / SIGC
  !--------------------------------------------------------------------!
  ! Stem radius                                                      (m)
  !--------------------------------------------------------------------!
  r (KI) = (V / (( FORMF / 3.0) * PI * alpha)) ** (1.0 / (2.0 + beta))
  !--------------------------------------------------------------------!
  ! Potential sapwood area                                         (m^2)
  !--------------------------------------------------------------------!
  Asapwood = PI * r (KI) ** 2 - Aheart (KI)
  Asapwood = MAX (0.0,Asapwood)
  !--------------------------------------------------------------------!
  ! Potential foliage area                                         (m^2)
  !--------------------------------------------------------------------!
  Afoliage (KI) = FASA * Asapwood
  !--------------------------------------------------------------------!
  ! Stem diameter                                                    (m)
  !--------------------------------------------------------------------!
  D = 2.0 * r (KI)
  !--------------------------------------------------------------------!
  ! Potential crown diameter                                         (m)
  !--------------------------------------------------------------------!
  Dcrown = a_cd + b_cd * D
  !--------------------------------------------------------------------!
  ! Potential crown area                                           (m^2)
  !--------------------------------------------------------------------!
  Acrown (KI) = PI * (Dcrown / 2.0) ** 2
  !--------------------------------------------------------------------!
  ! Tree height                                                      (m)
  !--------------------------------------------------------------------!
  H (KI) = alpha * r (KI) ** beta
  !--------------------------------------------------------------------!
  ! Tree height as integer                                    (DZ_CROWN)
  !--------------------------------------------------------------------!
  ih (KI) = CEILING (H (KI) / DZ_CROWN_M)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_potential_crowns
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
! Sum crown areas of each tree in each layer to compute space
! constraints.
!----------------------------------------------------------------------!
Acrowns_layers (:) = 0.0
!----------------------------------------------------------------------!
INDIVIDUALS_layers: DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! ib index starts at 0, so assume ib+1 is lowest layer.
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI))  &
  &                                  + Acrown (KI)
  !--------------------------------------------------------------------!
END DO INDIVIDUALS_layers
Acrowns_layers_saved (:) = Acrowns_layers (:)
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
INDIVIDUALS_squeeze: DO I = 1, NIND_alive
!----------------------------------------------------------------------!

  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Crown too big? Look only at top of crown so tallest tree does not
  ! have to shrink unless it is too big for plot itself.
  !--------------------------------------------------------------------!
  IF (Acrowns_layers (ih (KI)) > Aplot) THEN
    !------------------------------------------------------------------!
    ! Total overlap as a fraction of plot area.
    !------------------------------------------------------------------!
    flap = Acrowns_layers (ih (KI)) / Aplot
    !------------------------------------------------------------------!
    ! Reduce all crowns that contribute to this overlap by the fraction
    ! of overlap in the plot. Does this work OK? Does the order make
    ! any difference? I feel it should go from tallest down.
    !------------------------------------------------------------------!
    Acrown (KI) = Acrown (KI) / flap
    !------------------------------------------------------------------!
  END IF
  !--------------------------------------------------------------------!
  
  !--------------------------------------------------------------------!
  ! Now update plot crown area profile.
  !--------------------------------------------------------------------!
  Acrowns_layers (ib(KI)+1:ih(KI)) = Acrowns_layers (ib(KI)+1:ih(KI)) -&
  & Acrowns_layers_saved (ib(KI)+1:ih(KI)) + Acrown (KI)
  !--------------------------------------------------------------------!

!----------------------------------------------------------------------!
END DO INDIVIDUALS_squeeze
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
DO I = 1, NIND_alive
  !--------------------------------------------------------------------!
  KI = LIVING (I)
  !--------------------------------------------------------------------!
  ! Height to base of crown from foliage area                       (cm)
  !--------------------------------------------------------------------!
  !ib (KI) = ih (KI) - NINT (Afoliage (KI) / (SIGAF * Acrown (KI) * &
  !&         DZ_CROWN_M))
  !--------------------------------------------------------------------!
  ! Reduce foliage area and increase heartwood area if insufficient
  ! canopy volume for sapwood-area controlled foliage area.
  !--------------------------------------------------------------------!
  !IF (ib (KI) < 0) THEN
  !  ib (KI) = 0
  !  Afoliage (KI) = SIGAF * Acrown (KI) * FLOAT (ih (KI)) * DZ_CROWN_M
  !  Aheart (KI) = PI * r (KI) ** 2 - Afoliage (KI) / FASA
  !  Aheart (KI) = MAX (0.0,Aheart(KI))
  !ENDIF
  !--------------------------------------------------------------------!
  ! Tree crown LAI                                             (m^2/m^2)
  !--------------------------------------------------------------------!
  LAIcrown (KI) = Afoliage (KI) / (Acrown (KI) + EPS)
  !--------------------------------------------------------------------!
END DO
!----------------------------------------------------------------------!

! Carry over Aheart from previous year as that cannot fall.
! Also have new Cv from growth.
! From these, need to produce the size of each tree's crown and foliage
! area. Constraint on total area that can be occupied and that each
! crown must occupy a unique volume and that each LAI is limited by the
! light extinction.
! Need it to all fall out as a consistent set of volumes and areas.
! Order:
!  (i)   New Cv, which gives H, r.
!  (ii)  Each tree has a potential foliage area and crown area.
!  (iii) Then constrain these using plot area and iPAR.
!  (iv)  First do volume constraint, then calculate iPAR and do
!        LAI constraint, then increase heartwood areas if necessary.
!
! In a natural system, will grow in height, to fill gaps, and lose
! lower leaves gradually. Presumably the leaf bit could be annual.
! Height and gap filling are slow as well, so could be annual. At
! all time there is a constant leaf area density, so ib controls
! LAI. In fact, given iPAR, H then determines ib from density, so
! can easily set that up-front! ib should not be responding to
! changes in Acrown. But, this may make it harder to implement more
! physiological approaches later on - do not worry for now as need
! something efficient and accurate for now.



!----------------------------------------------------------------------!
END SUBROUTINE trees_structure
!======================================================================!