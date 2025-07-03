SUBROUTINE CRPS(forecast, observation, nFcMember, nObsMember, CRPSout)
  ! history:
  ! 21/08/2023: S.R.: get code from Fabien Stoop (crps_fs.f90) as base
  ! 21/08/2023: S.R.: refactor dimensions to get a version taking two ensembles and no neighborhood
  ! f2py threadsafe

  implicit none

  ! Declaration of input arguments
  INTEGER, INTENT(IN) :: nFcMember, nObsMember
  REAL(8), INTENT(IN), DIMENSION(nFcMember) :: forecast ! produit
  REAL(8), INTENT(IN), DIMENSION(nObsMember) :: observation ! Reference

  ! Declaration of output arguments
  REAL(8), INTENT(OUT) :: CRPSout

  ! Les arguments en interne
  REAL(8) :: xxtmp, xytmp, yytmp, xytot, xxtot, yytot
  INTEGER :: iOMember1, iOMember2, iFMember1, iFMember2, nObs, nFc
  INTEGER, DIMENSION(nFcMember) :: maskFc
  INTEGER, DIMENSION(nObsMember) :: maskObs
  REAL(8) :: fc1, ref1

  ! Les inits
  CRPSout = -9999.9
  maskObs = 1 ! for the moment, because routine is only called when there is an observation
  DO iFMember1 = 1, nFcMember
    IF (forecast(iFMember1) > -999.) THEN
      maskFc(iFMember1) = 1
    ELSE
      maskFc(iFMember1) = 0
    END IF
  END DO
  nObs = SUM(maskObs)
  nFc = SUM(maskFc)
  xytot = 0.0
  xxtot = 0.0
  yytot = 0.0

  ! Init pour la boucle
  xxtmp = 0.0
  xytmp = 0.0
  yytmp = 0.0

  ! 1st loop over observation members
  DO iOMember1=1, nObsMember
    IF (maskObs(iOMember1) == 0)  CYCLE
    ref1 = observation(iOMember1)

    ! 2nd loop over observation members
    DO iOMember2=iOMember1+1, nObsMember
      IF (maskObs(iOMember2) == 0)  CYCLE
      yytmp = yytmp + 2*ABS(observation(iOMember2)-ref1)
    ENDDO    ! obsmember 2

    ! boucle sur les membres du vecteur 1
    DO iFMember1=1,nFcMember
      IF (maskFc(iFMember1) == 0)  CYCLE
      fc1 = forecast(iFMember1)
      xytmp = xytmp + ABS(fc1-ref1)
    ENDDO
  ENDDO   ! obsmembers 1
  DO iFMember1=1,nFcMember
      IF (maskFc(iFMember1) == 0)  CYCLE
        ! boucle sur les membres du vecteur 2
      ! Don't do the same calculation twice...
      DO iFMember2=iFMember1+1,nFcMember
        IF (maskFc(iFMember2) == 0)  CYCLE
        xxtmp = xxtmp + 2*ABS(forecast(iFMember1)-forecast(iFMember2))
      ENDDO
  END DO


  ! contribution finale du point courant
  ! PRINT*, xytmp, xxtmp, yytmp
  xytmp = xytmp / REAL(nObs*nFc)
  IF (nObs>1) THEN
   !  yytmp = yytmp / REAL(nObs*(nObs-1.0))
    yytmp = yytmp / REAL(nObs*nObs) ! "unfair" version, but better for small ensemble sizes (zamo18a)
  ENDIF
  IF (nFc>1) THEN
    ! xxtmp = xxtmp / REAL(nFc*(nFc-1.0))
    xxtmp = xxtmp / REAL(nFc*nFc) ! "unfair" version, but better for small ensemble sizes (zamo18a)
  ENDIF
  ! PRINT*, xytmp, xxtmp, yytmp
  ! Calcul du CRPS fair final
  CRPSout = xytmp - 0.5 * (xxtmp + yytmp)
  ! PRINT*, CRPSout

END SUBROUTINE
