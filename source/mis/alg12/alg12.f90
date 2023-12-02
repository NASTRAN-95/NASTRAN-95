!*==alg12.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg12
   IMPLICIT NONE
   USE C_UD300C
!
! Local variable declarations rewritten by SPAG
!
   REAL :: delx , hs , xmax , xmin
   INTEGER :: j , k , l1
   REAL , DIMENSION(32) :: pstat , xx
   EXTERNAL alg4
!
! End of declarations rewritten by SPAG
!
!
!
!
!
   xmax = X(1,Nstns)
   xmin = X(1,1)
   DO j = 2 , Nstrms
      IF ( X(j,1)<xmin ) xmin = X(j,1)
      IF ( X(j,Nstns)>xmax ) xmax = X(j,Nstns)
   ENDDO
   IF ( xmin<0.0 ) xmin = xmin - 1.0
   l1 = xmin - 1.0
   xmin = float(l1)
   l1 = xmax + 1.0
   xmax = float(l1)
   delx = (xmax-xmin)/Xscale + 0.01
   xx(Nstns+1) = xmin
   xx(Nstns+2) = Xscale
   IF ( Nplot/=2 ) THEN
      pstat(Nstns+1) = Plow
      pstat(Nstns+2) = Pscale
      j = 1
      k = 1
      SPAG_Loop_1_1: DO
         DO I = 1 , Nstns
            hs = H(j,I) - (Vw(j,I)**2+Vm(j,I)**2)/(2.0*G*Ej)
            IF ( hs<Hmin ) hs = Hmin
            pstat(I) = alg4(hs,S(j,I))/Sclfac**2
            xx(I) = X(j,I)
         ENDDO
         IF ( j==Nstrms ) THEN
            IF ( Nplot/=1 ) EXIT SPAG_Loop_1_1
            RETURN
         ELSE
            k = k + 1
            IF ( j==Imid ) j = Nstrms
            IF ( j==1 ) j = Imid
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
   pstat(Nstns+1) = Rlow
   pstat(Nstns+2) = Xscale
   DO j = 1 , Nstrms
      DO I = 1 , Nstns
         xx(I) = X(j,I)
         pstat(I) = R(j,I)
      ENDDO
   ENDDO
   pstat(Nstrms+1) = Rlow
   pstat(Nstrms+2) = Xscale
   xx(Nstrms+1) = xmin
   xx(Nstrms+2) = Xscale
   DO I = 1 , Nstns
      DO j = 1 , Nstrms
         pstat(j) = R(j,I)
         xx(j) = X(j,I)
      ENDDO
   ENDDO
END SUBROUTINE alg12
