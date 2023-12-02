!*==alg12.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg12
   USE c_ud300c
   IMPLICIT NONE
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
   xmax = x(1,nstns)
   xmin = x(1,1)
   DO j = 2 , nstrms
      IF ( x(j,1)<xmin ) xmin = x(j,1)
      IF ( x(j,nstns)>xmax ) xmax = x(j,nstns)
   ENDDO
   IF ( xmin<0.0 ) xmin = xmin - 1.0
   l1 = xmin - 1.0
   xmin = float(l1)
   l1 = xmax + 1.0
   xmax = float(l1)
   delx = (xmax-xmin)/xscale + 0.01
   xx(nstns+1) = xmin
   xx(nstns+2) = xscale
   IF ( nplot/=2 ) THEN
      pstat(nstns+1) = plow
      pstat(nstns+2) = pscale
      j = 1
      k = 1
      SPAG_Loop_1_1: DO
         DO i = 1 , nstns
            hs = h(j,i) - (vw(j,i)**2+vm(j,i)**2)/(2.0*g*ej)
            IF ( hs<hmin ) hs = hmin
            pstat(i) = alg4(hs,s(j,i))/sclfac**2
            xx(i) = x(j,i)
         ENDDO
         IF ( j==nstrms ) THEN
            IF ( nplot/=1 ) EXIT SPAG_Loop_1_1
            RETURN
         ELSE
            k = k + 1
            IF ( j==imid ) j = nstrms
            IF ( j==1 ) j = imid
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
   pstat(nstns+1) = rlow
   pstat(nstns+2) = xscale
   DO j = 1 , nstrms
      DO i = 1 , nstns
         xx(i) = x(j,i)
         pstat(i) = r(j,i)
      ENDDO
   ENDDO
   pstat(nstrms+1) = rlow
   pstat(nstrms+2) = xscale
   xx(nstrms+1) = xmin
   xx(nstrms+2) = xscale
   DO i = 1 , nstns
      DO j = 1 , nstrms
         pstat(j) = r(j,i)
         xx(j) = x(j,i)
      ENDDO
   ENDDO
END SUBROUTINE alg12
