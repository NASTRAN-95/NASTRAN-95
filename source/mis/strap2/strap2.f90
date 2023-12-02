!*==strap2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strap2(Ti)
   IMPLICIT NONE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dt
   REAL , DIMENSION(225) :: dum3
   REAL , DIMENSION(25) :: force
   INTEGER :: i , iloc , j , k , kk , ldtemp , n , ncomp , ndof , ns , nsp , numpt
   INTEGER , DIMENSION(25) :: iforce
   INTEGER , DIMENSION(100) :: istres
   REAL , DIMENSION(100) :: stres
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE TRAPEZOIDAL
! CROSS SECTION RING
!*****
!
!
!
!
!
! SDR2 VARIABLE CORE
!
!
!
! SDR2 BLOCK FOR POINTERS AND LOADING TEMPERATURES
!
!
!
! SDR2 INPUT AND OUTPUT BLOCK
!
!
!
! SCRATCH BLOCK
!
!
!
   !>>>>EQUIVALENCE (Dum3(1),Idel)
   !>>>>EQUIVALENCE (Dum3(101),Stres(1),Istres(1))
   !>>>>EQUIVALENCE (Dum3(201),Force(1),Iforce(1))
   !>>>>EQUIVALENCE (Ldtemp,Templd)
!
!
! INITIALIZE COUNTERS
!
   ndof = 3
   numpt = 4
   n = ndof*numpt
   nsp = 5
   ncomp = 4
   ns = nsp*ncomp
!
!
! LOCATE THE DISPLACEMENTS
!
   k = 0
   DO i = 1 , numpt
      iloc = Ivec + Igp(i) - 2
      DO j = 1 , ndof
         iloc = iloc + 1
         k = k + 1
         Disp(k) = Zz(iloc)
      ENDDO
   ENDDO
!
!
! COMPUTE THE GRID POINT FORCES
!
   CALL gmmats(Ak(1),n,n,0,Disp(1),n,1,0,Eforc(1))
!
!
! COMPUTE THE STRESSES
!
   CALL gmmats(Sel(1),ns,n,0,Disp(1),n,1,0,Estres(1))
!
!
! COMPUTE THERMAL STRESS IF THERMAL LOAD EXISTS
! AND SUBTRACT FROM APPARENT STRESS
!
   IF ( ldtemp/=(-1) ) THEN
!
      k = 0
      DO i = 1 , nsp
         dt = Ti(i) - Tz
         IF ( i==5 ) dt = (Ti(1)+Ti(2)+Ti(3)+Ti(4))/4.0E0 - Tz
         DO j = 1 , ncomp
            k = k + 1
            Estres(k) = Estres(k) - dt*Ts(j)
         ENDDO
      ENDDO
   ENDIF
!
!
!
! STORE RESULTS FOR OUTPUT PRINT
!
   k = 0
   j = 1
   istres(1) = Idel
   DO kk = 1 , nsp
      DO i = 1 , ncomp
         j = j + 1
         k = k + 1
         stres(j) = Estres(k)
      ENDDO
   ENDDO
!
!
   k = 0
   j = 1
   iforce(1) = Idel
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         force(j) = Eforc(k)
      ENDDO
   ENDDO
!
END SUBROUTINE strap2
