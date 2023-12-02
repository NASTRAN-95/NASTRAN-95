!*==stord2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stord2(Ti)
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dtf1 , dtf2 , dtm1 , dtm2
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
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR AN AXI-SYMMETRIC
! TOROIDAL THIN SHELL RING
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
!
! INITIALIZE COUNTERS
!
   ndof = 6
   numpt = 2
   n = ndof*numpt
   nsp = 3
   ncomp = 5
   ns = nsp*ncomp
!
!
! LOCATE THE DISPLACEMENTS
!
   k = 0
   DO i = 1 , numpt
      iloc = ivec + igp(i) - 2
      DO j = 1 , ndof
         iloc = iloc + 1
         k = k + 1
         disp(k) = zz(iloc)
      ENDDO
   ENDDO
!
!
! COMPUTE THE GRID POINT FORCES
!
   CALL gmmats(ak(1),n,n,0,disp(1),n,1,0,eforc(1))
!
!
! COMPUTE THE STRESSES
!
   CALL gmmats(sel(1),ns,n,0,disp(1),n,1,0,estres(1))
!
!
! COMPUTE THERMAL STRESS IF THERMAL LOAD EXISTS
! AND SUBTRACT FROM APPARENT STRESS
!
   IF ( ldtemp/=(-1) ) THEN
!
      dtm1 = Ti(1) - tz
      dtm2 = Ti(2) - Ti(1)
      dtf1 = 0.0E0
      dtf2 = 0.0E0
!
! THE TERMS DTF1 AND DTF2 ARE FUNCTIONS OF THE FLEXURAL GRADIENT
! TEMPERATURE BUT SINCE THESE TEMPERATURES ARE NOT AVAILABLE
! THE TERMS WILL BE SET TO ZERO. THEY ARE USUALLY DEFINED AS FOLLOWS,
!     DTF1 = TF(1) - TZ
!     DTF2 = TF(2) - TF(1)
! WHERE TF(1) AND TF(2) ARE THE FLEXURAL GRADIENT TEMPERATURES AT
! GRID POINTS 1 AND 2 RESPECTIVELY.
!
      k = 0
      DO i = 1 , nsp
         DO j = 1 , ncomp
            k = k + 1
            IF ( j>2 ) THEN
               estres(k) = estres(k) - dtf1*ts(k) - dtf2*ts(k+15)
            ELSE
               estres(k) = estres(k) - dtm1*ts(k) - dtm2*ts(k+15)
            ENDIF
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
   istres(1) = idel
   DO kk = 1 , nsp
      DO i = 1 , ncomp
         j = j + 1
         k = k + 1
         stres(j) = estres(k)
      ENDDO
   ENDDO
!
!
   k = 0
   j = 1
   iforce(1) = idel
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         force(j) = eforc(k)
      ENDDO
   ENDDO
!
END SUBROUTINE stord2
