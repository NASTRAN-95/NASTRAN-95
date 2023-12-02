!*==pstrq2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pstrq2(Ntype)
   USE c_pla32s
   USE c_pla3es
   USE c_pla3uv
   USE c_sout
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j
   INTEGER , DIMENSION(4) :: nsil
   REAL , DIMENSION(36) :: si
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
! THIS ROUTINE CALCULATES PHASE II OUTPUT FOR PLA3
!
!     NTYPE = 1 TRI-MEMBRANE
!     NTYPE = 2 QUAD-MEMBRANE
!
!     PH1OUT CONTAINS THE FOLLOWING
!     *** NTYPE = 1 ***
!     ELEMENT ID
!     3 SILS
!     5 DUMMY-S
!     3 S ARRAYS EACH 3X3
!
!     *** NTYPE = 2 ***
!     ELEMENT ID
!     4 SILS
!     4 DUMMY-S
!     4 S ARRAYS EACH 3X3
!
!
!
   !>>>>EQUIVALENCE (Nsil(1),Ph1out(2)) , (Si(1),Ph1out(10))
!
!
!                        I=NSIZE
!     STRESS VECTOR = (SUMMATION  (S ) (U ))
!                        I=1        I    I
!
   nsize = Ntype + 2
   DO i = 1 , nsize
!     POINTER TO DISPLACEMENT VECTOR
      npoint = ivec + nsil(i) - 1
!
      CALL gmmats(si(9*i-8),3,3,0,z(npoint),3,1,0,vec(1))
!
      DO j = 1 , 3
         stress(j) = stress(j) + vec(j)
      ENDDO
   ENDDO
!
   stres(1) = ph1out(1)
   stres(2) = stress(1)
   stres(3) = stress(2)
   stres(4) = stress(3)
!
!     ******************************************************************
!
!     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
   temp = stres(2) - stres(3)
   stres(8) = sqrt((temp/2.0E0)**2+stres(4)**2)
   delta = (stres(2)+stres(3))/2.0E0
   stres(6) = delta + stres(8)
   stres(7) = delta - stres(8)
   delta = 2.0E0*stres(4)
   IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
      stres(5) = 0.0E0
      RETURN
   ENDIF
   stres(5) = atan2(delta,temp)*28.6478898E00
END SUBROUTINE pstrq2
