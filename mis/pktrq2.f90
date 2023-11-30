
SUBROUTINE pktrq2(Ntype)
   IMPLICIT NONE
   REAL Delta , Dum(315) , Ph1out(300) , Si(36) , Stress(3) , Temp , Vec(3) , Z(24)
   INTEGER Ivec , Npoint , Nsil(4) , Nsize
   COMMON /pla42s/ Stress , Vec , Temp , Delta , Nsize , Npoint , Dum
   COMMON /pla4es/ Ph1out
   COMMON /pla4uv/ Ivec , Z
   INTEGER Ntype
   INTEGER i , j
! THIS ROUTINE CALCULATES PHASE II OUTPUT FOR PLA4
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
   Nsize = Ntype + 2
   DO i = 1 , Nsize
!     POINTER TO DISPLACEMENT VECTOR
      Npoint = Ivec + Nsil(i) - 1
!
      CALL gmmats(Si(9*i-8),3,3,0,Z(Npoint),3,1,0,Vec(1))
!
      DO j = 1 , 3
         Stress(j) = Stress(j) + Vec(j)
      ENDDO
   ENDDO
!
END SUBROUTINE pktrq2