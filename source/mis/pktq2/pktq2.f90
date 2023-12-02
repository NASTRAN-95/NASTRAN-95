!*==pktq2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pktq2(Npts)
   USE c_pla42s
   USE c_pla4es
   USE c_pla4uv
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Npts
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: nph1ou
   INTEGER , DIMENSION(4) :: nsil
   REAL , DIMENSION(36) :: si
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES PHASE II OUTPUT FOR PLA4 FOR COMBINATION
!     ELEMENTS
!
!     **** PHASE II OF STRESS DATA RECOVERY *********
!
!     NPTS = 3 IMPLIES STRIA1 OR STRIA2  (PHASE II)
!     NPTS = 4 IMPLIES SQUAD1 OR SQUAD2  (PHASE II)
!
   !>>>>EQUIVALENCE (Nsil(1),Ph1out(2)) , (Nph1ou(1),Ph1out(1)) , (Si(1),Ph1out(9))
!
!     PHASE I OUTPUT FROM THE MEMBRANE IS THE FOLLOWING
!     NOTE..BEGIN = 30*NPTS+8
!
!     PH1OUT(BEGIN+ 1)               ELEMENT ID
!     PH1OUT(BEGIN+ 2 THRU BEGIN +5) 3 SILS AND DUMMY OR 4 SILS
!     PH1OUT(BEGIN+ 6 THRU BEGIN +9) DUMMY
!     PH1OUT(BEGIN+10 THRU BEGIN +9*NPTS+9) 3 OR 4 S SUB I 3X3 ARRAYS
!
!
!     FIND SIG X, SIG Y, SIG XY, FOR MEMBRANE CONSIDERATION
!
   IF ( nph1ou(30*Npts+10)==0 ) RETURN
!
!                       I=NPTS
!     STRESS VECTOR = (SUMMATION(S )(U ))
!                       I=1       I   I
!
   DO i = 1 , Npts
!
!     POINTER TO I-TH SIL IN PH1OUT
!
      npoint = 30*Npts + 9 + i
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
      npoint = ivec + nph1ou(npoint) - 1
!
!     POINTER TO S SUB I 3X3
!
      npt1 = 30*Npts + 9 + 9*i
!
      CALL gmmats(ph1out(npt1),3,3,0,z(npoint),3,1,0,vec(1))
!
      DO j = 1 , 3
         stress(j) = stress(j) + vec(j)
      ENDDO
!
   ENDDO
!
END SUBROUTINE pktq2
