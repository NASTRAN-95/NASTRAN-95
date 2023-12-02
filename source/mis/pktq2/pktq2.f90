!*==pktq2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pktq2(Npts)
   IMPLICIT NONE
   USE C_PLA42S
   USE C_PLA4ES
   USE C_PLA4UV
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
   DO I = 1 , Npts
!
!     POINTER TO I-TH SIL IN PH1OUT
!
      Npoint = 30*Npts + 9 + I
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
      Npoint = Ivec + nph1ou(Npoint) - 1
!
!     POINTER TO S SUB I 3X3
!
      Npt1 = 30*Npts + 9 + 9*I
!
      CALL gmmats(Ph1out(Npt1),3,3,0,Z(Npoint),3,1,0,Vec(1))
!
      DO J = 1 , 3
         Stress(J) = Stress(J) + Vec(J)
      ENDDO
!
   ENDDO
!
END SUBROUTINE pktq2
