
SUBROUTINE pktq2(Npts)
   IMPLICIT NONE
   REAL Delta , Dum1(308) , Ph1out(300) , Si(36) , Stress(3) , Tem , Temp , Vec(5) , Z(24) , Z1ovri , Z2ovri
   INTEGER I , Ivec , J , Nph1ou(1) , Npoint , Npt1 , Nsil(4)
   COMMON /pla42s/ Stress , Temp , Delta , Npoint , I , J , Npt1 , Vec , Tem , Z1ovri , Z2ovri , Dum1
   COMMON /pla4es/ Ph1out
   COMMON /pla4uv/ Ivec , Z
   INTEGER Npts
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
   IF ( Nph1ou(30*Npts+10)==0 ) RETURN
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
      Npoint = Ivec + Nph1ou(Npoint) - 1
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