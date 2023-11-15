
SUBROUTINE pstrq2(Ntype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Delta , Dum(315) , Ph1out(300) , Si(36) , Stres(9) , Stress(3) , Temp , Vec(3) , Z(24)
   INTEGER Ivec , Npoint , Nsil(4) , Nsize
   COMMON /pla32s/ Stress , Vec , Temp , Delta , Nsize , Npoint , Dum
   COMMON /pla3es/ Ph1out
   COMMON /pla3uv/ Ivec , Z
   COMMON /sout  / Stres
!
! Dummy argument declarations
!
   INTEGER Ntype
!
! Local variable declarations
!
   INTEGER i , j
!
! End of declarations
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
   EQUIVALENCE (Nsil(1),Ph1out(2)) , (Si(1),Ph1out(10))
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
   Stres(1) = Ph1out(1)
   Stres(2) = Stress(1)
   Stres(3) = Stress(2)
   Stres(4) = Stress(3)
!
!     ******************************************************************
!
!     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
   Temp = Stres(2) - Stres(3)
   Stres(8) = sqrt((Temp/2.0E0)**2+Stres(4)**2)
   Delta = (Stres(2)+Stres(3))/2.0E0
   Stres(6) = Delta + Stres(8)
   Stres(7) = Delta - Stres(8)
   Delta = 2.0E0*Stres(4)
   IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
      Stres(5) = 0.0E0
      GOTO 99999
   ENDIF
   Stres(5) = atan2(Delta,Temp)*28.6478898E00
   RETURN
99999 END SUBROUTINE pstrq2
