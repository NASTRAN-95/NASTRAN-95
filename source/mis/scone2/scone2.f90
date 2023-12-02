!*==scone2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scone2(Sorc)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Sorc
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra , iii
   REAL , DIMENSION(7) :: force
   INTEGER :: i , j , nelem
   INTEGER , DIMENSION(9,14) :: iblock
   INTEGER , DIMENSION(8) :: iforce
   INTEGER , DIMENSION(100) :: istres
   INTEGER , SAVE :: nelold
   REAL , DIMENSION(14) :: phi
   REAL , DIMENSION(96) :: s
   INTEGER , DIMENSION(2) :: sil
   REAL , DIMENSION(18) :: stress
   LOGICAL :: zero
   REAL , DIMENSION(2) :: zoff
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     PHASE II OF STRESS DATA RECOVERY
!
!     OUTPUTS FROM PHASE I ARE THE FOLLOWING (TOTAL OF 118 WORDS) -
!     1) ELEMENT ID
!     2 AND 3) SILS A AND B
!     4) S SUB T
!     5) N
!     6) I
!     7) Z1
!     8) Z2
!     9 THRU 22) PHI-S
!     23 THRU 118) TWO 8X6 S MATRICES
!
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Nelem,Commun(1)) , (Sil(1),Commun(2)) , (Iii,Commun(6)) , (Zoff(1),Commun(7)) ,                 &
!>>>>    & (Phi(1),Commun(9)) , (S(1),Commun(23)) , (Iblock(1,1),Block(1,1)) , (Stress(1),Commun(101),Istres(1)) ,                       &
!>>>>    & (Force(1),Commun(201),Iforce(1))
   DATA nelold/ - 1/
!
   DO i = 1 , 8
      Sum(i) = 0.0
   ENDDO
!
   Elemid = nelem/1000
   Nelhar = nelem - Elemid*1000
!
!     ZERO OUT BLOCK IF THIS IS FIRST CALL WITH HARMONIC = 0 FOR THIS
!     ELEMENT
!
   N = Nelhar - 1
   IF ( N==0 ) THEN
      IF ( Elemid/=nelold ) THEN
         nelold = Elemid
         DO i = 2 , 9
            DO j = 1 , 14
               Block(i,j) = 0.0
            ENDDO
         ENDDO
!
!     INSERT ANGLES FOR OUTPUT INTO FIRST ROW OF BLOCK
!
         zero = .FALSE.
         j = 0
         DO i = 1 , 14
            IF ( phi(i)==0 ) THEN
               IF ( zero ) CYCLE
               zero = .TRUE.
            ENDIF
            j = j + 1
            Block(1,j) = phi(i)
         ENDDO
         j = j + 1
         IF ( j<=14 ) iblock(1,j) = 1
      ENDIF
   ENDIF
   Harm = N
!
   DO i = 1 , 2
!
!     DISPLACEMENT VECTOR POINTER
!
      Npoint = Ivec + sil(i) - 1
!
      CALL gmmats(s(48*i-47),8,6,0,Z(Npoint),6,1,0,Vec(1))
!
      DO j = 1 , 8
         Sum(j) = Sum(j) + Vec(j)
      ENDDO
   ENDDO
!
!     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK FOR THIS HARMONIC
!
   SPAG_Loop_1_1: DO i = 1 , 14
      IF ( iblock(1,i)==1 ) EXIT SPAG_Loop_1_1
      Nphi = Harm*Block(1,i)*degra
      Sinphi = sin(Nphi)
      Conphi = cos(Nphi)
      IF ( Sorc==2 ) THEN
         Block(2,i) = Block(2,i) + Conphi*Sum(1)
         Block(3,i) = Block(3,i) + Conphi*Sum(2)
         Block(4,i) = Block(4,i) + Sinphi*Sum(3)
         Block(5,i) = Block(5,i) + Conphi*Sum(4)
         Block(6,i) = Block(6,i) + Conphi*Sum(5)
         Block(7,i) = Block(7,i) + Sinphi*Sum(6)
         Block(8,i) = Block(8,i) + Conphi*Sum(7)
         Block(9,i) = Block(9,i) + Sinphi*Sum(8)
      ELSE
         Block(2,i) = Block(2,i) + Sinphi*Sum(1)
         Block(3,i) = Block(3,i) + Sinphi*Sum(2)
         Block(4,i) = Block(4,i) - Conphi*Sum(3)
         Block(5,i) = Block(5,i) + Sinphi*Sum(4)
         Block(6,i) = Block(6,i) + Sinphi*Sum(5)
         Block(7,i) = Block(7,i) - Conphi*Sum(6)
         Block(8,i) = Block(8,i) + Sinphi*Sum(7)
         Block(9,i) = Block(9,i) - Conphi*Sum(8)
      ENDIF
   ENDDO SPAG_Loop_1_1
!
!     COPY FORCES INTO FORCE OUTPUT BLOCK
!
   iforce(1) = Elemid
   iforce(2) = Nelhar
   force(3) = Sum(4)
   force(4) = Sum(5)
   force(5) = Sum(6)
   force(6) = Sum(7)
   force(7) = Sum(8)
!
!     COMPUTE STRESSES AT Z1 AND Z2
!
   istres(1) = Elemid
   istres(2) = Nelhar
!
   DO i = 1 , 2
      Zoveri = 0.0
      IF ( iii/=0.0 ) Zoveri = zoff(i)/iii
!
      DO j = 1 , 3
         Sig(j) = Sum(j) + Sum(j+3)*Zoveri
      ENDDO
!
      Ipt = 8*i - 6
      stress(Ipt+1) = zoff(i)
      stress(Ipt+2) = Sig(1)
      stress(Ipt+3) = Sig(2)
      stress(Ipt+4) = Sig(3)
      istres(Ipt+5) = 1
      istres(Ipt+6) = 1
      istres(Ipt+7) = 1
      istres(Ipt+8) = 1
   ENDDO
!
END SUBROUTINE scone2
