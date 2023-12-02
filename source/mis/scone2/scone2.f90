!*==scone2.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scone2(Sorc)
   USE c_condas
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_zzzzzz
   IMPLICIT NONE
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
      sum(i) = 0.0
   ENDDO
!
   elemid = nelem/1000
   nelhar = nelem - elemid*1000
!
!     ZERO OUT BLOCK IF THIS IS FIRST CALL WITH HARMONIC = 0 FOR THIS
!     ELEMENT
!
   n = nelhar - 1
   IF ( n==0 ) THEN
      IF ( elemid/=nelold ) THEN
         nelold = elemid
         DO i = 2 , 9
            DO j = 1 , 14
               block(i,j) = 0.0
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
            block(1,j) = phi(i)
         ENDDO
         j = j + 1
         IF ( j<=14 ) iblock(1,j) = 1
      ENDIF
   ENDIF
   harm = n
!
   DO i = 1 , 2
!
!     DISPLACEMENT VECTOR POINTER
!
      npoint = ivec + sil(i) - 1
!
      CALL gmmats(s(48*i-47),8,6,0,z(npoint),6,1,0,vec(1))
!
      DO j = 1 , 8
         sum(j) = sum(j) + vec(j)
      ENDDO
   ENDDO
!
!     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK FOR THIS HARMONIC
!
   SPAG_Loop_1_1: DO i = 1 , 14
      IF ( iblock(1,i)==1 ) EXIT SPAG_Loop_1_1
      nphi = harm*block(1,i)*degra
      sinphi = sin(nphi)
      conphi = cos(nphi)
      IF ( Sorc==2 ) THEN
         block(2,i) = block(2,i) + conphi*sum(1)
         block(3,i) = block(3,i) + conphi*sum(2)
         block(4,i) = block(4,i) + sinphi*sum(3)
         block(5,i) = block(5,i) + conphi*sum(4)
         block(6,i) = block(6,i) + conphi*sum(5)
         block(7,i) = block(7,i) + sinphi*sum(6)
         block(8,i) = block(8,i) + conphi*sum(7)
         block(9,i) = block(9,i) + sinphi*sum(8)
      ELSE
         block(2,i) = block(2,i) + sinphi*sum(1)
         block(3,i) = block(3,i) + sinphi*sum(2)
         block(4,i) = block(4,i) - conphi*sum(3)
         block(5,i) = block(5,i) + sinphi*sum(4)
         block(6,i) = block(6,i) + sinphi*sum(5)
         block(7,i) = block(7,i) - conphi*sum(6)
         block(8,i) = block(8,i) + sinphi*sum(7)
         block(9,i) = block(9,i) - conphi*sum(8)
      ENDIF
   ENDDO SPAG_Loop_1_1
!
!     COPY FORCES INTO FORCE OUTPUT BLOCK
!
   iforce(1) = elemid
   iforce(2) = nelhar
   force(3) = sum(4)
   force(4) = sum(5)
   force(5) = sum(6)
   force(6) = sum(7)
   force(7) = sum(8)
!
!     COMPUTE STRESSES AT Z1 AND Z2
!
   istres(1) = elemid
   istres(2) = nelhar
!
   DO i = 1 , 2
      zoveri = 0.0
      IF ( iii/=0.0 ) zoveri = zoff(i)/iii
!
      DO j = 1 , 3
         sig(j) = sum(j) + sum(j+3)*zoveri
      ENDDO
!
      ipt = 8*i - 6
      stress(ipt+1) = zoff(i)
      stress(ipt+2) = sig(1)
      stress(ipt+3) = sig(2)
      stress(ipt+4) = sig(3)
      istres(ipt+5) = 1
      istres(ipt+6) = 1
      istres(ipt+7) = 1
      istres(ipt+8) = 1
   ENDDO
!
END SUBROUTINE scone2
