
SUBROUTINE scone2(Sorc)
   IMPLICIT NONE
   REAL Block(9,14) , Commun(225) , Conphi , Consts(5) , Degra , Delta , Dum11(3) , Dummy(35) , Force(7) , Harm , Iii , Nphi ,      &
      & Phi(14) , S(96) , Sig(3) , Sig1 , Sig12 , Sig2 , Sinphi , Stress(18) , Sum(8) , Temp , Theta , Vec(8) , Z(1) , Zoff(2) ,    &
      & Zoveri
   INTEGER Elemid , Iblock(9,14) , Iforce(8) , Ipt , Istres(100) , Ivec , N , Nangle , Nelem , Nelhar , Npoint , Sil(2)
   COMMON /condas/ Consts
   COMMON /sdr2x4/ Dummy , Ivec , Dum11
   COMMON /sdr2x7/ Commun
   COMMON /sdr2x8/ Vec , Sum , Sig , Sig1 , Sig2 , Sig12 , Temp , Delta , Theta , Npoint , Zoveri , Ipt , Block , Nelhar , Elemid , &
                 & Harm , N , Sinphi , Conphi , Nphi , Nangle
   COMMON /zzzzzz/ Z
   INTEGER Sorc
   INTEGER i , j , nelold
   LOGICAL zero
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
   EQUIVALENCE (Consts(4),Degra) , (Nelem,Commun(1)) , (Sil(1),Commun(2)) , (Iii,Commun(6)) , (Zoff(1),Commun(7)) ,                 &
    & (Phi(1),Commun(9)) , (S(1),Commun(23)) , (Iblock(1,1),Block(1,1)) , (Stress(1),Commun(101),Istres(1)) ,                       &
    & (Force(1),Commun(201),Iforce(1))
   DATA nelold/ - 1/
!
   DO i = 1 , 8
      Sum(i) = 0.0
   ENDDO
!
   Elemid = Nelem/1000
   Nelhar = Nelem - Elemid*1000
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
            IF ( Phi(i)==0 ) THEN
               IF ( zero ) CYCLE
               zero = .TRUE.
            ENDIF
            j = j + 1
            Block(1,j) = Phi(i)
         ENDDO
         j = j + 1
         IF ( j<=14 ) Iblock(1,j) = 1
      ENDIF
   ENDIF
   Harm = N
!
   DO i = 1 , 2
!
!     DISPLACEMENT VECTOR POINTER
!
      Npoint = Ivec + Sil(i) - 1
!
      CALL gmmats(S(48*i-47),8,6,0,Z(Npoint),6,1,0,Vec(1))
!
      DO j = 1 , 8
         Sum(j) = Sum(j) + Vec(j)
      ENDDO
   ENDDO
!
!     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK FOR THIS HARMONIC
!
   DO i = 1 , 14
      IF ( Iblock(1,i)==1 ) EXIT
      Nphi = Harm*Block(1,i)*Degra
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
   ENDDO
!
!     COPY FORCES INTO FORCE OUTPUT BLOCK
!
   Iforce(1) = Elemid
   Iforce(2) = Nelhar
   Force(3) = Sum(4)
   Force(4) = Sum(5)
   Force(5) = Sum(6)
   Force(6) = Sum(7)
   Force(7) = Sum(8)
!
!     COMPUTE STRESSES AT Z1 AND Z2
!
   Istres(1) = Elemid
   Istres(2) = Nelhar
!
   DO i = 1 , 2
      Zoveri = 0.0
      IF ( Iii/=0.0 ) Zoveri = Zoff(i)/Iii
!
      DO j = 1 , 3
         Sig(j) = Sum(j) + Sum(j+3)*Zoveri
      ENDDO
!
      Ipt = 8*i - 6
      Stress(Ipt+1) = Zoff(i)
      Stress(Ipt+2) = Sig(1)
      Stress(Ipt+3) = Sig(2)
      Stress(Ipt+4) = Sig(3)
      Istres(Ipt+5) = 1
      Istres(Ipt+6) = 1
      Istres(Ipt+7) = 1
      Istres(Ipt+8) = 1
   ENDDO
!
END SUBROUTINE scone2
