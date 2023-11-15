
SUBROUTINE kcones
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A , B , Consts(5) , Costh , Cp , Cp2 , Cp2e22 , Cpe12 , Cpe22 , D11 , D12 , D22 , D33 , Dl , Dodet , Dum(5) , Dum1(10) ,    &
      & Dum2 , Dumcl(7) , E11 , E12 , E22 , E33 , Ecpt(100) , Eltemp , G , G11 , G12 , G13 , G22 , G23 , G33 , Gsube , H11 , H12 ,  &
      & H13 , H14 , H15 , H16 , H17 , H18 , H19 , H1ten , Huq(100) , Hyq(10) , Hyqf(10) , I00 , I01 , I02 , I03 , I04 , I10 , I11 , &
      & I12 , I13 , I14 , I20 , I21 , I22 , I23 , I24 , I31 , I32 , I33 , I34 , I42 , I43 , I44 , I52 , I53 , I54 , I62 , I63 ,     &
      & I64 , Integ(28) , Kij(36) , L2 , N , N2 , N2e22
   INTEGER Idetck , If4gg , Ifkgg , Inflag , Iopt4 , K4ggsw , Link(10) , Matid , Matid1 , Matid2 , Matid3 , Necpt(100) , Nogo , Npvt
   DOUBLE PRECISION Kijd(36) , Qq1 , Qq2 , Qq3 , Qq4 , Sum
   REAL N2e33 , Ncp , Nsp , Opi , Oq , Pi , Piovb , Ra , Rasq , Rb , Rbsq , Sign , Sinth , Sl , Sp , Sp2 , Sp2e22 , Sp2e33 , Spe12 ,&
      & Spe22 , Spe33 , Stress , T , Td , Temp , Temp1 , Temp2 , Temp3 , Temp4 , Temp5 , Temp6 , Temp60(60) , Temp7 , Tn , Tnsp ,   &
      & Ts , Twod33 , Za , Zb
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Dum , Gsube
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Dumcl , Link , Idetck , Dodet , Nogo
   COMMON /sma1dp/ Sum , Qq1 , Qq2 , Qq3 , Qq4 , Kijd , Integ , Kij , Huq , Hyqf , Hyq , Temp60 , Opi , Za , E11 , Cp , Spe22 , Zb ,&
                 & E12 , Sp , Cpe22 , A , E22 , Cp2 , Sp2e22 , B , E33 , Sp2 , Cp2e22 , Sign , T , D11 , Temp1 , Ra , Ts , D12 ,    &
                 & Temp2 , Rb , N , D22 , Temp3 , Rasq , N2 , D33 , Temp4 , Rbsq , Sl , Nsp , Temp5 , Tn , L2 , Ncp , Temp6 ,       &
                 & Piovb , Dl , Spe12 , Temp7 , Td , Temp , Cpe12 , Oq , N2e22 , Twod33 , Tnsp , N2e33 , Sp2e33 , Spe33
   COMMON /sma1et/ Ecpt
   COMMON /sma1io/ Dum1 , Ifkgg , Dum2 , If4gg
!
! Local variable declarations
!
   REAL determ , fac(7) , h(120) , kqe(10,10) , kqn(10,10) , kqx(10,10) , kqy(10,10) , n2d33 , nspopi , one , sp2d22
   INTEGER i , idx , inc1 , inc2 , iretrn , ising , iten , j , k , kk , mk1 , mn2 , mnk2 , mplus1 , na(7) , nbegin , nerror(2) ,    &
         & nint , npivot , oldpt1 , oldpt2
!
! End of declarations
!
!
!     SINGLE PRECISION CONEAX ROUTINE, MACHINE INDEPENDENT VERSION
!
!     FOUR KCONE VERSIONS
!     KCONES  FOR MACHINES WITH 60 OR 64 BIT WORD (e.g. CDC, CRAY).
!             S.P. COMPUTATION IS USED
!     KCONE2, SIMILAR TO KCONES, EXECPT CERTAIN CRITICAL AREAS ARE
!             COMPUTED IN D.P. FOR IMPROVED ACCURACY
!     KCONED  FOR MAHCINES WITH LESS THEN 60 BIT WORD, WITHOUT QUAD
!             PRECISION SOFTWARE SUPPORT (e.g. DEC3100)
!             C.P. COMPUTAION IS USED
!     KCONEQ, SIMILAR TO KCONED, EXECPT CERTAIN CRITICAL AREAS ARE
!             COMPUTED IN QUAD PREC. FOR IMPROVED ACCURACY
!
!     ORIGINALLY, THIS ROUTINE CALLS KCONEX AND KCONEY/Z. THESE THREE
!     SUPPORTING ROUTINES ARE NOW MOVED INTO KCONES (AND ALSO KCONED)
!
!     ECPT( 1) = ELEMENT ID             INTEGER        ECT
!     ECPT( 2) = SIL PT A               INTEGER        ECT
!     ECPT( 3) = SIL PT B               INTEGER        ECT
!     ECPT( 4) = MATID 1                INTEGER        EPT
!     ECPT( 5) = T   (MEMBRANE THICK)   REAL           EPT
!     ECPT( 6) = MATID 2                INTEGER        EPT
!     ECPT( 7) = I   (MOM.OF INERTIA)   REAL           EPT
!     ECPT( 8) = MATID 3                INTEGER        EPT
!     ECPT( 9) = TS  (SHEAR THICKNESS)  REAL           EPT
!     ECPT(10) = NON-STRUCTURAL-MASS    REAL           EPT
!     ECPT(11) = Z1                     REAL           EPT
!     ECPT(12) = Z2                     REAL           EPT
!     ECPT(13) = PHI  1                 REAL           EPT
!     ECPT(14) = PHI  2                 REAL           EPT
!     ECPT(15) = PHI  3                 REAL           EPT
!     ECPT(16) = PHI  4                 REAL           EPT
!     ECPT(17) = PHI  5                 REAL           EPT
!     ECPT(18) = PHI  6                 REAL           EPT
!     ECPT(19) = PHI  7                 REAL           EPT
!     ECPT(20) = PHI  8                 REAL           EPT
!     ECPT(21) = PHI  9                 REAL           EPT
!     ECPT(22) = PHI 10                 REAL           EPT
!     ECPT(23) = PHI 11                 REAL           EPT
!     ECPT(24) = PHI 12                 REAL           EPT
!     ECPT(25) = PHI 13                 REAL           EPT
!     ECPT(26) = PHI 14                 REAL           EPT
!     ECPT(27) = COORD. SYS. ID PT.1    INTEGER        BGPDT
!     ECPT(28) = RADIUS PT. 1           REAL           BGPDT
!     ECPT(29) = DISTANCE TO PT.1       REAL           BGPDT
!     ECPT(30) = NULL                   REAL           BGPDT
!     ECPT(31) = COORD. SYS. ID PT.2    INTEGER        BGPDT
!     ECPT(32) = RADIUS PT 2            REAL           BGPDT
!     ECPT(33) = DISTANCE TO PT. 2      REAL           BGPDT
!     ECPT(34) = NULL                   REAL           BGPDT
!     ECPT(35) = ELEMENT TEMPERATURE    REAL           GEOM3
!
!
   EQUIVALENCE (Consts(1),Pi) , (Ecpt(4),Matid1) , (Ecpt(6),Matid2) , (Ecpt(8),Matid3) , (Ecpt(1),Necpt(1))
   EQUIVALENCE (G,G12) , (kqn(1,1),kqe(1,1),kqx(1,1),kqy(1,1))
   EQUIVALENCE (Hyq(1),H11) , (Hyq(2),H12) , (Hyq(3),H13) , (Hyq(4),H14) , (Hyq(5),H15) , (Hyq(6),H16) , (Hyq(7),H17) , (Hyq(8),H18)&
    & , (Hyq(9),H19) , (Hyq(10),H1ten)
   EQUIVALENCE (I00,Integ(1)) , (I20,Integ(11)) , (I01,Integ(2)) , (I21,Integ(12)) , (I02,Integ(3)) , (I22,Integ(13)) ,             &
    & (I03,Integ(4)) , (I23,Integ(14)) , (I04,Integ(5)) , (I24,Integ(15)) , (I10,Integ(6)) , (I31,Integ(16)) , (I11,Integ(7)) ,     &
    & (I32,Integ(17)) , (I12,Integ(8)) , (I33,Integ(18)) , (I13,Integ(9)) , (I34,Integ(19)) , (I14,Integ(10)) , (I52,Integ(23)) ,   &
    & (I42,Integ(20)) , (I53,Integ(24)) , (I43,Integ(21)) , (I54,Integ(25)) , (I44,Integ(22)) , (I62,Integ(26)) , (I63,Integ(27)) , &
    & (I64,Integ(28))
   DATA oldpt1 , oldpt2/0 , 0/
   DATA fac/1.0 , 1.0 , 2.0 , 6.0 , 24.0 , 120.0 , 720.0/
   DATA na/1 , 1 , 1 , 2 , 3 , 3 , 3/
   DATA one/1.0/
!
!     DOES PIVOT POINT EQUAL EITHER OF THE LAST TWO SILS
!
   IF ( oldpt1==Necpt(2) ) THEN
      IF ( oldpt2==Necpt(3) ) GOTO 100
   ELSEIF ( oldpt2==Necpt(2) ) THEN
      IF ( oldpt1==Necpt(3) ) GOTO 100
   ENDIF
!
!     NO MATCH THUS DO ENTIRE COMPUTATION
!
   Sinth = 0.0
   Costh = 1.0
   nint = Necpt(1) - (Necpt(1)/1000)*1000 - 1
   N = nint
   Ra = Ecpt(28)
   Za = Ecpt(29)
   Rb = Ecpt(32)
   Zb = Ecpt(33)
   Temp1 = Rb - Ra
   Temp2 = Zb - Za
   Sl = sqrt(Temp1**2+Temp2**2)
   L2 = Sl*Sl
   IF ( Sl/=0 ) THEN
!
      Sp = Temp1/Sl
      Cp = Temp2/Sl
      A = Ra
      B = Sp
      IF ( abs(B)>0.001 ) THEN
!
!     ABOVE COMPLETES ALL INTEGRALS FOR B = 0.
!
!
!     FOR B .NE. ZERO
!
!     IF AN OVERFLOW RESULTS BELOW POSSIBLY B IS NOT ZERO, BUT SMALL
!
!     OK BELOW IS FOR B NOT EQUAL TO ZERO
!
!     FIRST M = 0 CASE
!
!                             2-N     2-N
!                       PI (RB    - RA   )
!               I    = --------------------   (N NOT EQUAL TO 2)
!                0,N        (2-N) B
!
!
!     FOR N=2   I    = PI * (LOG RB  -  LOG RA) / B
!                0,2            E          E
!
         Rasq = Ra*Ra
         Rbsq = Rb*Rb
         Piovb = Pi/B
!
         Integ(1) = 0.5*Piovb*(Rbsq-Rasq)
         Integ(2) = Piovb*(Rb-Ra)
         Integ(3) = Piovb*log(Rb/Ra)
         Integ(4) = -Piovb*(one/Rb-one/Ra)
         Integ(5) = -0.5*Piovb*(one/Rbsq-one/Rasq)
!
         idx = 5
         DO i = 1 , 6
            mplus1 = i + 1
            nbegin = na(mplus1)
            DO j = nbegin , 5
!
!     M = I
!     N = J - 1
!
!     WE ARE GETTING INTEGRAL(M,N)
!     M = POWER OF S
!     N = POWER OF R
!
!     EVALUATING AT R = RB,  THEN AT R = RA
!
!                                    K   MNK2
!                (M)FAC.     M   (-A) (R)
!     I  = (PI) (-------) ((SUM -------------------------) + (TERM-X))
!      MN          (M+1)    K=0  (M-K)FAC. (K)FAC. (MNK2)
!                 B        (FOR K.NE. MN2                   (FOR K=MN2)
!
!       WHERE    MNK2 = M-N-K+2
!                MN2  = M-N  +2
!             (X)FAC. = X!
!                             MN2
!                         (-A)    LOG(R)
!              TERM-X = --------------------
!                       (M-N+2)FAC. (N-2)FAC.
!
!     NOTE IN DATA STATEMENT THAT 0 FACTORIAL = FAC(1)
!                                 1 FACTORIAL = FAC(2)
!                                 2 FACTORIAL = FAC(3)    ETC.
!
               Sum = 0.0
               Sign = -1.0
               DO kk = 1 , mplus1
                  Sign = -Sign
                  k = kk - 1
                  mn2 = i - j + 3
                  Qq1 = dble(A)
                  Qq2 = dble(Rb)
                  Qq3 = dble(Ra)
                  IF ( k==mn2 ) THEN
!
!     QQ4 = A**MN2*DLOG(RB/RA)/(FAC(MN2+1)*FAC(J-2))
!
                     Qq1 = Qq1**mn2
                     Qq3 = Qq2/Qq3
                     Qq2 = dlog(Qq3)
                     Qq3 = dble(fac(mn2+1)*fac(j-2))
                  ELSE
                     mnk2 = mn2 - k
                     mk1 = mplus1 - k
                     Temp = mnk2
!
!     QQ4  = A**K*(RB**MNK2-RA**MNK2)/(FAC(MK1)*FAC(KK)*TEMP)
!
                     Qq1 = Qq1**k
                     Qq2 = Qq2**mnk2
                     Qq3 = Qq3**mnk2
                     Qq2 = Qq2 - Qq3
                     Qq3 = dble(fac(mk1)*fac(kk)*Temp)
                  ENDIF
                  Qq4 = Qq1*Qq2/Qq3
                  Sum = Sum + dble(Sign)*Qq4
               ENDDO
!
               Qq1 = dble(Pi*fac(mplus1))
               Qq2 = dble(B)
               Qq3 = Qq2**mplus1
               Qq4 = Sum*Qq1/Qq3
               idx = idx + 1
               Integ(idx) = sngl(Qq4)
            ENDDO
         ENDDO
      ELSE
!
!     GO TO 40 FOR B = 0.
!
!                               1-N
!                         PI  RA     M+1
!     FOR B = 0,   I   = --------- SL    (FOR ALL M,N .GE. 0)
!                   M,N    M + 1
!
!  40 CONTINUE
!
         idx = 0
         DO i = 1 , 7
            nbegin = na(i)
!
            DO j = nbegin , 5
!
!     M = I - 1
!     N = J - 1
!     MPLUS1 THUS EQUALS I
!
               idx = idx + 1
               Integ(idx) = (Pi*Sl**i)/(float(i)*Ra**(j-2))
            ENDDO
         ENDDO
      ENDIF
!
      oldpt1 = Necpt(2)
      oldpt2 = Necpt(3)
!
!     ZERO OUT THE KQN MATRIX
!
      DO i = 1 , 10
         DO j = 1 , 10
            kqn(i,j) = 0.0
         ENDDO
      ENDDO
!
!     IF MEMBRANE THICKNESS IS NOT ZERO FORM THE KQE MATRIX
!
      T = Ecpt(5)
      IF ( T==0 ) GOTO 400
      ASSIGN 300 TO iretrn
      Matid = Matid1
      Inflag = 2
      GOTO 200
   ELSE
      nerror(1) = Necpt(1)/1000
      nerror(2) = N + .3
      CALL mesage(30,39,nerror(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
!
!     WE HAVE A MATCH ON OLD SIL NUMBER 1
!
 100  IF ( Npvt/=oldpt1 ) THEN
!
!     WE HAVE A MATCH ON OLD SIL NUMBER 2
!
      npivot = 2
   ELSE
      npivot = 1
   ENDIF
   GOTO 900
 200  Eltemp = Ecpt(35)
   CALL mat(Ecpt(1))
   GOTO iretrn
 300  E11 = G11
   E12 = G12
   E22 = G22
   E33 = G33
   Tn = T*N
   Cp2 = Cp*Cp
   Sp2 = Sp*Sp
   N2 = N*N
   Cp2e22 = Cp2*E22
   Sp2e22 = Sp2*E22
   Cpe22 = Cp*E22
   Spe22 = Sp*E22
   Cpe12 = Cp*E12
   Spe12 = Sp*E12
   N2e33 = N2*E33
   N2e22 = N2*E22
   Sp2e33 = Sp2*E33
   Spe33 = Sp*E33
!
! /// FURTHER REDUCTION IS NEEDED HERE ///
!
   kqe(1,1) = T*(N2e22+Sp2e33)*I02
   kqe(1,2) = T*(N2e22*I12-Spe33*I01+Sp2e33*I12)
   Temp = E22 + E33
   Tnsp = Tn*Sp
   kqe(1,3) = Tnsp*Temp*I02
   kqe(1,4) = Tn*(E12*I01+Sp*Temp*I12)
   Temp = Tn*Cp*E22
   kqe(1,5) = Temp*I02
   kqe(1,6) = Temp*I12
   kqe(1,7) = Temp*I22
   kqe(1,8) = Temp*I32
   Temp4 = 2.*Sp*I11
   kqe(2,2) = T*(N2e22*I22+E33*(I00-Temp4+Sp2*I22))
   kqe(2,3) = Tn*(Spe22*I12-E33*I01+Spe33*I12)
   kqe(2,4) = Tn*(E12*I11+Spe22*I22-E33*I11+Spe33*I22)
   kqe(2,5) = kqe(1,6)
   kqe(2,6) = kqe(1,7)
   kqe(2,7) = kqe(1,8)
   kqe(2,8) = Tn*Cpe22*I42
   kqe(3,3) = T*(Sp2e22*I02+N2e33*I02)
   kqe(3,4) = T*(Spe12*I01+Sp2e22*I12+N2e33*I12)
   Temp = T*Cp*Spe22
   kqe(3,5) = Temp*I02
   kqe(3,6) = Temp*I12
   kqe(3,7) = Temp*I22
   kqe(3,8) = Temp*I32
   kqe(4,4) = T*(E11*I00+Temp4*E12+Sp2e22*I22+N2e33*I22)
   Temp = Sp*Cpe22
   kqe(4,5) = T*(Cpe12*I01+Temp*I12)
   kqe(4,6) = T*(Cpe12*I11+Temp*I22)
   kqe(4,7) = T*(Cpe12*I21+Temp*I32)
   kqe(4,8) = T*(Cpe12*I31+Temp*I42)
   Temp = T*Cp2e22
   kqe(5,5) = Temp*I02
   kqe(5,6) = Temp*I12
   kqe(5,7) = Temp*I22
   kqe(5,8) = Temp*I32
   kqe(6,6) = kqe(5,7)
   kqe(6,7) = kqe(5,8)
   kqe(6,8) = Temp*I42
   kqe(7,7) = kqe(6,8)
   kqe(7,8) = Temp*I52
   kqe(8,8) = Temp*I62
!
 400  IF ( Ecpt(7)==0.0 ) GOTO 800
!
!     NOW GET G MATERIAL MATRIX ID = MATID2
!
   Matid = Matid2
   ASSIGN 500 TO iretrn
   Inflag = 2
   GOTO 200
!
!     NOW FORM D = I DOT G
!
 500  D11 = Ecpt(7)*G11
   D12 = Ecpt(7)*G12
   D22 = Ecpt(7)*G22
   D33 = Ecpt(7)*G33
!
!     IF SHEAR THICKNESS IS NOT ZERO FORM THE HYQ AND KQY MATRICES
!
   Ts = Ecpt(9)
   IF ( Ts==0 ) GOTO 700
!
!     GET G FOR MATID3
!
   Matid = Matid3
   Inflag = 1
   ASSIGN 600 TO iretrn
   GOTO 200
!
 600  IF ( G==0.0 ) THEN
      Ts = 0.0
   ELSE
!
!     FORMING 1.0/Q DIRECTLY
!
      Opi = one/Pi
!
! /// MAKE SURE ALL BASIC PRODUCTS ARE AT TOP BEFORE ANY SKIPS
!
      n2d33 = N2*D33
      sp2d22 = Sp2*D22
      Oq = Sl*Ts*G*(Ra+Rb)*0.5 + I02*(n2d33+sp2d22)*Opi
      Oq = one/Oq
      Nsp = N*Sp
      Ncp = N*Cp
      nspopi = Nsp*Opi
      Twod33 = 2.0*D33
      Temp1 = D12*(one/Rb-one/Ra)
      Temp2 = nspopi*(D22+D33)
      Temp3 = N*nspopi*(Twod33+D22)
      Temp4 = Oq*0.5*Ncp*N*D33*Opi
      Temp5 = Opi*(N2*Twod33+Sp2*D22)
      Temp6 = D12*N2*L2/Rb
      Temp7 = nspopi*Cp*0.5
      Hyq(1) = Oq*(Temp1*Ncp-Temp7*I03*(D33+2.0*D22))
      Hyq(2) = Oq*(Ncp*Sl/Rb*D12-Temp7*I13*(3.0*D33+D22)+1.5*Ncp*Opi*I02*D33)
      Hyq(3) = Temp4*I03
      Hyq(4) = Temp4*I13
      Hyq(5) = Oq*(Temp1*N2-Temp3*I03)
      Hyq(6) = Oq*(D12*N2*Sl/Rb-Temp3*I13+Temp5*I02)
      Hyq(7) = Oq*(2.0*D11*(Ra-Rb)+Temp6+2.0*I12*Temp5-Temp3*I23)
      Hyq(8) = Oq*(-D11*6.*Sl*Rb+Temp6*Sl+3.*I22*Temp5-Temp3*I33)
      Hyq(9) = -Oq*Temp2*I02
      Hyq(10) = Oq*(N*Sl*(D12+D33)-Temp2*I12)
!
      Temp = Ts*G*I00
      DO i = 1 , 10
         Hyqf(i) = Hyq(i)*Temp
      ENDDO
      DO i = 1 , 10
         DO j = i , 10
            kqy(i,j) = kqy(i,j) + Hyq(i)*Hyqf(j)
         ENDDO
      ENDDO
!
!     ADD IN TERMS PER EQUATION-90- PAGE -27- MS-28
!
      Temp = Ts*G
      kqy(9,10) = kqy(9,10) + Temp*I10
      kqy(10,10) = kqy(10,10) + Temp*I20
!
!     END OF KQY COMPUTATION
!
      kqy(9,9) = kqy(9,9) + Temp*I00
   ENDIF
!
!     THE FOLLOWING CODES WERE MOVED HERE FROM KCONEX
!
!     KQX MATRIX FOR SHEAR THICKNESS CONSIDERATION
!
!     (THE FOLLOWING CODE WAS MACHINE GENERATED AND WILL NOT BE SIMPLI-
!     FIED FURTHER UNTIL FORMULATION VERIFICATION IS COMPLETED)
!
 700  kqx(1,1) = kqx(1,1) + Cp*Cp*I04*(+D22*N**2+2.25*D33*Sp**2)
   kqx(1,2) = kqx(1,2) + Cp*Cp*(D33*Sp*(+2.25*Sp*I14-2.25*I03)+D22*N*N*I14)
   kqx(1,3) = kqx(1,3) + D33*Cp*Cp*Sp*N*I04*(-0.75)
   kqx(1,4) = kqx(1,4) + D33*Cp*Cp*Sp*N*I14*(-0.75)
   kqx(1,5) = kqx(1,5) + Cp*N*I04*(+D22*N**2+3.0*D33*Sp**2)
   kqx(1,6) = kqx(1,6) + Cp*N*(Sp*(D33*(+3.0*Sp*I14-3.0*I03)-D22*I03)+D22*N*N*I14)
   kqx(1,7) = kqx(1,7) + Cp*N*(Sp*(D33*(+3.0*Sp*I24-6.0*I13)+D22*I13*(-2.0))-2.0*D12*I02+D22*N**2*I24)
   kqx(1,8) = kqx(1,8) + Cp*N*(Sp*(D33*(+3.0*Sp*I34-9.0*I23)+D22*I23*(-3.0))-6.0*D12*I12+D22*N**2*I34)
   kqx(1,9) = kqx(1,9) + Cp*I03*(+D22*N**2+1.5*D33*Sp**2)
   kqx(1,10) = kqx(1,10) + Cp*(D33*Sp*(-1.5*I02+1.5*Sp*I13)+D22*N*N*I13)
   kqx(2,2) = kqx(2,2) + Cp*Cp*(D33*(Sp*(I13*(-4.5)+Sp*I24*2.25)+I02*2.25)+D22*N*N*I24)
   kqx(2,3) = kqx(2,3) + D33*Cp*Cp*N*(-0.75*Sp*I14+0.75*I03)
   kqx(2,4) = kqx(2,4) + D33*Cp*Cp*N*(-0.75*Sp*I24+0.75*I13)
   kqx(2,5) = kqx(2,5) + Cp*N*(D33*Sp*(+3.0*Sp*I14-3.0*I03)+D22*N*N*I14)
   kqx(2,6) = kqx(2,6) + Cp*N*(D33*(Sp*(I13*(-6.0)+Sp*I24*3.0)+I02*3.0)+D22*(-Sp*I13+N**2*I24))
   kqx(2,7) = kqx(2,7) + Cp*N*(D33*(Sp*(I23*(-9.0)+Sp*I34*3.0)+I12*6.0)+D22*(-2.0*Sp*I23+N**2*I34)+D12*I12*(-2.0))
   kqx(2,8) = kqx(2,8) + Cp*N*(D33*(Sp*(I33*(-12.0)+Sp*I44*3.0)+I22*9.0)+D22*(-3.0*Sp*I33+N**2*I44)+D12*I22*(-6.0))
   kqx(2,9) = kqx(2,9) + Cp*(D33*Sp*(+1.5*Sp*I13-1.5*I02)+D22*N*N*I13)
   kqx(2,10) = kqx(2,10) + Cp*(D33*(Sp*(I12*(-3.0)+Sp*I23*1.5)+I01*1.5)+D22*N*N*I23)
   kqx(3,3) = kqx(3,3) + D33*Cp*Cp*N*N*I04*0.25
   kqx(3,4) = kqx(3,4) + D33*Cp*Cp*N*N*I14*0.25
   kqx(3,5) = kqx(3,5) + D33*Cp*Sp*N*N*I04*(-1.0)
   kqx(3,6) = kqx(3,6) + D33*Cp*N*N*(-Sp*I14+I03)
   kqx(3,7) = kqx(3,7) + D33*Cp*N*N*(-Sp*I24+2.0*I13)
   kqx(3,8) = kqx(3,8) + D33*Cp*N*N*(-Sp*I34+3.0*I23)
   kqx(3,9) = kqx(3,9) + D33*Cp*Sp*N*I03*(-0.5)
   kqx(3,10) = kqx(3,10) + D33*Cp*N*(+0.5*I02-0.5*Sp*I13)
   kqx(4,4) = kqx(4,4) + D33*Cp*Cp*N*N*I24*0.25
   kqx(4,5) = kqx(4,5) + D33*Cp*Sp*N*N*I14*(-1.0)
   kqx(4,6) = kqx(4,6) + D33*Cp*N*N*(-Sp*I24+I13)
   kqx(4,7) = kqx(4,7) + D33*Cp*N*N*(-Sp*I34+2.0*I23)
   kqx(4,8) = kqx(4,8) + D33*Cp*N*N*(-Sp*I44+3.0*I33)
   kqx(4,9) = kqx(4,9) + D33*Cp*Sp*N*I13*(-0.5)
   kqx(4,10) = kqx(4,10) + D33*Cp*N*(+0.5*I12-0.5*Sp*I23)
   kqx(5,5) = kqx(5,5) + N*N*I04*(+D22*N**2+4.0*D33*Sp**2)
   kqx(5,6) = kqx(5,6) + N*N*(Sp*(D33*(+4.0*Sp*I14-4.0*I03)+D22*I03*(-1.0))+D22*N*N*I14)
   kqx(5,7) = kqx(5,7) + N*N*(Sp*(D33*(+4.0*Sp*I24-8.0*I13)+D22*I13*(-2.0))-2.0*D12*I02+D22*N**2*I24)
   kqx(5,8) = kqx(5,8) + N*N*(Sp*(D33*(+4.0*Sp*I34-12.0*I23)+D22*I23*(-3.0))-6.0*D12*I12+D22*N**2*I34)
   kqx(5,9) = kqx(5,9) + N*I03*(+D22*N**2+2.0*D33*Sp**2)
   kqx(5,10) = kqx(5,10) + N*(D33*Sp*(-2.0*I02+2.0*Sp*I13)+D22*N*N*I13)
   kqx(6,6) = kqx(6,6) + N*N*(Sp*(I13*(D22*(-2.0)+D33*(-8.0))+D33*Sp*I24*4.0)+D22*N**2*I24+4.0*D33*I02) + D22*Sp*Sp*I02
   kqx(6,7) = kqx(6,7) + N*N*(Sp*(I23*(D22*(-3.0)+D33*(-12.0))+D33*Sp*I34*4.0)+I12*(-2.0*D12+8.0*D33)+D22*N*N*I34)                  &
            & + Sp*(+2.0*D12*I01+2.0*D22*Sp*I12)
   kqx(6,8) = kqx(6,8) + N*N*(Sp*(I33*(D22*(-4.0)+D33*(-16.0))+D33*Sp*I44*4.0)+I22*(-6.0*D12+12.0*D33)+D22*N*N*I44)                 &
            & + Sp*(+6.0*D12*I11+3.0*D22*Sp*I22)
   kqx(6,9) = kqx(6,9) + N*(Sp*(D33*(+2.0*Sp*I13-2.0*I02)+D22*I02*(-1.0))+D22*N*N*I13)
   kqx(6,10) = kqx(6,10) + N*(D33*(Sp*(I12*(-4.0)+Sp*I23*2.0)+I01*2.0)+D22*(+N**2*I23-Sp*I12))
   kqx(7,7) = kqx(7,7) + N*N*(Sp*(I33*(D22*(-4.0)+D33*(-16.0))+D33*Sp*I44*4.0)+I22*(D12*(-4.0)+D33*16.0)+D22*N*N*I44)               &
            & + Sp*(D12*I11*8.0+D22*Sp*I22*4.0) + D11*I00*4.0
   kqx(7,8) = kqx(7,8) + N*N*(Sp*(I43*(D22*(-5.0)+D33*(-20.0))+D33*Sp*I54*4.0)+I32*(D12*(-8.0)+D33*24.0)+D22*N*N*I54)               &
            & + Sp*(D12*I21*18.0+D22*Sp*I32*6.0) + D11*I10*12.0
   kqx(7,9) = kqx(7,9) + N*(Sp*(D33*(+2.0*Sp*I23-4.0*I12)+D22*I12*(-2.0))-2.0*D12*I01+D22*N**2*I23)
   kqx(7,10) = kqx(7,10) + N*(D33*(Sp*(I22*(-6.0)+Sp*I33*2.0)+I11*4.0)+D22*(+N**2*I33-2.0*Sp*I22)+D12*I11*(-2.0))
   kqx(8,8) = kqx(8,8) + N*N*(Sp*(I53*(D22*(-6.0)+D33*(-24.0))+D33*Sp*I64*4.0)+I42*(D12*(-12.0)+D33*36.0)+D22*N*N*I64)              &
            & + Sp*(D12*I31*36.0+D22*Sp*I42*9.0) + D11*I20*36.0
   kqx(8,9) = kqx(8,9) + N*(Sp*(D33*(+2.0*Sp*I33-6.0*I22)+D22*I22*(-3.0))-6.0*D12*I11+D22*N**2*I33)
   kqx(8,10) = kqx(8,10) + N*(D33*(Sp*(I32*(-8.0)+Sp*I43*2.0)+I21*6.0)+D22*(+N**2*I43-3.0*Sp*I32)+D12*I21*(-6.0))
   kqx(9,9) = kqx(9,9) + I02*(+D22*N**2+D33*Sp**2)
   kqx(9,10) = kqx(9,10) + D33*Sp*(-I01+Sp*I12) + D22*N*N*I12
   kqx(10,10) = kqx(10,10) + D33*(Sp*(I11*(-2.0)+Sp*I22)+I00) + D22*N*N*I22
   IF ( Ts/=0.0 ) THEN
!
!     THE FOLLOWING CODES WERE MOVED HERE FROM KCONEY
!
      kqx(1,1) = kqx(1,1) + H11*(Sp*(Cp*N*I03*(D22*2.0+D33*3.0)+D22*Sp*H11*I02)+D33*N*N*H11*I02)
      kqx(1,2) = kqx(1,2) + N*(Cp*(Sp*(D22*(+H12*I03+H11*I13)+D33*(+1.5*H12*I03+1.5*H11*I13))+D33*H11*I02*(-1.5))+D33*N*H11*H12*I02)&
               & + D22*Sp*Sp*H11*H12*I02
      kqx(1,3) = kqx(1,3) + N*(D33*(Cp*I03*(+1.5*Sp*H13-0.5*N*H11)+N*H11*H13*I02)+D22*Cp*Sp*H13*I03) + D22*Sp*Sp*H11*H13*I02
      kqx(1,4) = kqx(1,4) + N*(D33*(Cp*(+1.5*Sp*H14*I03-0.5*N*H11*I13)+N*H11*H14*I02)+D22*Cp*Sp*H14*I03) + D22*Sp*Sp*H11*H14*I02
      kqx(1,5) = kqx(1,5) + Sp*(N*I03*(D22*(+Cp*H15+N*H11)+D33*(+1.5*Cp*H15+2.0*N*H11))+D22*Sp*H11*H15*I02) + D33*N*N*H11*H15*I02
      kqx(1,6) = kqx(1,6) + Sp*(D22*(H11*(Sp*I02*(-1.0+H16)+N*N*I13)+Cp*N*H16*I03)+D33*N*(+1.5*Cp*H16*I03+2.0*N*H11*I13))           &
               & + D33*N*N*H11*I02*(-2.0+H16)
      kqx(1,7) = kqx(1,7) + Sp*(H11*(D22*(Sp*(-2.0*I12+H17*I02)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+Cp*N*H17*I03*(+D22+1.5*D33)) &
               & + D33*N*N*H11*(-4.0*I12+H17*I02)
      kqx(1,8) = kqx(1,8) + Sp*(H11*(D22*(Sp*(-3.0*I22+H18*I02)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+Cp*N*H18*I03*(+D22+1.5*D33)) &
               & + D33*N*N*H11*(-6.0*I22+H18*I02)
      kqx(1,9) = kqx(1,9) + Sp*(N*(D22*(+Cp*H19*I03+H11*I02)+D33*(+1.5*Cp*H19*I03+H11*I02))+D22*Sp*H11*H19*I02)                     &
               & + D33*N*N*H11*H19*I02
      kqx(1,10) = kqx(1,10) + N*(D33*(H11*(-I01+Sp*I12+N*H1ten*I02)+Cp*Sp*H1ten*I03*1.5)+D22*Sp*(+Cp*H1ten*I03+H11*I12))            &
                & + D22*Sp*Sp*H11*H1ten*I02
      kqx(2,2) = kqx(2,2) + H12*(N*(Cp*(D33*(Sp*I13*3.+I02*(-3.))+D22*Sp*I13*2.)+D33*N*H12*I02)+D22*Sp*Sp*H12*I02)
      kqx(2,3) = kqx(2,3) + N*(D33*(Cp*(H13*(+1.5*Sp*I13-1.5*I02)+N*H12*I03*(-0.5))+N*H12*H13*I02)+D22*Cp*Sp*H13*I13)               &
               & + D22*Sp*Sp*H12*H13*I02
      kqx(2,4) = kqx(2,4) + N*(D33*(Cp*(H14*(+1.5*Sp*I13-1.5*I02)+N*H12*I13*(-0.5))+N*H12*H14*I02)+D22*Cp*Sp*H14*I13)               &
               & + D22*Sp*Sp*H12*H14*I02
      kqx(2,5) = kqx(2,5) + N*(D33*(H15*(Cp*(+1.5*Sp*I13-1.5*I02)+N*H12*I02)+Sp*N*H12*I03*2.0)+D22*Sp*(+Cp*H15*I13+N*H12*I03))      &
               & + D22*Sp*Sp*H12*H15*I02
      kqx(2,6) = kqx(2,6) + N*(D33*(N*H12*(I02*(-2.0+H16)+Sp*I13*2.0)+Cp*H16*(+1.5*Sp*I13-1.5*I02))+D22*Sp*I13*(+Cp*H16+N*H12))     &
               & + D22*Sp*Sp*H12*I02*(-1.0+H16)
      kqx(2,7) = kqx(2,7) + Sp*(H12*(D22*(Sp*(-2.0*I12+H17*I02)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+Cp*N*H17*I13*(+D22+1.5*D33)) &
               & + D33*N*(N*H12*(-4.0*I12+H17*I02)+Cp*H17*I02*(-1.5))
      kqx(2,8) = kqx(2,8) + Sp*(H12*(D22*(Sp*(-3.0*I22+H18*I02)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+Cp*N*H18*I13*(+D22+1.5*D33)) &
               & + D33*N*(N*H12*(-6.0*I22+H18*I02)+Cp*H18*I02*(-1.5))
      kqx(2,9) = kqx(2,9) + N*(D33*(H19*(Cp*(+1.5*Sp*I13-1.5*I02)+N*H12*I02)+Sp*H12*I02)+D22*Sp*(+Cp*H19*I13+H12*I02))              &
               & + D22*Sp*Sp*H12*H19*I02
      kqx(2,10) = kqx(2,10) + N*(D33*(H12*(-I01+Sp*I12+N*H1ten*I02)+Cp*H1ten*(+1.5*Sp*I13-1.5*I02))+D22*Sp*(+Cp*H1ten*I13+H12*I12)) &
                & + D22*Sp*Sp*H12*H1ten*I02
      kqx(3,3) = kqx(3,3) + H13*(D33*N*N*(Cp*I03*(-1.0)+H13*I02)+D22*Sp*Sp*H13*I02)
      kqx(3,4) = kqx(3,4) + D33*N*N*(Cp*(-0.5*H14*I03-0.5*H13*I13)+H13*H14*I02) + D22*Sp*Sp*H13*H14*I02
      kqx(3,5) = kqx(3,5) + N*N*(D33*(H13*(+2.0*Sp*I03+H15*I02)+Cp*H15*I03*(-0.5))+D22*Sp*H13*I03) + D22*Sp*Sp*H13*H15*I02
      kqx(3,6) = kqx(3,6) + H13*(Sp*(D22*(Sp*I02*(-1.+H16)+N*N*I13)+D33*N*N*I13*2.0)+D33*N*N*I02*(-2.0+H16))                        &
               & + D33*Cp*N*N*H16*I03*(-0.5)
      kqx(3,7) = kqx(3,7) + H13*(Sp*(D22*(Sp*(-2.0*I12+H17*I02)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+D33*N*N*(-4.0*I12+H17*I02))  &
               & + D33*Cp*N*N*H17*I03*(-0.5)
      kqx(3,8) = kqx(3,8) + H13*(Sp*(D22*(Sp*(-3.0*I22+H18*I02)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+D33*N*N*(-6.0*I22+H18*I02))  &
               & + D33*Cp*N*N*H18*I03*(-0.5)
      kqx(3,9) = kqx(3,9) + N*(D33*(N*H19*(-0.5*Cp*I03+H13*I02)+Sp*H13*I02)+D22*Sp*H13*I02) + D22*Sp*Sp*H13*H19*I02
      kqx(3,10) = kqx(3,10) + N*(D33*(H13*(-I01+Sp*I12+N*H1ten*I02)+Cp*N*H1ten*I03*(-0.5))+D22*Sp*H13*I12) + D22*Sp*Sp*H13*H1ten*I02
      kqx(4,4) = kqx(4,4) + H14*(D33*N*N*(Cp*I13*(-1.0)+H14*I02)+D22*Sp*Sp*H14*I02)
      kqx(4,5) = kqx(4,5) + N*N*(D33*(H14*(+2.0*Sp*I03+H15*I02)+Cp*H15*I13*(-0.5))+D22*Sp*H14*I03) + D22*Sp*Sp*H14*H15*I02
!
!     THE FOLLOWING CODES, THRU 270, WERE MOVED HERE FROM KCONEZ
!
      kqx(4,6) = kqx(4,6) + H14*(Sp*(D22*(Sp*I02*(-1.+H16)+N*N*I13)+D33*N*N*I13*2.0)+D33*N*N*I02*(-2.0+H16))                        &
               & + D33*Cp*N*N*H16*I13*(-0.5)
      kqx(4,7) = kqx(4,7) + H14*(Sp*(D22*(Sp*(-2.0*I12+H17*I02)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+D33*N*N*(-4.0*I12+H17*I02))  &
               & + D33*Cp*N*N*H17*I13*(-0.5)
      kqx(4,8) = kqx(4,8) + H14*(Sp*(D22*(Sp*(-3.0*I22+H18*I02)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+D33*N*N*(-6.0*I22+H18*I02))  &
               & + D33*Cp*N*N*H18*I13*(-0.5)
      kqx(4,9) = kqx(4,9) + N*(D33*(N*H19*(-0.5*Cp*I13+H14*I02)+Sp*H14*I02)+D22*Sp*H14*I02) + D22*Sp*Sp*H14*H19*I02
      kqx(4,10) = kqx(4,10) + N*(D33*(H14*(-I01+Sp*I12+N*H1ten*I02)+Cp*N*H1ten*I13*(-0.5))+D22*Sp*H14*I12) + D22*Sp*Sp*H14*H1ten*I02
      kqx(5,5) = kqx(5,5) + H15*(Sp*(N*N*I03*(D22*2.0+D33*4.0)+D22*Sp*H15*I02)+D33*N*N*H15*I02)
      kqx(5,6) = kqx(5,6) + Sp*(D22*(H15*(Sp*I02*(-1.+H16)+N*N*I13)+N*N*H16*I03)+D33*N*N*(+2.0*H16*I03+2.0*H15*I13))                &
               & + D33*N*N*H15*I02*(-2.0+H16)
      kqx(5,7) = kqx(5,7) + Sp*(H15*(D22*(Sp*(-2.0*I12+H17*I02)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+N*N*H17*I03*(+D22+2.0*D33))  &
               & + D33*N*N*H15*(-4.0*I12+H17*I02)
      kqx(5,8) = kqx(5,8) + Sp*(H15*(D22*(Sp*(-3.0*I22+H18*I02)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+N*N*H18*I03*(+D22+2.0*D33))  &
               & + D33*N*N*H15*(-6.0*I22+H18*I02)
      kqx(5,9) = kqx(5,9) + Sp*(N*(D22*(+N*H19*I03+H15*I02)+D33*(+2.0*N*H19*I03+H15*I02))+D22*Sp*H15*H19*I02) + D33*N*N*H15*H19*I02
      kqx(5,10) = kqx(5,10) + N*(D33*(H15*(-I01+Sp*I12+N*H1ten*I02)+Sp*N*H1ten*I03*2.)+D22*Sp*(+N*H1ten*I03+H15*I12))               &
                & + D22*Sp*Sp*H15*H1ten*I02
      kqx(6,6) = kqx(6,6) + H16*(Sp*(D22*(Sp*I02*(-2.0+H16)+N*N*I13*2.0)+D33*N*N*I13*4.0)+D33*N*N*I02*(-4.0+H16))
      kqx(6,7) = kqx(6,7) + Sp*(D22*(Sp*(H16*(-2.0*I12+H17*I02)+H17*I02*(-1.0))+N*N*(+H17*I13+H16*I23))                             &
               & +D33*N*N*(+2.0*H17*I13+2.0*H16*I23)+D12*H16*I01*(-2.0)) + D33*N*N*(H16*(-4.0*I12+H17*I02)+H17*I02*(-2.0))
      kqx(6,8) = kqx(6,8) + Sp*(D22*(Sp*(H16*(-3.0*I22+H18*I02)+H18*I02*(-1.0))+N*N*(+H18*I13+H16*I33))                             &
               & +D33*N*N*(+2.0*H18*I13+2.0*H16*I33)+D12*H16*I11*(-6.0)) + D33*N*N*(H16*(-6.0*I22+H18*I02)+H18*I02*(-2.0))
      kqx(6,9) = kqx(6,9) + Sp*(D22*(H19*(Sp*I02*(-1.+H16)+N*N*I13)+N*H16*I02)+D33*N*(+2.0*N*H19*I13+H16*I02))                      &
               & + D33*N*N*H19*I02*(-2.0+H16)
      kqx(6,10) = kqx(6,10) + N*(D33*(N*H1ten*(I02*(-2.0+H16)+Sp*I13*2.0)+H16*(-I01+Sp*I12))+D22*Sp*(+N*H1ten*I13+H16*I12))         &
                & + D22*Sp*Sp*H1ten*I02*(-1.0+H16)
      kqx(7,7) = kqx(7,7) + H17*(Sp*(D22*(Sp*(I12*(-4.0)+H17*I02)+N*N*I23*2.0)+D12*I01*(-4.0)+D33*N*N*I23*4.0)                      &
               & +D33*N*N*(I12*(-8.0)+H17*I02))
      kqx(7,8) = kqx(7,8) + Sp*(D22*(Sp*(H17*(-3.0*I22+H18*I02)+H18*I12*(-2.0))+N*N*(+H18*I23+H17*I33))                             &
               & +D12*(-6.0*H17*I11-2.0*H18*I01)+D33*N*N*(+2.0*H18*I23+2.0*H17*I33))                                                &
               & + D33*N*N*(H17*(-6.0*I22+H18*I02)+H18*I12*(-4.0))
      kqx(7,9) = kqx(7,9) + Sp*(H19*(D22*(Sp*(+H17*I02-2.0*I12)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+N*H17*I02*(+D22+D33))        &
               & + D33*N*N*H19*(-4.*I12+H17*I02)
      kqx(7,10) = kqx(7,10) + Sp*(H1ten*(D22*(Sp*(+H17*I02-2.0*I12)+N*N*I23)-2.0*D12*I01+2.0*D33*N**2*I23)+N*H17*I12*(+D22+D33))    &
                & + D33*N*(N*H1ten*(-4.0*I12+H17*I02)+H17*I01*(-1.0))
      kqx(8,8) = kqx(8,8) + H18*(Sp*(D22*(Sp*(I22*(-6.0)+H18*I02)+N*N*I33*2.0)+D12*I11*(-12.0)+D33*N*N*I33*4.0)                     &
               & +D33*N*N*(I22*(-12.0)+H18*I02))
      kqx(8,9) = kqx(8,9) + Sp*(H19*(D22*(Sp*(+H18*I02-3.0*I22)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+N*H18*I02*(+D22+D33))        &
               & + D33*N*N*H19*(-6.*I22+H18*I02)
      kqx(8,10) = kqx(8,10) + Sp*(H1ten*(D22*(Sp*(+H18*I02-3.0*I22)+N*N*I33)-6.0*D12*I11+2.0*D33*N**2*I33)+N*H18*I12*(+D22+D33))    &
                & + D33*N*(N*H1ten*(-6.0*I22+H18*I02)+H18*I01*(-1.0))
      kqx(9,9) = kqx(9,9) + H19*I02*(Sp*(N*(D22*2.0+D33*2.0)+D22*Sp*H19)+D33*N*N*H19)
      kqx(9,10) = kqx(9,10) + N*(D33*(H19*(-I01+Sp*I12+N*H1ten*I02)+Sp*H1ten*I02)+D22*Sp*(+H1ten*I02+H19*I12))                      &
                & + D22*Sp*Sp*H19*H1ten*I02
      kqx(10,10) = kqx(10,10) + H1ten*(N*(D33*(Sp*I12*2.0+I01*(-2.0)+N*H1ten*I02)+D22*Sp*I12*2.0)+D22*Sp*Sp*H1ten*I02)
   ENDIF
!
!     SET LOWER TRIANGLE EQUAL TO UPPER TRIANGLE OF KQN MATRIX
!
 800  DO i = 1 , 10
      DO j = i , 10
         kqn(j,i) = kqn(i,j)
      ENDDO
   ENDDO
!
!     FILL HUQ PER PAGE 15 MS-28
!
   DO i = 1 , 100
      Huq(i) = 0.0
   ENDDO
   Huq(1) = one
   Huq(13) = one
   Huq(25) = one
   Huq(36) = one
   Huq(49) = one
   Huq(51) = one
   Huq(52) = Sl
   Huq(63) = one
   Huq(64) = Sl
   Huq(75) = one
   Huq(76) = Sl
   Huq(77) = L2
   Huq(78) = Huq(77)*Sl
   Huq(86) = one
   Huq(87) = 2.0*Sl
   Huq(88) = 3.0*Huq(77)
   Huq(100) = Sl
!
   IF ( Ts/=0 ) THEN
      Huq(41) = Cp/Ra
      Huq(45) = N/Ra
      Huq(91) = Cp/Rb
      Huq(92) = Huq(91)*Sl
      Huq(95) = N/Rb
      Huq(96) = Huq(95)*Sl
      Huq(97) = Huq(95)*L2
      Huq(98) = Huq(96)*L2
      Huq(99) = one
!
!     SUBTRACT FROM ROWS 4 AND 9 OF THE ABOVE MATRIX, THE HYQ MATRIX
!
      DO i = 1 , 10
         Huq(i+30) = Huq(i+30) - Hyq(i)
         Huq(i+80) = Huq(i+80) - Hyq(i)
      ENDDO
   ENDIF
!
!     NO NEED TO CALCULATE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY
!
   ising = -1
   CALL invers(10,Huq(1),10,Dum,0,determ,ising,Temp60(1))
!     CHECK SINGULARITY
!
   IF ( ising==1 ) THEN
!
!     NOT SINGULAR, CONTINUE ON..
!
      IF ( Ts==0.0 ) THEN
         Huq(85) = 0.0
         Huq(100) = 0.0
      ENDIF
!
!                                 T    N       T
!     NOW SOLVE FOR (K  ) = (E)(H  )(K  )(H )(E )   I = PIVOT A OR B
!                     IJ         I    Q    J        J = A,B
!
!
!                             T    N        T  T
!     WE WILL SOLVE FOR (E)(H  )(K  )((E)(H  ))
!                            A    Q        B
!
!
!                            T                      T
!     FIRST GET EHAT = (E)(H  ),  AND  EHBT = (E)(H  )
!                           A                      B
!
!
!     EHAT WILL BE STORED AT H(1)...H(60) AND EHBT AT H(61)...H(120)
!
!                0    SP   CP   0    0
!                1    0    0    0    0
!                0    CP  -SP   0    0
!     MATRIX E = 0    0    0    0    SP
!                0    0    0    1    0
!                0    0    0    0    CP
!
      inc1 = 0
      inc2 = 0
      DO
         DO i = 1 , 10
            idx = i + inc1
            iten = 10*i - 9 + inc2
            h(idx) = Huq(iten+1)*Sp + Huq(iten+2)*Cp
            h(idx+10) = Huq(iten)
            h(idx+20) = Huq(iten+1)*Cp - Huq(iten+2)*Sp
            h(idx+30) = Huq(iten+4)*Sp
            h(idx+40) = Huq(iten+3)
            h(idx+50) = Huq(iten+4)*Cp
         ENDDO
         IF ( inc1/=0 ) THEN
!
!     DETERMINE PIVOT POINT NUMBER
!
            IF ( Necpt(2)==Npvt ) THEN
               npivot = 1
            ELSEIF ( Necpt(3)==Npvt ) THEN
               npivot = 2
            ELSE
               CALL mesage(-30,34,Necpt(1))
               npivot = 1
            ENDIF
            EXIT
         ELSE
            inc1 = 60
            inc2 = 5
         ENDIF
      ENDDO
   ELSE
      CALL mesage(30,40,Necpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
!
!     EHAT(1) IS AT H( 1)
!     EHBT(1) IS AT H(61)
!
 900  CALL gmmats(h(60*npivot-59),6,10,0,kqn(1,1),10,10,0,Temp60(1))
!
!     IF N = 0 DOUBLE RESULT FOR KIJ
!
   IF ( N==0 ) THEN
      DO i = 1 , 60
         Temp60(i) = Temp60(i)*2.0
      ENDDO
   ENDIF
!
   DO j = 1 , 2
      CALL gmmats(Temp60(1),6,10,0,h(60*j-59),6,10,1,Kij(1))
      DO i = 1 , 36
         Kijd(i) = Kij(i)
      ENDDO
      CALL sma1b(Kijd(1),Necpt(j+1),-1,Ifkgg,0.0D0)
      IF ( Iopt4/=0 ) THEN
         IF ( Gsube/=0 ) THEN
            Sum = Gsube
            K4ggsw = 1
            CALL sma1b(Kijd(1),Necpt(j+1),-1,If4gg,Sum)
         ENDIF
      ENDIF
   ENDDO
!
END SUBROUTINE kcones
