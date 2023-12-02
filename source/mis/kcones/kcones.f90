!*==kcones.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE kcones
USE C_CONDAS
USE C_MATIN
USE C_MATOUT
USE C_SMA1CL
USE C_SMA1DP
USE C_SMA1ET
USE C_SMA1IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: determ , g , h11 , h12 , h13 , h14 , h15 , h16 , h17 , h18 , h19 , h1ten , i00 , i01 , i02 , i03 , i04 , i10 , i11 ,     &
         & i12 , i13 , i14 , i20 , i21 , i22 , i23 , i24 , i31 , i32 , i33 , i34 , i42 , i43 , i44 , i52 , i53 , i54 , i62 , i63 ,  &
         & i64 , n2d33 , nspopi , pi , sp2d22
   REAL , DIMENSION(7) , SAVE :: fac
   REAL , DIMENSION(120) :: h
   INTEGER :: i , idx , inc1 , inc2 , iretrn , ising , iten , j , k , kk , matid1 , matid2 , matid3 , mk1 , mn2 , mnk2 , mplus1 ,   &
            & nbegin , nint , npivot
   REAL , DIMENSION(10,10) :: kqe , kqn , kqx , kqy
   INTEGER , DIMENSION(7) , SAVE :: na
   INTEGER , DIMENSION(100) :: necpt
   INTEGER , DIMENSION(2) :: nerror
   INTEGER , SAVE :: oldpt1 , oldpt2
   REAL , SAVE :: one
   EXTERNAL gmmats , invers , mat , mesage , sma1b
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Consts(1),Pi) , (Ecpt(4),Matid1) , (Ecpt(6),Matid2) , (Ecpt(8),Matid3) , (Ecpt(1),Necpt(1))
   !>>>>EQUIVALENCE (G,G12) , (kqn(1,1),kqe(1,1),kqx(1,1),kqy(1,1))
   !>>>>EQUIVALENCE (Hyq(1),H11) , (Hyq(2),H12) , (Hyq(3),H13) , (Hyq(4),H14) , (Hyq(5),H15) , (Hyq(6),H16) , (Hyq(7),H17) , (Hyq(8),H18)&
!>>>>    & , (Hyq(9),H19) , (Hyq(10),H1ten)
   !>>>>EQUIVALENCE (I00,Integ(1)) , (I20,Integ(11)) , (I01,Integ(2)) , (I21,Integ(12)) , (I02,Integ(3)) , (I22,Integ(13)) ,             &
!>>>>    & (I03,Integ(4)) , (I23,Integ(14)) , (I04,Integ(5)) , (I24,Integ(15)) , (I10,Integ(6)) , (I31,Integ(16)) , (I11,Integ(7)) ,     &
!>>>>    & (I32,Integ(17)) , (I12,Integ(8)) , (I33,Integ(18)) , (I13,Integ(9)) , (I34,Integ(19)) , (I14,Integ(10)) , (I52,Integ(23)) ,   &
!>>>>    & (I42,Integ(20)) , (I53,Integ(24)) , (I43,Integ(21)) , (I54,Integ(25)) , (I44,Integ(22)) , (I62,Integ(26)) , (I63,Integ(27)) , &
!>>>>    & (I64,Integ(28))
   DATA oldpt1 , oldpt2/0 , 0/
   DATA fac/1.0 , 1.0 , 2.0 , 6.0 , 24.0 , 120.0 , 720.0/
   DATA na/1 , 1 , 1 , 2 , 3 , 3 , 3/
   DATA one/1.0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DOES PIVOT POINT EQUAL EITHER OF THE LAST TWO SILS
!
         IF ( oldpt1==necpt(2) ) THEN
            IF ( oldpt2==necpt(3) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( oldpt2==necpt(2) ) THEN
            IF ( oldpt1==necpt(3) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     NO MATCH THUS DO ENTIRE COMPUTATION
!
         Sinth = 0.0
         Costh = 1.0
         nint = necpt(1) - (necpt(1)/1000)*1000 - 1
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
               Piovb = pi/B
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
                     Qq1 = dble(pi*fac(mplus1))
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
                     Integ(idx) = (pi*Sl**i)/(float(i)*Ra**(j-2))
                  ENDDO
               ENDDO
            ENDIF
!
            oldpt1 = necpt(2)
            oldpt2 = necpt(3)
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
            IF ( T==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 20 TO iretrn
            Matid = matid1
            Inflag = 2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nerror(1) = necpt(1)/1000
            nerror(2) = N + .3
            CALL mesage(30,39,nerror(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
            Nogo = 1
            RETURN
         ENDIF
      CASE (2)
!
!     WE HAVE A MATCH ON OLD SIL NUMBER 1
!
         IF ( Npvt/=oldpt1 ) THEN
!
!     WE HAVE A MATCH ON OLD SIL NUMBER 2
!
            npivot = 2
         ELSE
            npivot = 1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         Eltemp = Ecpt(35)
         CALL mat(Ecpt(1))
         GOTO iretrn
 20      E11 = G11
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
         kqe(1,1) = T*(N2e22+Sp2e33)*i02
         kqe(1,2) = T*(N2e22*i12-Spe33*i01+Sp2e33*i12)
         Temp = E22 + E33
         Tnsp = Tn*Sp
         kqe(1,3) = Tnsp*Temp*i02
         kqe(1,4) = Tn*(E12*i01+Sp*Temp*i12)
         Temp = Tn*Cp*E22
         kqe(1,5) = Temp*i02
         kqe(1,6) = Temp*i12
         kqe(1,7) = Temp*i22
         kqe(1,8) = Temp*i32
         Temp4 = 2.*Sp*i11
         kqe(2,2) = T*(N2e22*i22+E33*(i00-Temp4+Sp2*i22))
         kqe(2,3) = Tn*(Spe22*i12-E33*i01+Spe33*i12)
         kqe(2,4) = Tn*(E12*i11+Spe22*i22-E33*i11+Spe33*i22)
         kqe(2,5) = kqe(1,6)
         kqe(2,6) = kqe(1,7)
         kqe(2,7) = kqe(1,8)
         kqe(2,8) = Tn*Cpe22*i42
         kqe(3,3) = T*(Sp2e22*i02+N2e33*i02)
         kqe(3,4) = T*(Spe12*i01+Sp2e22*i12+N2e33*i12)
         Temp = T*Cp*Spe22
         kqe(3,5) = Temp*i02
         kqe(3,6) = Temp*i12
         kqe(3,7) = Temp*i22
         kqe(3,8) = Temp*i32
         kqe(4,4) = T*(E11*i00+Temp4*E12+Sp2e22*i22+N2e33*i22)
         Temp = Sp*Cpe22
         kqe(4,5) = T*(Cpe12*i01+Temp*i12)
         kqe(4,6) = T*(Cpe12*i11+Temp*i22)
         kqe(4,7) = T*(Cpe12*i21+Temp*i32)
         kqe(4,8) = T*(Cpe12*i31+Temp*i42)
         Temp = T*Cp2e22
         kqe(5,5) = Temp*i02
         kqe(5,6) = Temp*i12
         kqe(5,7) = Temp*i22
         kqe(5,8) = Temp*i32
         kqe(6,6) = kqe(5,7)
         kqe(6,7) = kqe(5,8)
         kqe(6,8) = Temp*i42
         kqe(7,7) = kqe(6,8)
         kqe(7,8) = Temp*i52
         kqe(8,8) = Temp*i62
         spag_nextblock_1 = 4
      CASE (4)
!
         IF ( Ecpt(7)==0.0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NOW GET G MATERIAL MATRIX ID = MATID2
!
         Matid = matid2
         ASSIGN 40 TO iretrn
         Inflag = 2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     NOW FORM D = I DOT G
!
 40      D11 = Ecpt(7)*G11
         D12 = Ecpt(7)*G12
         D22 = Ecpt(7)*G22
         D33 = Ecpt(7)*G33
!
!     IF SHEAR THICKNESS IS NOT ZERO FORM THE HYQ AND KQY MATRICES
!
         Ts = Ecpt(9)
         IF ( Ts==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     GET G FOR MATID3
!
         Matid = matid3
         Inflag = 1
         ASSIGN 60 TO iretrn
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 60      IF ( g==0.0 ) THEN
            Ts = 0.0
         ELSE
!
!     FORMING 1.0/Q DIRECTLY
!
            Opi = one/pi
!
! /// MAKE SURE ALL BASIC PRODUCTS ARE AT TOP BEFORE ANY SKIPS
!
            n2d33 = N2*D33
            sp2d22 = Sp2*D22
            Oq = Sl*Ts*g*(Ra+Rb)*0.5 + i02*(n2d33+sp2d22)*Opi
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
            Hyq(1) = Oq*(Temp1*Ncp-Temp7*i03*(D33+2.0*D22))
            Hyq(2) = Oq*(Ncp*Sl/Rb*D12-Temp7*i13*(3.0*D33+D22)+1.5*Ncp*Opi*i02*D33)
            Hyq(3) = Temp4*i03
            Hyq(4) = Temp4*i13
            Hyq(5) = Oq*(Temp1*N2-Temp3*i03)
            Hyq(6) = Oq*(D12*N2*Sl/Rb-Temp3*i13+Temp5*i02)
            Hyq(7) = Oq*(2.0*D11*(Ra-Rb)+Temp6+2.0*i12*Temp5-Temp3*i23)
            Hyq(8) = Oq*(-D11*6.*Sl*Rb+Temp6*Sl+3.*i22*Temp5-Temp3*i33)
            Hyq(9) = -Oq*Temp2*i02
            Hyq(10) = Oq*(N*Sl*(D12+D33)-Temp2*i12)
!
            Temp = Ts*g*i00
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
            Temp = Ts*g
            kqy(9,10) = kqy(9,10) + Temp*i10
            kqy(10,10) = kqy(10,10) + Temp*i20
!
!     END OF KQY COMPUTATION
!
            kqy(9,9) = kqy(9,9) + Temp*i00
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     THE FOLLOWING CODES WERE MOVED HERE FROM KCONEX
!
!     KQX MATRIX FOR SHEAR THICKNESS CONSIDERATION
!
!     (THE FOLLOWING CODE WAS MACHINE GENERATED AND WILL NOT BE SIMPLI-
!     FIED FURTHER UNTIL FORMULATION VERIFICATION IS COMPLETED)
!
         kqx(1,1) = kqx(1,1) + Cp*Cp*i04*(+D22*N**2+2.25*D33*Sp**2)
         kqx(1,2) = kqx(1,2) + Cp*Cp*(D33*Sp*(+2.25*Sp*i14-2.25*i03)+D22*N*N*i14)
         kqx(1,3) = kqx(1,3) + D33*Cp*Cp*Sp*N*i04*(-0.75)
         kqx(1,4) = kqx(1,4) + D33*Cp*Cp*Sp*N*i14*(-0.75)
         kqx(1,5) = kqx(1,5) + Cp*N*i04*(+D22*N**2+3.0*D33*Sp**2)
         kqx(1,6) = kqx(1,6) + Cp*N*(Sp*(D33*(+3.0*Sp*i14-3.0*i03)-D22*i03)+D22*N*N*i14)
         kqx(1,7) = kqx(1,7) + Cp*N*(Sp*(D33*(+3.0*Sp*i24-6.0*i13)+D22*i13*(-2.0))-2.0*D12*i02+D22*N**2*i24)
         kqx(1,8) = kqx(1,8) + Cp*N*(Sp*(D33*(+3.0*Sp*i34-9.0*i23)+D22*i23*(-3.0))-6.0*D12*i12+D22*N**2*i34)
         kqx(1,9) = kqx(1,9) + Cp*i03*(+D22*N**2+1.5*D33*Sp**2)
         kqx(1,10) = kqx(1,10) + Cp*(D33*Sp*(-1.5*i02+1.5*Sp*i13)+D22*N*N*i13)
         kqx(2,2) = kqx(2,2) + Cp*Cp*(D33*(Sp*(i13*(-4.5)+Sp*i24*2.25)+i02*2.25)+D22*N*N*i24)
         kqx(2,3) = kqx(2,3) + D33*Cp*Cp*N*(-0.75*Sp*i14+0.75*i03)
         kqx(2,4) = kqx(2,4) + D33*Cp*Cp*N*(-0.75*Sp*i24+0.75*i13)
         kqx(2,5) = kqx(2,5) + Cp*N*(D33*Sp*(+3.0*Sp*i14-3.0*i03)+D22*N*N*i14)
         kqx(2,6) = kqx(2,6) + Cp*N*(D33*(Sp*(i13*(-6.0)+Sp*i24*3.0)+i02*3.0)+D22*(-Sp*i13+N**2*i24))
         kqx(2,7) = kqx(2,7) + Cp*N*(D33*(Sp*(i23*(-9.0)+Sp*i34*3.0)+i12*6.0)+D22*(-2.0*Sp*i23+N**2*i34)+D12*i12*(-2.0))
         kqx(2,8) = kqx(2,8) + Cp*N*(D33*(Sp*(i33*(-12.0)+Sp*i44*3.0)+i22*9.0)+D22*(-3.0*Sp*i33+N**2*i44)+D12*i22*(-6.0))
         kqx(2,9) = kqx(2,9) + Cp*(D33*Sp*(+1.5*Sp*i13-1.5*i02)+D22*N*N*i13)
         kqx(2,10) = kqx(2,10) + Cp*(D33*(Sp*(i12*(-3.0)+Sp*i23*1.5)+i01*1.5)+D22*N*N*i23)
         kqx(3,3) = kqx(3,3) + D33*Cp*Cp*N*N*i04*0.25
         kqx(3,4) = kqx(3,4) + D33*Cp*Cp*N*N*i14*0.25
         kqx(3,5) = kqx(3,5) + D33*Cp*Sp*N*N*i04*(-1.0)
         kqx(3,6) = kqx(3,6) + D33*Cp*N*N*(-Sp*i14+i03)
         kqx(3,7) = kqx(3,7) + D33*Cp*N*N*(-Sp*i24+2.0*i13)
         kqx(3,8) = kqx(3,8) + D33*Cp*N*N*(-Sp*i34+3.0*i23)
         kqx(3,9) = kqx(3,9) + D33*Cp*Sp*N*i03*(-0.5)
         kqx(3,10) = kqx(3,10) + D33*Cp*N*(+0.5*i02-0.5*Sp*i13)
         kqx(4,4) = kqx(4,4) + D33*Cp*Cp*N*N*i24*0.25
         kqx(4,5) = kqx(4,5) + D33*Cp*Sp*N*N*i14*(-1.0)
         kqx(4,6) = kqx(4,6) + D33*Cp*N*N*(-Sp*i24+i13)
         kqx(4,7) = kqx(4,7) + D33*Cp*N*N*(-Sp*i34+2.0*i23)
         kqx(4,8) = kqx(4,8) + D33*Cp*N*N*(-Sp*i44+3.0*i33)
         kqx(4,9) = kqx(4,9) + D33*Cp*Sp*N*i13*(-0.5)
         kqx(4,10) = kqx(4,10) + D33*Cp*N*(+0.5*i12-0.5*Sp*i23)
         kqx(5,5) = kqx(5,5) + N*N*i04*(+D22*N**2+4.0*D33*Sp**2)
         kqx(5,6) = kqx(5,6) + N*N*(Sp*(D33*(+4.0*Sp*i14-4.0*i03)+D22*i03*(-1.0))+D22*N*N*i14)
         kqx(5,7) = kqx(5,7) + N*N*(Sp*(D33*(+4.0*Sp*i24-8.0*i13)+D22*i13*(-2.0))-2.0*D12*i02+D22*N**2*i24)
         kqx(5,8) = kqx(5,8) + N*N*(Sp*(D33*(+4.0*Sp*i34-12.0*i23)+D22*i23*(-3.0))-6.0*D12*i12+D22*N**2*i34)
         kqx(5,9) = kqx(5,9) + N*i03*(+D22*N**2+2.0*D33*Sp**2)
         kqx(5,10) = kqx(5,10) + N*(D33*Sp*(-2.0*i02+2.0*Sp*i13)+D22*N*N*i13)
         kqx(6,6) = kqx(6,6) + N*N*(Sp*(i13*(D22*(-2.0)+D33*(-8.0))+D33*Sp*i24*4.0)+D22*N**2*i24+4.0*D33*i02) + D22*Sp*Sp*i02
         kqx(6,7) = kqx(6,7) + N*N*(Sp*(i23*(D22*(-3.0)+D33*(-12.0))+D33*Sp*i34*4.0)+i12*(-2.0*D12+8.0*D33)+D22*N*N*i34)            &
                  & + Sp*(+2.0*D12*i01+2.0*D22*Sp*i12)
         kqx(6,8) = kqx(6,8) + N*N*(Sp*(i33*(D22*(-4.0)+D33*(-16.0))+D33*Sp*i44*4.0)+i22*(-6.0*D12+12.0*D33)+D22*N*N*i44)           &
                  & + Sp*(+6.0*D12*i11+3.0*D22*Sp*i22)
         kqx(6,9) = kqx(6,9) + N*(Sp*(D33*(+2.0*Sp*i13-2.0*i02)+D22*i02*(-1.0))+D22*N*N*i13)
         kqx(6,10) = kqx(6,10) + N*(D33*(Sp*(i12*(-4.0)+Sp*i23*2.0)+i01*2.0)+D22*(+N**2*i23-Sp*i12))
         kqx(7,7) = kqx(7,7) + N*N*(Sp*(i33*(D22*(-4.0)+D33*(-16.0))+D33*Sp*i44*4.0)+i22*(D12*(-4.0)+D33*16.0)+D22*N*N*i44)         &
                  & + Sp*(D12*i11*8.0+D22*Sp*i22*4.0) + D11*i00*4.0
         kqx(7,8) = kqx(7,8) + N*N*(Sp*(i43*(D22*(-5.0)+D33*(-20.0))+D33*Sp*i54*4.0)+i32*(D12*(-8.0)+D33*24.0)+D22*N*N*i54)         &
                  & + Sp*(D12*i21*18.0+D22*Sp*i32*6.0) + D11*i10*12.0
         kqx(7,9) = kqx(7,9) + N*(Sp*(D33*(+2.0*Sp*i23-4.0*i12)+D22*i12*(-2.0))-2.0*D12*i01+D22*N**2*i23)
         kqx(7,10) = kqx(7,10) + N*(D33*(Sp*(i22*(-6.0)+Sp*i33*2.0)+i11*4.0)+D22*(+N**2*i33-2.0*Sp*i22)+D12*i11*(-2.0))
         kqx(8,8) = kqx(8,8) + N*N*(Sp*(i53*(D22*(-6.0)+D33*(-24.0))+D33*Sp*i64*4.0)+i42*(D12*(-12.0)+D33*36.0)+D22*N*N*i64)        &
                  & + Sp*(D12*i31*36.0+D22*Sp*i42*9.0) + D11*i20*36.0
         kqx(8,9) = kqx(8,9) + N*(Sp*(D33*(+2.0*Sp*i33-6.0*i22)+D22*i22*(-3.0))-6.0*D12*i11+D22*N**2*i33)
         kqx(8,10) = kqx(8,10) + N*(D33*(Sp*(i32*(-8.0)+Sp*i43*2.0)+i21*6.0)+D22*(+N**2*i43-3.0*Sp*i32)+D12*i21*(-6.0))
         kqx(9,9) = kqx(9,9) + i02*(+D22*N**2+D33*Sp**2)
         kqx(9,10) = kqx(9,10) + D33*Sp*(-i01+Sp*i12) + D22*N*N*i12
         kqx(10,10) = kqx(10,10) + D33*(Sp*(i11*(-2.0)+Sp*i22)+i00) + D22*N*N*i22
         IF ( Ts/=0.0 ) THEN
!
!     THE FOLLOWING CODES WERE MOVED HERE FROM KCONEY
!
            kqx(1,1) = kqx(1,1) + h11*(Sp*(Cp*N*i03*(D22*2.0+D33*3.0)+D22*Sp*h11*i02)+D33*N*N*h11*i02)
            kqx(1,2) = kqx(1,2) + N*(Cp*(Sp*(D22*(+h12*i03+h11*i13)+D33*(+1.5*h12*i03+1.5*h11*i13))+D33*h11*i02*(-1.5))             &
                     & +D33*N*h11*h12*i02) + D22*Sp*Sp*h11*h12*i02
            kqx(1,3) = kqx(1,3) + N*(D33*(Cp*i03*(+1.5*Sp*h13-0.5*N*h11)+N*h11*h13*i02)+D22*Cp*Sp*h13*i03) + D22*Sp*Sp*h11*h13*i02
            kqx(1,4) = kqx(1,4) + N*(D33*(Cp*(+1.5*Sp*h14*i03-0.5*N*h11*i13)+N*h11*h14*i02)+D22*Cp*Sp*h14*i03)                      &
                     & + D22*Sp*Sp*h11*h14*i02
            kqx(1,5) = kqx(1,5) + Sp*(N*i03*(D22*(+Cp*h15+N*h11)+D33*(+1.5*Cp*h15+2.0*N*h11))+D22*Sp*h11*h15*i02)                   &
                     & + D33*N*N*h11*h15*i02
            kqx(1,6) = kqx(1,6) + Sp*(D22*(h11*(Sp*i02*(-1.0+h16)+N*N*i13)+Cp*N*h16*i03)+D33*N*(+1.5*Cp*h16*i03+2.0*N*h11*i13))     &
                     & + D33*N*N*h11*i02*(-2.0+h16)
            kqx(1,7) = kqx(1,7) + Sp*(h11*(D22*(Sp*(-2.0*i12+h17*i02)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)                        &
                     & +Cp*N*h17*i03*(+D22+1.5*D33)) + D33*N*N*h11*(-4.0*i12+h17*i02)
            kqx(1,8) = kqx(1,8) + Sp*(h11*(D22*(Sp*(-3.0*i22+h18*i02)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)                        &
                     & +Cp*N*h18*i03*(+D22+1.5*D33)) + D33*N*N*h11*(-6.0*i22+h18*i02)
            kqx(1,9) = kqx(1,9) + Sp*(N*(D22*(+Cp*h19*i03+h11*i02)+D33*(+1.5*Cp*h19*i03+h11*i02))+D22*Sp*h11*h19*i02)               &
                     & + D33*N*N*h11*h19*i02
            kqx(1,10) = kqx(1,10) + N*(D33*(h11*(-i01+Sp*i12+N*h1ten*i02)+Cp*Sp*h1ten*i03*1.5)+D22*Sp*(+Cp*h1ten*i03+h11*i12))      &
                      & + D22*Sp*Sp*h11*h1ten*i02
            kqx(2,2) = kqx(2,2) + h12*(N*(Cp*(D33*(Sp*i13*3.+i02*(-3.))+D22*Sp*i13*2.)+D33*N*h12*i02)+D22*Sp*Sp*h12*i02)
            kqx(2,3) = kqx(2,3) + N*(D33*(Cp*(h13*(+1.5*Sp*i13-1.5*i02)+N*h12*i03*(-0.5))+N*h12*h13*i02)+D22*Cp*Sp*h13*i13)         &
                     & + D22*Sp*Sp*h12*h13*i02
            kqx(2,4) = kqx(2,4) + N*(D33*(Cp*(h14*(+1.5*Sp*i13-1.5*i02)+N*h12*i13*(-0.5))+N*h12*h14*i02)+D22*Cp*Sp*h14*i13)         &
                     & + D22*Sp*Sp*h12*h14*i02
            kqx(2,5) = kqx(2,5) + N*(D33*(h15*(Cp*(+1.5*Sp*i13-1.5*i02)+N*h12*i02)+Sp*N*h12*i03*2.0)+D22*Sp*(+Cp*h15*i13+N*h12*i03))&
                     & + D22*Sp*Sp*h12*h15*i02
            kqx(2,6) = kqx(2,6) + N*(D33*(N*h12*(i02*(-2.0+h16)+Sp*i13*2.0)+Cp*h16*(+1.5*Sp*i13-1.5*i02))+D22*Sp*i13*(+Cp*h16+N*h12)&
                     & ) + D22*Sp*Sp*h12*i02*(-1.0+h16)
            kqx(2,7) = kqx(2,7) + Sp*(h12*(D22*(Sp*(-2.0*i12+h17*i02)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)                        &
                     & +Cp*N*h17*i13*(+D22+1.5*D33)) + D33*N*(N*h12*(-4.0*i12+h17*i02)+Cp*h17*i02*(-1.5))
            kqx(2,8) = kqx(2,8) + Sp*(h12*(D22*(Sp*(-3.0*i22+h18*i02)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)                        &
                     & +Cp*N*h18*i13*(+D22+1.5*D33)) + D33*N*(N*h12*(-6.0*i22+h18*i02)+Cp*h18*i02*(-1.5))
            kqx(2,9) = kqx(2,9) + N*(D33*(h19*(Cp*(+1.5*Sp*i13-1.5*i02)+N*h12*i02)+Sp*h12*i02)+D22*Sp*(+Cp*h19*i13+h12*i02))        &
                     & + D22*Sp*Sp*h12*h19*i02
            kqx(2,10) = kqx(2,10) + N*(D33*(h12*(-i01+Sp*i12+N*h1ten*i02)+Cp*h1ten*(+1.5*Sp*i13-1.5*i02))                           &
                      & +D22*Sp*(+Cp*h1ten*i13+h12*i12)) + D22*Sp*Sp*h12*h1ten*i02
            kqx(3,3) = kqx(3,3) + h13*(D33*N*N*(Cp*i03*(-1.0)+h13*i02)+D22*Sp*Sp*h13*i02)
            kqx(3,4) = kqx(3,4) + D33*N*N*(Cp*(-0.5*h14*i03-0.5*h13*i13)+h13*h14*i02) + D22*Sp*Sp*h13*h14*i02
            kqx(3,5) = kqx(3,5) + N*N*(D33*(h13*(+2.0*Sp*i03+h15*i02)+Cp*h15*i03*(-0.5))+D22*Sp*h13*i03) + D22*Sp*Sp*h13*h15*i02
            kqx(3,6) = kqx(3,6) + h13*(Sp*(D22*(Sp*i02*(-1.+h16)+N*N*i13)+D33*N*N*i13*2.0)+D33*N*N*i02*(-2.0+h16))                  &
                     & + D33*Cp*N*N*h16*i03*(-0.5)
            kqx(3,7) = kqx(3,7) + h13*(Sp*(D22*(Sp*(-2.0*i12+h17*i02)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)                        &
                     & +D33*N*N*(-4.0*i12+h17*i02)) + D33*Cp*N*N*h17*i03*(-0.5)
            kqx(3,8) = kqx(3,8) + h13*(Sp*(D22*(Sp*(-3.0*i22+h18*i02)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)                        &
                     & +D33*N*N*(-6.0*i22+h18*i02)) + D33*Cp*N*N*h18*i03*(-0.5)
            kqx(3,9) = kqx(3,9) + N*(D33*(N*h19*(-0.5*Cp*i03+h13*i02)+Sp*h13*i02)+D22*Sp*h13*i02) + D22*Sp*Sp*h13*h19*i02
            kqx(3,10) = kqx(3,10) + N*(D33*(h13*(-i01+Sp*i12+N*h1ten*i02)+Cp*N*h1ten*i03*(-0.5))+D22*Sp*h13*i12)                    &
                      & + D22*Sp*Sp*h13*h1ten*i02
            kqx(4,4) = kqx(4,4) + h14*(D33*N*N*(Cp*i13*(-1.0)+h14*i02)+D22*Sp*Sp*h14*i02)
            kqx(4,5) = kqx(4,5) + N*N*(D33*(h14*(+2.0*Sp*i03+h15*i02)+Cp*h15*i13*(-0.5))+D22*Sp*h14*i03) + D22*Sp*Sp*h14*h15*i02
!
!     THE FOLLOWING CODES, THRU 270, WERE MOVED HERE FROM KCONEZ
!
            kqx(4,6) = kqx(4,6) + h14*(Sp*(D22*(Sp*i02*(-1.+h16)+N*N*i13)+D33*N*N*i13*2.0)+D33*N*N*i02*(-2.0+h16))                  &
                     & + D33*Cp*N*N*h16*i13*(-0.5)
            kqx(4,7) = kqx(4,7) + h14*(Sp*(D22*(Sp*(-2.0*i12+h17*i02)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)                        &
                     & +D33*N*N*(-4.0*i12+h17*i02)) + D33*Cp*N*N*h17*i13*(-0.5)
            kqx(4,8) = kqx(4,8) + h14*(Sp*(D22*(Sp*(-3.0*i22+h18*i02)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)                        &
                     & +D33*N*N*(-6.0*i22+h18*i02)) + D33*Cp*N*N*h18*i13*(-0.5)
            kqx(4,9) = kqx(4,9) + N*(D33*(N*h19*(-0.5*Cp*i13+h14*i02)+Sp*h14*i02)+D22*Sp*h14*i02) + D22*Sp*Sp*h14*h19*i02
            kqx(4,10) = kqx(4,10) + N*(D33*(h14*(-i01+Sp*i12+N*h1ten*i02)+Cp*N*h1ten*i13*(-0.5))+D22*Sp*h14*i12)                    &
                      & + D22*Sp*Sp*h14*h1ten*i02
            kqx(5,5) = kqx(5,5) + h15*(Sp*(N*N*i03*(D22*2.0+D33*4.0)+D22*Sp*h15*i02)+D33*N*N*h15*i02)
            kqx(5,6) = kqx(5,6) + Sp*(D22*(h15*(Sp*i02*(-1.+h16)+N*N*i13)+N*N*h16*i03)+D33*N*N*(+2.0*h16*i03+2.0*h15*i13))          &
                     & + D33*N*N*h15*i02*(-2.0+h16)
            kqx(5,7) = kqx(5,7) + Sp*(h15*(D22*(Sp*(-2.0*i12+h17*i02)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)                        &
                     & +N*N*h17*i03*(+D22+2.0*D33)) + D33*N*N*h15*(-4.0*i12+h17*i02)
            kqx(5,8) = kqx(5,8) + Sp*(h15*(D22*(Sp*(-3.0*i22+h18*i02)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)                        &
                     & +N*N*h18*i03*(+D22+2.0*D33)) + D33*N*N*h15*(-6.0*i22+h18*i02)
            kqx(5,9) = kqx(5,9) + Sp*(N*(D22*(+N*h19*i03+h15*i02)+D33*(+2.0*N*h19*i03+h15*i02))+D22*Sp*h15*h19*i02)                 &
                     & + D33*N*N*h15*h19*i02
            kqx(5,10) = kqx(5,10) + N*(D33*(h15*(-i01+Sp*i12+N*h1ten*i02)+Sp*N*h1ten*i03*2.)+D22*Sp*(+N*h1ten*i03+h15*i12))         &
                      & + D22*Sp*Sp*h15*h1ten*i02
            kqx(6,6) = kqx(6,6) + h16*(Sp*(D22*(Sp*i02*(-2.0+h16)+N*N*i13*2.0)+D33*N*N*i13*4.0)+D33*N*N*i02*(-4.0+h16))
            kqx(6,7) = kqx(6,7) + Sp*(D22*(Sp*(h16*(-2.0*i12+h17*i02)+h17*i02*(-1.0))+N*N*(+h17*i13+h16*i23))                       &
                     & +D33*N*N*(+2.0*h17*i13+2.0*h16*i23)+D12*h16*i01*(-2.0)) + D33*N*N*(h16*(-4.0*i12+h17*i02)+h17*i02*(-2.0))
            kqx(6,8) = kqx(6,8) + Sp*(D22*(Sp*(h16*(-3.0*i22+h18*i02)+h18*i02*(-1.0))+N*N*(+h18*i13+h16*i33))                       &
                     & +D33*N*N*(+2.0*h18*i13+2.0*h16*i33)+D12*h16*i11*(-6.0)) + D33*N*N*(h16*(-6.0*i22+h18*i02)+h18*i02*(-2.0))
            kqx(6,9) = kqx(6,9) + Sp*(D22*(h19*(Sp*i02*(-1.+h16)+N*N*i13)+N*h16*i02)+D33*N*(+2.0*N*h19*i13+h16*i02))                &
                     & + D33*N*N*h19*i02*(-2.0+h16)
            kqx(6,10) = kqx(6,10) + N*(D33*(N*h1ten*(i02*(-2.0+h16)+Sp*i13*2.0)+h16*(-i01+Sp*i12))+D22*Sp*(+N*h1ten*i13+h16*i12))   &
                      & + D22*Sp*Sp*h1ten*i02*(-1.0+h16)
            kqx(7,7) = kqx(7,7) + h17*(Sp*(D22*(Sp*(i12*(-4.0)+h17*i02)+N*N*i23*2.0)+D12*i01*(-4.0)+D33*N*N*i23*4.0)                &
                     & +D33*N*N*(i12*(-8.0)+h17*i02))
            kqx(7,8) = kqx(7,8) + Sp*(D22*(Sp*(h17*(-3.0*i22+h18*i02)+h18*i12*(-2.0))+N*N*(+h18*i23+h17*i33))                       &
                     & +D12*(-6.0*h17*i11-2.0*h18*i01)+D33*N*N*(+2.0*h18*i23+2.0*h17*i33))                                          &
                     & + D33*N*N*(h17*(-6.0*i22+h18*i02)+h18*i12*(-4.0))
            kqx(7,9) = kqx(7,9) + Sp*(h19*(D22*(Sp*(+h17*i02-2.0*i12)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)+N*h17*i02*(+D22+D33))  &
                     & + D33*N*N*h19*(-4.*i12+h17*i02)
            kqx(7,10) = kqx(7,10) + Sp*(h1ten*(D22*(Sp*(+h17*i02-2.0*i12)+N*N*i23)-2.0*D12*i01+2.0*D33*N**2*i23)                    &
                      & +N*h17*i12*(+D22+D33)) + D33*N*(N*h1ten*(-4.0*i12+h17*i02)+h17*i01*(-1.0))
            kqx(8,8) = kqx(8,8) + h18*(Sp*(D22*(Sp*(i22*(-6.0)+h18*i02)+N*N*i33*2.0)+D12*i11*(-12.0)+D33*N*N*i33*4.0)               &
                     & +D33*N*N*(i22*(-12.0)+h18*i02))
            kqx(8,9) = kqx(8,9) + Sp*(h19*(D22*(Sp*(+h18*i02-3.0*i22)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)+N*h18*i02*(+D22+D33))  &
                     & + D33*N*N*h19*(-6.*i22+h18*i02)
            kqx(8,10) = kqx(8,10) + Sp*(h1ten*(D22*(Sp*(+h18*i02-3.0*i22)+N*N*i33)-6.0*D12*i11+2.0*D33*N**2*i33)                    &
                      & +N*h18*i12*(+D22+D33)) + D33*N*(N*h1ten*(-6.0*i22+h18*i02)+h18*i01*(-1.0))
            kqx(9,9) = kqx(9,9) + h19*i02*(Sp*(N*(D22*2.0+D33*2.0)+D22*Sp*h19)+D33*N*N*h19)
            kqx(9,10) = kqx(9,10) + N*(D33*(h19*(-i01+Sp*i12+N*h1ten*i02)+Sp*h1ten*i02)+D22*Sp*(+h1ten*i02+h19*i12))                &
                      & + D22*Sp*Sp*h19*h1ten*i02
            kqx(10,10) = kqx(10,10) + h1ten*(N*(D33*(Sp*i12*2.0+i01*(-2.0)+N*h1ten*i02)+D22*Sp*i12*2.0)+D22*Sp*Sp*h1ten*i02)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     SET LOWER TRIANGLE EQUAL TO UPPER TRIANGLE OF KQN MATRIX
!
         DO i = 1 , 10
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
            SPAG_Loop_1_1: DO
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
                  IF ( necpt(2)==Npvt ) THEN
                     npivot = 1
                  ELSEIF ( necpt(3)==Npvt ) THEN
                     npivot = 2
                  ELSE
                     CALL mesage(-30,34,necpt(1))
                     npivot = 1
                  ENDIF
                  EXIT SPAG_Loop_1_1
               ELSE
                  inc1 = 60
                  inc2 = 5
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSE
            CALL mesage(30,40,necpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
            Nogo = 1
            RETURN
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     EHAT(1) IS AT H( 1)
!     EHBT(1) IS AT H(61)
!
         CALL gmmats(h(60*npivot-59),6,10,0,kqn(1,1),10,10,0,Temp60(1))
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
            CALL sma1b(Kijd(1),necpt(j+1),-1,Ifkgg,0.0D0)
            IF ( Iopt4/=0 ) THEN
               IF ( Gsube/=0 ) THEN
                  Sum = Gsube
                  K4ggsw = 1
                  CALL sma1b(Kijd(1),necpt(j+1),-1,If4gg,Sum)
               ENDIF
            ENDIF
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE kcones
