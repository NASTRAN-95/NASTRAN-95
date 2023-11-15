
SUBROUTINE scone1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alpha , Costh , D11 , D12 , D22 , D33 , Degra , Ecpt(100) , Eltemp , G(9) , G11 , G12 , G13 , G22 , G23 , G33 , H(120) ,    &
      & H11 , H12 , H13 , H14 , H15 , H16 , H17 , H18 , H19 , H1ten , Huq(100) , Hyq(20) , I00 , I01 , I02 , I03 , I04 , I10 , I11 ,&
      & I12 , I13 , I14 , I20 , I21 , I22 , I23 , I24 , I31 , I32 , I33 , I34 , I42 , I43 , I44 , I52 , I53 , I54 , I62 , I63 ,     &
      & I64 , Iii , Integ(28) , Ks(80) , Ph1out(118) , Pi , Ra , Radeg , Rb , S4pisq , Sinth , Stress , T , T30(30) , Ts , Twopi ,  &
      & Z1 , Z2 , Za , Zb
   INTEGER Inflag , Matid , Matid1 , Matid2 , Matid3 , Necpt(100)
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Alpha
   COMMON /sdr2x5/ Ecpt , Ph1out
   COMMON /sdr2x6/ Huq , H , Ks
!
! Local variable declarations
!
   REAL a , b , cp , determ , dum , fac(7) , l2 , n , n2 , n2d33 , n2rsq , ncp , ncprsq , novr , nsp , nspopi , nsprsq , one , opi ,&
      & oq , piovb , r , rasq , rbsq , rsq , s , s2 , s3 , sign , sl , sp , sp2 , sp2d22 , spcpr2 , spovr , ssubt , sum , temp ,    &
      & temp1 , temp2 , temp3 , temp4 , temp5 , temp6 , temp7 , tsg3 , twod33 , var
   INTEGER i , icont , ifac , inc1 , inc2 , ising , isub , iten , j , k , kk , mplus1 , na(7) , nbegin , nerror(2) , npow
!
! End of declarations
!
!     ******* PHASE I OF STRESS DATA RECOVERY FOR CONICAL SHELL*********
!     OUTPUTS FROM THIS ROUTINE FOR USE IN PHASE II ARE...
!     1) ELEMENT ID
!     2 AND 3) SILS A AND B
!     4) S SUB T
!     5) N
!     6) I
!     7) Z1
!     8) Z2
!     9 THRU 22) PHI-S
!     23 THRU 118) TWO 8X6 S MATRICES
!     TOTAL OF 118 WORDS
!***********************************************************************
!     ECPT( 1) = ELEMENT ID             INTEGER        ECT
!     ECPT( 2) = SIL PT A               INTEGER        ECT
!     ECPT( 3) = SIL PT B B             INTEGER        ECT
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
!***********************************************************************
!
!
!
!
!
!
!
!
   EQUIVALENCE (Ecpt(1),Necpt(1))
   EQUIVALENCE (Ecpt(4),Matid1)
   EQUIVALENCE (G(1),Huq(1))
   EQUIVALENCE (Ecpt(5),T)
   EQUIVALENCE (Ecpt(6),Matid2)
   EQUIVALENCE (Ecpt(7),Iii)
   EQUIVALENCE (Ecpt(8),Matid3)
   EQUIVALENCE (Ecpt(9),Ts)
   EQUIVALENCE (Ecpt(11),Z1)
   EQUIVALENCE (Ecpt(12),Z2)
   EQUIVALENCE (Ecpt(28),Ra)
   EQUIVALENCE (Ecpt(29),Za)
   EQUIVALENCE (Ecpt(32),Rb)
   EQUIVALENCE (Ecpt(33),Zb)
   EQUIVALENCE (D11,G(1))
   EQUIVALENCE (D12,G(2))
   EQUIVALENCE (D22,G(5))
   EQUIVALENCE (D33,G(9))
   EQUIVALENCE (Integ(1),Huq(1))
   EQUIVALENCE (T30(1),H(1))
   EQUIVALENCE (Hyq(1),H(31))
   EQUIVALENCE (Hyq(1),H11)
   EQUIVALENCE (Hyq(2),H12)
   EQUIVALENCE (Hyq(3),H13)
   EQUIVALENCE (Hyq(4),H14)
   EQUIVALENCE (Hyq(5),H15)
   EQUIVALENCE (Hyq(6),H16)
   EQUIVALENCE (Hyq(7),H17)
   EQUIVALENCE (Hyq(8),H18)
   EQUIVALENCE (Hyq(9),H19)
   EQUIVALENCE (Hyq(10),H1ten)
   EQUIVALENCE (I00,Integ(1)) , (I20,Integ(11)) , (I01,Integ(2)) , (I21,Integ(12)) , (I02,Integ(3)) , (I22,Integ(13)) ,             &
    & (I03,Integ(4)) , (I23,Integ(14)) , (I04,Integ(5)) , (I24,Integ(15)) , (I10,Integ(6)) , (I31,Integ(16)) , (I11,Integ(7)) ,     &
    & (I32,Integ(17)) , (I12,Integ(8)) , (I33,Integ(18)) , (I13,Integ(9)) , (I34,Integ(19)) , (I14,Integ(10)) , (I52,Integ(23)) ,   &
    & (I42,Integ(20)) , (I53,Integ(24)) , (I43,Integ(21)) , (I54,Integ(25)) , (I44,Integ(22)) , (I62,Integ(26)) , (I63,Integ(27)) , &
    & (I64,Integ(28))
!
   DATA fac/1.0E0 , 1.0E0 , 2.0E0 , 6.0E0 , 24.0E0 , 120.0E0 , 720.0E0/
   DATA na/1 , 1 , 1 , 2 , 3 , 3 , 3/
   DATA one/1.0E0/
!
   Costh = 1.0
   Sinth = 0.0
   n = Necpt(1) - ((Necpt(1)/1000)*1000) - 1
   temp1 = Rb - Ra
   temp2 = Zb - Za
   sl = sqrt(temp1**2+temp2**2)
   l2 = sl*sl
   IF ( sl==0 ) THEN
      nerror(1) = Necpt(1)/1000
      nerror(2) = n + .3E0
      CALL mesage(-30,39,nerror(1))
   ENDIF
   sp = temp1/sl
   cp = temp2/sl
   nsp = n*sp
   ncp = n*cp
   n2 = n*n
   sp2 = sp*sp
   a = Ra
   b = sp
   IF ( b/=0 ) THEN
!
!     OK BELOW IS FOR B NOT EQUAL TO ZERO
!
!     FIRST M = 0 CASE...
!
!                             2-N     2-N
!                      PI ( RB    - RA   )
!               I     =--------------------   (N NOT EQUAL TO 2)
!                0,N       (2-N)  B
!
!
!     FOR N=2   I     = PI * (LOG RB  -  LOG RA) / B
!                0,2             E          E
!
!
      rasq = Ra*Ra
      rbsq = Rb*Rb
      piovb = Pi/b
!
      Integ(1) = 0.5E0*piovb*(rbsq-rasq)
      Integ(2) = piovb*(Rb-Ra)
      Integ(3) = piovb*alog(Rb/Ra)
      Integ(4) = -piovb*(one/Rb-one/Ra)
      Integ(5) = -0.5E0*piovb*(one/rbsq-one/rasq)
!
      isub = 5
      DO i = 1 , 6
         mplus1 = i + 1
         nbegin = na(mplus1)
         DO j = nbegin , 5
            isub = isub + 1
!
!     M = I
!     N = J - 1
!
!     WE ARE GETTING INTEGRAL(M,N)
!     M = POWER OF S
!     N = POWER OF R
!
!
!     EVALUATING AT R = RB  THEN AT R = RA...
!
!                                    K   NPOW
!                 M FAC.     M   (-A) (R)
! I    = (PI)(-----------)( SUM ------------------------) + (TERM-X)
!  MN               (M+1)   K=0  (M-K)FAC.(K)FAC.(NPOW)
!                  B        (K.NE.M-N+2)                    (K.EQ.M-N+2)
!
!
!     WHERE NPOW = M - N - K + 2
!
!
!                    M-N+2
!                (-A)     LOG(R)
!       TERM-X = --------------------
!               (M-N+2)FAC.(N-2)FAC.
!
!     NOTE IN DATA STATEMENT THAT 0 FACTORIAL = FAC(1)
!                                 1 FACTORIAL = FAC(2)
!                                 2 FACTORIAL = FAC(3)    ETC...
!
            sum = 0.0E0
            sign = -1.0E0
            DO kk = 1 , mplus1
               sign = -sign
               k = kk - 1
               npow = i - j + 3
               IF ( k==npow ) THEN
                  sum = sum + sign*a**npow*alog(Rb/Ra)/(fac(npow+1)*fac(j-2))
               ELSE
                  npow = npow - k
                  ifac = mplus1 - k
                  temp = npow
                  sum = sum + sign*a**k*(Rb**npow-Ra**npow)/(fac(ifac)*fac(k+1)*temp)
               ENDIF
            ENDDO
!
            Integ(isub) = sum*Pi*fac(mplus1)/b**mplus1
         ENDDO
      ENDDO
   ELSE
!
!     GO TO 302 FOR B = 0
!
!                            1-N
!                      PI  RA     M+1
!     FOR B = 0 I   = --------- SL    (FOR ALL M,N .GE. 0)
!                M,N    M + 1
!
      isub = 0
      DO i = 1 , 7
         nbegin = na(i)
!
!
         DO j = nbegin , 5
!
!     M = I - 1
!     N = J - 1
!     MPLUS1 THUS EQUALS I
            isub = isub + 1
            Integ(isub) = (Pi*sl**i)/(float(i)*Ra**(j-2))
         ENDDO
!
!     ABOVE COMPLETES ALL INTEGRALS FOR B = 0...
!
!     IF AN OVERFLOW RESULTS BELOW POSSIBLY B IS NOT ZERO, BUT SMALL..
!
      ENDDO
   ENDIF
!
!
   DO i = 1 , 80
      Ks(i) = 0.0E0
   ENDDO
!
   r = 0.50E0*(Ra+Rb)
   s = 0.50E0*sl
!
   IF ( T==0 ) GOTO 300
   var = 1.0
   Matid = Matid1
   ASSIGN 200 TO icont
!
 100  Eltemp = Ecpt(35)
   Inflag = 2
   CALL mat(Ecpt(1))
   G(1) = G11*var
   G(2) = G12*var
   G(3) = G13*var
   G(4) = G12*var
   G(5) = G22*var
   G(6) = G23*var
   G(7) = G13*var
   G(8) = G23*var
   G(9) = G33*var
!
   GOTO icont
!
 200  DO i = 1 , 30
      T30(i) = 0.0E0
   ENDDO
!
   T30(4) = 1.0E0
   T30(11) = n/r
   T30(12) = T30(11)*s
   T30(13) = sp/r
   T30(14) = s*T30(13)
   T30(15) = cp/r
   T30(16) = s*T30(15)
   T30(17) = s*T30(16)
   T30(18) = s*T30(17)
   T30(21) = -T30(13)
   T30(22) = 1.0E0 - T30(14)
   T30(23) = -T30(11)
   T30(24) = -T30(12)
!
   CALL gmmats(G(1),3,3,0,T30(1),3,10,0,Ks(1))
!
 300  IF ( Iii/=0 ) THEN
!
!     GET G MATERIAL MATRIX FOR MATERIAL ID 2 AND MULTIPLY BY I...
!     THIS THEN IS THE D 3X3 MATRIX BY EQUIVALENCE...
!
      var = Iii
      Matid = Matid2
      ASSIGN 400 TO icont
      GOTO 100
   ELSE
      DO i = 1 , 9
         G(i) = 0.0E0
      ENDDO
   ENDIF
!
!     FORMING 1.0/Q DIRECTLY
!
 400  opi = one/Pi
   DO i = 1 , 20
      Hyq(i) = 0.0E0
   ENDDO
   IF ( Ts/=0 ) THEN
!
      Eltemp = Ecpt(35)
      Inflag = 1
      Matid = Matid3
      CALL mat(Necpt(1))
!
      IF ( G12==0.0 ) THEN
         Ts = 0.0
      ELSE
         n2d33 = n2*D33
         sp2d22 = sp2*D22
         oq = sl*Ts*G12*(Ra+Rb)*0.5E0 + I02*(n2d33+sp2d22)*opi
         oq = one/oq
         nspopi = nsp*opi
         twod33 = 2.0E0*D33
         temp1 = D12*(one/Rb-one/Ra)
         temp2 = nspopi*(D22+D33)
         temp3 = n*nspopi*(twod33+D22)
         temp4 = oq*0.5E0*ncp*n*D33*opi
         temp5 = opi*(n2*twod33+sp2*D22)
         temp6 = D12*n2*l2/Rb
         temp7 = nspopi*cp*0.50E0
!
         Hyq(1) = oq*(temp1*ncp-temp7*I03*(D33+2.0E0*D22))
         Hyq(2) = oq*(ncp*sl/Rb*D12-temp7*I13*(3.0E0*D33+D22)+1.0E0*ncp*opi*I02*D33)
         Hyq(3) = temp4*I03
         Hyq(4) = temp4*I13
         Hyq(5) = oq*(temp1*n2-temp3*I03)
         Hyq(6) = oq*(D12*n2*sl/Rb-temp3*I13+temp5*I02)
         Hyq(7) = oq*(2.0E0*D11*(Ra-Rb)+temp6+2.0E0*I12*temp5-temp3*I23)
         Hyq(8) = oq*(-D11*6.E0*sl*Rb+temp6*sl+3.E0*I22*temp5-temp3*I33)
         Hyq(9) = -oq*temp2*I02
         Hyq(10) = oq*(n*sl*(D12+D33)-temp2*I12)
         Hyq(19) = 1.0E0
         Hyq(20) = s
!
         tsg3 = Ts*G12
         DO i = 1 , 20
            Ks(i+60) = Hyq(i)*tsg3
!     FILL HXQ MATIX
!
         ENDDO
      ENDIF
   ENDIF
   IF ( Iii/=0 ) THEN
      s2 = s*s
      s3 = s*s2
      rsq = r*r
      spovr = sp/r
      ncprsq = ncp/rsq
      nsprsq = nsp/rsq
      n2rsq = n2/rsq
      spcpr2 = sp*cp/rsq
      novr = n/r
      T30(7) = 2.0E0
      T30(8) = 6.0E0*s
      T30(11) = -ncprsq - spovr*H11
      T30(12) = -s*ncprsq - spovr*H12
      T30(13) = -spovr*H13
      T30(14) = -spovr*H14
      T30(15) = -n2rsq - spovr*H15
      T30(16) = spovr - n2rsq*s - spovr*H16
      T30(17) = 2.0E0*s*spovr - n2rsq*s2 - spovr*H17
      T30(18) = 3.0E0*s2*spovr - n2rsq*s3 - spovr*H18
      T30(19) = -novr - spovr*H19
      T30(20) = -novr*s - spovr*H1ten
      T30(21) = 0.5E0*spcpr2 + novr*H11
      T30(22) = 0.5E0*(s*spcpr2-3.0E0*cp/r) + novr*H12
      T30(23) = -0.50E0*ncprsq + novr*H13
      T30(24) = -ncprsq*s*0.50E0*novr*H14
      T30(25) = nsprsq + novr*H15
      T30(26) = nsprsq*s - novr*(2.0E0-H16)
      T30(27) = nsprsq*s2 - novr*(4.0E0*s-H17)
      T30(28) = nsprsq*s3 - novr*(6.0E0*s2-H18)
      T30(29) = spovr + novr*H19
      T30(30) = -1.0E0 + spovr*s + novr*H1ten
!
      CALL gmmats(G(1),3,3,0,T30(1),3,10,0,Ks(31))
   ENDIF
!
!
!
!     FILL HUQ PER PAGE 15 MS-28
!
   DO i = 1 , 100
      Huq(i) = 0.0E0
   ENDDO
   Huq(1) = one
   Huq(13) = one
   Huq(25) = one
   Huq(36) = one
   Huq(49) = one
   Huq(51) = one
   Huq(52) = sl
   Huq(63) = one
   Huq(64) = sl
   Huq(75) = one
   Huq(76) = sl
   Huq(77) = l2
   Huq(78) = Huq(77)*sl
   Huq(86) = one
   Huq(87) = 2.0E0*sl
   Huq(88) = 3.0E0*Huq(77)
   Huq(100) = sl
!
   IF ( Ts/=0 ) THEN
!
      Huq(41) = cp/Ra
      Huq(45) = n/Ra
      Huq(91) = cp/Rb
      Huq(92) = Huq(91)*sl
      Huq(95) = n/Rb
      Huq(96) = Huq(95)*sl
      Huq(97) = Huq(95)*l2
      Huq(98) = Huq(96)*l2
      Huq(99) = one
      Huq(100) = sl
!
!     SUBTRACT FROM ROWS 4 AND 9 OF THE ABOVE MATRIX, THE HYQ MATRIX...
!
      DO i = 1 , 10
         Huq(i+30) = Huq(i+30) - Hyq(i)
         Huq(i+80) = Huq(i+80) - Hyq(i)
      ENDDO
   ENDIF
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL invers(10,Huq(1),10,dum,0,determ,ising,T30(1))
!
!     CHECK SINGULARITY
   IF ( ising/=1 ) CALL mesage(-30,40,Necpt(1))
!
!
!     NOT SINGULAR, CONTINUE ON..
   IF ( Ts==0.0 ) THEN
      Huq(85) = 0.0
      Huq(100) = 0.0
   ENDIF
!                            T                      T
!           GET EHAT = (E)(H  ),  AND  EHBT = (E)(H  )
!                           A                      B
!     EHAT WILL BE STORED AT H(1)...H(60) AND EHBT AT H(61)...H(120)
!
!
!              0    SP   CP   0    0
!
!              1    0    0    0    0
!
!              0    CP  -SP   0    0
!   MATRIX E =
!              0    0    0    0    SP
!
!              0    0    0    1    0
!
!              0    0    0    0    CP
   inc1 = 0
   inc2 = 0
   DO
      DO i = 1 , 10
         isub = i + inc1
         iten = 10*i - 9 + inc2
         H(isub) = Huq(iten+1)*sp + Huq(iten+2)*cp
         H(isub+10) = Huq(iten)
         H(isub+20) = Huq(iten+1)*cp - Huq(iten+2)*sp
         H(isub+30) = Huq(iten+4)*sp
         H(isub+40) = Huq(iten+3)
         H(isub+50) = Huq(iten+4)*cp
      ENDDO
      IF ( inc1/=0 ) THEN
!
         DO i = 1 , 2
            CALL gmmats(Ks(1),8,10,0,H(60*i-59),6,10,1,Ph1out(48*i-25))
         ENDDO
         ssubt = 0.0E0
         IF ( Matid1/=0 ) THEN
!     COMPUTE S SUB T
!
            Inflag = 1
            Matid = Matid1
            Eltemp = Ecpt(35)
            CALL mat(Ecpt(1))
            ssubt = G11*Pi*Alpha/(1.0E0-G13)
            IF ( n==0.0E0 ) ssubt = 2.0E0*ssubt
         ENDIF
!
         Ph1out(1) = Ecpt(1)
         Ph1out(2) = Ecpt(2)
         Ph1out(3) = Ecpt(3)
         Ph1out(4) = ssubt
         Ph1out(5) = n
         Ph1out(6) = Iii
         Ph1out(7) = Z1
         Ph1out(8) = Z2
         DO i = 9 , 22
            Ph1out(i) = Ecpt(i+4)
         ENDDO
         EXIT
      ELSE
         inc1 = 60
         inc2 = 5
      ENDIF
   ENDDO
!
END SUBROUTINE scone1
