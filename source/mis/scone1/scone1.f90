!*==scone1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scone1
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , cp , d11 , d12 , d22 , d33 , determ , dum , h11 , h12 , h13 , h14 , h15 , h16 , h17 , h18 , h19 , h1ten , i00 ,  &
         & i01 , i02 , i03 , i04 , i10 , i11 , i12 , i13 , i14 , i20 , i21 , i22 , i23 , i24 , i31 , i32 , i33 , i34 , i42 , i43 ,  &
         & i44 , i52 , i53 , i54 , i62 , i63 , i64 , iii , l2 , n , n2 , n2d33 , n2rsq , ncp , ncprsq , novr , nsp , nspopi ,       &
         & nsprsq , opi , oq , piovb , r , ra , rasq , rb , rbsq , rsq , s , s2 , s3 , sign , sl , sp , sp2 , sp2d22 , spcpr2 ,     &
         & spovr , ssubt , sum
   REAL , DIMENSION(7) , SAVE :: fac
   REAL , DIMENSION(9) :: g
   REAL , DIMENSION(20) :: hyq
   INTEGER :: i , icont , ifac , inc1 , inc2 , ising , isub , iten , j , k , kk , matid1 , matid2 , matid3 , mplus1 , nbegin , npow
   REAL , DIMENSION(28) :: integ
   INTEGER , DIMENSION(7) , SAVE :: na
   INTEGER , DIMENSION(100) :: necpt
   INTEGER , DIMENSION(2) :: nerror
   REAL , SAVE :: one
   REAL :: t , temp , temp1 , temp2 , temp3 , temp4 , temp5 , temp6 , temp7 , ts , tsg3 , twod33 , var , z1 , z2 , za , zb
   REAL , DIMENSION(30) :: t30
   EXTERNAL gmmats , invers , mat , mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
   !>>>>EQUIVALENCE (Ecpt(4),Matid1)
   !>>>>EQUIVALENCE (G(1),Huq(1))
   !>>>>EQUIVALENCE (Ecpt(5),T)
   !>>>>EQUIVALENCE (Ecpt(6),Matid2)
   !>>>>EQUIVALENCE (Ecpt(7),Iii)
   !>>>>EQUIVALENCE (Ecpt(8),Matid3)
   !>>>>EQUIVALENCE (Ecpt(9),Ts)
   !>>>>EQUIVALENCE (Ecpt(11),Z1)
   !>>>>EQUIVALENCE (Ecpt(12),Z2)
   !>>>>EQUIVALENCE (Ecpt(28),Ra)
   !>>>>EQUIVALENCE (Ecpt(29),Za)
   !>>>>EQUIVALENCE (Ecpt(32),Rb)
   !>>>>EQUIVALENCE (Ecpt(33),Zb)
   !>>>>EQUIVALENCE (D11,G(1))
   !>>>>EQUIVALENCE (D12,G(2))
   !>>>>EQUIVALENCE (D22,G(5))
   !>>>>EQUIVALENCE (D33,G(9))
   !>>>>EQUIVALENCE (Integ(1),Huq(1))
   !>>>>EQUIVALENCE (T30(1),H(1))
   !>>>>EQUIVALENCE (Hyq(1),H(31))
   !>>>>EQUIVALENCE (Hyq(1),H11)
   !>>>>EQUIVALENCE (Hyq(2),H12)
   !>>>>EQUIVALENCE (Hyq(3),H13)
   !>>>>EQUIVALENCE (Hyq(4),H14)
   !>>>>EQUIVALENCE (Hyq(5),H15)
   !>>>>EQUIVALENCE (Hyq(6),H16)
   !>>>>EQUIVALENCE (Hyq(7),H17)
   !>>>>EQUIVALENCE (Hyq(8),H18)
   !>>>>EQUIVALENCE (Hyq(9),H19)
   !>>>>EQUIVALENCE (Hyq(10),H1ten)
   !>>>>EQUIVALENCE (I00,Integ(1)) , (I20,Integ(11)) , (I01,Integ(2)) , (I21,Integ(12)) , (I02,Integ(3)) , (I22,Integ(13)) ,             &
!>>>>    & (I03,Integ(4)) , (I23,Integ(14)) , (I04,Integ(5)) , (I24,Integ(15)) , (I10,Integ(6)) , (I31,Integ(16)) , (I11,Integ(7)) ,     &
!>>>>    & (I32,Integ(17)) , (I12,Integ(8)) , (I33,Integ(18)) , (I13,Integ(9)) , (I34,Integ(19)) , (I14,Integ(10)) , (I52,Integ(23)) ,   &
!>>>>    & (I42,Integ(20)) , (I53,Integ(24)) , (I43,Integ(21)) , (I54,Integ(25)) , (I44,Integ(22)) , (I62,Integ(26)) , (I63,Integ(27)) , &
!>>>>    & (I64,Integ(28))
!
   DATA fac/1.0E0 , 1.0E0 , 2.0E0 , 6.0E0 , 24.0E0 , 120.0E0 , 720.0E0/
   DATA na/1 , 1 , 1 , 2 , 3 , 3 , 3/
   DATA one/1.0E0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         costh = 1.0
         sinth = 0.0
         n = necpt(1) - ((necpt(1)/1000)*1000) - 1
         temp1 = rb - ra
         temp2 = zb - za
         sl = sqrt(temp1**2+temp2**2)
         l2 = sl*sl
         IF ( sl==0 ) THEN
            nerror(1) = necpt(1)/1000
            nerror(2) = n + .3E0
            CALL mesage(-30,39,nerror(1))
         ENDIF
         sp = temp1/sl
         cp = temp2/sl
         nsp = n*sp
         ncp = n*cp
         n2 = n*n
         sp2 = sp*sp
         a = ra
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
            rasq = ra*ra
            rbsq = rb*rb
            piovb = pi/b
!
            integ(1) = 0.5E0*piovb*(rbsq-rasq)
            integ(2) = piovb*(rb-ra)
            integ(3) = piovb*alog(rb/ra)
            integ(4) = -piovb*(one/rb-one/ra)
            integ(5) = -0.5E0*piovb*(one/rbsq-one/rasq)
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
                        sum = sum + sign*a**npow*alog(rb/ra)/(fac(npow+1)*fac(j-2))
                     ELSE
                        npow = npow - k
                        ifac = mplus1 - k
                        temp = npow
                        sum = sum + sign*a**k*(rb**npow-ra**npow)/(fac(ifac)*fac(k+1)*temp)
                     ENDIF
                  ENDDO
!
                  integ(isub) = sum*pi*fac(mplus1)/b**mplus1
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
                  integ(isub) = (pi*sl**i)/(float(i)*ra**(j-2))
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
            ks(i) = 0.0E0
         ENDDO
!
         r = 0.50E0*(ra+rb)
         s = 0.50E0*sl
!
         IF ( t==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         var = 1.0
         matid = matid1
         ASSIGN 20 TO icont
         spag_nextblock_1 = 2
      CASE (2)
!
         eltemp = ecpt(35)
         inflag = 2
         CALL mat(ecpt(1))
         g(1) = g11*var
         g(2) = g12*var
         g(3) = g13*var
         g(4) = g12*var
         g(5) = g22*var
         g(6) = g23*var
         g(7) = g13*var
         g(8) = g23*var
         g(9) = g33*var
!
         GOTO icont
!
 20      DO i = 1 , 30
            t30(i) = 0.0E0
         ENDDO
!
         t30(4) = 1.0E0
         t30(11) = n/r
         t30(12) = t30(11)*s
         t30(13) = sp/r
         t30(14) = s*t30(13)
         t30(15) = cp/r
         t30(16) = s*t30(15)
         t30(17) = s*t30(16)
         t30(18) = s*t30(17)
         t30(21) = -t30(13)
         t30(22) = 1.0E0 - t30(14)
         t30(23) = -t30(11)
         t30(24) = -t30(12)
!
         CALL gmmats(g(1),3,3,0,t30(1),3,10,0,ks(1))
         spag_nextblock_1 = 3
      CASE (3)
!
         IF ( iii/=0 ) THEN
!
!     GET G MATERIAL MATRIX FOR MATERIAL ID 2 AND MULTIPLY BY I...
!     THIS THEN IS THE D 3X3 MATRIX BY EQUIVALENCE...
!
            var = iii
            matid = matid2
            ASSIGN 40 TO icont
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = 1 , 9
               g(i) = 0.0E0
            ENDDO
         ENDIF
!
!     FORMING 1.0/Q DIRECTLY
!
 40      opi = one/pi
         DO i = 1 , 20
            hyq(i) = 0.0E0
         ENDDO
         IF ( ts/=0 ) THEN
!
            eltemp = ecpt(35)
            inflag = 1
            matid = matid3
            CALL mat(necpt(1))
!
            IF ( g12==0.0 ) THEN
               ts = 0.0
            ELSE
               n2d33 = n2*d33
               sp2d22 = sp2*d22
               oq = sl*ts*g12*(ra+rb)*0.5E0 + i02*(n2d33+sp2d22)*opi
               oq = one/oq
               nspopi = nsp*opi
               twod33 = 2.0E0*d33
               temp1 = d12*(one/rb-one/ra)
               temp2 = nspopi*(d22+d33)
               temp3 = n*nspopi*(twod33+d22)
               temp4 = oq*0.5E0*ncp*n*d33*opi
               temp5 = opi*(n2*twod33+sp2*d22)
               temp6 = d12*n2*l2/rb
               temp7 = nspopi*cp*0.50E0
!
               hyq(1) = oq*(temp1*ncp-temp7*i03*(d33+2.0E0*d22))
               hyq(2) = oq*(ncp*sl/rb*d12-temp7*i13*(3.0E0*d33+d22)+1.0E0*ncp*opi*i02*d33)
               hyq(3) = temp4*i03
               hyq(4) = temp4*i13
               hyq(5) = oq*(temp1*n2-temp3*i03)
               hyq(6) = oq*(d12*n2*sl/rb-temp3*i13+temp5*i02)
               hyq(7) = oq*(2.0E0*d11*(ra-rb)+temp6+2.0E0*i12*temp5-temp3*i23)
               hyq(8) = oq*(-d11*6.E0*sl*rb+temp6*sl+3.E0*i22*temp5-temp3*i33)
               hyq(9) = -oq*temp2*i02
               hyq(10) = oq*(n*sl*(d12+d33)-temp2*i12)
               hyq(19) = 1.0E0
               hyq(20) = s
!
               tsg3 = ts*g12
               DO i = 1 , 20
                  ks(i+60) = hyq(i)*tsg3
!     FILL HXQ MATIX
!
               ENDDO
            ENDIF
         ENDIF
         IF ( iii/=0 ) THEN
            s2 = s*s
            s3 = s*s2
            rsq = r*r
            spovr = sp/r
            ncprsq = ncp/rsq
            nsprsq = nsp/rsq
            n2rsq = n2/rsq
            spcpr2 = sp*cp/rsq
            novr = n/r
            t30(7) = 2.0E0
            t30(8) = 6.0E0*s
            t30(11) = -ncprsq - spovr*h11
            t30(12) = -s*ncprsq - spovr*h12
            t30(13) = -spovr*h13
            t30(14) = -spovr*h14
            t30(15) = -n2rsq - spovr*h15
            t30(16) = spovr - n2rsq*s - spovr*h16
            t30(17) = 2.0E0*s*spovr - n2rsq*s2 - spovr*h17
            t30(18) = 3.0E0*s2*spovr - n2rsq*s3 - spovr*h18
            t30(19) = -novr - spovr*h19
            t30(20) = -novr*s - spovr*h1ten
            t30(21) = 0.5E0*spcpr2 + novr*h11
            t30(22) = 0.5E0*(s*spcpr2-3.0E0*cp/r) + novr*h12
            t30(23) = -0.50E0*ncprsq + novr*h13
            t30(24) = -ncprsq*s*0.50E0*novr*h14
            t30(25) = nsprsq + novr*h15
            t30(26) = nsprsq*s - novr*(2.0E0-h16)
            t30(27) = nsprsq*s2 - novr*(4.0E0*s-h17)
            t30(28) = nsprsq*s3 - novr*(6.0E0*s2-h18)
            t30(29) = spovr + novr*h19
            t30(30) = -1.0E0 + spovr*s + novr*h1ten
!
            CALL gmmats(g(1),3,3,0,t30(1),3,10,0,ks(31))
         ENDIF
!
!
!
!     FILL HUQ PER PAGE 15 MS-28
!
         DO i = 1 , 100
            huq(i) = 0.0E0
         ENDDO
         huq(1) = one
         huq(13) = one
         huq(25) = one
         huq(36) = one
         huq(49) = one
         huq(51) = one
         huq(52) = sl
         huq(63) = one
         huq(64) = sl
         huq(75) = one
         huq(76) = sl
         huq(77) = l2
         huq(78) = huq(77)*sl
         huq(86) = one
         huq(87) = 2.0E0*sl
         huq(88) = 3.0E0*huq(77)
         huq(100) = sl
!
         IF ( ts/=0 ) THEN
!
            huq(41) = cp/ra
            huq(45) = n/ra
            huq(91) = cp/rb
            huq(92) = huq(91)*sl
            huq(95) = n/rb
            huq(96) = huq(95)*sl
            huq(97) = huq(95)*l2
            huq(98) = huq(96)*l2
            huq(99) = one
            huq(100) = sl
!
!     SUBTRACT FROM ROWS 4 AND 9 OF THE ABOVE MATRIX, THE HYQ MATRIX...
!
            DO i = 1 , 10
               huq(i+30) = huq(i+30) - hyq(i)
               huq(i+80) = huq(i+80) - hyq(i)
            ENDDO
         ENDIF
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
         ising = -1
         CALL invers(10,huq(1),10,dum,0,determ,ising,t30(1))
!
!     CHECK SINGULARITY
         IF ( ising/=1 ) CALL mesage(-30,40,necpt(1))
!
!
!     NOT SINGULAR, CONTINUE ON..
         IF ( ts==0.0 ) THEN
            huq(85) = 0.0
            huq(100) = 0.0
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
         SPAG_Loop_1_1: DO
            DO i = 1 , 10
               isub = i + inc1
               iten = 10*i - 9 + inc2
               h(isub) = huq(iten+1)*sp + huq(iten+2)*cp
               h(isub+10) = huq(iten)
               h(isub+20) = huq(iten+1)*cp - huq(iten+2)*sp
               h(isub+30) = huq(iten+4)*sp
               h(isub+40) = huq(iten+3)
               h(isub+50) = huq(iten+4)*cp
            ENDDO
            IF ( inc1/=0 ) THEN
!
               DO i = 1 , 2
                  CALL gmmats(ks(1),8,10,0,h(60*i-59),6,10,1,ph1out(48*i-25))
               ENDDO
               ssubt = 0.0E0
               IF ( matid1/=0 ) THEN
!     COMPUTE S SUB T
!
                  inflag = 1
                  matid = matid1
                  eltemp = ecpt(35)
                  CALL mat(ecpt(1))
                  ssubt = g11*pi*alpha/(1.0E0-g13)
                  IF ( n==0.0E0 ) ssubt = 2.0E0*ssubt
               ENDIF
!
               ph1out(1) = ecpt(1)
               ph1out(2) = ecpt(2)
               ph1out(3) = ecpt(3)
               ph1out(4) = ssubt
               ph1out(5) = n
               ph1out(6) = iii
               ph1out(7) = z1
               ph1out(8) = z2
               DO i = 9 , 22
                  ph1out(i) = ecpt(i+4)
               ENDDO
               EXIT SPAG_Loop_1_1
            ELSE
               inc1 = 60
               inc2 = 5
            ENDIF
         ENDDO SPAG_Loop_1_1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE scone1
