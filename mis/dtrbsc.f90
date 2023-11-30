
SUBROUTINE dtrbsc(Iopt,Npivot)
   IMPLICIT NONE
   DOUBLE PRECISION A(144) , Ar , C(24,3) , D(9) , Di(5,5) , Dumdp(81) , Eye , G(9) , G2x2(4) , J2x2(4) , Px2 , Pxy2 , Py2 , R ,    &
                  & R2 , S2 , Sp , Sx , Sxy , Sy , T , T2 , U , Xbar , Xbar3 , Xbsq , Xcsq , Xcyc , Xsubb , Xsubc , Ybar , Ybar2 ,  &
                  & Ybar3 , Ycsq , Ysubc
   REAL Alph12 , Alpha1 , Alpha2 , Costh , Ecpt(100) , Eltemp , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 ,      &
      & Gsube , Rho , Sigcon , Sigshe , Sigten , Sinth , Stress , Tsubd
   INTEGER Inflag , Matid , Necpt(51)
   COMMON /ds1adp/ G , G2x2 , Ar , Eye , Xbar , Ybar , Xcsq , Ycsq , Xbsq , Xcyc , Px2 , Py2 , Pxy2 , Xbar3 , Ybar3 , Ybar2 , T2 ,  &
                 & R , Sp , T , U , R2 , S2 , Dumdp , C , Sx , Sy , Sxy , Xsubb , Xsubc , Ysubc
   COMMON /ds1aet/ Ecpt
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alph12 , Tsubd , Gsube , Sigten , Sigcon , Sigshe ,  &
                 & G2x211 , G2x212 , G2x222
   INTEGER Iopt , Npivot
   DOUBLE PRECISION determ , s(18) , xb2 , xbc , xc2 , yc2
   INTEGER i , ic , ih , ising , j , jc , jh , nohyq
   REAL temp
!
!     IOPT = 1  IMPLIES THAT A CLOUGH TRIANGLE IS CALLING
!     IOPT = 2  IMPLIES THAT A QUADRILATERAL IS CALLING
!
!     ECPT LISTS OF NECESSARY VARIABLES
!
!     POSITION     TRIA1      QUAD1
!     ========     =====      =====
!     ECPT(51)      EID        EID
!     ECPT(52)      SIL1       SIL1
!     ECPT(53)      SIL2       SIL2
!     ECPT(54)      SIL3       SIL3
!     ECPT(55)      THETA      SIL4
!     ECPT(56)      MATID1     THETA
!     ECPT(57)      T1         MATID1
!     ECPT(58)      MATID2     T1
!     ECPT(59)      EYE        MATID2
!     ECPT(60)      MATID3     EYE
!     ECPT(61)      T2         MATID3
!     ECPT(62)      NSMASS     T2
!       :
!     ECT.
!
   EQUIVALENCE (A(1),D(1),G(1)) , (Necpt(1),Ecpt(1)) , (J2x2(1),Dumdp(1)) , (Di(1,1),G(1))
!
!//////
!     CALL BUG (4HTBIG,30,SX,12)
!//////
!     IF NO TRANSVERSE SHEAR FLEXIBILITY EXISTS THE H-INVERSE IS
!     CALCULATED DIRECTLY.  TEST AS FOLLOWS
!
   IF ( Ecpt(Iopt+60)/=0.0 .AND. Necpt(Iopt+59)/=0 ) THEN
!
!     THE  MATERIAL COEFFICIENTS FOR TRANSVERSE SHEAR ARE CALCULATE HERE
!     AND THE H-INVERSE MATRIX IS GENERATED THE NORMAL WAY
!
!     GET THE G2X2 MATRIX
!
      Matid = Necpt(Iopt+59)
      Inflag = 3
      CALL mat(Ecpt(51))
      IF ( G2x211/=0. .OR. G2x212/=0. .OR. G2x222/=0. ) THEN
         T2 = Ecpt(Iopt+60)
         G2x2(1) = G2x211*T2
         G2x2(2) = G2x212*T2
         G2x2(3) = G2x212*T2
         G2x2(4) = G2x222*T2
!
         determ = G2x2(1)*G2x2(4) - G2x2(3)*G2x2(2)
         J2x2(1) = G2x2(4)/determ
         J2x2(2) = -G2x2(2)/determ
         J2x2(3) = -G2x2(3)/determ
         J2x2(4) = G2x2(1)/determ
!
!     SETTING UP G MATRIX
!
         Inflag = 2
         Matid = Necpt(Iopt+57)
         CALL mat(Necpt(51))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
!
         G(1) = G11
         G(2) = G12
         G(3) = G13
         G(4) = G12
         G(5) = G22
         G(6) = G23
         G(7) = G13
         G(8) = G23
         G(9) = G33
!
!     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
!
         Eye = Ecpt(Iopt+58)
         DO i = 1 , 9
            D(i) = G(i)*Eye
         ENDDO
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(73) THRU A(78) UNTIL NOT NEEDED ANY FURTHER.
!
!
!
         temp = 2.0D0*D(2) + 4.0D0*D(9)
         A(73) = -6.0D0*(J2x2(1)*D(1)+J2x2(2)*D(3))
         A(74) = -J2x2(1)*temp - 6.0D0*J2x2(2)*D(6)
         A(75) = -6.0D0*(J2x2(1)*D(6)+J2x2(2)*D(5))
         A(76) = -6.0D0*(J2x2(2)*D(1)+J2x2(4)*D(3))
         A(77) = -J2x2(2)*temp - 6.0D0*J2x2(4)*D(6)
         A(78) = -6.0D0*(J2x2(2)*D(6)+J2x2(4)*D(5))
!
!     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
!                                              YQ
!
         Xbar = (Xsubb+Xsubc)/3.0D0
         Ybar = Ysubc/3.0D0
!
         Xcsq = Xsubc**2
         Ycsq = Ysubc**2
         Xbsq = Xsubb**2
         Xcyc = Xsubc*Ysubc
         Px2 = (Xbsq+Xsubb*Xsubc+Xcsq)/6.0D0
         Py2 = Ycsq/6.0D0
         Pxy2 = Ysubc*(Xsubb+2.0D0*Xsubc)/12.0D0
         Xbar3 = 3.0D0*Xbar
         Ybar3 = 3.0D0*Ybar
         Ybar2 = 2.0D0*Ybar
!
!     F1LL (HBAR) MATRIX STORING AT A(37) THRU A(72)
!
         DO i = 37 , 72
            A(i) = 0.0D0
         ENDDO
!
         A(37) = Xbsq
         A(40) = Xbsq*Xsubb
         A(44) = Xsubb
         A(49) = -2.0D0*Xsubb
         A(52) = -3.0D0*Xbsq
         A(55) = Xcsq
         A(56) = Xcyc
         A(57) = Ycsq
         A(58) = Xcsq*Xsubc
         A(59) = Ycsq*Xsubc
         A(60) = Ycsq*Ysubc
         A(62) = Xsubc
         A(63) = Ysubc*2.0D0
         A(65) = Xcyc*2.0D0
         A(66) = Ycsq*3.0D0
         A(67) = -2.0D0*Xsubc
         A(68) = -Ysubc
         A(70) = -3.0D0*Xcsq
         A(71) = -Ycsq
!
!     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF (H  )(H  )
!                                                     UY   YQ
!     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
!     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
!          UY
!
!     THE FOLLOWING IS THEN PER STEPS 6 AND 7 PAGE -16- MS-17.
!
         DO i = 1 , 3
            A(i+39) = A(i+39) + Xsubb*A(i+72)
            A(i+57) = A(i+57) + Xsubc*A(i+72) + Ysubc*A(i+75)
         ENDDO
!
!     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37) THRU A(72)
!     STORE INVERSE BACK IN A(37) THRU A(72)
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
         ising = -1
         CALL inverd(6,A(37),6,A(73),0,determ,ising,A(79))
!
!     CHECK TO SEE IF H WAS SINGULAR
!
!     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
         IF ( ising/=2 ) THEN
!
!     PARTITION H-INVERSE AND STORE IN C2 AND C3 LOCATIONS 7 THRU 24
!
            DO i = 1 , 6
               ih = 6*i - 6
               ic = 3*i - 3
!
               DO j = 1 , 3
                  jh = ih + j + 36
                  jc = ic + j + 6
                  C(jc,2) = A(jh)
                  C(jc,3) = A(jh+3)
               ENDDO
            ENDDO
            nohyq = 0
            GOTO 100
         ELSE
            CALL mesage(-30,33,Ecpt(1))
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
!     THE H-INVERSE MATRIX IS GENERATED IN TWO PARTITIONS
!         HB IS IN POSITIONS C(7,2) TO C(24,2)
!         HC IS IN POSITIONS C(7,3) TO C(24,3)
!
   nohyq = 1
   R = 1.0/Xsubb
   Sp = 1.0/Ysubc
   T = Sp*Xsubc
   U = R*R*Sp*T
   R2 = R*R
   S2 = Sp**2
!
   DO i = 1 , 72
      C(i,1) = 0.0D0
   ENDDO
!
   C(7,2) = 3.0D0*R2
   C(9,2) = R
   C(11,2) = R
   C(13,2) = -C(7,2)*T**2
   C(14,2) = -R*T
   C(15,2) = C(14,2)*T
   C(16,2) = -2.0D0*R2*R
   C(18,2) = -R2
   C(19,2) = -6.0D0*R*U*(Xsubb-Xsubc)
   C(20,2) = -R*Sp
   C(21,2) = U*(3.0D0*Xsubc-2.0D0*Xsubb)
   C(22,2) = R*T*U*(6.0D0*Xsubb-4.0D0*Xsubc)
   C(23,2) = R*Sp*T
   C(24,2) = 2.0D0*T*U*(Xsubb-Xsubc)
!
   C(13,3) = 3.0D0*S2
   C(14,3) = -Sp
   C(15,3) = Sp*T
   C(21,3) = -S2
   C(22,3) = -2.0D0*S2*Sp
   C(23,3) = S2
!
!     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
!
!     THE C1, C2, AND C3 MATRICES ARE GENERATED WITH THE FOLLOWING CODE
!     FIRST GENERATE THE S MATRICES IN POSITIONS 1 THRU 9 AND 10 THRU 18
!
 100  DO i = 1 , 18
      s(i) = 0.0
   ENDDO
   DO i = 1 , 9 , 4
      s(i) = 1.0
      s(i+9) = 1.0
   ENDDO
   s(3) = -Xsubb
   s(11) = Ysubc
   s(12) = -Xsubc
!
!     COMPUTE HA  AND STORE IN  CA, POSITIONS 7 THRU 24
!
!         HA =  -(HB TIMES SB + HC TIMES SC)
!
   CALL gmmatd(C(7,2),6,3,0,s(1),3,3,0,A(37))
   CALL gmmatd(C(7,3),6,3,0,s(10),3,3,0,A(55))
!
   DO i = 1 , 18
!
      C(i+6,1) = -A(i+36) - A(i+54)
   ENDDO
!
!     COMPUTE  HYQ TIMES HX  AND STORE IN CX POSITIONS 1 THRU 6
!     (THE FIRST THREE COLUMNS OF HYQ ARE NULL)
!
   IF ( nohyq/=1 ) THEN
!
      DO i = 1 , 3
         CALL gmmatd(A(73),2,3,0,C(16,i),3,3,0,C(1,i))
      ENDDO
   ENDIF
!
   C(3,1) = C(3,1) - 1.0D0
   C(5,1) = C(5,1) + 1.0D0
!
!     THE INTEGRALS FOR THE  KDQQ MATRIX ARE GENERATED HERE
!
   yc2 = Ysubc**2
   xb2 = Xsubb**2
   xc2 = Xsubc**2
   xbc = Xsubb*Xsubc
!
   Di(1,1) = 1.0D0
   Di(1,2) = Ysubc/3.0D0
   Di(1,3) = yc2/6.0D0
   Di(1,4) = yc2*Ysubc/10.0D0
   Di(1,5) = yc2**2/15.0D0
   Di(2,1) = (Xsubb+Xsubc)/3.0D0
   Di(2,2) = Ysubc*(Xsubb+2.0D0*Xsubc)/12.0D0
   Di(2,3) = Di(1,3)*(Xsubb+3.0D0*Xsubc)/5.0D0
   Di(2,4) = Di(1,4)*(Xsubb+4.0D0*Xsubc)/6.0D0
   Di(3,1) = (xb2+xbc+xc2)/6.0D0
   Di(3,2) = Di(1,2)*(xb2+2.0D0*xbc+3.0D0*xc2)/10.0D0
   Di(3,3) = Di(1,3)*(xb2+3.0D0*xbc+6.0D0*xc2)/15.0D0
   Di(4,1) = (Xsubb+Xsubc)*(xb2+xc2)/10.0D0
   Di(4,2) = Di(1,2)*((Xsubb+2.0D0*Xsubc)*xb2+(3.0D0*Xsubb+4.0D0*Xsubc)*xc2)/20.0D0
   Di(5,1) = (xb2*xb2+xb2*xbc+xbc*xbc+xbc*xc2+xc2*xc2)/15.0
!
   Ar = Xsubb*Ysubc*dble(Ecpt(Iopt+56))/2.0D0
   DO i = 1 , 5
      ic = 6 - i
      DO j = 1 , ic
         Di(i,j) = Di(i,j)*Ar
      ENDDO
   ENDDO
!
!     THE ABOVE INTEGRALS  D(I,J) CORRESPOND TO THE DOCUMENTED
!     VALUES  I(I-1,J-1).  ZERO INDICES DONT ALWAYS COMPILE.
!
!     THE DIFFERENTIAL STIFFNESS MATRIX IN GENERALIZED COORDINATES IS
!     CREATED BELOW AT POSITIONS A(28) TO A(91)
!
   A(28) = Sx*Di(1,1)
   A(29) = Sxy*Di(1,1)
   A(30) = 2.0D0*Sx*Di(2,1)
   A(31) = Sx*Di(1,2) + Sxy*Di(2,1)
   A(32) = 2.0D0*Sxy*Di(1,2)
   A(33) = 3.0D0*Sx*Di(3,1)
   A(34) = Sx*Di(1,3) + 2.0*Sxy*Di(2,2)
   A(35) = 3.0D0*Sxy*Di(1,3)
!
   A(37) = Sy*Di(1,1)
   A(38) = 2.0D0*Sxy*Di(2,1)
   A(39) = Sxy*Di(1,2) + Sy*Di(2,1)
   A(40) = 2.0D0*Sy*Di(1,2)
   A(41) = 3.0D0*Sxy*Di(3,1)
   A(42) = Sxy*Di(1,3) + 2.0D0*Sy*Di(2,2)
   A(43) = 3.0D0*Sy*Di(1,3)
!
   A(46) = 4.0D0*Sx*Di(3,1)
   A(47) = 2.0D0*(Sx*Di(2,2)+Sxy*Di(3,1))
   A(48) = 4.0D0*Sxy*Di(2,2)
   A(49) = 6.0D0*Sx*Di(4,1)
   A(50) = 2.0D0*(Sx*Di(2,3)+2.0D0*Sxy*Di(3,2))
   A(51) = 6.0D0*Sxy*Di(2,3)
!
   A(55) = Sx*Di(1,3) + 2.0D0*Sxy*Di(2,2) + Sy*Di(3,1)
   A(56) = 2.0D0*(Sxy*Di(1,3)+Sy*Di(2,2))
   A(57) = 3.0D0*(Sx*Di(3,2)+Sxy*Di(4,1))
   A(58) = Sx*Di(1,4) + 3.0D0*Sxy*Di(2,3) + 2.0D0*Sy*Di(3,2)
   A(59) = 3.0D0*(Sxy*Di(1,4)+Sy*Di(2,3))
!
   A(64) = 4.0D0*Sy*Di(1,3)
   A(65) = 6.0D0*Sxy*Di(3,2)
   A(66) = 2.0D0*(Sxy*Di(1,4)+2.0D0*Sy*Di(2,3))
   A(67) = 6.0D0*Sy*Di(1,4)
!
   A(73) = 9.0D0*Sx*Di(5,1)
   A(74) = 3.0D0*(Sx*Di(3,3)+2.0D0*Sxy*Di(4,2))
   A(75) = 9.0D0*Sxy*Di(3,3)
!
   A(82) = Sx*Di(1,5) + 4.0D0*Sxy*Di(2,4) + 4.0D0*Sy*Di(3,3)
   A(83) = 3.0D0*Sxy*Di(1,5) + 6.0D0*Sy*Di(2,4)
!
   A(91) = 9.0D0*Sy*Di(1,5)
!
!     FILL IN SYMMETRIC TERMS
!
   DO i = 2 , 8
      ih = i - 1
      DO j = 1 , ih
         ic = 8*(i-1) + j
         jc = 8*(j-1) + i
         A(ic+27) = A(jc+27)
      ENDDO
   ENDDO
!
!     AT THIS STAGE THE 3X3 MATRIX PARTITIONS MAY BE GENERATED
!     THE ACTUAL MATRICES DEPEND ON IOPT
!
   ic = Npivot
   IF ( ic/=0 ) THEN
      CALL gmmatd(C(1,ic),8,3,1,A(28),8,8,0,A(92))
      DO i = 1 , 3
         ih = 9*(i-1) + 1
         CALL gmmatd(A(92),3,8,0,C(1,i),8,3,0,A(ih))
      ENDDO
   ENDIF
!//////
!     CALL BUG (4HTBKD,300,A,54)
!//////
!
!     AT THIS STAGE THE QUADRILATERAL CALCULATIONS ARE COMPLETE
!
   IF ( Iopt==2 ) RETURN
!
!     THE TRIANGLE SUBROUTINE  MUST RETURN THE FOLLOWING DATA
!         KAC,KBC,KCC  IN POSITIONS  A(28) THRU A(54) -I=NPIVOT
!             S        IN POSITIONS  A(55) THRU A(72)
!           H-INVERSE  IN POSITIONS  A(73) THRU A(108)
!
   CALL gmmatd(A(28),8,8,0,C(1,3),8,3,0,A(92))
   DO i = 1 , 3
      ih = 28 + 9*(i-1)
      CALL gmmatd(C(1,i),8,3,1,A(92),8,3,0,A(ih))
   ENDDO
!
!     RECALCULATE THE S MATRIX (IT WAS DESTROYED) -
!     PLACE IN A(55 THRU 72)
!
   DO i = 1 , 18
      A(i+54) = 0.0
   ENDDO
   DO i = 1 , 9 , 4
      A(i+54) = 1.0
      A(i+63) = 1.0
   ENDDO
   A(57) = -Xsubb
   A(65) = Ysubc
   A(66) = -Xsubc
!
!     EXTRACT THE H-INVERSE MATRIX FROM THE C MATRICES
!     STORE AT POSITIONS A(73) THRU A(108)
!
   DO i = 1 , 6
      ih = 6*i - 6
      ic = 3*i - 3
!
      DO j = 1 , 3
         jh = ih + j + 72
         jc = ic + j + 6
         A(jh) = C(jc,2)
         A(jh+3) = C(jc,3)
      ENDDO
   ENDDO
END SUBROUTINE dtrbsc
