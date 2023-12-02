!*==dtrbsc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dtrbsc(Iopt,Npivot)
   USE c_ds1adp
   USE c_ds1aet
   USE c_matin
   USE c_matout
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
   INTEGER :: Npivot
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(144) :: a
   REAL(REAL64) , DIMENSION(9) :: d
   REAL(REAL64) :: determ , xb2 , xbc , xc2 , yc2
   REAL(REAL64) , DIMENSION(5,5) :: di
   INTEGER :: i , ic , ih , ising , j , jc , jh , nohyq
   REAL(REAL64) , DIMENSION(4) :: j2x2
   INTEGER , DIMENSION(51) :: necpt
   REAL(REAL64) , DIMENSION(18) :: s
   REAL :: temp
   EXTERNAL gmmatd , inverd , mat , mesage
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (A(1),D(1),G(1)) , (Necpt(1),Ecpt(1)) , (J2x2(1),Dumdp(1)) , (Di(1,1),G(1))
!
!//////
!     CALL BUG (4HTBIG,30,SX,12)
!//////
!     IF NO TRANSVERSE SHEAR FLEXIBILITY EXISTS THE H-INVERSE IS
!     CALCULATED DIRECTLY.  TEST AS FOLLOWS
!
   IF ( ecpt(Iopt+60)/=0.0 .AND. necpt(Iopt+59)/=0 ) THEN
!
!     THE  MATERIAL COEFFICIENTS FOR TRANSVERSE SHEAR ARE CALCULATE HERE
!     AND THE H-INVERSE MATRIX IS GENERATED THE NORMAL WAY
!
!     GET THE G2X2 MATRIX
!
      matid = necpt(Iopt+59)
      inflag = 3
      CALL mat(ecpt(51))
      IF ( g2x211/=0. .OR. g2x212/=0. .OR. g2x222/=0. ) THEN
         t2 = ecpt(Iopt+60)
         g2x2(1) = g2x211*t2
         g2x2(2) = g2x212*t2
         g2x2(3) = g2x212*t2
         g2x2(4) = g2x222*t2
!
         determ = g2x2(1)*g2x2(4) - g2x2(3)*g2x2(2)
         j2x2(1) = g2x2(4)/determ
         j2x2(2) = -g2x2(2)/determ
         j2x2(3) = -g2x2(3)/determ
         j2x2(4) = g2x2(1)/determ
!
!     SETTING UP G MATRIX
!
         inflag = 2
         matid = necpt(Iopt+57)
         CALL mat(necpt(51))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
!
         g(1) = g11
         g(2) = g12
         g(3) = g13
         g(4) = g12
         g(5) = g22
         g(6) = g23
         g(7) = g13
         g(8) = g23
         g(9) = g33
!
!     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
!
         eye = ecpt(Iopt+58)
         DO i = 1 , 9
            d(i) = g(i)*eye
         ENDDO
!
!     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
!       YQ  RIGHT PORTION IS COMPUTED AND USED AS A (2X3). THE LEFT
!           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
!           A(73) THRU A(78) UNTIL NOT NEEDED ANY FURTHER.
!
!
!
         temp = 2.0D0*d(2) + 4.0D0*d(9)
         a(73) = -6.0D0*(j2x2(1)*d(1)+j2x2(2)*d(3))
         a(74) = -j2x2(1)*temp - 6.0D0*j2x2(2)*d(6)
         a(75) = -6.0D0*(j2x2(1)*d(6)+j2x2(2)*d(5))
         a(76) = -6.0D0*(j2x2(2)*d(1)+j2x2(4)*d(3))
         a(77) = -j2x2(2)*temp - 6.0D0*j2x2(4)*d(6)
         a(78) = -6.0D0*(j2x2(2)*d(6)+j2x2(4)*d(5))
!
!     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
!                                              YQ
!
         xbar = (xsubb+xsubc)/3.0D0
         ybar = ysubc/3.0D0
!
         xcsq = xsubc**2
         ycsq = ysubc**2
         xbsq = xsubb**2
         xcyc = xsubc*ysubc
         px2 = (xbsq+xsubb*xsubc+xcsq)/6.0D0
         py2 = ycsq/6.0D0
         pxy2 = ysubc*(xsubb+2.0D0*xsubc)/12.0D0
         xbar3 = 3.0D0*xbar
         ybar3 = 3.0D0*ybar
         ybar2 = 2.0D0*ybar
!
!     F1LL (HBAR) MATRIX STORING AT A(37) THRU A(72)
!
         DO i = 37 , 72
            a(i) = 0.0D0
         ENDDO
!
         a(37) = xbsq
         a(40) = xbsq*xsubb
         a(44) = xsubb
         a(49) = -2.0D0*xsubb
         a(52) = -3.0D0*xbsq
         a(55) = xcsq
         a(56) = xcyc
         a(57) = ycsq
         a(58) = xcsq*xsubc
         a(59) = ycsq*xsubc
         a(60) = ycsq*ysubc
         a(62) = xsubc
         a(63) = ysubc*2.0D0
         a(65) = xcyc*2.0D0
         a(66) = ycsq*3.0D0
         a(67) = -2.0D0*xsubc
         a(68) = -ysubc
         a(70) = -3.0D0*xcsq
         a(71) = -ycsq
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
            a(i+39) = a(i+39) + xsubb*a(i+72)
            a(i+57) = a(i+57) + xsubc*a(i+72) + ysubc*a(i+75)
         ENDDO
!
!     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37) THRU A(72)
!     STORE INVERSE BACK IN A(37) THRU A(72)
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
         ising = -1
         CALL inverd(6,a(37),6,a(73),0,determ,ising,a(79))
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
                  c(jc,2) = a(jh)
                  c(jc,3) = a(jh+3)
               ENDDO
            ENDDO
            nohyq = 0
            CALL spag_block_1
            RETURN
         ELSE
            CALL mesage(-30,33,ecpt(1))
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
   r = 1.0/xsubb
   sp = 1.0/ysubc
   t = sp*xsubc
   u = r*r*sp*t
   r2 = r*r
   s2 = sp**2
!
   DO i = 1 , 72
      c(i,1) = 0.0D0
   ENDDO
!
   c(7,2) = 3.0D0*r2
   c(9,2) = r
   c(11,2) = r
   c(13,2) = -c(7,2)*t**2
   c(14,2) = -r*t
   c(15,2) = c(14,2)*t
   c(16,2) = -2.0D0*r2*r
   c(18,2) = -r2
   c(19,2) = -6.0D0*r*u*(xsubb-xsubc)
   c(20,2) = -r*sp
   c(21,2) = u*(3.0D0*xsubc-2.0D0*xsubb)
   c(22,2) = r*t*u*(6.0D0*xsubb-4.0D0*xsubc)
   c(23,2) = r*sp*t
   c(24,2) = 2.0D0*t*u*(xsubb-xsubc)
!
   c(13,3) = 3.0D0*s2
   c(14,3) = -sp
   c(15,3) = sp*t
   c(21,3) = -s2
   c(22,3) = -2.0D0*s2*sp
   c(23,3) = s2
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
!
!     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
!
!     THE C1, C2, AND C3 MATRICES ARE GENERATED WITH THE FOLLOWING CODE
!     FIRST GENERATE THE S MATRICES IN POSITIONS 1 THRU 9 AND 10 THRU 18
!
      DO I = 1 , 18
         S(I) = 0.0
      ENDDO
      DO I = 1 , 9 , 4
         S(I) = 1.0
         S(I+9) = 1.0
      ENDDO
      S(3) = -Xsubb
      S(11) = Ysubc
      S(12) = -Xsubc
!
!     COMPUTE HA  AND STORE IN  CA, POSITIONS 7 THRU 24
!
!         HA =  -(HB TIMES SB + HC TIMES SC)
!
      CALL gmmatd(c(7,2),6,3,0,S(1),3,3,0,A(37))
      CALL gmmatd(c(7,3),6,3,0,S(10),3,3,0,A(55))
!
      DO I = 1 , 18
!
         c(I+6,1) = -A(I+36) - A(I+54)
      ENDDO
!
!     COMPUTE  HYQ TIMES HX  AND STORE IN CX POSITIONS 1 THRU 6
!     (THE FIRST THREE COLUMNS OF HYQ ARE NULL)
!
      IF ( Nohyq/=1 ) THEN
!
         DO I = 1 , 3
            CALL gmmatd(A(73),2,3,0,c(16,I),3,3,0,c(1,I))
         ENDDO
      ENDIF
!
      c(3,1) = c(3,1) - 1.0D0
      c(5,1) = c(5,1) + 1.0D0
!
!     THE INTEGRALS FOR THE  KDQQ MATRIX ARE GENERATED HERE
!
      Yc2 = Ysubc**2
      Xb2 = Xsubb**2
      Xc2 = Xsubc**2
      Xbc = Xsubb*Xsubc
!
      Di(1,1) = 1.0D0
      Di(1,2) = Ysubc/3.0D0
      Di(1,3) = Yc2/6.0D0
      Di(1,4) = Yc2*Ysubc/10.0D0
      Di(1,5) = Yc2**2/15.0D0
      Di(2,1) = (Xsubb+Xsubc)/3.0D0
      Di(2,2) = Ysubc*(Xsubb+2.0D0*Xsubc)/12.0D0
      Di(2,3) = Di(1,3)*(Xsubb+3.0D0*Xsubc)/5.0D0
      Di(2,4) = Di(1,4)*(Xsubb+4.0D0*Xsubc)/6.0D0
      Di(3,1) = (Xb2+Xbc+Xc2)/6.0D0
      Di(3,2) = Di(1,2)*(Xb2+2.0D0*Xbc+3.0D0*Xc2)/10.0D0
      Di(3,3) = Di(1,3)*(Xb2+3.0D0*Xbc+6.0D0*Xc2)/15.0D0
      Di(4,1) = (Xsubb+Xsubc)*(Xb2+Xc2)/10.0D0
      Di(4,2) = Di(1,2)*((Xsubb+2.0D0*Xsubc)*Xb2+(3.0D0*Xsubb+4.0D0*Xsubc)*Xc2)/20.0D0
      Di(5,1) = (Xb2*Xb2+Xb2*Xbc+Xbc*Xbc+Xbc*Xc2+Xc2*Xc2)/15.0
!
      ar = Xsubb*Ysubc*dble(ecpt(Iopt+56))/2.0D0
      DO I = 1 , 5
         Ic = 6 - I
         DO J = 1 , Ic
            Di(I,J) = Di(I,J)*ar
         ENDDO
      ENDDO
!
!     THE ABOVE INTEGRALS  D(I,J) CORRESPOND TO THE DOCUMENTED
!     VALUES  I(I-1,J-1).  ZERO INDICES DONT ALWAYS COMPILE.
!
!     THE DIFFERENTIAL STIFFNESS MATRIX IN GENERALIZED COORDINATES IS
!     CREATED BELOW AT POSITIONS A(28) TO A(91)
!
      A(28) = sx*Di(1,1)
      A(29) = sxy*Di(1,1)
      A(30) = 2.0D0*sx*Di(2,1)
      A(31) = sx*Di(1,2) + sxy*Di(2,1)
      A(32) = 2.0D0*sxy*Di(1,2)
      A(33) = 3.0D0*sx*Di(3,1)
      A(34) = sx*Di(1,3) + 2.0*sxy*Di(2,2)
      A(35) = 3.0D0*sxy*Di(1,3)
!
      A(37) = sy*Di(1,1)
      A(38) = 2.0D0*sxy*Di(2,1)
      A(39) = sxy*Di(1,2) + sy*Di(2,1)
      A(40) = 2.0D0*sy*Di(1,2)
      A(41) = 3.0D0*sxy*Di(3,1)
      A(42) = sxy*Di(1,3) + 2.0D0*sy*Di(2,2)
      A(43) = 3.0D0*sy*Di(1,3)
!
      A(46) = 4.0D0*sx*Di(3,1)
      A(47) = 2.0D0*(sx*Di(2,2)+sxy*Di(3,1))
      A(48) = 4.0D0*sxy*Di(2,2)
      A(49) = 6.0D0*sx*Di(4,1)
      A(50) = 2.0D0*(sx*Di(2,3)+2.0D0*sxy*Di(3,2))
      A(51) = 6.0D0*sxy*Di(2,3)
!
      A(55) = sx*Di(1,3) + 2.0D0*sxy*Di(2,2) + sy*Di(3,1)
      A(56) = 2.0D0*(sxy*Di(1,3)+sy*Di(2,2))
      A(57) = 3.0D0*(sx*Di(3,2)+sxy*Di(4,1))
      A(58) = sx*Di(1,4) + 3.0D0*sxy*Di(2,3) + 2.0D0*sy*Di(3,2)
      A(59) = 3.0D0*(sxy*Di(1,4)+sy*Di(2,3))
!
      A(64) = 4.0D0*sy*Di(1,3)
      A(65) = 6.0D0*sxy*Di(3,2)
      A(66) = 2.0D0*(sxy*Di(1,4)+2.0D0*sy*Di(2,3))
      A(67) = 6.0D0*sy*Di(1,4)
!
      A(73) = 9.0D0*sx*Di(5,1)
      A(74) = 3.0D0*(sx*Di(3,3)+2.0D0*sxy*Di(4,2))
      A(75) = 9.0D0*sxy*Di(3,3)
!
      A(82) = sx*Di(1,5) + 4.0D0*sxy*Di(2,4) + 4.0D0*sy*Di(3,3)
      A(83) = 3.0D0*sxy*Di(1,5) + 6.0D0*sy*Di(2,4)
!
      A(91) = 9.0D0*sy*Di(1,5)
!
!     FILL IN SYMMETRIC TERMS
!
      DO I = 2 , 8
         Ih = I - 1
         DO J = 1 , Ih
            Ic = 8*(I-1) + J
            Jc = 8*(J-1) + I
            A(Ic+27) = A(Jc+27)
         ENDDO
      ENDDO
!
!     AT THIS STAGE THE 3X3 MATRIX PARTITIONS MAY BE GENERATED
!     THE ACTUAL MATRICES DEPEND ON IOPT
!
      Ic = Npivot
      IF ( Ic/=0 ) THEN
         CALL gmmatd(c(1,Ic),8,3,1,A(28),8,8,0,A(92))
         DO I = 1 , 3
            Ih = 9*(I-1) + 1
            CALL gmmatd(A(92),3,8,0,c(1,I),8,3,0,A(Ih))
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
      CALL gmmatd(A(28),8,8,0,c(1,3),8,3,0,A(92))
      DO I = 1 , 3
         Ih = 28 + 9*(I-1)
         CALL gmmatd(c(1,I),8,3,1,A(92),8,3,0,A(Ih))
      ENDDO
!
!     RECALCULATE THE S MATRIX (IT WAS DESTROYED) -
!     PLACE IN A(55 THRU 72)
!
      DO I = 1 , 18
         A(I+54) = 0.0
      ENDDO
      DO I = 1 , 9 , 4
         A(I+54) = 1.0
         A(I+63) = 1.0
      ENDDO
      A(57) = -Xsubb
      A(65) = Ysubc
      A(66) = -Xsubc
!
!     EXTRACT THE H-INVERSE MATRIX FROM THE C MATRICES
!     STORE AT POSITIONS A(73) THRU A(108)
!
      DO I = 1 , 6
         Ih = 6*I - 6
         Ic = 3*I - 3
!
         DO J = 1 , 3
            Jh = Ih + J + 72
            Jc = Ic + J + 6
            A(Jh) = c(Jc,2)
            A(Jh+3) = c(Jc,3)
         ENDDO
      ENDDO
   END SUBROUTINE spag_block_1
END SUBROUTINE dtrbsc
