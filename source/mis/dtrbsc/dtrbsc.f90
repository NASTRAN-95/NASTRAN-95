!*==dtrbsc.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dtrbsc(Iopt,Npivot)
USE C_DS1ADP
USE C_DS1AET
USE C_MATIN
USE C_MATOUT
USE ISO_FORTRAN_ENV                 
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
   IF ( Ecpt(Iopt+60)/=0.0 .AND. necpt(Iopt+59)/=0 ) THEN
!
!     THE  MATERIAL COEFFICIENTS FOR TRANSVERSE SHEAR ARE CALCULATE HERE
!     AND THE H-INVERSE MATRIX IS GENERATED THE NORMAL WAY
!
!     GET THE G2X2 MATRIX
!
      Matid = necpt(Iopt+59)
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
         j2x2(1) = G2x2(4)/determ
         j2x2(2) = -G2x2(2)/determ
         j2x2(3) = -G2x2(3)/determ
         j2x2(4) = G2x2(1)/determ
!
!     SETTING UP G MATRIX
!
         Inflag = 2
         Matid = necpt(Iopt+57)
         CALL mat(necpt(51))
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
            d(i) = G(i)*Eye
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
            a(i) = 0.0D0
         ENDDO
!
         a(37) = Xbsq
         a(40) = Xbsq*Xsubb
         a(44) = Xsubb
         a(49) = -2.0D0*Xsubb
         a(52) = -3.0D0*Xbsq
         a(55) = Xcsq
         a(56) = Xcyc
         a(57) = Ycsq
         a(58) = Xcsq*Xsubc
         a(59) = Ycsq*Xsubc
         a(60) = Ycsq*Ysubc
         a(62) = Xsubc
         a(63) = Ysubc*2.0D0
         a(65) = Xcyc*2.0D0
         a(66) = Ycsq*3.0D0
         a(67) = -2.0D0*Xsubc
         a(68) = -Ysubc
         a(70) = -3.0D0*Xcsq
         a(71) = -Ycsq
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
            a(i+39) = a(i+39) + Xsubb*a(i+72)
            a(i+57) = a(i+57) + Xsubc*a(i+72) + Ysubc*a(i+75)
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
                  C(jc,2) = a(jh)
                  C(jc,3) = a(jh+3)
               ENDDO
            ENDDO
            nohyq = 0
            CALL spag_block_1
            RETURN
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
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
!
!     THE C1, C2, AND C3 MATRICES ARE GENERATED WITH THE FOLLOWING CODE
!     FIRST GENERATE THE S MATRICES IN POSITIONS 1 THRU 9 AND 10 THRU 18
!
      DO i = 1 , 18
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
      CALL gmmatd(C(7,2),6,3,0,s(1),3,3,0,a(37))
      CALL gmmatd(C(7,3),6,3,0,s(10),3,3,0,a(55))
!
      DO i = 1 , 18
!
         C(i+6,1) = -a(i+36) - a(i+54)
      ENDDO
!
!     COMPUTE  HYQ TIMES HX  AND STORE IN CX POSITIONS 1 THRU 6
!     (THE FIRST THREE COLUMNS OF HYQ ARE NULL)
!
      IF ( nohyq/=1 ) THEN
!
         DO i = 1 , 3
            CALL gmmatd(a(73),2,3,0,C(16,i),3,3,0,C(1,i))
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
      di(1,1) = 1.0D0
      di(1,2) = Ysubc/3.0D0
      di(1,3) = yc2/6.0D0
      di(1,4) = yc2*Ysubc/10.0D0
      di(1,5) = yc2**2/15.0D0
      di(2,1) = (Xsubb+Xsubc)/3.0D0
      di(2,2) = Ysubc*(Xsubb+2.0D0*Xsubc)/12.0D0
      di(2,3) = di(1,3)*(Xsubb+3.0D0*Xsubc)/5.0D0
      di(2,4) = di(1,4)*(Xsubb+4.0D0*Xsubc)/6.0D0
      di(3,1) = (xb2+xbc+xc2)/6.0D0
      di(3,2) = di(1,2)*(xb2+2.0D0*xbc+3.0D0*xc2)/10.0D0
      di(3,3) = di(1,3)*(xb2+3.0D0*xbc+6.0D0*xc2)/15.0D0
      di(4,1) = (Xsubb+Xsubc)*(xb2+xc2)/10.0D0
      di(4,2) = di(1,2)*((Xsubb+2.0D0*Xsubc)*xb2+(3.0D0*Xsubb+4.0D0*Xsubc)*xc2)/20.0D0
      di(5,1) = (xb2*xb2+xb2*xbc+xbc*xbc+xbc*xc2+xc2*xc2)/15.0
!
      Ar = Xsubb*Ysubc*dble(Ecpt(Iopt+56))/2.0D0
      DO i = 1 , 5
         ic = 6 - i
         DO j = 1 , ic
            di(i,j) = di(i,j)*Ar
         ENDDO
      ENDDO
!
!     THE ABOVE INTEGRALS  D(I,J) CORRESPOND TO THE DOCUMENTED
!     VALUES  I(I-1,J-1).  ZERO INDICES DONT ALWAYS COMPILE.
!
!     THE DIFFERENTIAL STIFFNESS MATRIX IN GENERALIZED COORDINATES IS
!     CREATED BELOW AT POSITIONS A(28) TO A(91)
!
      a(28) = Sx*di(1,1)
      a(29) = Sxy*di(1,1)
      a(30) = 2.0D0*Sx*di(2,1)
      a(31) = Sx*di(1,2) + Sxy*di(2,1)
      a(32) = 2.0D0*Sxy*di(1,2)
      a(33) = 3.0D0*Sx*di(3,1)
      a(34) = Sx*di(1,3) + 2.0*Sxy*di(2,2)
      a(35) = 3.0D0*Sxy*di(1,3)
!
      a(37) = Sy*di(1,1)
      a(38) = 2.0D0*Sxy*di(2,1)
      a(39) = Sxy*di(1,2) + Sy*di(2,1)
      a(40) = 2.0D0*Sy*di(1,2)
      a(41) = 3.0D0*Sxy*di(3,1)
      a(42) = Sxy*di(1,3) + 2.0D0*Sy*di(2,2)
      a(43) = 3.0D0*Sy*di(1,3)
!
      a(46) = 4.0D0*Sx*di(3,1)
      a(47) = 2.0D0*(Sx*di(2,2)+Sxy*di(3,1))
      a(48) = 4.0D0*Sxy*di(2,2)
      a(49) = 6.0D0*Sx*di(4,1)
      a(50) = 2.0D0*(Sx*di(2,3)+2.0D0*Sxy*di(3,2))
      a(51) = 6.0D0*Sxy*di(2,3)
!
      a(55) = Sx*di(1,3) + 2.0D0*Sxy*di(2,2) + Sy*di(3,1)
      a(56) = 2.0D0*(Sxy*di(1,3)+Sy*di(2,2))
      a(57) = 3.0D0*(Sx*di(3,2)+Sxy*di(4,1))
      a(58) = Sx*di(1,4) + 3.0D0*Sxy*di(2,3) + 2.0D0*Sy*di(3,2)
      a(59) = 3.0D0*(Sxy*di(1,4)+Sy*di(2,3))
!
      a(64) = 4.0D0*Sy*di(1,3)
      a(65) = 6.0D0*Sxy*di(3,2)
      a(66) = 2.0D0*(Sxy*di(1,4)+2.0D0*Sy*di(2,3))
      a(67) = 6.0D0*Sy*di(1,4)
!
      a(73) = 9.0D0*Sx*di(5,1)
      a(74) = 3.0D0*(Sx*di(3,3)+2.0D0*Sxy*di(4,2))
      a(75) = 9.0D0*Sxy*di(3,3)
!
      a(82) = Sx*di(1,5) + 4.0D0*Sxy*di(2,4) + 4.0D0*Sy*di(3,3)
      a(83) = 3.0D0*Sxy*di(1,5) + 6.0D0*Sy*di(2,4)
!
      a(91) = 9.0D0*Sy*di(1,5)
!
!     FILL IN SYMMETRIC TERMS
!
      DO i = 2 , 8
         ih = i - 1
         DO j = 1 , ih
            ic = 8*(i-1) + j
            jc = 8*(j-1) + i
            a(ic+27) = a(jc+27)
         ENDDO
      ENDDO
!
!     AT THIS STAGE THE 3X3 MATRIX PARTITIONS MAY BE GENERATED
!     THE ACTUAL MATRICES DEPEND ON IOPT
!
      ic = Npivot
      IF ( ic/=0 ) THEN
         CALL gmmatd(C(1,ic),8,3,1,a(28),8,8,0,a(92))
         DO i = 1 , 3
            ih = 9*(i-1) + 1
            CALL gmmatd(a(92),3,8,0,C(1,i),8,3,0,a(ih))
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
      CALL gmmatd(a(28),8,8,0,C(1,3),8,3,0,a(92))
      DO i = 1 , 3
         ih = 28 + 9*(i-1)
         CALL gmmatd(C(1,i),8,3,1,a(92),8,3,0,a(ih))
      ENDDO
!
!     RECALCULATE THE S MATRIX (IT WAS DESTROYED) -
!     PLACE IN A(55 THRU 72)
!
      DO i = 1 , 18
         a(i+54) = 0.0
      ENDDO
      DO i = 1 , 9 , 4
         a(i+54) = 1.0
         a(i+63) = 1.0
      ENDDO
      a(57) = -Xsubb
      a(65) = Ysubc
      a(66) = -Xsubc
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
            a(jh) = C(jc,2)
            a(jh+3) = C(jc,3)
         ENDDO
      ENDDO
   END SUBROUTINE spag_block_1
END SUBROUTINE dtrbsc
