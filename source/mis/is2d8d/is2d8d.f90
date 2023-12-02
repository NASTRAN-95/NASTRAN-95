!*==is2d8d.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE is2d8d
USE C_BLANK
USE C_EMGDIC
USE C_EMGEST
USE C_EMGPRM
USE C_HMTOUT
USE C_MATIN
USE C_MATOUT
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: area , tth , vecil , veckl
   REAL(REAL64) , DIMENSION(12) :: b , bt
   INTEGER , DIMENSION(2) , SAVE :: bcd , nam
   REAL(REAL64) :: bij , dc , determ , dhh , drho , dumarg , kij , mij , term , thick
   INTEGER , DIMENSION(13) :: dict
   REAL(REAL64) , DIMENSION(8) :: dn
   REAL(REAL64) , DIMENSION(16) :: dnc , dnl , xx
   REAL(REAL64) , DIMENSION(1) :: dneta , dnx , dnxi , dny
   REAL(REAL64) , DIMENSION(6) :: e1t , premul
   REAL , DIMENSION(46) :: ecpt
   REAL(REAL64) , DIMENSION(8) , SAVE :: eta , xi
   REAL(REAL64) , DIMENSION(9) :: g , pstmul , tb , temp , temp1 , temp2 , temp3
   REAL(REAL64) , DIMENSION(3) :: h , pt
   INTEGER :: i , ic , iii , ising , isub , isub1 , ixx , j , jjj , k , kk , l , ll , m , n , ncol , nrow , nsq
   INTEGER , DIMENSION(36) , SAVE :: ind6
   INTEGER , DIMENSION(8) :: isil
   INTEGER , DIMENSION(2,3) :: iws
   INTEGER , DIMENSION(1) :: iz
   REAL , DIMENSION(15) :: qq
   REAL(REAL64) , DIMENSION(144) :: save
   REAL(REAL64) , DIMENSION(36) :: savm
   INTEGER , SAVE :: scr4
   REAL(REAL64) , DIMENSION(7) :: tempar
   REAL(REAL64) , DIMENSION(216) :: tsave
   REAL , DIMENSION(3) :: vec , veci , vecj , veck , vvec , xy1 , xy2
   REAL(REAL64) , DIMENSION(4) :: xjb
   REAL(REAL64) , DIMENSION(2,2) :: xxjb
   EXTERNAL emgout , gmmatd , hmat , insert , inverd , mat , mesage , transd , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     2-D, 8 GRID POINT ISOPARAMETRIC STRUCTURAL  ELEMENT STIFFNESS,
!     MASS, CONDUCTIVITY, AND CAPACITANCE ROUTINE
!
!     DOUBLE PRECISION VERSION
!
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1)) , (Z(1),Iz(1)) , (temp(1),b(1)) , (dnc(1),dnxi(1)) , (dnc(9),dneta(1)) , (dnl(1),dnx(1)) ,        &
!>>>>    & (dnl(9),dny(1)) , (Qq(1),G11) , (tempar(1),bt(1)) , (Xy1(1),X1) , (Xy2(1),X2) , (Isil(1),Ngrid(1))
   DATA xi/ - 1.D0 , 1.D0 , 1.D0 , -1.D0 , 0.D0 , 1.D0 , 0.D0 , -1.D0/
   DATA eta/ - 1.D0 , -1.D0 , 1.D0 , 1.D0 , -1.D0 , 0.D0 , 1.D0 , 0.D0/
   DATA ind6/1 , 7 , 49 , 13 , 55 , 91 , 19 , 61 , 97 , 127 , 25 , 67 , 103 , 133 , 157 , 31 , 73 , 109 , 139 , 163 , 181 , 37 ,    &
      & 79 , 115 , 145 , 169 , 187 , 199 , 43 , 85 , 121 , 151 , 175 , 193 , 205 , 211/
   DATA nam , bcd/4HIS2D , 4H8D   , 4HCIS2 , 4HD8  /
   DATA scr4/304/
!
!     ECPT LIST
!                                      IN
!                                      THIS
!     ECPT       DESCRIPTION           ROUTINE        TYPE
!     ******************************************************************
!     ECPT( 1) = ELEMENT ID            NECPT(1)       INTEGER
!     ECPT( 2) = GRID POINT 1          NGRID(1)       INTEGER
!     ECPT( 3) = GRID POINT 2          NGRID(2)       INTEGER
!     ECPT( 4) = GRID POINT 3          NGRID(3)       INTEGER
!     ECPT( 5) = GRID POINT 4          NGRID(4)       INTEGER
!     ECPT( 6) = GRID POINT 5          NGRID(5)       INTEGER
!     ECPT( 7) = GRID POINT 6          NGRID(6)       INTEGER
!     ECPT( 8) = GRID POINT 7          NGRID(7)       INTEGER
!     ECPT( 9) = GRID POINT 8          NGRID(8)       INTEGER
!     ECPT(10) = NO. OF GAUSS POINTS   ID1            INTEGER
!     ECPT(11) = ANIS. MATERIAL ANGLE  TH             REAL
!     ECPT(12) = MATERIAL ID           MATID1         INTEGER
!     ECPT(13) = THICKNESS             T              REAL
!     ECPT(14) = COORD SYS ID 1        ISYS1          INTEGER
!     ECPT(15) = X1                    X1             REAL
!     ECPT(16) = Y1                    Y1             REAL
!     ECPT(17) = Z1                    Z1             REAL
!     ECPT(18) = COORD SYS ID 2        ISYS2          INTEGER
!     ECPT(19) = X2                    X2             REAL
!     ECPT(20) = Y2                    Y2             REAL
!     ECPT(21) = Z2                    Z2             REAL
!     ECPT(22) = COORD SYS ID 3        ISYS3          INTEGER
!     ECPT(23) = X3                    X3             REAL
!     ECPT(24) = Y3                    Y3             REAL
!     ECPT(25) = Z3                    Z3             REAL
!     ECPT(26) = COORD SYS ID 4        ISYS4          INTEGER
!     ECPT(27) = X4                    X4             REAL
!     ECPT(28) = Y4                    Y4             REAL
!     ECPT(29) = Z4                    Z4             REAL
!     ECPT(30) = COORD SYS ID 5        ISYS5          INTEGER
!     ECPT(31) = X5                    X5             REAL
!     ECPT(32) = Y5                    Y5             REAL
!     ECPT(33) = Z5                    Z5             REAL
!     ECPT(34) = COORD SYS ID 6        ISYS6          INTEGER
!     ECPT(35) = X6                    XL             REAL
!     ECPT(36) = Y6                    Y6             REAL
!     ECPT(37) = Z6                    Z6             REAL
!     ECPT(38) = COORD SYS ID 7        ISYS7          INTEGER
!     ECPT(39) = X7                    X7             REAL
!     ECPT(40) = Y7                    Y7             REAL
!     ECPT(41) = Z7                    Z7             REAL
!     ECPT(42) = COORD SYS ID 8        ISYS8          INTEGER
!     ECPT(43) = X8                    X8             REAL
!     ECPT(44) = Y8                    Y8             REAL
!     ECPT(45) = Z8                    Z8             REAL
!     ECPT(46) = ELEMENT TEMP          TTEMP          REAL
!
   IF ( Jcore+576>Ncore ) CALL mesage(-8,0,nam)
   dict(1) = Iestid
   dict(2) = 1
   IF ( Heat ) THEN
      dict(3) = 8
      dict(4) = 1
      nsq = 64
   ELSE
      dict(3) = 24
      dict(4) = 7
      nsq = 576
   ENDIF
!
!     SAVE NGRID IN DUMB
!
   DO i = 1 , 9
      Dumb(i) = ecpt(i)
   ENDDO
   area = 0.0
!
!     SET UP SIL ARRAY SO THAT MATRICES ARE SET UP IN INCREASING SIL
!     ORDER SIL(I)=PARTITION NUMBER OF ITH GRID POINT
!
   i = -8
   SPAG_Loop_1_1: DO
      j = 0
      DO k = 1 , 8
         IF ( isil(k)>=j ) THEN
            j = isil(k)
            l = k
         ENDIF
      ENDDO
      isil(l) = i
      i = i + 1
      IF ( i>=0 ) THEN
         DO i = 1 , 8
            isil(i) = -isil(i)
         ENDDO
!
         DO i = 1 , nsq
            Z(Jcore+i) = 0.0D0
         ENDDO
!
!     UNIT I VECTOR IS FROM GRID POINT 1 TO GRID POINT 2
!
         DO i = 1 , 3
            veci(i) = xy2(i) - xy1(i)
         ENDDO
         vecil = sqrt(veci(1)**2+veci(2)**2+veci(3)**2)
         IF ( vecil/=0.0 ) THEN
            veci(1) = veci(1)/vecil
            veci(2) = veci(2)/vecil
            veci(3) = veci(3)/vecil
!
!     K VECTOR IS OBTAINED BY CROSSING I INTO VECTOR FROM GRID PT. 1 TO
!     GRID
!
            veck(1) = veci(2)*(Z4-Z1) - veci(3)*(Y4-Y1)
            veck(2) = veci(3)*(X4-X1) - veci(1)*(Z4-Z1)
            veck(3) = veci(1)*(Y4-Y1) - veci(2)*(X4-X1)
            veckl = sqrt(veck(1)**2+veck(2)**2+veck(3)**2)
            IF ( veckl/=0.0 ) THEN
               veck(1) = veck(1)/veckl
               veck(2) = veck(2)/veckl
               veck(3) = veck(3)/veckl
!
!     J VECTOR IS OBTAINED BY CROSSING K INTO I
!
               vecj(1) = veck(2)*veci(3) - veck(3)*veci(2)
               vecj(2) = veck(3)*veci(1) - veck(1)*veci(3)
               vecj(3) = veck(1)*veci(2) - veck(2)*veci(1)
!
               e1t(1) = veci(1)
               e1t(2) = veci(2)
               e1t(3) = veci(3)
               e1t(4) = vecj(1)
               e1t(5) = vecj(2)
               e1t(6) = vecj(3)
!
!     STORE ELEMENT COORDS FOR GRIDS 1 AND 2
!
               xx(1) = 0.D0
               xx(2) = 0.D0
               xx(3) = vecil
               xx(4) = 0.D0
!
!     FOR GRIDS 3-8, THE X COORDINATE IS THE DOT PRODUCT OF HTE VECTOR
!     FROM GRID POINT 1 TO THE GRID POINT AND THE I VECTOR. THE Y COORD.
!     IS THE L OF THE I VECTOR CROSSED INTO THE VECTOR FROM GRID 1 TO
!     THE GRID POINT.
!
               DO i = 3 , 8
                  ixx = 2*i - 1
                  isub = 4*i + 11
                  vec(1) = ecpt(isub) - X1
                  vec(2) = ecpt(isub+1) - Y1
                  vec(3) = ecpt(isub+2) - Z1
                  xx(ixx) = vec(1)*veci(1) + vec(2)*veci(2) + vec(3)*veci(3)
                  vvec(1) = veci(2)*vec(3) - veci(3)*vec(2)
                  vvec(2) = veci(3)*vec(1) - veci(1)*vec(3)
                  vvec(3) = veci(1)*vec(2) - veci(2)*vec(1)
                  xx(ixx+1) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
               ENDDO
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDIF
!
!     INAPPROPRIATE GEOMETRY
!
         CALL mesage(30,31,ecpt(1))
         Error = .TRUE.
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
!
   IF ( Error ) RETURN
!
!     SET UP QUADRATURE POINTS AND WEIGHTS
!
   pt(1) = -0.57735027D0
   pt(2) = -pt(1)
   h(1) = 1.D0
   h(2) = 1.D0
   IF ( Id1/=2 ) THEN
      pt(1) = -0.77459667D0
      pt(2) = 0.D0
      pt(3) = -pt(1)
      h(1) = 5.D0/9.D0
      h(2) = 8.D0/9.D0
      h(3) = h(1)
   ENDIF
!
   IF ( Heat ) THEN
!
!     HEAT FORMULATION
!
!     COMPUTE MATERIAL PROPERTIES
!
      Sinth = 0.
      Costh = 0.
      Eltemp = Ttemp
      Inflag = 2
      Matid = Matid1
      CALL hmat(ecpt(1))
      thick = T
      dc = C*T
!
!     ZERO OUT THE SAVE MATRIX
!
      DO i = 1 , 36
         savm(i) = 0.D0
         save(i) = 0.D0
      ENDDO
!
      DO iii = 1 , Id1
         DO jjj = 1 , Id1
!
!     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
!     EACH GRID POINT
!
            DO n = 1 , 4
               IF ( Kmbgg(3)/=0 ) dn(n) = .25D0*(1.D0+pt(iii)*xi(n))*(1.D0+pt(jjj)*eta(n))*(pt(iii)*xi(n)+pt(jjj)*eta(n)-1.D0)
               dnxi(n) = .25D0*xi(n)*(1.D0+pt(jjj)*eta(n))*(2.D0*pt(iii)*xi(n)+pt(jjj)*eta(n))
               dneta(n) = .25D0*eta(n)*(1.D0+pt(iii)*xi(n))*(pt(iii)*xi(n)+2.D0*pt(jjj)*eta(n))
            ENDDO
!
            DO n = 5 , 7 , 2
               IF ( Kmbgg(3)/=0 ) dn(n) = .5D0*(1.D0-pt(iii)*pt(iii))*(1.D0+pt(jjj)*eta(n))
               dnxi(n) = -pt(iii)*(1.D0+pt(jjj)*eta(n))
               dneta(n) = .5D0*(1.D0-pt(iii)*pt(iii))*eta(n)
            ENDDO
!
            DO n = 6 , 8 , 2
               IF ( Kmbgg(3)/=0 ) dn(n) = .5D0*(1.D0+pt(iii)*xi(n))*(1.D0-pt(jjj)*pt(jjj))
               dnxi(n) = .5D0*xi(n)*(1.D0-pt(jjj)*pt(jjj))
               dneta(n) = -pt(jjj)*(1.D0+pt(iii)*xi(n))
            ENDDO
!
!     COMPUTE JACOBEAN
!
!           N1XI   N2XI   N3XI   N4XI   N5XI   N6XI   N7XI   N8XI
!     DNC = N1ETA  N2ETA  N3ETA  N4ETA  N5ETA  N6ETA  N7ETA  N8ETA
!
!          X1  Y1
!          X2  Y2
!          X3  Y3
!     XX = X4  Y4
!          X5  Y5
!          X6  Y6
!          X7  Y7
!          X8  Y8
!
            CALL gmmatd(dnc,2,8,0,xx,8,2,0,xjb)
!
!     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
!     FOR INVERSION
!
            k = 0
            DO i = 1 , 2
               DO j = 1 , 2
                  k = k + 1
                  xxjb(i,j) = xjb(k)
               ENDDO
            ENDDO
!
!     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
!
            CALL inverd(2,xxjb,2,dumarg,0,determ,ising,iws)
            IF ( ising/=2 ) THEN
!
               dhh = determ*h(iii)*h(jjj)
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X,Y,AND Z
!
               k = 0
               DO i = 1 , 2
                  DO j = 1 , 2
                     k = k + 1
                     xjb(k) = xxjb(i,j)
                  ENDDO
               ENDDO
               CALL gmmatd(xjb,2,2,0,dnc,2,8,0,dnl)
!
!           N1X N2X N3X N4X N5X N6X N7X N8X
!     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
!
!     SET UP THE BT MATRIX
!
               ic = 0
               DO kk = 1 , 8
                  IF ( Kmbgg(1)/=0 ) THEN
                     bt(1) = Kx*dnx(kk) + Kxy*dny(kk)
                     bt(2) = Kxy*dnx(kk) + Ky*dny(kk)
                  ENDIF
!
!     DO NOT TRANSFORM FROM MATERIAL COORD SYSTEM TO BASIC AND GLOBAL
!     SINCE THIS IS A SCALAR PROBLEM
!
                  DO n = kk , 8
                     ic = ic + 1
!
!     SET UP THE B MATRIX
!
                     IF ( Kmbgg(1)/=0 ) THEN
                        b(1) = dnx(n)
                        b(2) = dny(n)
!
!     O.K. NOW PERFORM FINAL MULTIPLICATION
!
                        kij = bt(1)*b(1) + bt(2)*b(2)
!
!     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
!
                        kij = kij*dhh
!
!     ADD THE RESULTS OF THIS INTEGRATION TO PREVIOUS RESULTS
!
                        save(ic) = save(ic) + kij
                     ENDIF
                     IF ( Kmbgg(3)/=0 ) THEN
                        bij = dn(kk)*dn(n)*dhh
                        savm(ic) = savm(ic) + bij
                     ENDIF
!
!     LOOP FOR MORE PARTITIONS
!
                  ENDDO
               ENDDO
            ELSE
               CALL mesage(30,143,ecpt(1))
               Error = .TRUE.
               RETURN
            ENDIF
!
!     LOOP FOR ADDITIONAL GAUSS POINTS
!
         ENDDO
      ENDDO
!
      DO i = 1 , 36
         IF ( Kmbgg(1)/=0 ) save(i) = save(i)*thick
         IF ( Kmbgg(3)/=0 ) savm(i) = savm(i)*dc
      ENDDO
!
!     INSERT INTO OVERALL STIFFNESS MATRIX
!
      ic = 0
      DO i = 1 , 8
         DO j = i , 8
            ic = ic + 1
            nrow = isil(i)
            ncol = isil(j)
            IF ( Kmbgg(1)/=0 ) CALL insert(ncol,nrow,1,8,Jcore,Z,Z,save(ic),save(ic),Iprec)
            IF ( Kmbgg(3)/=0 ) CALL insert(ncol,nrow,1,8,Jcore+64,Z,Z,savm(ic),savm(ic),Iprec)
         ENDDO
      ENDDO
!
      IF ( Kmbgg(1)/=0 ) CALL emgout(Z(Jcore),Z(Jcore),nsq,1,dict,1,Iprec)
      IF ( Kmbgg(3)/=0 ) CALL emgout(Z(Jcore+64),Z(Jcore+64),nsq,1,dict,3,Iprec)
   ELSE
!
!     COMPUTE MATERIAL PROPERTIES
!
      tth = Th*3.1415927/180.
      Sinth = sin(tth)
      Costh = cos(tth)
      Eltemp = Ttemp
      Inflag = 2
      Matid = Matid1
      CALL mat(ecpt(1))
      DO i = 1 , 3
         g(i) = qq(i)
      ENDDO
      g(4) = qq(2)
      g(5) = qq(4)
      g(6) = qq(5)
      g(7) = qq(3)
      g(8) = qq(5)
      g(9) = qq(6)
      thick = T
      drho = Rho*T
!
!     ZERO THE SAVE MATRICES TO COLLECT INTEGRATIONS
!
      DO i = 1 , 36
         savm(i) = 0.D0
      ENDDO
      DO i = 1 , 144
         save(i) = 0.D0
      ENDDO
!
!     2 OR 3 QUADRATURE POINTS
!
      DO iii = 1 , Id1
         DO jjj = 1 , Id1
!
!     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
!     EACH GRID POINT
!
            DO n = 1 , 4
               IF ( Kmbgg(2)/=0 ) dn(n) = .25D0*(1.D0+pt(iii)*xi(n))*(1.D0+pt(jjj)*eta(n))*(pt(iii)*xi(n)+pt(jjj)*eta(n)-1.D0)
               dnxi(n) = .25D0*xi(n)*(1.D0+pt(jjj)*eta(n))*(2.D0*pt(iii)*xi(n)+pt(jjj)*eta(n))
               dneta(n) = .25D0*eta(n)*(1.D0+pt(iii)*xi(n))*(pt(iii)*xi(n)+2.D0*pt(jjj)*eta(n))
            ENDDO
!
            DO n = 5 , 7 , 2
               IF ( Kmbgg(2)/=0 ) dn(n) = .5D0*(1.D0-pt(iii)*pt(iii))*(1.D0+pt(jjj)*eta(n))
               dnxi(n) = -pt(iii)*(1.D0+pt(jjj)*eta(n))
               dneta(n) = .5D0*(1.D0-pt(iii)*pt(iii))*eta(n)
            ENDDO
!
            DO n = 6 , 8 , 2
               IF ( Kmbgg(2)/=0 ) dn(n) = .5D0*(1.D0+pt(iii)*xi(n))*(1.D0-pt(jjj)*pt(jjj))
               dnxi(n) = .5D0*xi(n)*(1.D0-pt(jjj)*pt(jjj))
               dneta(n) = -pt(jjj)*(1.D0+pt(iii)*xi(n))
            ENDDO
!
!     COMPUTE JACOBEAN
!
!           N1XI   N2XI   N3XI   N4XI   N5XI   N6XI   N7XI   N8XI
!     DNC = N1ETA  N2ETA  N3ETA  N4ETA  N5ETA  N6ETA  N7ETA  N8ETA
!
!          X1  Y1
!          X2  Y2
!          X3  Y3
!     XX = X4  Y4
!          X5  Y5
!          X6  Y6
!          X7  Y7
!          X8  Y8
!
            CALL gmmatd(dnc,2,8,0,xx,8,2,0,xjb)
!
!     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
!     FOR INVERSION
!
            k = 0
            DO i = 1 , 2
               DO j = 1 , 2
                  k = k + 1
                  xxjb(i,j) = xjb(k)
               ENDDO
            ENDDO
!
!     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
!
            CALL inverd(2,xxjb,2,dumarg,0,determ,ising,iws)
            IF ( ising/=2 ) THEN
!
               dhh = determ*h(iii)*h(jjj)
               area = area + dhh
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y
!
               k = 0
               DO i = 1 , 2
                  DO j = 1 , 2
                     k = k + 1
                     xjb(k) = xxjb(i,j)
                  ENDDO
               ENDDO
               CALL gmmatd(xjb,2,2,0,dnc,2,8,0,dnl)
!
!           N1X N2X N3X N4X N5X N6X N7X N8X
!     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
!
!     SET UP THE BT MATRIX
!
               ic = 0
               DO kk = 1 , 8
                  IF ( Kmbgg(1)/=0 ) THEN
!
                     DO i = 1 , 12
                        bt(i) = 0.D0
                     ENDDO
                     bt(1) = dnx(kk)
                     bt(3) = dny(kk)
                     bt(5) = dny(kk)
                     bt(6) = dnx(kk)
!
                     CALL gmmatd(tempar(1),2,3,0,g,3,3,0,tempar(7))
                  ENDIF
!
!     MULTIPLY G MATRIX BY PRESENT RESULTS
!
!     LOOP FOR THE 8 6X6 PARTITIONS CORRESPONDING TO THE PRESENT
!     PIVOT POINT
!
                  DO n = kk , 8
                     ic = ic + 1
                     IF ( Kmbgg(1)/=0 ) THEN
!
!     SET UP THE B MATRIX
!
                        DO i = 1 , 12
                           b(i) = 0.D0
                        ENDDO
                        b(1) = dnx(n)
                        b(4) = dny(n)
                        b(5) = dny(n)
                        b(6) = dnx(n)
!                                    T
!     PERFORM MULTIPLICATION TO GET B *D*B
!
                        CALL gmmatd(tempar(7),2,3,0,b,3,2,0,tempar(1))
!
!     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
!
                        DO i = 1 , 4
                           tempar(i) = tempar(i)*dhh
                        ENDDO
!
!     ADD THE RESULTS OF THIS INTEGRATION TO THE PREVIOUS RESULTS
!
                        ll = 4*(ic-1)
                        DO i = 1 , 4
                           l = ll + i
                           save(l) = save(l) + tempar(i)
                        ENDDO
                     ENDIF
!
                     IF ( Kmbgg(2)/=0 ) THEN
!
                        mij = dn(kk)*dn(n)*dhh
                        savm(ic) = savm(ic) + mij
                     ENDIF
!
!     LOOP FOR MORE PARTITIONS
!
                  ENDDO
               ENDDO
            ELSE
               CALL mesage(30,143,ecpt(1))
               Error = .TRUE.
               RETURN
            ENDIF
!
!     LOOP FOR MORE GAUSS POINTS
!
         ENDDO
      ENDDO
      IF ( Kmbgg(2)/=0 ) THEN
         DO i = 1 , 36
            savm(i) = savm(i)*drho
         ENDDO
      ENDIF
!
!     CHECK ON NECESSITY OF PRE-MULTIPLYING COORDINATE TRANSFORMATIONS
!
      IF ( Kmbgg(1)/=0 ) THEN
         ic = 0
         DO kk = 1 , 8
            isub = 4*kk + 10
            IF ( Necpt(isub)==0 ) THEN
               DO i = 1 , 6
                  premul(i) = e1t(i)
               ENDDO
            ELSE
!
!     ELEMENT TO GLOBAL
!
               CALL transd(Necpt(isub),tb)
               CALL gmmatd(e1t,2,3,0,tb,3,3,0,premul)
            ENDIF
            DO n = kk , 8
               ic = ic + 1
               ll = 4*ic - 3
               CALL gmmatd(premul,2,3,1,save(ll),2,2,0,temp)
!
!     STORE THE 3 X 2 IN TSAVE
!
               DO i = 1 , 6
                  l = 6*ic + i - 6
                  tsave(l) = temp(i)
               ENDDO
!
            ENDDO
         ENDDO
!
!     NOW CHECK ON THE NECESSITY FOR POST-MULTIPLYING TRANSFORMATIONS
!
         ic = 0
         DO n = 1 , 8
            isub = 4*n + 10
            IF ( Necpt(isub)==0 ) THEN
               DO i = 1 , 6
                  pstmul(i) = e1t(i)
               ENDDO
            ELSE
!
!     GLOBAL TO ELEMENT
!
               CALL transd(Necpt(isub),tb)
               CALL gmmatd(e1t,2,3,0,tb,3,3,0,pstmul)
            ENDIF
!
!     POST-MULTIPLY
!
!     IND6 GIVES STARTING POSITIONS OF VERTICAL 3X3 PARTITIONS, SINCE
!     THE NTH COLUMN MULTIPLIES INTO THE NTH POST-MULTIPLIER
!
            DO m = 1 , n
               ic = ic + 1
               ll = ind6(ic)
               CALL gmmatd(tsave(ll),3,2,0,pstmul,2,3,0,temp)
               DO i = 1 , 9
                  temp(i) = temp(i)*thick
               ENDDO
!
!     PICK UP ROW AND COLUMN PARTITION NUMBERS AND CONVERT TO STARTING
!     POINTS IN OPEN CORE FOR THIS PARTITION  AND ITS TRANSPOSE.
!     TEMP IS PUT INTO ONE PARTITION AND TEMP-TRANSPOSE INTO THE OTHER
!
               ncol = isil(n)
               nrow = isil(m)
               CALL insert(ncol,nrow,3,8,Jcore,Z,Z,temp,temp,Iprec)
!
!     LOOP FOR ANOTHER PARTITION FOR THIS POST-MULTIPLIER
!
            ENDDO
!
!     LOOP FOR ANOTHER POST-MULTIPLIER
!
         ENDDO
!
!     ADD TO DICTIONARY
!
         dict(5) = Ge
         CALL emgout(Z(Jcore),Z(Jcore),nsq,1,dict,1,Iprec)
      ENDIF
!
      IF ( Kmbgg(2)/=0 ) THEN
!
         ic = 0
         DO kk = 1 , 8
            DO n = kk , 8
               spag_nextblock_1 = 1
               SPAG_DispatchLoop_1: DO
                  SELECT CASE (spag_nextblock_1)
                  CASE (1)
                     ic = ic + 1
                     DO i = 1 , 9
                        temp(i) = 0.D0
                     ENDDO
!
!     CHECK ON TEH NECESSITY OF COORDINATE TRANSFORMATIONS.
!     SINCE EACH PARTITION IS A MULTIPLE OF A 3X3 IDENTITY AND SINCE
!     THE TRANSFORAMATION MATRICES ARE ORTHOGONAL, NO EXPLICIT
!     TRANSFORMA-TIONS FROM THE ELEMENT COORDINATE SYSTEM ARE REQUIRED.
!     ALSO, NO TRANSFORAMTION IS REQUIRED IF TRANSFORMATION MATRICES ARE
!     THE SAME FOR THE GRIDS CORRESPONDING TO THE THE ROW AND COLUMN
!
                     term = savm(ic)
                     IF ( kk/=n ) THEN
                        isub = 4*kk + 10
                        isub1 = 4*n + 10
                        IF ( Necpt(isub)/=0 .OR. Necpt(isub1)/=0 ) THEN
                           IF ( Necpt(isub)/=0 ) THEN
                              CALL transd(Necpt(isub),temp1)
                              IF ( Necpt(isub1)==0 ) THEN
                                 temp3(1) = temp1(1)
                                 temp3(2) = temp1(4)
                                 temp3(3) = temp1(7)
                                 temp3(4) = temp1(2)
                                 temp3(5) = temp1(5)
                                 temp3(6) = temp1(8)
                                 temp3(7) = temp1(3)
                                 temp3(8) = temp1(6)
                                 temp3(9) = temp1(9)
                                 GOTO 2
                              ENDIF
                           ENDIF
                           CALL transd(Necpt(isub1),temp2)
                           IF ( Necpt(isub)==0 ) THEN
                              DO i = 1 , 9
                                 temp3(i) = temp2(i)
                              ENDDO
                           ELSE
!
!     MULTIPLY THE TRANSFORMATION MATRICES
!
                              CALL gmmatd(temp1,3,3,1,temp2,3,3,0,temp3)
                           ENDIF
!
 2                         DO i = 1 , 9
                              temp(i) = term*temp3(i)
                           ENDDO
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                     temp(1) = term
                     temp(5) = term
                     temp(9) = term
                     spag_nextblock_1 = 2
                  CASE (2)
!
                     nrow = isil(kk)
                     ncol = isil(n)
                     CALL insert(ncol,nrow,3,8,Jcore,Z,Z,temp,temp,Iprec)
                     EXIT SPAG_DispatchLoop_1
                  END SELECT
               ENDDO SPAG_DispatchLoop_1
            ENDDO
         ENDDO
!
         CALL emgout(Z(Jcore),Z(Jcore),nsq,1,dict,2,Iprec)
      ENDIF
!
!     SAVE ELEMENT NAME, ID, THICKNESS, DENSITY, NO. OF GRID POINTS,
!     GRID POINT DATA, AND AREA IF USER REQUESTED VOLUME AND AREA
!     COMPUTATION
!
      IF ( Volume>0.0 .OR. Surfac>0.0 ) THEN
         ecpt(2) = ecpt(13)
         ecpt(3) = Rho
         j = 4
         Necpt(j) = 8
         ecpt(46) = area
         CALL write(scr4,bcd,2,0)
         CALL write(scr4,ecpt(1),4,0)
         CALL write(scr4,Dumb(2),8,0)
         CALL write(scr4,ecpt(14),33,1)
      ENDIF
   ENDIF
END SUBROUTINE is2d8d
