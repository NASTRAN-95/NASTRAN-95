!*==ss2d81.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ss2d81
   USE c_matin
   USE c_matout
   USE c_sdr2x4
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(8) :: dneta , dnxi
   REAL , DIMENSION(6) :: e1t
   REAL , DIMENSION(1) :: ecpt
   REAL , DIMENSION(8) , SAVE :: eta , xi
   REAL , DIMENSION(9) :: g
   INTEGER :: i , iii , ising , isub , ixx , j , jjj , k , kk , l , n
   INTEGER , DIMENSION(2,3) :: iws
   INTEGER , DIMENSION(62) :: nph1
   REAL , DIMENSION(3) :: pt , vec , veci , vecj , veck , vvec , xy1 , xy2
   REAL , DIMENSION(15) :: qq
   REAL :: tth , vecil , veckl
   REAL , DIMENSION(16) :: xx
   EXTERNAL gmmats , invers , mat , mesage , transs
!
! End of declarations rewritten by SPAG
!
!
!     PHASE 1 OF STRESS DATA RECOVERY FOR 2-D, 8 GRID POINT
!     ISOPARAMETRIC STRUCTURAL ELEMENT
!
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1)) , (Nph1(1),Ph1out(1)) , (Xy1(1),X1) , (Xy2(1),X2) , (Dnc(1),Dnxi(1)) , (Dnc(9),Dneta(1)) ,        &
!>>>>    & (Qq(1),G11)
   DATA xi/ - 1. , 1. , 1. , -1. , 0. , 1. , 0. , -1./
   DATA eta/ - 1. , -1. , 1. , 1. , -1. , 0. , 1. , 0./
!
!     ECPT LIST
!                                      IN
!                                      THIS
!     ECPT       DESCRIPTION           ROUTINE        TYPE
!     --------   --------------------  ----------  -----------
!     ECPT( 1) = ELEMENT ID            NECPT(1)       INTEGER
!     ECPT( 2) = GRID POINT 1          NGRID(1)       INTEGER
!     ECPT( 3) = GRID POINT 2          NGRID(2)       INTEGER
!     ECPT( 4) = GRID POINT 3          NGRID(3)       INTEGER
!     ECPT( 5) = GRID POINT 4          NGRID(4)       INTEGER
!     ECPT( 6) = GRID POINT 5          NGRID(5)       INTEGER
!     ECPT( 7) = GRID POINT 6          NGRID(6)       INTEGER
!     ECPT( 8) = GRID POINT 7          NGRID(7)       INTEGER
!     ECPT( 9) = GRID POINT 8          NGRID(8)       INTEGER
!     ECPT(10) = COORD SYS ID-STRESS   ID1            INTEGER
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
!
!     UNIT I VECTOR IS FROM GRID POINT 1 TO GRID POINT 2
!
   DO i = 1 , 3
      veci(i) = xy2(i) - xy1(i)
   ENDDO
   vecil = sqrt(veci(1)**2+veci(2)**2+veci(3)**2)
   IF ( vecil==0.0 ) THEN
!
!     INAPPROPRIATE GEOMETRY
!
      CALL mesage(-30,31,ecpt(1))
   ELSE
      veci(1) = veci(1)/vecil
      veci(2) = veci(2)/vecil
      veci(3) = veci(3)/vecil
!
!     K VECTOR IS OBTAINED BY CROSSING I INTO VECTOR FROM GRID PT. 1 TO
!     GRID
!
      veck(1) = veci(2)*(z4-z1) - veci(3)*(y4-y1)
      veck(2) = veci(3)*(x4-x1) - veci(1)*(z4-z1)
      veck(3) = veci(1)*(y4-y1) - veci(2)*(x4-x1)
      veckl = sqrt(veck(1)**2+veck(2)**2+veck(3)**2)
      IF ( veckl==0.0 ) THEN
         CALL mesage(-30,31,ecpt(1))
      ELSE
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
         xx(1) = 0.
         xx(2) = 0.
         xx(3) = vecil
         xx(4) = 0.
!
!     FOR GRIDS 3-8, THE X COORDINATE IS THE DOT PRODUCT OF HTE VECTOR
!     FROM GRID POINT 1 TO THE GRID POINT AND THE I VECTOR. THE Y COORD.
!     IS THE L OF THE I VECTOR CROSSED INTO THE VECTOR FROM GRID 1 TO
!     THE GRID POINT.
!
         DO i = 3 , 8
            ixx = 2*i - 1
            isub = 4*i + 11
            vec(1) = ecpt(isub) - x1
            vec(2) = ecpt(isub+1) - y1
            vec(3) = ecpt(isub+2) - z1
            xx(ixx) = vec(1)*veci(1) + vec(2)*veci(2) + vec(3)*veci(3)
            vvec(1) = veci(2)*vec(3) - veci(3)*vec(2)
            vvec(2) = veci(3)*vec(1) - veci(1)*vec(3)
            vvec(3) = veci(1)*vec(2) - veci(2)*vec(1)
            xx(ixx+1) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
         ENDDO
      ENDIF
   ENDIF
!
!
!     COMPUTE MATERIAL PROPERTIES
!
   tth = th*3.1415927/180.
   sinth = sin(tth)
   costh = cos(tth)
   eltemp = ttemp
   inflag = 2
   matid = matid1
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
!
!     STORE G MATRIX IN PH1OUT
!
   DO i = 1 , 9
      ph1out(i+62) = g(i)
   ENDDO
!
!     COMPUTE AND STORE TRANSFORMATION MATRICES IF NECESSARY
!
   DO i = 1 , 8
      isub = 4*i + 10
      IF ( necpt(isub)==0 ) THEN
         DO j = 1 , 6
            tsave(j) = e1t(j)
         ENDDO
      ELSE
         CALL transs(necpt(isub),tb)
         CALL gmmats(e1t,2,3,0,tb,3,3,0,tsave)
      ENDIF
      k = 6*i + 7
      DO j = 1 , 6
         kk = k + j
         ph1out(kk) = tsave(j)
      ENDDO
   ENDDO
!
!     START MAJOR LOOP
!
   pt(1) = -0.57735027
   pt(2) = -pt(1)
   IF ( id1/=2 ) THEN
      pt(1) = -0.77459667
      pt(2) = 0.
      pt(3) = -pt(1)
   ENDIF
   l = 0
   DO iii = 1 , id1
      DO jjj = 1 , id1
         l = l + 1
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y EACH GRID POINT
!
         DO n = 1 , 4
            dnxi(n) = .25*xi(n)*(1.+pt(jjj)*eta(n))*(2.*pt(iii)*xi(n)+pt(jjj)*eta(n))
            dneta(n) = .25*eta(n)*(1.+pt(iii)*xi(n))*(pt(iii)*xi(n)+2.*pt(jjj)*eta(n))
         ENDDO
!
         DO n = 5 , 7 , 2
            dnxi(n) = -pt(iii)*(1.+pt(jjj)*eta(n))
            dneta(n) = .5*(1.-pt(iii)*pt(iii))*eta(n)
         ENDDO
!
         DO n = 6 , 8 , 2
            dnxi(n) = .5*xi(n)*(1.-pt(jjj)*pt(jjj))
            dneta(n) = -pt(jjj)*(1.+pt(iii)*xi(n))
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
         CALL gmmats(dnc,2,8,0,xx,8,2,0,xjb)
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
         CALL invers(2,xxjb,2,dumarg,0,determ,ising,iws)
         IF ( ising==2 ) CALL mesage(-30,143,ecpt(1))
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
         CALL gmmats(xjb,2,2,0,dnc,2,8,0,dnl)
!
!           N1X N2X N3X N4X N5X N6X N7X N8X
!     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
!
!
!     STORE DERIVATIVES IN PH1OUT
!
         k = 16*l + 55
         DO i = 1 , 16
            kk = k + i
            ph1out(kk) = dnl(i)
         ENDDO
!
!     LOOP FOR OTHER GRID POINTS
!
      ENDDO
   ENDDO
   ph1out(1) = ecpt(1)
   DO i = 1 , 8
      ph1out(i+1) = ecpt(i+1)
   ENDDO
   ph1out(10) = tref
!
!     COMPUTE VECTOR FOR THERMAL EXPANSION
!
   alphas(1) = alpha1
   alphas(2) = alpha2
   alphas(3) = alp12
!
   CALL gmmats(g,3,3,0,alphas,3,1,0,ph1out(11))
!
   nph1(62) = id1
!
END SUBROUTINE ss2d81
