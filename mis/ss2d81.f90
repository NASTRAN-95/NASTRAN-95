
SUBROUTINE ss2d81
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alp12 , Alpha1 , Alpha2 , Alphas(3) , C , Costh , Determ , Dnc(16) , Dneta(8) , Dnl(16) , Dnxi(8) , Dumarg , Dumb(54) ,     &
      & Ecpt(1) , Eltemp , G11 , G12 , G13 , G22 , G23 , G33 , Ge , Kx , Ky , Ph1out(400) , Qq(15) , Rho , Sinth , Stress , T ,     &
      & Tb(9) , Th , Tref , Tsave(6) , Ttemp , X1 , X2 , X3 , X4 , X5 , X6 , X7 , X8 , Xjb(4) , Xxjb(2,2) , Xy , Xy1(3) , Xy2(3) ,  &
      & Y1 , Y2 , Y3 , Y4 , Y5 , Y6 , Y7 , Y8 , Z1 , Z2 , Z3 , Z4 , Z5 , Z6 , Z7 , Z8
   INTEGER Icstm , Id1 , Idum(33) , Inflag , Isys1 , Isys2 , Isys3 , Isys4 , Isys5 , Isys6 , Isys7 , Isys8 , Matid , Matid1 ,       &
         & Ncstm , Necpt(1) , Ngrid(8) , Nph1(62)
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tref , Ge , Kx , Ky , C
   COMMON /sdr2x4/ Idum , Icstm , Ncstm
   COMMON /sdr2x5/ Necpt , Ngrid , Id1 , Th , Matid1 , T , Isys1 , X1 , Y1 , Z1 , Isys2 , X2 , Y2 , Z2 , Isys3 , X3 , Y3 , Z3 ,     &
                 & Isys4 , X4 , Y4 , Z4 , Isys5 , X5 , Y5 , Z5 , Isys6 , X6 , Y6 , Z6 , Isys7 , X7 , Y7 , Z7 , Isys8 , X8 , Y8 ,    &
                 & Z8 , Ttemp , Dumb , Ph1out
   COMMON /sdr2x6/ Dnc , Dnl , Xxjb , Xjb , Tb , Determ , Dumarg , Xy , Alphas , Tsave
!
! Local variable declarations
!
   REAL e1t(6) , eta(8) , g(9) , pt(3) , tth , vec(3) , veci(3) , vecil , vecj(3) , veck(3) , veckl , vvec(3) , xi(8) , xx(16)
   INTEGER i , iii , ising , isub , iws(2,3) , ixx , j , jjj , k , kk , l , n
!
! End of declarations
!
!
!     PHASE 1 OF STRESS DATA RECOVERY FOR 2-D, 8 GRID POINT
!     ISOPARAMETRIC STRUCTURAL ELEMENT
!
   EQUIVALENCE (Ecpt(1),Necpt(1)) , (Nph1(1),Ph1out(1)) , (Xy1(1),X1) , (Xy2(1),X2) , (Dnc(1),Dnxi(1)) , (Dnc(9),Dneta(1)) ,        &
    & (Qq(1),G11)
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
      veci(i) = Xy2(i) - Xy1(i)
   ENDDO
   vecil = sqrt(veci(1)**2+veci(2)**2+veci(3)**2)
   IF ( vecil==0.0 ) THEN
!
!     INAPPROPRIATE GEOMETRY
!
      CALL mesage(-30,31,Ecpt(1))
   ELSE
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
      IF ( veckl==0.0 ) THEN
         CALL mesage(-30,31,Ecpt(1))
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
            vec(1) = Ecpt(isub) - X1
            vec(2) = Ecpt(isub+1) - Y1
            vec(3) = Ecpt(isub+2) - Z1
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
   tth = Th*3.1415927/180.
   Sinth = sin(tth)
   Costh = cos(tth)
   Eltemp = Ttemp
   Inflag = 2
   Matid = Matid1
   CALL mat(Ecpt(1))
   DO i = 1 , 3
      g(i) = Qq(i)
   ENDDO
   g(4) = Qq(2)
   g(5) = Qq(4)
   g(6) = Qq(5)
   g(7) = Qq(3)
   g(8) = Qq(5)
   g(9) = Qq(6)
!
!     STORE G MATRIX IN PH1OUT
!
   DO i = 1 , 9
      Ph1out(i+62) = g(i)
   ENDDO
!
!     COMPUTE AND STORE TRANSFORMATION MATRICES IF NECESSARY
!
   DO i = 1 , 8
      isub = 4*i + 10
      IF ( Necpt(isub)==0 ) THEN
         DO j = 1 , 6
            Tsave(j) = e1t(j)
         ENDDO
      ELSE
         CALL transs(Necpt(isub),Tb)
         CALL gmmats(e1t,2,3,0,Tb,3,3,0,Tsave)
      ENDIF
      k = 6*i + 7
      DO j = 1 , 6
         kk = k + j
         Ph1out(kk) = Tsave(j)
      ENDDO
   ENDDO
!
!     START MAJOR LOOP
!
   pt(1) = -0.57735027
   pt(2) = -pt(1)
   IF ( Id1/=2 ) THEN
      pt(1) = -0.77459667
      pt(2) = 0.
      pt(3) = -pt(1)
   ENDIF
   l = 0
   DO iii = 1 , Id1
      DO jjj = 1 , Id1
         l = l + 1
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y EACH GRID POINT
!
         DO n = 1 , 4
            Dnxi(n) = .25*xi(n)*(1.+pt(jjj)*eta(n))*(2.*pt(iii)*xi(n)+pt(jjj)*eta(n))
            Dneta(n) = .25*eta(n)*(1.+pt(iii)*xi(n))*(pt(iii)*xi(n)+2.*pt(jjj)*eta(n))
         ENDDO
!
         DO n = 5 , 7 , 2
            Dnxi(n) = -pt(iii)*(1.+pt(jjj)*eta(n))
            Dneta(n) = .5*(1.-pt(iii)*pt(iii))*eta(n)
         ENDDO
!
         DO n = 6 , 8 , 2
            Dnxi(n) = .5*xi(n)*(1.-pt(jjj)*pt(jjj))
            Dneta(n) = -pt(jjj)*(1.+pt(iii)*xi(n))
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
         CALL gmmats(Dnc,2,8,0,xx,8,2,0,Xjb)
!
!     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
!     FOR INVERSION
!
         k = 0
         DO i = 1 , 2
            DO j = 1 , 2
               k = k + 1
               Xxjb(i,j) = Xjb(k)
            ENDDO
         ENDDO
!
!     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
!
         CALL invers(2,Xxjb,2,Dumarg,0,Determ,ising,iws)
         IF ( ising==2 ) CALL mesage(-30,143,Ecpt(1))
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X,Y,AND Z
!
         k = 0
         DO i = 1 , 2
            DO j = 1 , 2
               k = k + 1
               Xjb(k) = Xxjb(i,j)
            ENDDO
         ENDDO
         CALL gmmats(Xjb,2,2,0,Dnc,2,8,0,Dnl)
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
            Ph1out(kk) = Dnl(i)
         ENDDO
!
!     LOOP FOR OTHER GRID POINTS
!
      ENDDO
   ENDDO
   Ph1out(1) = Ecpt(1)
   DO i = 1 , 8
      Ph1out(i+1) = Ecpt(i+1)
   ENDDO
   Ph1out(10) = Tref
!
!     COMPUTE VECTOR FOR THERMAL EXPANSION
!
   Alphas(1) = Alpha1
   Alphas(2) = Alpha2
   Alphas(3) = Alp12
!
   CALL gmmats(g,3,3,0,Alphas,3,1,0,Ph1out(11))
!
   Nph1(62) = Id1
!
END SUBROUTINE ss2d81
