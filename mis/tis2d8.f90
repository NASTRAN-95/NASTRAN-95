
SUBROUTINE tis2d8(Temp,Pg)
   IMPLICIT NONE
   REAL Alp12 , Alpha1 , Alpha2 , Alphas(3) , Bt(12) , C , Costh , Determ , Dnc(16) , Dneta(1) , Dnl(16) , Dnx(1) , Dnxi(1) , Dny(1)&
      & , Dumarg , Ecpt(1) , Eltemp , G(9) , G11 , G12 , G13 , G22 , G23 , G33 , Ge , H(3) , Kx , Ky , Pt(3) , Qq(15) , Rho ,       &
      & Rtside(3) , Save(16) , Sinth , Stress , T , Tempar(8) , Tempav , Th , Tref , Ttemp , X1 , X2 , X3 , X4 , X5 , X6 , X7 , X8 ,&
      & Xjb(4) , Xx(16) , Xxjb(2,2) , Xy1(3) , Xy2(3) , Y1 , Y2 , Y3 , Y4 , Y5 , Y6 , Y7 , Y8 , Z(14) , Z1 , Z2 , Z3 , Z4 , Z5 ,    &
      & Z6 , Z7 , Z8
   INTEGER Id1 , Inflag , Isys1 , Isys2 , Isys3 , Isys4 , Isys5 , Isys6 , Isys7 , Isys8 , Iz(1) , Matid , Matid1 , Necpt(1) ,       &
         & Ngrid(8)
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tref , Ge , Kx , Ky , C
   COMMON /tranx / Z
   COMMON /trimex/ Necpt , Ngrid , Id1 , Th , Matid1 , T , Isys1 , X1 , Y1 , Z1 , Isys2 , X2 , Y2 , Z2 , Isys3 , X3 , Y3 , Z3 ,     &
                 & Isys4 , X4 , Y4 , Z4 , Isys5 , X5 , Y5 , Z5 , Isys6 , X6 , Y6 , Z6 , Isys7 , X7 , Y7 , Z7 , Isys8 , X8 , Y8 ,    &
                 & Z8 , Ttemp , Save , Rtside , Tempav , Alphas , Xx , Dnc , Dnl , Xxjb , Xjb , Pt , H , G , Bt , Determ , Dumarg
   REAL Pg(1) , Temp(8)
   REAL coef , dn(8) , e1t(6) , eta(8) , gstemp , rgtemp , tth , vec(3) , veci(3) , vecil , vecj(3) , veck(3) , veckl , vvec(3) ,   &
      & xi(8)
   INTEGER i , iii , ising , isub , iws(2,3) , ixx , j , jjj , k , kk , l , lll , n
!
!     THIS ROUTINE COMPUTES EQUIVALENT LOADS DUE TO GRID POINT
!     TEMPERATURES FOR THE 2-D, 8 GRID POINT ISOPARAMETRIC ELEMENT
!
!     ECPT LIST                        IN
!                                      THIS
!     ECPT       DESCRIPTION           ROUTINE         TYPE
!     --------   --------------------  --------       -------
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
   EQUIVALENCE (Ecpt(1),Necpt(1)) , (Z(1),Iz(1)) , (Qq(1),G11) , (Dnc(1),Dnxi(1)) , (Dnc(9),Dneta(1)) , (Dnl(1),Dnx(1)) ,           &
    & (Dnl(9),Dny(1)) , (Tempar(1),Bt(1)) , (Xy1(1),X1) , (Xy2(1),X2)
   DATA xi/ - 1. , 1. , 1. , -1. , 0. , 1. , 0. , -1./
   DATA eta/ - 1. , -1. , 1. , 1. , -1. , 0. , 1. , 0./
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
         Xx(1) = 0.
         Xx(2) = 0.
         Xx(3) = vecil
         Xx(4) = 0.
!
!     FOR GRIDS 3-8, THE X COORDINATE IS THE DOT PRODUCT OF HTE VECTOR
!     FROM THE GRID POINT TO
!     GRID POINT 1 TO THE GRID POINT AND THE I VECTOR. THE Y COORD. IS
!     THE L OF THE I VECTOR CROSSED INTO THE VECTOR FROM GRID 1 TO THE
!     GRID POINT.
!
         DO i = 3 , 8
            ixx = 2*i - 1
            isub = 4*i + 11
            vec(1) = Ecpt(isub) - X1
            vec(2) = Ecpt(isub+1) - Y1
            vec(3) = Ecpt(isub+2) - Z1
            Xx(ixx) = vec(1)*veci(1) + vec(2)*veci(2) + vec(3)*veci(3)
            vvec(1) = veci(2)*vec(3) - veci(3)*vec(2)
            vvec(2) = veci(3)*vec(1) - veci(1)*vec(3)
            vvec(3) = veci(1)*vec(2) - veci(2)*vec(1)
            Xx(ixx+1) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
         ENDDO
      ENDIF
   ENDIF
!
!     COMPUTE MATERIAL PROPERTIES
!
   tth = Th*3.1415927/180.
   Sinth = sin(tth)
   Costh = cos(tth)
   Inflag = 2
   Matid = Matid1
!
!     ZERO OUT SOME MATRICES
!
   DO i = 1 , 16
      Save(i) = 0.
   ENDDO
!
   Pt(1) = -0.57735027
   Pt(2) = -Pt(1)
   H(1) = 1.
   H(2) = 1.
   IF ( Id1/=2 ) THEN
      Pt(1) = -0.77459667D0
      Pt(2) = 0.D0
      Pt(3) = -Pt(1)
      H(1) = 5.0/9.0
      H(2) = 8.0/9.0
      H(3) = H(1)
   ENDIF
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
            dn(n) = 0.25*(1.+Pt(iii)*xi(n))*(1.+Pt(jjj)*eta(n))*(Pt(iii)*xi(n)+Pt(jjj)*eta(n)-1.)
            Dnxi(n) = 0.25*xi(n)*(1.+Pt(jjj)*eta(n))*(2.*Pt(iii)*xi(n)+Pt(jjj)*eta(n))
            Dneta(n) = 0.25*eta(n)*(1.+Pt(iii)*xi(n))*(Pt(iii)*xi(n)+2.*Pt(jjj)*eta(n))
         ENDDO
!
         DO n = 5 , 7 , 2
            dn(n) = 0.5*(1.-Pt(iii)*Pt(iii))*(1.+Pt(jjj)*eta(n))
            Dnxi(n) = -Pt(iii)*(1.+Pt(jjj)*eta(n))
            Dneta(n) = 0.5*(1.-Pt(iii)*Pt(iii))*eta(n)
         ENDDO
!
         DO n = 6 , 8 , 2
            dn(n) = 0.5*(1.+Pt(iii)*xi(n))*(1.-Pt(jjj)*Pt(jjj))
            Dnxi(n) = 0.5*xi(n)*(1.-Pt(jjj)*Pt(jjj))
            Dneta(n) = -Pt(jjj)*(1.+Pt(iii)*xi(n))
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
         CALL gmmats(Dnc,2,8,0,Xx,8,2,0,Xjb)
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
         coef = Determ*H(iii)*H(jjj)
!
!     COMPUTE GAUSS POINT TEMPERATURE
!
         gstemp = 0.
         DO n = 1 , 8
            gstemp = gstemp + dn(n)*(Temp(n))
         ENDDO
!
!     GSTEMP IS THE GAUSS POINT TEMPERATURE. FIND MATERIAL PROPERTIES
!     BASED ON THIS TEMPERATURE. IF SAME AS PREVIOUS TEMPERATURE,DO NOT
!     RECOMPUTE.
!
         lll = iii*jjj
         IF ( lll/=1 ) THEN
            IF ( gstemp==Eltemp ) GOTO 20
         ENDIF
         Eltemp = gstemp
         CALL mat(Ecpt(1))
         DO i = 1 , 3
            G(i) = Qq(i)
         ENDDO
         G(4) = Qq(2)
         G(5) = Qq(4)
         G(6) = Qq(5)
         G(7) = Qq(3)
         G(8) = Qq(5)
         G(9) = Qq(6)
         Alphas(1) = Alpha1
         Alphas(2) = Alpha2
         Alphas(3) = Alp12
!
         CALL gmmats(G,3,3,0,Alphas,3,1,0,Rtside)
!
!     COMPUTE RELATIVE GAUSS POINT TEMPERATURE
!
         rgtemp = gstemp - Tref
!
!
 20      coef = coef*rgtemp
!
!     SET UP BT
!
         DO kk = 1 , 8
!
            DO i = 1 , 6
               Bt(i) = 0.
            ENDDO
!
            Bt(1) = Dnx(kk)
            Bt(3) = Dny(kk)
            Bt(5) = Bt(3)
            Bt(6) = Bt(1)
!
            CALL gmmats(Bt,2,3,0,Rtside,3,1,0,Tempar(7))
!
!     ADD TO PREVIOUS RESULTS
!
            Save(2*kk-1) = Save(2*kk-1) + Tempar(7)*coef
            Save(2*kk) = Save(2*kk) + Tempar(8)*coef
!
!     CONTINUE FOR MORE GRID POINTS
!
         ENDDO
!
!     CONTINUE FOR MORE GAUSS POINTS
!
      ENDDO
   ENDDO
!
!     TRANSFORMATIONS AND ADD TO OVERALL VECTOR
!
   DO kk = 1 , 8
      Tempar(7) = Save(2*kk-1)
      Tempar(8) = Save(2*kk)
!
!     CONVERT FROM ELEMENT COORDINATES TO BASIC
!
      CALL gmmats(e1t,2,3,1,Tempar(7),2,1,0,Tempar(1))
      isub = 4*kk + 10
!
!     MUST TRANSFORM FROM BASIC COORDS TO GLOBAL
!
      IF ( Necpt(isub)/=0 ) CALL basglb(Tempar(1),Tempar(1),Ecpt(isub+1),Necpt(isub))
!
!     ADD THIS VECTOR TO OVERALL LOAD VECTOR
!
      DO i = 1 , 3
         l = Ngrid(kk) + i - 1
         Pg(l) = Pg(l) + Tempar(i)*T
      ENDDO
!
!     GET ANOTHER PARTITION
!
   ENDDO
END SUBROUTINE tis2d8
