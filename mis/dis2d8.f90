
SUBROUTINE dis2d8
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alp12 , Alpha1 , Alpha2 , Alphas(3) , C , Costh , Disp(24) , Ecpt(1) , Edt , Eltemp , G11 , G12 , G13 , G22 , G23 , G33 ,   &
      & Ge , Kx , Ky , Qq(15) , Rho , Sinth , Stress , T , Tgrid(8) , Th , Tref , Ttemp , X1 , X2 , X3 , X4 , X5 , X6 , X7 , X8 ,   &
      & Xy1(3) , Xy2(3) , Y1 , Y2 , Y3 , Y4 , Y5 , Y6 , Y7 , Y8 , Z1 , Z2 , Z3 , Z4 , Z5 , Z6 , Z7 , Z8
   DOUBLE PRECISION B(12) , Bt(12) , Determ , Dnc(16) , Dneta(1) , Dnl(16) , Dnx(1) , Dnxi(1) , Dny(1) , Dumarg , G(9) , Gsube ,    &
                  & H(3) , Kij(36) , Pt(3) , Tb(9) , Temp(9) , Tempar(1) , Tsave(72) , Xjb(4) , Xx(16) , Xxjb(2,2)
   INTEGER Icstm , Id1 , Inflag , Isetno , Isys1 , Isys2 , Isys3 , Isys4 , Isys5 , Isys6 , Isys7 , Isys8 , Matid , Matid1 , Ncstm , &
         & Necpt(1) , Ngrid(8) , Npvt
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm
   COMMON /ds1adp/ Kij , Xx , Dnc , Dnl , Xxjb , Xjb , Pt , H , G , B , Bt , Tb , Determ , Gsube , Dumarg , Tsave
   COMMON /ds1aet/ Necpt , Ngrid , Id1 , Th , Matid1 , T , Isys1 , X1 , Y1 , Z1 , Isys2 , X2 , Y2 , Z2 , Isys3 , X3 , Y3 , Z3 ,     &
                 & Isys4 , X4 , Y4 , Z4 , Isys5 , X5 , Y5 , Z5 , Isys6 , X6 , Y6 , Z6 , Isys7 , X7 , Y7 , Z7 , Isys8 , X8 , Y8 ,    &
                 & Z8 , Ttemp , Edt , Isetno , Tgrid , Disp
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tref , Ge , Kx , Ky , C
!
! Local variable declarations
!
   REAL bb(72) , db(72) , dn(8) , gstemp , r(9) , rgtemp , s(6) , se1t(6) , semp(9) , sig(3) , st(3) , stb(9) , ttb(9) , tth ,      &
      & vec(3) , veci(3) , vecil , vecj(3) , veck(3) , veckl , vvec(3)
   DOUBLE PRECISION cid(18) , cjd(18) , dhh , e1t(9) , eta(8) , kmult(18) , kwd(36) , premul(9) , pstmul(9) , save(72) , thick ,    &
                  & xi(8)
   INTEGER i , idtemp , iii , ising , isub , iws(2,3) , ixx , j , jjj , k , kk , l , ll , n , n3 , nogo
!
! End of declarations
!
!
!     2-D, 8 GRID POINT ISOPARAMETRIC STRUCTURAL ELEMENT
!     DIFFERENTIAL STIFFNESS MATRIX ROUTINE
!
   EQUIVALENCE (Alphas(1),Alpha1) , (Ecpt(1),Necpt(1)) , (Temp(1),B(1)) , (Dnc(1),Dnxi(1)) , (Dnc(9),Dneta(1)) , (Dnl(1),Dnx(1)) ,  &
    & (Dnl(9),Dny(1)) , (Qq(1),G11) , (Tempar(1),Bt(1)) , (Xy1(1),X1) , (Xy2(1),X2)
   DATA xi/ - 1.D0 , 1.D0 , 1.D0 , -1.D0 , 0.D0 , 1.D0 , 0.D0 , -1.D0/
   DATA eta/ - 1.D0 , -1.D0 , 1.D0 , 1.D0 , -1.D0 , 0.D0 , 1.D0 , 0.D0/
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
!     ECPT(47) = 0.                    EDT            REAL
!     ECPT(48) = TEMPERATURE SET       ISETNO         INTEGER
!     ECPT(49) = *
!     ECPT(. ) = *  GRID POINT TEMPERATURES
!     ECPT(56) = *
!     ECPT(57) = *
!     ECPT(. ) = *  TRANSLATIONAL DOF-S OF GRIDS FOR THIS ELEMENT
!     ECPT(80) = *
!
!
!     TEST FOR PIVOT POINT
!
   DO kk = 1 , 8
      IF ( Ngrid(kk)==Npvt ) GOTO 100
   ENDDO
!
!     IF FALL HERE NO ELEMENT GRID POINT IS THE PIVOT POINT
!
   CALL mesage(-30,34,Ecpt(1))
!
!     UNIT I VECTOR IS FROM GRID POINT 1 TO GRID POINT 2
!
 100  DO i = 1 , 3
      veci(i) = Xy2(i) - Xy1(i)
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
         e1t(7) = veck(1)
         e1t(8) = veck(2)
         e1t(9) = veck(3)
         DO i = 1 , 6
            se1t(i) = e1t(i)
         ENDDO
!
!     STORE ELEMENT COORDS FOR GRIDS 1 AND 2
!
         Xx(1) = 0.
         Xx(2) = 0.
         Xx(3) = vecil
         Xx(4) = 0.
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
            Xx(ixx) = vec(1)*veci(1) + vec(2)*veci(2) + vec(3)*veci(3)
            vvec(1) = veci(2)*vec(3) - veci(3)*vec(2)
            vvec(2) = veci(3)*vec(1) - veci(1)*vec(3)
            vvec(3) = veci(1)*vec(2) - veci(2)*vec(1)
            Xx(ixx+1) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
         ENDDO
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
            G(i) = Qq(i)
         ENDDO
         G(4) = Qq(2)
         G(5) = Qq(4)
         G(6) = Qq(5)
         G(7) = Qq(3)
         G(8) = Qq(5)
         G(9) = Qq(6)
         thick = T
         DO i = 1 , 9
            r(i) = G(i)
         ENDDO
         IF ( Isetno/=0 ) CALL gmmats(r,3,3,0,Alphas,3,1,0,st)
!
!     ZERO OUT THE KIJ AND SAVE MATRICES
!
         DO i = 1 , 36
            kwd(i) = 0.D0
            Kij(i) = 0.D0
         ENDDO
         DO i = 1 , 72
            save(i) = 0.D0
         ENDDO
!
         Pt(1) = -0.57735027D0
         Pt(2) = -Pt(1)
         H(1) = 1.D0
         H(2) = 1.D0
         IF ( Id1/=2 ) THEN
            Pt(1) = -0.77459667D0
            Pt(2) = 0.D0
            Pt(3) = -Pt(1)
            H(1) = 5.D0/9.D0
            H(2) = 8.D0/9.D0
            H(3) = H(1)
         ENDIF
!
!     2 OR 3 QUADRATURE POINTS
!
         DO iii = 1 , Id1
            DO jjj = 1 , Id1
!
!     COMPUTE GAUSS POINT STRESSES
!
!
!     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
!     EACH GRID POINT
!
               DO n = 1 , 4
                  Dnxi(n) = .25D0*xi(n)*(1.D0+Pt(jjj)*eta(n))*(2.D0*Pt(iii)*xi(n)+Pt(jjj)*eta(n))
                  Dneta(n) = .25D0*eta(n)*(1.D0+Pt(iii)*xi(n))*(Pt(iii)*xi(n)+2.D0*Pt(jjj)*eta(n))
               ENDDO
               DO n = 5 , 7 , 2
!
                  Dnxi(n) = -Pt(iii)*(1.D0+Pt(jjj)*eta(n))
                  Dneta(n) = .5D0*(1.D0-Pt(iii)*Pt(iii))*eta(n)
               ENDDO
!
               DO n = 6 , 8 , 2
                  Dnxi(n) = .5D0*xi(n)*(1.D0-Pt(jjj)*Pt(jjj))
                  Dneta(n) = -Pt(jjj)*(1.D0+Pt(iii)*xi(n))
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
               CALL gmmatd(Dnc,2,8,0,Xx,8,2,0,Xjb)
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
               CALL inverd(2,Xxjb,2,Dumarg,0,Determ,ising,iws)
               IF ( ising==2 ) CALL mesage(-30,143,Ecpt(1))
               dhh = Determ*H(iii)*H(jjj)
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y
!
               k = 0
               DO i = 1 , 2
                  DO j = 1 , 2
                     k = k + 1
                     Xjb(k) = Xxjb(i,j)
                  ENDDO
               ENDDO
               CALL gmmatd(Xjb,2,2,0,Dnc,2,8,0,Dnl)
!
!           N1X N2X N3X N4X N5X N6X N7X N8X
!     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
!
               DO i = 1 , 72
                  bb(i) = 0.
               ENDDO
!
!     SET UP INDICATOR FOR GRID POINT TEMPERATURES
!
               idtemp = 0
               DO i = 1 , 8
                  IF ( Tgrid(i)/=0. ) GOTO 105
               ENDDO
               GOTO 110
 105           idtemp = 1
!
 110           DO n = 1 , 8
!
                  DO i = 1 , 9
                     semp(i) = 0.
                  ENDDO
                  DO i = 1 , 6
                     s(i) = 0.
                  ENDDO
                  s(1) = Dnx(n)
                  s(4) = Dny(n)
                  s(5) = Dny(n)
                  s(6) = Dnx(n)
!
!     TRANSFORM TO ELEMENT COORDINATES
!
                  IF ( Necpt(4*n+10)==0 ) THEN
                     DO i = 1 , 6
                        stb(i) = se1t(i)
                     ENDDO
                  ELSE
                     CALL transs(Necpt(4*n+10),ttb)
                     CALL gmmats(se1t,2,3,0,ttb,3,3,0,stb)
                  ENDIF
                  CALL gmmats(s,3,2,0,stb,2,3,0,semp(1))
                  n3 = 3*n
                  bb(n3-2) = semp(1)
                  bb(n3-1) = semp(2)
                  bb(n3) = semp(3)
                  bb(n3+22) = semp(4)
                  bb(n3+23) = semp(5)
                  bb(n3+24) = semp(6)
                  bb(n3+46) = semp(7)
                  bb(n3+47) = semp(8)
                  bb(n3+48) = semp(9)
               ENDDO
!
!     BRING IN G MATRIX
!
               CALL gmmats(r,3,3,0,bb,3,24,0,db)
!
!     COMPUTE STRESSES
!
               CALL gmmats(db,3,24,0,Disp,24,1,0,sig)
!
!
!     COMPUTE GAUSS POINT  TEMPERATURES
!
               IF ( Isetno/=0 ) THEN
                  IF ( idtemp==1 ) THEN
!
!     ALL TEMPERATURES ARE DEFAULT VALUE
!
                     DO n = 1 , 4
                        dn(n) = .25*(1.+Pt(iii)*xi(n))*(1.+Pt(jjj)*eta(n))*(Pt(iii)*xi(n)+Pt(jjj)*eta(n)-1.)
                     ENDDO
                     DO n = 5 , 7 , 2
                        dn(n) = .5*(1.-Pt(iii)*Pt(iii))*(1.+Pt(jjj)*eta(n))
                     ENDDO
                     DO n = 6 , 8 , 2
                        dn(n) = .5*(1.+Pt(iii)*xi(n))*(1.-Pt(jjj)*Pt(jjj))
                     ENDDO
                     gstemp = 0.
                     DO n = 1 , 8
                        gstemp = gstemp + dn(n)*Tgrid(n)
                     ENDDO
                     rgtemp = gstemp - Tref
                  ELSE
                     rgtemp = Eltemp - Tref
                  ENDIF
                  DO i = 1 , 3
                     sig(i) = sig(i) - st(i)*rgtemp
                  ENDDO
               ENDIF
!
!
!     FORM KWD MATRIX
!
               kwd(1) = sig(2)
               kwd(2) = -sig(3)
               kwd(7) = -sig(3)
               kwd(8) = sig(1)
               kwd(15) = sig(1) + sig(2)
               kwd(16) = -sig(3)
               kwd(17) = sig(3)
               kwd(18) = sig(1) - sig(2)
               kwd(21) = -sig(3)
               kwd(27) = sig(3)
               kwd(33) = sig(1) - sig(2)
!
!     FORM CID FOR I = NPVT
!
               DO i = 1 , 18
                  cid(i) = 0.D0
               ENDDO
               cid(3) = Dny(kk)
               cid(6) = -Dnx(kk)
               cid(7) = -.5*Dny(kk)
               cid(8) = .5*Dnx(kk)
               cid(10) = Dnx(kk)
               cid(14) = Dny(kk)
               cid(16) = .5*Dny(kk)
               cid(17) = .5*Dnx(kk)
!
               CALL gmmatd(cid,6,3,1,kwd,6,6,0,kmult)
!
!     LOOP FOR THE 8 6X6 PARTITIONS CORRESPONDING TO THE PRESENT
!     PIVOT POINT
!
               DO n = 1 , 8
!
                  DO i = 1 , 18
                     cjd(i) = 0.D0
                  ENDDO
!
                  cjd(3) = Dny(n)
                  cjd(6) = -Dnx(n)
                  cjd(7) = -.5*Dny(n)
                  cjd(8) = .5*Dnx(n)
                  cjd(10) = Dnx(n)
                  cjd(14) = Dny(n)
                  cjd(16) = .5*Dny(n)
                  cjd(17) = .5*Dnx(n)
!
                  CALL gmmatd(kmult,3,6,0,cjd,6,3,0,Tempar(1))
!
!     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
!
                  DO i = 1 , 9
                     Tempar(i) = Tempar(i)*dhh
                  ENDDO
!
!     ADD THE RESULTS OF THIS INTEGRATION TO THE PREVIOUS RESULTS
!
                  ll = 9*(n-1)
                  DO i = 1 , 9
                     l = ll + i
                     save(l) = save(l) + Tempar(i)
                  ENDDO
!
!     LOOP FOR MORE PARTITIONS
!
               ENDDO
!
!     LOOP FOR MORE GAUSS POINTS
!
            ENDDO
         ENDDO
!
!     CHECK ON NECESSITY OF PRE-MULTIPLYING COORDINATE TRANSFORMATIONS
!
         isub = 4*kk + 10
         IF ( Necpt(isub)==0 ) THEN
            DO i = 1 , 9
               premul(i) = e1t(i)
            ENDDO
         ELSE
!
!     ELEMENT TO GLOBAL
!
            CALL transd(Necpt(isub),Tb)
            CALL gmmatd(e1t,3,3,0,Tb,3,3,0,premul)
         ENDIF
         DO n = 1 , 8
            ll = 9*n - 8
            CALL gmmatd(premul,3,3,1,save(ll),3,3,0,Temp)
!
!     STORE THE 3 X 3 IN TSAVE
!
            DO i = 1 , 9
               l = 9*n + i - 9
               Tsave(l) = Temp(i)
            ENDDO
!
         ENDDO
!
!     NOW CHECK ON THE NECESSITY FOR POST-MULTIPLYING TRANSFORMATIONS
!
         DO n = 1 , 8
            isub = 4*n + 10
            ll = 9*n - 8
            IF ( Necpt(isub)==0 ) THEN
               DO i = 1 , 9
                  pstmul(i) = e1t(i)
               ENDDO
            ELSE
!
!     GLOBAL TO ELEMENT
!
               CALL transd(Necpt(isub),Tb)
               CALL gmmatd(e1t,3,3,0,Tb,3,3,0,pstmul)
            ENDIF
!
!     POST-MULTIPLY
!
            CALL gmmatd(Tsave(ll),3,3,0,pstmul,3,3,0,Temp)
!
!     FILL OUT THE 6 X 6 PARTITION
!
            Kij(1) = Temp(1)*thick
            Kij(2) = Temp(2)*thick
            Kij(3) = Temp(3)*thick
            Kij(7) = Temp(4)*thick
            Kij(8) = Temp(5)*thick
            Kij(9) = Temp(6)*thick
            Kij(13) = Temp(7)*thick
            Kij(14) = Temp(8)*thick
            Kij(15) = Temp(9)*thick
!
!     INSERT INTO THE OVERALL STIFFNESS MATRIX
!
            CALL ds1b(Kij,Necpt(n+1))
!
!     LOOP FOR MORE PARTITIONS
!
         ENDDO
         GOTO 99999
      ENDIF
   ENDIF
!
!     INAPPROPRIATE GEOMETRY
!
   CALL mesage(30,31,Ecpt(1))
   nogo = 1
   RETURN
!
99999 RETURN
END SUBROUTINE dis2d8
