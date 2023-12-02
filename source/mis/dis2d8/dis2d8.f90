!*==dis2d8.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dis2d8
USE C_DS1AAA
USE C_DS1ADP
USE C_DS1AET
USE C_MATIN
USE C_MATOUT
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: alphas , sig , st , vec , veci , vecj , veck , vvec , xy1 , xy2
   REAL , DIMENSION(72) :: bb , db
   REAL(REAL64) , DIMENSION(18) :: cid , cjd , kmult
   REAL(REAL64) :: dhh , thick
   REAL , DIMENSION(8) :: dn
   REAL(REAL64) , DIMENSION(1) :: dneta , dnx , dnxi , dny , tempar
   REAL(REAL64) , DIMENSION(9) :: e1t , premul , pstmul , temp
   REAL , DIMENSION(1) :: ecpt
   REAL(REAL64) , DIMENSION(8) , SAVE :: eta , xi
   REAL :: gstemp , rgtemp , tth , vecil , veckl
   INTEGER :: i , idtemp , iii , ising , isub , ixx , j , jjj , k , kk , l , ll , n , n3 , nogo
   INTEGER , DIMENSION(2,3) :: iws
   REAL(REAL64) , DIMENSION(36) :: kwd
   REAL , DIMENSION(15) :: qq
   REAL , DIMENSION(9) :: r , semp , stb , ttb
   REAL , DIMENSION(6) :: s , se1t
   REAL(REAL64) , DIMENSION(72) :: save
   EXTERNAL ds1b , gmmatd , gmmats , inverd , mat , mesage , transd , transs
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     2-D, 8 GRID POINT ISOPARAMETRIC STRUCTURAL ELEMENT
!     DIFFERENTIAL STIFFNESS MATRIX ROUTINE
!
   !>>>>EQUIVALENCE (Alphas(1),Alpha1) , (Ecpt(1),Necpt(1)) , (Temp(1),B(1)) , (Dnc(1),Dnxi(1)) , (Dnc(9),Dneta(1)) , (Dnl(1),Dnx(1)) ,  &
!>>>>    & (Dnl(9),Dny(1)) , (Qq(1),G11) , (Tempar(1),Bt(1)) , (Xy1(1),X1) , (Xy2(1),X2)
   DATA xi/ - 1.D0 , 1.D0 , 1.D0 , -1.D0 , 0.D0 , 1.D0 , 0.D0 , -1.D0/
   DATA eta/ - 1.D0 , -1.D0 , 1.D0 , 1.D0 , -1.D0 , 0.D0 , 1.D0 , 0.D0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
            IF ( Ngrid(kk)==Npvt ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     IF FALL HERE NO ELEMENT GRID POINT IS THE PIVOT POINT
!
         CALL mesage(-30,34,ecpt(1))
         spag_nextblock_1 = 2
      CASE (2)
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
                  vec(1) = ecpt(isub) - X1
                  vec(2) = ecpt(isub+1) - Y1
                  vec(3) = ecpt(isub+2) - Z1
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
               CALL mat(ecpt(1))
               DO i = 1 , 3
                  G(i) = qq(i)
               ENDDO
               G(4) = qq(2)
               G(5) = qq(4)
               G(6) = qq(5)
               G(7) = qq(3)
               G(8) = qq(5)
               G(9) = qq(6)
               thick = T
               DO i = 1 , 9
                  r(i) = G(i)
               ENDDO
               IF ( Isetno/=0 ) CALL gmmats(r,3,3,0,alphas,3,1,0,st)
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
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
!
!     COMPUTE GAUSS POINT STRESSES
!
!
!     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
!     EACH GRID POINT
!
                           DO n = 1 , 4
                              dnxi(n) = .25D0*xi(n)*(1.D0+Pt(jjj)*eta(n))*(2.D0*Pt(iii)*xi(n)+Pt(jjj)*eta(n))
                              dneta(n) = .25D0*eta(n)*(1.D0+Pt(iii)*xi(n))*(Pt(iii)*xi(n)+2.D0*Pt(jjj)*eta(n))
                           ENDDO
                           DO n = 5 , 7 , 2
!
                              dnxi(n) = -Pt(iii)*(1.D0+Pt(jjj)*eta(n))
                              dneta(n) = .5D0*(1.D0-Pt(iii)*Pt(iii))*eta(n)
                           ENDDO
!
                           DO n = 6 , 8 , 2
                              dnxi(n) = .5D0*xi(n)*(1.D0-Pt(jjj)*Pt(jjj))
                              dneta(n) = -Pt(jjj)*(1.D0+Pt(iii)*xi(n))
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
                           IF ( ising==2 ) CALL mesage(-30,143,ecpt(1))
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
                              IF ( Tgrid(i)/=0. ) THEN
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDDO
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        CASE (2)
                           idtemp = 1
                           spag_nextblock_2 = 3
                        CASE (3)
!
                           DO n = 1 , 8
!
                              DO i = 1 , 9
                                 semp(i) = 0.
                              ENDDO
                              DO i = 1 , 6
                                 s(i) = 0.
                              ENDDO
                              s(1) = dnx(n)
                              s(4) = dny(n)
                              s(5) = dny(n)
                              s(6) = dnx(n)
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
                           cid(3) = dny(kk)
                           cid(6) = -dnx(kk)
                           cid(7) = -.5*dny(kk)
                           cid(8) = .5*dnx(kk)
                           cid(10) = dnx(kk)
                           cid(14) = dny(kk)
                           cid(16) = .5*dny(kk)
                           cid(17) = .5*dnx(kk)
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
                              cjd(3) = dny(n)
                              cjd(6) = -dnx(n)
                              cjd(7) = -.5*dny(n)
                              cjd(8) = .5*dnx(n)
                              cjd(10) = dnx(n)
                              cjd(14) = dny(n)
                              cjd(16) = .5*dny(n)
                              cjd(17) = .5*dnx(n)
!
                              CALL gmmatd(kmult,3,6,0,cjd,6,3,0,tempar(1))
!
!     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
!
                              DO i = 1 , 9
                                 tempar(i) = tempar(i)*dhh
                              ENDDO
!
!     ADD THE RESULTS OF THIS INTEGRATION TO THE PREVIOUS RESULTS
!
                              ll = 9*(n-1)
                              DO i = 1 , 9
                                 l = ll + i
                                 save(l) = save(l) + tempar(i)
                              ENDDO
!
!     LOOP FOR MORE PARTITIONS
!
                           ENDDO
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
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
                  CALL gmmatd(premul,3,3,1,save(ll),3,3,0,temp)
!
!     STORE THE 3 X 3 IN TSAVE
!
                  DO i = 1 , 9
                     l = 9*n + i - 9
                     Tsave(l) = temp(i)
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
                  CALL gmmatd(Tsave(ll),3,3,0,pstmul,3,3,0,temp)
!
!     FILL OUT THE 6 X 6 PARTITION
!
                  Kij(1) = temp(1)*thick
                  Kij(2) = temp(2)*thick
                  Kij(3) = temp(3)*thick
                  Kij(7) = temp(4)*thick
                  Kij(8) = temp(5)*thick
                  Kij(9) = temp(6)*thick
                  Kij(13) = temp(7)*thick
                  Kij(14) = temp(8)*thick
                  Kij(15) = temp(9)*thick
!
!     INSERT INTO THE OVERALL STIFFNESS MATRIX
!
                  CALL ds1b(Kij,Necpt(n+1))
!
!     LOOP FOR MORE PARTITIONS
!
               ENDDO
               RETURN
            ENDIF
         ENDIF
!
!     INAPPROPRIATE GEOMETRY
!
         CALL mesage(30,31,ecpt(1))
         nogo = 1
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE dis2d8
