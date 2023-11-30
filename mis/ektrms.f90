
SUBROUTINE ektrms(Ntype)
   IMPLICIT NONE
   REAL A(225) , Alp12 , Alpha1 , Alpha2 , Angle , Consts(5) , Costh , Degra , Dict5 , Dum(19) , Dumb(80) , Dummy1 , Dummy2 ,       &
      & Dummy3 , E(18) , Ecpt(21) , Eltemp , Fmu , G11 , G12 , G13 , G22 , G23 , G2x211 , G2x212 , G2x222 , G33 , Gsube , K(324) ,  &
      & Kij(81) , Kout(324) , Matbuf(4) , Prod9(9) , Rho , Sigcom , Sigshe , Sigten , Sinth , Stress , T , Temp9(9) , Tsub0 , X1 ,  &
      & X2 , X3 , Xsubb , Xsubc , Y1 , Y2 , Y3 , Ysubc , Z1 , Z2 , Z3
   LOGICAL Heat , Nogo
   INTEGER Iheat , Inflag , Ioutpt , Ksystm(60) , Matid , Matid1 , Mecpt(1) , Necpt(6) , Ngrid(3)
   COMMON /condas/ Consts
   COMMON /emgest/ Mecpt , Ngrid , Angle , Matid1 , T , Fmu , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 , Z2 , Dummy3 , X3 , Y3 ,    &
                 & Z3 , Dumb
   COMMON /emgprm/ Dum , Nogo , Heat
   COMMON /emgtrx/ A , Prod9 , Temp9 , Xsubb , Xsubc , Ysubc , Dict5 , E , K , Kout , Kij
   COMMON /hmtout/ Matbuf
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rho , Alpha1 , Alpha2 , Alp12 , Tsub0 , Gsube , Sigten , Sigcom , Sigshe ,   &
                 & G2x211 , G2x212 , G2x222
   COMMON /system/ Ksystm
   INTEGER Ntype
   REAL c(18) , delta , flamda , g(9) , reelmu , temp , tempar(27) , theta , ti(9) , tt(2) , vol
   INTEGER i , j , ka , kmax , kq , n1 , n2 , ncom , npoint , npt1 , npt2 , npt36 , npvt , nsave
!
!     TRIANGULAR MEMBRANE ELEMENT
!
!     ECPT LIST
!                                                      THIS
!       ECPT       DESCRIPTION                         ROUTINE   TYPE
!     ******************************************************************
!       ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
!       ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
!       ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
!       ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
!       ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
!       ECPT( 6) = MATERIAL ID                         MATID     INTEGER
!       ECPT( 7) = T                                   T         REAL
!       ECPT( 8) = NON-STRUCTURAL MASS                 FMU       REAL
!       ECPT( 9) = COORD. SYSTEM ID 1                  NECPT(9)  INTEGER
!       ECPT(10) = X1                                  X1        REAL
!       ECPT(11) = Y1                                  Y1        REAL
!       ECPT(12) = Z1                                  Z1        REAL
!       ECPT(13) = COORD. SYSTEM ID 2                  NECPT(13) INTEGER
!       ECPT(14) = X2                                  X2        REAL
!       ECPT(15) = Y2                                  Y2        REAL
!       ECPT(16) = Z2                                  Z2        REAL
!       ECPT(17) = COORD. SYSTEM ID 3                  NECPT(17) INTEGER
!       ECPT(18) = X3                                  X3        REAL
!       ECPT(19) = Y3                                  Y3        REAL
!       ECPT(20) = Z3                                  Z3        REAL
!       ECPT(21) = ELEMENT TEMPERATURE                 ELTEMP    REAL
!
!     IF NTYPE = 0  COMPLETE MEMBRANE COMPUTATION IS PERFORMED
!     IF NTYPE = 1  9  3X3 MATRICES FOR THE GRID POINTS  IN ECPT
!
   EQUIVALENCE (Consts(4),Degra) , (Ecpt(1),Necpt(1),Mecpt(1)) , (Ksystm(2),Ioutpt) , (Ksystm(56),Iheat)
!
!     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
!
!     E(1), E(3), E(5) WILL BE THE I-VECTOR
!     E(2), E(4), E(6) WILL BE THE J-VECTOR
!     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
!
!     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
!
   E(1) = X2 - X1
   E(3) = Y2 - Y1
   E(5) = Z2 - Z1
!
!     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
!
   Xsubb = sqrt(E(1)**2+E(3)**2+E(5)**2)
   IF ( Xsubb<=1.E-06 ) THEN
!
!     ERROR  EXITS
!
      CALL mesage(30,31,Necpt(1))
   ELSE
!
!  20 NOW NORMALIZE I-VECTOR WITH X-SUB-B
!
      E(1) = E(1)/Xsubb
      E(3) = E(3)/Xsubb
      E(5) = E(5)/Xsubb
!
!     HERE WE NOW TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN
!     E(2), E(4), E(6) WHICH IS WHERE THE J-VECTOR WILL FIT LATER
!
      E(2) = X3 - X1
      E(4) = Y3 - Y1
      E(6) = Z3 - Z1
!
!     X-SUB-C  =  I . (RSUBC - RSUBA), THUS
!
      Xsubc = E(1)*E(2) + E(3)*E(4) + E(5)*E(6)
!
!     AND CROSSING THE I-VECTOR TO (RSUBC-RSUBA) GIVES THE K-VECTOR
!     (NON-NORMALIZED)
!
      E(7) = E(3)*E(6) - E(5)*E(4)
      E(8) = E(5)*E(2) - E(1)*E(6)
      E(9) = E(1)*E(4) - E(3)*E(2)
!
!     THE LENGTH OF THE K-VECTOR IS NOW FOUND AND EQUALS Y-SUB-C
!     COORD. IN ELEMENT SYSTEM
!
      Ysubc = sqrt(E(7)**2+E(8)**2+E(9)**2)
      IF ( Ysubc<=1.E-06 ) THEN
         CALL mesage(30,32,Necpt(1))
      ELSE
!
!  25 NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
!
         E(7) = E(7)/Ysubc
         E(8) = E(8)/Ysubc
         E(9) = E(9)/Ysubc
!
!     J VECTOR = K CROSS I
!     STORE IN THE SPOT FOR J
!
         E(2) = E(5)*E(8) - E(3)*E(9)
         E(4) = E(1)*E(9) - E(5)*E(7)
         E(6) = E(3)*E(7) - E(1)*E(8)
!
!     AND JUST FOR COMPUTER EXACTNESS NORMALIZE J-VECTOR TO MAKE SURE.
!
         temp = sqrt(E(2)**2+E(4)**2+E(6)**2)
         IF ( temp==0.0 ) THEN
            CALL mesage(30,26,Necpt(1))
         ELSE
            E(2) = E(2)/temp
            E(4) = E(4)/temp
            E(6) = E(6)/temp
!
!     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
!
            vol = Xsubb*Ysubc*T/2.
            reelmu = 1./Xsubb
            flamda = 1./Ysubc
            delta = Xsubc/Xsubb - 1.
!
!     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
!         CSUBA = (3X2) STORED IN C( 1) THRU C( 6) BY ROWS
!         CSUBB = (3X2) STORED IN C( 7) THRU C(12) BY ROWS
!         CSUBC = (3X2) STORED IN C(13) THRU C(18) BY ROWS
!
            c(1) = -reelmu
            c(2) = 0.
            c(3) = 0.
            c(4) = flamda*delta
            c(5) = c(4)
            c(6) = -reelmu
            c(7) = reelmu
            c(8) = 0.
            c(9) = 0.
            c(10) = -flamda*reelmu*Xsubc
            c(11) = c(10)
            c(12) = reelmu
            c(13) = 0.
            c(14) = 0.
            c(15) = 0.
            c(16) = flamda
            c(17) = flamda
            c(18) = 0.
!
            IF ( Ntype/=1 ) THEN
!
               theta = Angle*Degra
               Sinth = sin(theta)
               Costh = cos(theta)
            ENDIF
            IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0
!
!     BRANCH ON -HEAT- PROBLEM AT THIS POINT.
!
            IF ( Heat ) THEN
!
!     HEAT PROBLEM LOGIC PICKS UP HERE.  CALL HMAT FOR MATERIAL DATA.
!
               Inflag = 2
               Matid = Necpt(6)
               Eltemp = Ecpt(21)
               CALL hmat(Necpt)
               g(1) = Matbuf(1)
               g(2) = Matbuf(2)
               g(3) = Matbuf(2)
               g(4) = Matbuf(3)
!
!     CONDENSE C MATRIX FOR HEAT PROBLEM (FORMED ABOVE)  C IS (2X3)
!
               c(2) = c(4)
               c(3) = c(7)
               c(4) = c(10)
               c(5) = c(13)
               c(6) = c(16)
!
!     DETERMINE THE PIVOT POINT.
!
               kq = 3
               kmax = kq*3
               DO i = 1 , kmax
                  Kij(i) = 0.
               ENDDO
               DO npvt = 1 , 3
!
!     PIVOT C MATRIX TIMES VOLUME (STORED INTO TT(1) AND TT(2).)
!
                  tt(1) = vol*c(2*npvt-1)
                  tt(2) = vol*c(2*npvt)
!
!     OUTPUT THE CONDUCTIVITY MATRICES
!
                  npoint = (npvt-1)*kq
!
                  DO i = 1 , 3
                     n2 = 2*i
                     n1 = n2 - 1
                     tempar(1) = (g(1)*c(n1)+g(2)*c(n2))*tt(1) + (g(3)*c(n1)+g(4)*c(n2))*tt(2)
!
!     SUB-TRIANGLE (RETURN 3X3-S AS ABOVE IN STIFFNESS PORTION)
!
                     Kij(npoint+1) = tempar(1)
                     npoint = npoint + 1
                  ENDDO
               ENDDO
               RETURN
            ELSE
               Eltemp = Ecpt(21)
               Matid = Matid1
               Inflag = 2
               CALL mat(Ecpt(1))
!
!     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
!
               g(1) = G11
               g(2) = G12
               g(3) = G13
               g(4) = G12
               g(5) = G22
               g(6) = G23
               g(7) = G13
               g(8) = G23
               g(9) = G33
!
!     AT 50 G, E, AND C MATRICES ARE COMPLETE
!
!     AT THIS POINT THE FOLLOWING EQUATION CAN BE SOLVED FOR K-SUB-IJ
!
!                     T        T             T
!       K   = VOL . T  * E * C  * G * C  * E  * T
!        IJ          I        I        J         J
!
!     T-SUB-I WILL BE USED IN THE ABOVE ONLY IF THE PIVOT COORDINATE
!     SYSTEM ID IS NOT ZERO, OTHERWISE IT IS ASSUMED TO BE THE
!     IDENTITY MATRIX.
!
!     THE I SUBSCRIPT IMPLIES THE PIVOT POINT  1,2, OR 3 (ELEMENT SYST)
!     THE J SUBSCRIPT IMPLIES  1 THRU 3  FOR EACH CALL TO THIS ROUTINE.
!
!     DO COMPUTATIONS FOR EACH POINT IN ECPT LIST
!
               DO i = 1 , 81
                  Kij(i) = 0.
               ENDDO
               DO npvt = 1 , 3
                  ka = 4*npvt + 5
                  npoint = 6*npvt - 5
!
!                     T
!     COMPUTE   E * C   * G       AND STORE IN TEMPAR(1 THRU 9)
!                    I
!
                  CALL gmmats(E,3,2,0,c(npoint),3,2,1,tempar(10))
                  CALL gmmats(tempar(10),3,3,0,g,3,3,0,tempar(1))
!
!     NCOM WILL ALWAYS POINT TO THE COMMON 3 X 3 PRODUCT ABOVE
!     NPT1 WILL POINT TO FREE WORKING SPACE LENGTH 9
!
                  ncom = 1
                  npt1 = 10
!
!     MULTIPLY COMMON PRODUCT BY SCALER VOL
!
                  DO i = 1 , 9
                     tempar(i) = tempar(i)*vol
                  ENDDO
!
!     CHECK FOR PIVOT  CSID = 0,  IF ZERO SKIP TRANSFORMATION TSUBI.
!
                  IF ( Necpt(ka)/=0 ) THEN
!
!     NOT-ZERO THUS GET TI
!
                     CALL transs(Necpt(ka),ti)
!
!     INTRODUCE TI INTO THE COMMON PRODUCT AND STORE AT
!     TEMPAR(10 THRU 18)
!
                     CALL gmmats(ti,3,3,1,tempar(1),3,3,0,tempar(10))
!
!     COMMON PRODUCT NOW STARTS AT TEMPAR(10) THUS CHANGE NCOM AND NPT1
!
                     ncom = 10
                     npt1 = 1
                  ENDIF
!
!  80 NOW HAVE COMMON PRODUCT STORED BEGINNING TEMPAR(NCOM),  (3X3).
!     NPT1 POINTS TO FREE WORKING SPACE LENGTH 9.
!
!     PROCEED NOW AND RUN OUT THE 3 6X6 MATRICES KIJ-SUB-1,2,3.
!
                  nsave = npt1
                  npoint = (npvt-1)*27
!
!     INSERT G INTO TEMPAR
!
                  DO i = 1 , 9
                     tempar(i+18) = g(i)
                  ENDDO
                  DO i = 1 , 3
                     CALL gmmats(c(6*i-5),3,2,0,E,3,2,1,tempar(nsave))
!
!     NPT2 IS SET TO POINT TO THE BEGINNING OF THE PRODUCT  C * E * T
!                                                            J       J
                     npt2 = nsave
                     npt1 = 19
!
!     CHECK FOR ZERO CSID IN WHICH CASE TJ IS NOT NEEDED
!
                     IF ( Necpt(4*i+5)/=0 ) THEN
!
!     COMMING HERE IMPLIES NEED FOR TJ
!     WILL STORE TJ IN TI
!
                        CALL transs(Necpt(4*i+5),ti)
                        CALL gmmats(tempar(npt2),3,3,0,ti,3,3,0,tempar(19))
                        npt1 = npt2
                        npt2 = 19
                     ENDIF
!
!  60 AT THIS POINT COMPLETE COMPUTATION FOR  K-SUB-I,J
!
                     CALL gmmats(tempar(ncom),3,3,0,tempar(npt2),3,3,0,tempar(npt1))
                     npt36 = npt1 + 35
!
                     DO j = 1 , 9
                        npoint = npoint + 1
                        npt2 = npt1 + j - 1
                        Kij(npoint) = tempar(npt2)
                     ENDDO
                  ENDDO
               ENDDO
!
               Dict5 = Gsube
               RETURN
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   Nogo = .TRUE.
   RETURN
!
END SUBROUTINE ektrms
