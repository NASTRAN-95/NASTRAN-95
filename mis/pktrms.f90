
SUBROUTINE pktrms(Ntype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Angle , Consts(5) , Costh , Degra , Dum1(148) , Dumb(80) , Dummy(382) , Dummy1 , Dummy2 , Dummy3 , Ecpt(1) , Eltemp , Fmu , &
      & G11 , G12 , G13 , G22 , G23 , G33 , Sinth , Stress , T , Theta , X1 , X2 , X3 , Y1 , Y2 , Y3 , Z1 , Z2 , Z3
   DOUBLE PRECISION C(18) , Delta , E(9) , Flamda , G(9) , Kij(36) , Reelmu , Temp , Tempar(27) , Ti(9) , Vol , Xsubb , Xsubc ,     &
                  & Ysubc
   INTEGER Inflag , Ka , Matid , Matid1 , Necpt(1) , Ngrid(3) , Nogo , Npoint , Npvt , Nsave
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33
   COMMON /pla42c/ Npvt , Dum1 , Nogo
   COMMON /pla42d/ Kij , C , E , Tempar , Ti , Temp , Xsubb , Xsubc , Ysubc , Vol , Reelmu , Delta , Flamda , Theta , Ka , Npoint , &
                 & Nsave , Dummy
   COMMON /pla4es/ Necpt , Ngrid , Angle , Matid1 , T , Fmu , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 , Z2 , Dummy3 , X3 , Y3 ,    &
                 & Z3 , Dumb
!
! Dummy argument declarations
!
   INTEGER Ntype
!
! Local variable declarations
!
   INTEGER i , j , ncom , npt1 , npt2
!
! End of declarations
!
!
!     THIS ROUTINE CALCULATES AND SHIPS TO PLA4B THE STIFFNESS MATRIX
!     FOR PLA4
!
!     *** TRIANGULAR MEMBRANE ELEMENT ***
!
!     CALLS FROM THIS ROUTINE ARE MADE TO
!
!     PLAMAT - ROTATES AND RETURNS GP
!     PLA4B  - INSERTION ROUTINE
!     TRANSD - DOUBLE PRECISION TRANSFORMATION SUPPLIER
!     GMMATD - DOUBLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!     MESAGE - ERROR MESSAGE WRITER
!
!     IF NTYPE = 0  COMPLETE MEMBRANE COMPUTATION IS PERFORMED
!
!     IF NTYPE = 1 RETURN 3 TRANSFORMED  3X3 MATRICES ONLY FOR THE PIVOT
!
!     ECPT LIST
!                                                      IN
!                                                      THIS
!     ECPT       DESCRIPTION                         ROUTINE   TYPE
!     ===============================================================
!     ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
!     ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
!     ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
!     ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
!     ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
!     ECPT( 6) = MATERIAL ID                         MATID     INTEGER
!     ECPT( 7) = T                                   T         REAL
!     ECPT( 8) = NON-STRUCTURAL MASS                 FMU       REAL
!     ECPT( 9) = COORD. SYSTEM ID 1                  NECPT(9)  INTEGER
!     ECPT(10) = X1                                  X1        REAL
!     ECPT(11) = Y1                                  Y1        REAL
!     ECPT(12) = Z1                                  Z1        REAL
!     ECPT(13) = COORD. SYSTEM ID 2                  NECPT(13) INTEGER
!     ECPT(14) = X2                                  X2        REAL
!     ECPT(15) = Y2                                  Y2        REAL
!     ECPT(16) = Z2                                  Z2        REAL
!     ECPT(17) = COORD. SYSTEM ID 3                  NECPT(17) INTEGER
!     ECPT(18) = X3                                  X3        REAL
!     ECPT(19) = Y3                                  Y3        REAL
!     ECPT(20) = Z3                                  Z3        REAL
!     ECPT(21) = ELEMENT TEMPERATURE                 ELTEMP    REAL
!
   EQUIVALENCE (Consts(4),Degra) , (G(1),Tempar(19)) , (Ecpt(1),Necpt(1))
!
!     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
!
!     E(1), E(3), E(5) WILL BE THE I-VECTOR
!     E(2), E(4), E(6) WILL BE THE J-VECTOR
!     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
!
!     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
!
   E(1) = dble(X2) - dble(X1)
   E(3) = dble(Y2) - dble(Y1)
   E(5) = dble(Z2) - dble(Z1)
!
!     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
!
   Xsubb = dsqrt(E(1)**2+E(3)**2+E(5)**2)
   IF ( Xsubb>1.0D-06 ) THEN
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
      E(2) = dble(X3) - dble(X1)
      E(4) = dble(Y3) - dble(Y1)
      E(6) = dble(Z3) - dble(Z1)
!
!     X-SUB-C  =  I . (RSUBC - RSUBA),  THUS
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
      Ysubc = dsqrt(E(7)**2+E(8)**2+E(9)**2)
      IF ( Ysubc>1.0D-06 ) THEN
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
         Temp = dsqrt(E(2)**2+E(4)**2+E(6)**2)
         IF ( Temp/=0.0D0 ) THEN
!
            E(2) = E(2)/Temp
            E(4) = E(4)/Temp
            E(6) = E(6)/Temp
!
!     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
!
            Vol = Xsubb*Ysubc*dble(T)/2.0D0
            Reelmu = 1.0D0/Xsubb
            Flamda = 1.0D0/Ysubc
            Delta = Xsubc/Xsubb - 1.0D0
!
!     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
!         CSUBA = (3X2) STORED IN C( 1) THRU C( 6) BY ROWS
!         CSUBB = (3X2) STORED IN C( 7) THRU C(12) BY ROWS
!         CSUBC = (3X2) STORED IN C(13) THRU C(18) BY ROWS
!
            C(1) = -Reelmu
            C(2) = 0.0D0
            C(3) = 0.0D0
            C(4) = Flamda*Delta
            C(5) = C(4)
            C(6) = -Reelmu
            C(7) = Reelmu
            C(8) = 0.0D0
            C(9) = 0.0D0
            C(10) = -Flamda*Reelmu*Xsubc
            C(11) = C(10)
            C(12) = Reelmu
            C(13) = 0.0D0
            C(14) = 0.0D0
            C(15) = 0.0D0
            C(16) = Flamda
            C(17) = Flamda
            C(18) = 0.0D0
            IF ( Ntype/=1 ) THEN
!
               Theta = Angle*Degra
               Sinth = sin(Theta)
               Costh = cos(Theta)
            ENDIF
            IF ( abs(Sinth)<1.0E-06 ) Sinth = 0.0E0
            Matid = Matid1
            Inflag = -1
            CALL plamat
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
!     AT THIS POINT, G, E, AND C MATRICES ARE COMPLETE
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
!     FIRST LOCATE WHICH POINT IS THE PIVOT
!
            DO i = 1 , 3
               IF ( Ngrid(i)==Npvt ) THEN
                  Ka = 4*i + 5
                  Npoint = 6*i - 5
                  GOTO 10
               ENDIF
            ENDDO
!
!     FALLING THRU ABOVE LOOP INDICATES THE PIVOT POINT SPECIFIED BY
!     NPVT WAS NOT FOUND EQUAL TO ANY OF THE 3 GRID POINTS IN THE ECPT
!     THUS ERROR CONDITION.
!
            CALL mesage(-30,34,Ecpt(1))
!
!                     T
!     COMPUTE   E * C   * G       AND STORE IN TEMPAR( 1 THRU 9 )
!                    I
!
 10         CALL gmmatd(E,3,2,0,C(Npoint),3,2,1,Tempar(10))
            CALL gmmatd(Tempar(10),3,3,0,G,3,3,0,Tempar(1))
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
               Tempar(i) = Tempar(i)*Vol
            ENDDO
!
!     CHECK FOR PIVOT  CSID = 0,  IF ZERO SKIP TRANSFORMATION TSUBI.
!
            IF ( Necpt(Ka)/=0 ) THEN
!
!     NOT-ZERO THUS GET TI
!
               CALL transd(Necpt(Ka),Ti)
!
!     INTRODUCE TI INTO THE COMMON PRODUCT AND STORE AT
!     TEMPAR(10 THRU 18)
!
               CALL gmmatd(Ti,3,3,1,Tempar(1),3,3,0,Tempar(10))
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
!     FIRST ZERO OUT (6 X 6) K
!                             IJ
!
            Nsave = npt1
            DO i = 1 , 36
               Kij(i) = 0.0D0
            ENDDO
            Npoint = 0
!
            DO i = 1 , 3
               CALL gmmatd(C(6*i-5),3,2,0,E,3,2,1,Tempar(Nsave))
!
!                                                                 T
!     NPT2 IS SET TO POINT TO THE BEGINNING OF THE PRODUCT  C  * E  * T
!                                                            J         J
!
               npt2 = Nsave
               npt1 = 19
!
!     CHECK FOR ZERO CSID IN WHICH CASE TJ IS NOT NEEDED
!
               IF ( Necpt(4*i+5)/=0 ) THEN
!
!     COMMING HERE IMPLIES NEED FOR TJ
!     WILL STORE TJ IN TI
!
                  CALL transd(Necpt(4*i+5),Ti)
                  CALL gmmatd(Tempar(npt2),3,3,0,Ti,3,3,0,Tempar(19))
                  npt1 = npt2
                  npt2 = 19
               ENDIF
!
!  60 AT THIS POINT COMPLETE COMPUTATION FOR  K-SUB-I,J
!
               CALL gmmatd(Tempar(ncom),3,3,0,Tempar(npt2),3,3,0,Tempar(npt1))
!
               IF ( Ntype==0 ) THEN
!
                  Kij(1) = Tempar(npt1)
                  Kij(2) = Tempar(npt1+1)
                  Kij(3) = Tempar(npt1+2)
                  Kij(7) = Tempar(npt1+3)
                  Kij(8) = Tempar(npt1+4)
                  Kij(9) = Tempar(npt1+5)
                  Kij(13) = Tempar(npt1+6)
                  Kij(14) = Tempar(npt1+7)
                  Kij(15) = Tempar(npt1+8)
                  CALL pla4b(Kij(1),Necpt(i+1))
               ELSE
                  DO j = 1 , 9
                     Npoint = Npoint + 1
                     npt2 = npt1 + j - 1
                     Kij(Npoint) = Tempar(npt2)
                  ENDDO
               ENDIF
!
            ENDDO
         ELSE
            CALL mesage(30,26,Ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
            Nogo = 1
            RETURN
         ENDIF
      ELSE
         CALL mesage(30,32,Ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
         Nogo = 1
         RETURN
      ENDIF
   ELSE
      CALL mesage(30,31,Ecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
END SUBROUTINE pktrms
