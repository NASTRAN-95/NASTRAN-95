
SUBROUTINE pktrm1(Ntype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Angle , C(18) , Consts(5) , Costh , Degra , Delta , Dumb(80) , Dumcl(149) , Dummy(244) , Dummy1 , Dummy2 , Dummy3 , E(18) , &
      & Ecpt(4) , Eltemp , Flamda , Fmu , G(9) , G11 , G12 , G13 , G22 , G23 , G33 , Ph1out(200) , Reelmu , Sinth , Stress , T ,    &
      & Temp , Tempar(27) , Theta , Ti(9) , Vol , X1 , X2 , X3 , Xsubb , Xsubc , Y1 , Y2 , Y3 , Ysubc , Z1 , Z2 , Z3
   INTEGER Inflag , Matid , Matid1 , Necpt(1) , Ngrid(3) , Nogo
   COMMON /condas/ Consts
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33
   COMMON /pla42c/ Dumcl , Nogo
   COMMON /pla42s/ C , E , Ti , Tempar , Temp , Xsubb , Xsubc , Ysubc , Vol , Reelmu , Delta , Flamda , Theta , Dummy
   COMMON /pla4es/ Necpt , Ngrid , Angle , Matid1 , T , Fmu , Dummy1 , X1 , Y1 , Z1 , Dummy2 , X2 , Y2 , Z2 , Dummy3 , X3 , Y3 ,    &
                 & Z3 , Dumb , Ph1out
!
! Dummy argument declarations
!
   INTEGER Ntype
!
! Local variable declarations
!
   INTEGER i , j , npt1
!
! End of declarations
!
!  THIS ROUTINE CALCULATES PHASE I OUTPUT FOR PLA4
!  BOTH FOR THE TRI-MEMBRANE AND SUB-CALCULATIONS FOR THE QUAD MEMBRANE
!
!     ******** PHASE I OF STRESS DATA RECOVERY *************************
!     ******** TRIANGULAR MEMBRANE ELEMENT *****************************
!
!     CALLS FROM THIS ROUTINE ARE MADE TO. . .
!
!     PLAMAT - RETURNS STANDARD GP MATRIS ROTATED
!     TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
!     GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
!     MESAGE - ERROR MESSAGE WRITER
!
!     IF NTYPE = 0 TRI-MEMBRANE CALCULATIONS WILL BE DONE
!
!     IF NTYPE = 1 QUAD-MEMBRANE CALCULATIONS WILL BE DONE
!
!
!
!
   EQUIVALENCE (Consts(4),Degra)
   EQUIVALENCE (G(1),Tempar(19)) , (Ecpt(1),Necpt(1))
!
!     ECPT LIST
!                                                      IN
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
!     ******************************************************************
!
!     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
!
!     E(1), E(3), E(5) WILL BE THE I-VECTOR
!     E(2), E(4), E(6) WILL BE THE J-VECTOR
!     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
!
!     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
   E(1) = X2 - X1
   E(3) = Y2 - Y1
   E(5) = Z2 - Z1
!
!     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
   Xsubb = sqrt(E(1)**2+E(3)**2+E(5)**2)
   IF ( Xsubb>1.0E-06 ) THEN
!
!     NOW NORMALIZE I-VECTOR WITH X-SUB-B
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
!     X-SUB-C  =  I . (RSUBC - RSUBA) ,  THUS
      Xsubc = E(1)*E(2) + E(3)*E(4) + E(5)*E(6)
!
!     AND CROSSING THE I-VECTOR TO (RSUBC-RSUBA) GIVES THE K-VECTOR
!     (NON-NORMALIZED)
!
      E(7) = E(3)*E(6) - E(5)*E(4)
      E(8) = E(5)*E(2) - E(1)*E(6)
      E(9) = E(1)*E(4) - E(3)*E(2)
!
!
!     THE LENGTH OF THE K-VECTOR IS NOW FOUND AND EQUALS Y-SUB-C
!     COORD. IN ELEMENT SYSTEM
      Ysubc = sqrt(E(7)**2+E(8)**2+E(9)**2)
      IF ( Ysubc>1.0E-06 ) THEN
!
!     NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
!
         E(7) = E(7)/Ysubc
         E(8) = E(8)/Ysubc
         E(9) = E(9)/Ysubc
!
!     NOW HAVING I AND K VECTORS.GET J = I CROSS K AND
!     STORE IN THE SPOT FOR J
!
         E(2) = E(5)*E(8) - E(3)*E(9)
         E(4) = E(1)*E(9) - E(5)*E(7)
         E(6) = E(3)*E(7) - E(1)*E(8)
!
!     AND JUST FOR COMPUTER EXACTNESS NORMALIZE J-VECTOR TO MAKE SURE.
         Temp = sqrt(E(2)**2+E(4)**2+E(6)**2)
         E(2) = E(2)/Temp
         E(4) = E(4)/Temp
         E(6) = E(6)/Temp
!
!     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
!
         Reelmu = 1.0E0/Xsubb
         Flamda = 1.0E0/Ysubc
         Delta = Xsubc/Xsubb - 1.0E0
!
!     ******************************************************************
!
!     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
!                 CSUBA = (3X2) STORED IN C(1) . . .C(6)  BY ROWS
!                 CSUBB = (3X2) STORED IN C(7) . . .C(12) BY ROWS
!                 CSUBC = (3X2) STORED IN C(13). . .C(18) BY ROWS
!
         C(1) = -Reelmu
         C(2) = 0.0E0
         C(3) = 0.0E0
         C(4) = Flamda*Delta
         C(5) = C(4)
         C(6) = -Reelmu
         C(7) = Reelmu
         C(8) = 0.0E0
         C(9) = 0.0E0
         C(10) = -Flamda*Reelmu*Xsubc
         C(11) = C(10)
         C(12) = Reelmu
         C(13) = 0.0E0
         C(14) = 0.0E0
         C(15) = 0.0E0
         C(16) = Flamda
         C(17) = Flamda
         C(18) = 0.0E0
!
         IF ( Ntype/=1 ) THEN
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
!     ******************************************************************
!
!     G, E, AND C MATRICES ARE COMPLETE
!
!
!
!                           T
!     COMPUTE  S  = G  C   E   T   , I = 1,2,3.
!               I       I       I
!
         DO i = 1 , 3
!
!     POINTER TO C   = 6*I - 5
!                 I
!
            CALL gmmats(G,3,3,0,C(6*i-5),3,2,0,Tempar(1))
            CALL gmmats(Tempar(1),3,2,0,E,3,2,1,Tempar(10))
!
!     DO WE NEED TRANSFORMATION TI
!
            IF ( Necpt(4*i+5)==0 ) THEN
               npt1 = 9*i
               DO j = 10 , 18
                  npt1 = npt1 + 1
                  Ph1out(npt1) = Tempar(j)
               ENDDO
            ELSE
               CALL transs(Necpt(4*i+5),Ti)
               CALL gmmats(Tempar(10),3,3,0,Ti,3,3,0,Ph1out(9*i+1))
            ENDIF
         ENDDO
         Ph1out(1) = Ecpt(1)
         Ph1out(2) = Ecpt(2)
         Ph1out(3) = Ecpt(3)
         Ph1out(4) = Ecpt(4)
      ELSE
         CALL mesage(30,32,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
         Nogo = 1
         RETURN
      ENDIF
   ELSE
      CALL mesage(30,31,Ecpt(1))
!
!  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
!
      Nogo = 1
      RETURN
   ENDIF
!
!     THIS CONCLUDES PHASE 1 FOR TRIANGULAR MEMBRANE OR SUB CALCULATION
!     TO ANOTHER ROUTINE...
!
END SUBROUTINE pktrm1
