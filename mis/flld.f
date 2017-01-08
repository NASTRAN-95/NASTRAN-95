      SUBROUTINE FLLD (X01,X02,Y0,Z0,SGR,CGR,SGS,CGS,KR,CBAR,FMACH,E,
     1                 L,KD1R,KD1I,KD2R,KD2I)
C
C     CALCULATION OF THE NUMERATOR OF A DOUBLET LINE OF FINITE LENGTH.
C     LIKE KERN, THERE ARE TWO OUTPUT COMPLEX VALUES REPRESENTED BY
C     FOUR REAL NUMBERS AND AN INPUT OPTION.
C
C     WRITTEN BY D. H. LARSON, STRUCTURAL MECHANICS MDAC 11/70
C
C     X01  -   X - XI1
C     X02  -   X - XI2
C     Y0   -   Y - ETA
C     Z0   -   Z - ZETA
C     SGR  -   SIN ( GAMMA-R)
C     CGR  -   COS ( GAMMA-R)
C     SGS  -   SIN ( GAMMA-S)
C     CGS  -   COS ( GAMMA-S)
C     KR   -   REDUCED FREQUENCY
C     BR   -   REFERENCE LENGTH
C     FMACH-   MACH NUMBER
C     E    -
C     L    -   OPTION FLAG USED IN TKER
C     KD1R -   REAL PART OF  KD1
C     KD1I -   IMAGINARY PART OF KD1
C     KD2R -   REAL PART OF  KD2
C     KD2I -   IMAGINARY PART OF KD2
C
      REAL           KR,KK1R,KK1I,KK2R,KK2I,KD1R,KD1I,KD2R,KD2I,K10T1,
     1               K20T2P,K1RT1,K10,K2IT2P,K20,K2RT2P,K1IT1
      COMPLEX        KD1,KD2,K1XI1,K1XI2,TEMP1,TEMP2,K2XI1,K2XI2
      COMMON  /KDS/  IND,KK1R,KK1I,KK2R,KK2I
      COMMON  /DLM/  K10,K20,K1RT1,K1IT1,K2RT2P,K2IT2P,K10T1,K20T2P
C
C     X01 = X-XI1  AND  X02 = X-XI2, DELXI = XI2-XI1
C
      DELXI = X01 - X02
C
C     FULL KERNEL FROM -TKER-
C
      IND  = 0
      KD1R = 0.0
      KD2R = 0.0
      T1   = KR*DELXI/CBAR
      BR   = CBAR/2.0
      ST1  = SIN(T1)
      CT1  = COS(T1)
      I    = 1
      X0   = X01
C
   10 CALL TKER (X0,Y0,Z0,KR,BR,SGR,CGR,SGS,CGS,RT1,RT2,FMACH)
C
      GO TO (30,40), I
   30 K1XI1 = CMPLX(KK1R,KK1I)
      K2XI1 = CMPLX(KK2R,KK2I)
      IF (L .EQ. 0) GO TO 35
      KD1R  = KD1R - K10T1
      KD2R  = KD2R - K20T2P
   35 CONTINUE
C
C     NOW GO CALCULATE FOR XI = XI2
C
      X0 = X02
      I  = 2
      GO TO 10
C
   40 K1XI2 = CMPLX(KK1R,KK1I)
      K2XI2 = CMPLX(KK2R,KK2I)
      IF (L .EQ. 0) GO TO 50
      KD1R  = KD1R + K10T1
      KD2R  = KD2R + K20T2P
   50 CONTINUE
C
      TEMP1 = CMPLX(CT1, ST1)
      TEMP2 = CMPLX(CT1,-ST1)
C
C     DESIRED RESULTS (COMPLEX)
C
      KD1   = K1XI1*TEMP1 - K1XI2*TEMP2
      KD2   = K2XI1*TEMP1 - K2XI2*TEMP2
C
C     CONVERT TO REAL AND IMAGINARY PARTS
C
      KD1R  = REAL (KD1) + KD1R
      KD1I  = AIMAG(KD1)
      KD2R  = REAL (KD2) + KD2R
      KD2I  = AIMAG(KD2)
      RETURN
      END
