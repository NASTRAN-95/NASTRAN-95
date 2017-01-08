       SUBROUTINE BREAD (IG,INV,II3,NORIG,KG)
C
C      THIS ROUTINE IS USED ONLY IN BANDIT MODULE
C      IT READS THE CONNECTING ELEMENTS AND GENEL ELEM. FROM GEOM2 FILE
C      AND PREPROCESS THE MPC CARDS AND THE RIGID ELEMENTS FROM GEOM4
C
C      REVISED BY G.CHAN/UNISYS
C      12/89, TO INCLUDE NEW RIGID ELEMENTS CRROD, CRBAR, CRTRPLT,
C      CRBE1, CREB2, CRBE3 AND CRSPLINE
C      03/92, TO INCLUDE DUMMY ELEMENTS, CDUM1,...,CDUM9
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL          DEBUG
      DIMENSION        SUB(2),   XXX(3),   IZ(3),    KG(7),    IG(1),
     1                 NORIG(1), INV(II3,1)
      CHARACTER        UFM*23,   UWM*25
      COMMON /XMSSG /  UFM,      UWM
      COMMON /BANDA /  IBUF1,    NOMPC
      COMMON /BANDB /  NBITIN,   KORE,     IFL,      NGRID,    IPNW(2),
     1                 KDIM
      COMMON /BANDD /  DUM6(6),  NEL,      NEQ,      NEQR
      COMMON /BANDS /  NN(10)
      COMMON /GEOMX /  GEOM1,    GEOM2,    GEOM4,    SCR1
      COMMON /NAMES /  RD,       RDREW,    WRT,      WRTREW,   REW
      COMMON /SYSTEM/  IBUF,     NOUT,     DUM43(43),KDUM(9)
      COMMON /GPTA1 /  NE,       LAST,     INCR,     KE(1)
      COMMON /ZZZZZZ/  Z(1)
      DATA             CRIGDR,   CRIGD1,   CRIGD2,   CRIGD3,   GENEL  /
     1                 8210,     5310,     5410,     8310,     4301   /
      DATA             CHBDY,    PLOTEL,   CRROD,    CRBAR,    CRTRPT /
     1                 4208,     5201,     6510,     6610,     6710   /
      DATA             CRBE1,    CRBE2,    CRBE3,    CRSPLN,   MSET   /
     1                 6810,     6910,     7010,     7110,     4HMSET /
      DATA             SUB,                MPC,      MAXMPC,   DEBUG  /
     1                 4HBREA,   4HD   ,   4901,     150,      .FALSE./
C
C
C     CHECK THE PRESENCE OF GEOM2 FILE
C
      KG(1) = GEOM2
      CALL RDTRL (KG(1))
      J = KG(2) + KG(3) + KG(4) + KG(5) + KG(6) + KG(7)
      IF (KG(1).LT.0 .OR. J.EQ.0) GO TO 370
      DO 10 I = 1,7
 10   KG(I) = 0
C
C     UPDATE /GPTA1/ IF DUMMY ELEMENTS ARE PRESENT
C
      DO 15 I = 1,9
      IF (KDUM(I) .EQ. 0) GO TO 15
      K = KDUM(I)/10000000
      L = (KDUM(I)-K*10000000)/10000
      J = (I+51)*INCR
      KE(J+ 6) = 2 + K + L
      KE(J+10) = K
 15   CONTINUE
C
C     CHECK THE PRESENCE OF MPC CARDS AND RIGID ELEMENTS.  SAVE THEIR
C     GRID DATA IN SCR1 FILE FOR TIGER AND UPDATE NEQ AND NEQR COUNTERS
C
      IF (NOMPC .EQ. 0) GO TO 200
      Z(1) = GEOM4
      CALL RDTRL (Z(1))
      J = 0
      DO 20 I = 2,7
 20   J = J + Z(I)
      IF (Z(1).LT.0 .OR. J.EQ.0) GO TO 200
C
      IBUF2 = IBUF1 - IBUF
      CALL OPEN (*290,SCR1,Z(IBUF2),WRTREW)
      IFILE  = GEOM4
      CALL PRELOC (*190,Z(IBUF1),GEOM4)
C
      IF (NOMPC .EQ. 1) GO TO 40
C
      XXX(1) = MPC
      XXX(2) = XXX(1)/100
      CALL LOCATE (*40,Z(IBUF1),XXX,J)
 25   J = 1
      CALL READ (*300,*40,GEOM4,IZ,1,0,M)
 30   J = J + 1
      CALL READ (*300,*40,GEOM4,KG(J),3,0,M)
      IF (KG(J) .NE. -1) IF (J+3-MAXMPC) 30,30,320
      J = J - 1
      KG(1) = J - 1
      CALL WRITE (SCR1,KG,J,1)
      NEQ = NEQ + 1
      GO TO 25
C
C     LOCATE ANY CRIGDR AND CRROD ELEMENTS, AND SAVE THE GRID DATA IN
C     SCR1. (DEPENDENT GRID FIRST, AND ONE INDEPENDENT GRID LAST)
C
C     FOR ALL RIGID ELEMENTS, THE FIRST WORD OF KG ARRAY CONTAINS
C     (NO. OF DEPENDENT + INDEP. GRIDS)*1000 + (NO. OF INDEP. GRIDS)
C     THE DATA IN SCR1 WILL BE PROCESSED BY TIGER
C
 40   IF (NOMPC .EQ. 3) GO TO 180
      XXX(1) = CRIGDR
 50   XXX(2) = XXX(1)/100
      CALL LOCATE (*60,Z(IBUF1),XXX,J)
 55   CALL READ (*300,*60,GEOM4,IZ,1,0,M)
      CALL READ (*300,*60,GEOM4,KG(3),3,0,M)
      KG(1) = 2*1000 + 1
      KG(2) = KG(4)
      CALL WRITE (SCR1,KG,3,1)
      NEQR  = NEQR + 1
      GO TO 55
C
 60   IF (XXX(1) .EQ. CRROD) GO TO 70
      XXX(1) = CRROD
      GO TO 50
C
C     LOCATE ANY CRIGD1, CRIGD2  AND CRBE2  ELEMENTS, AND SAVE GRID
C     DATA IN SCR1. PUT THE ONE INDEPENDENT GRID LAST
C
 70   XXX(1) = CRIGD1
 75   XXX(2) = XXX(1)/100
      CALL LOCATE (*90,Z(IBUF1),XXX,J)
 80   J = 1
      CALL READ (*300,*90,GEOM4,IZ,2,0,M)
      IZ2 = IZ(2)
 85   J = J + 1
      CALL READ (*300,*90,GEOM4,KG(J),1,0,M)
      CALL READ (*300,*90,GEOM4,   0,-6,0,M)
      IF (KG(J) .NE. -1) IF (J-MAXMPC) 85,85,320
      KG(J) = IZ2
      KG(1) = (J-1)*1000 + 1
      CALL WRITE (SCR1,KG,J,1)
      NEQR  = NEQR + 1
      GO TO 80
 90   IF (XXX(1) .EQ. CRBE2) GO TO 110
C
C     LOCATE ANY CRIGD2 ELEMENT
C
      IF (XXX(1) .EQ. CRIGD2) GO TO 100
      XXX(1) = CRIGD2
      GO TO 75
C
C     LOCATE ANY CRBE2 ELEMENT
C
 100  XXX(1) = CRBE2
      GO TO 75
C
C     LOCATE ANY CRIGD3, CRBE1, CRBAR AND CRTRPLT ELEMENTS, AND SAVE
C     GRID DATA IN SCR1 FILE. PUT THE INDEPENDENT GRID LAST
C
 110  XXX(1) = CRBAR
      ASSIGN 115 TO IRTN
      GO TO 130
 115  XXX(1) = CRTRPT
      ASSIGN 120 TO IRTN
      GO TO 130
 120  XXX(1) = CRBE1
      ASSIGN 125 TO IRTN
      GO TO 130
 125  XXX(1) = CRIGD3
      ASSIGN 150 TO IRTN
 130  XXX(2) = XXX(1)/100
      CALL LOCATE (*145,Z(IBUF1),XXX,J)
 133  J = 2
      K = 1
      CALL READ (*300,*145,GEOM4,IZ,1,0,M)
 135  CALL READ (*300,*145,GEOM4,IZ(K),1,0,M)
      IF (IZ(K) .EQ. MSET) GO TO 137
      CALL READ (*300,*145,GEOM4,0,-6,0,M)
      K = K + 1
      IF (K .GT. 999) GO TO 340
      GO TO 135
 137  CALL READ (*300,*145,GEOM4,KG(J),1,0,M)
      CALL READ (*300,*145,GEOM4,0,-6,0,M)
      IF (KG(J) .EQ. -1) GO TO 140
      J = J + 1
      IF (J .GT. MAXMPC) GO TO 320
      GO TO 137
 140  K = K - 1
      DO 142 I = 1,K
      KG(J) = IZ(I)
 142  J = J + 1
      J = J - 1
      KG(1) = (J-1)*1000 + K
      CALL WRITE (SCR1,KG,J,1)
      NEQR = NEQR + 1
      GO TO 133
C
C     LOCATE ANY CRSPLINE ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE.
C     PUT THE INDEPENDENT GRIDS LAST
 145  GO TO IRTN, (115,120,125,150)
C
C     LOCATE ANY CRBE3 ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE. PUT
C     THE INDEPENDENT GRID LAST
C
 150  XXX(1) = CRBE3
      XXX(2) = XXX(1)/100
      CALL LOCATE (*165,Z(IBUF1),XXX,J)
 151  CALL READ (*300,*165,GEOM4,IZ,3,0,M)
      IZ2 = IZ(2)
      J = 2
      CALL READ (*300,*165,GEOM4,0,-2,0,M)
 153  CALL READ (*300,*165,GEOM4,KG(J),1,0,M)
      K = -KG(J)
      IF (K .GT. 0) GO TO (155,157,160) K
      J = J + 1
      IF (J-MAXMPC) 153,153,320
 155  CALL READ (*300,*165,GEOM4,I,1,0,M)
      IF (I .EQ. -2) GO TO 157
      CALL READ (*300,*165,GEOM4,0,-1,0,M)
      GO TO 153
 157  CALL READ (*300,*165,GEOM4,KG(J),1,0,M)
      IF (KG(J) .LT. 0) GO TO 160
      CALL READ (*300,*165,GEOM4,0,-1,0,M)
      J = J + 1
      GO TO 157
 160  KG(J) = IZ2
      KG(1) = (J-1)*1000 + 1
      CALL WRITE (SCR1,KG,J,1)
      NEQR = NEQR + 1
      GO TO 151
C
C     LOCATE ANY CRSPLINE ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE.
C     PUT THE INDEPENDENT GRIDS LAST
C
 165  XXX(1) = CRSPLN
      XXX(2) = XXX(1)/100
      CALL LOCATE (*180,Z(IBUF1),XXX,J)
 167  CALL READ (*300,*180,GEOM4,IZ,3,0,M)
      K = 1
      IZ(K) = IZ(3)
      J = 1
 170  J = J + 1
 173  CALL READ (*300,*175,GEOM4,KG(J),2,0,M)
      IF (KG(J) .EQ. -1) GO TO 175
      IF (J+2 .GT. MAXMPC) GO TO 320
      IF (KG(J+1) .NE.  0) GO TO 170
      K = K + 1
      IF (K .GT. 999) GO TO 340
      IZ(K) = KG(J)
      GO TO 173
 175  DO 177 I = 1,K
      KG(J) = IZ(I)
 177  J = J + 1
      J = J - 1
      KG(1) = (J-1)*1000 + K
      CALL WRITE (SCR1,KG,J,1)
      NEQR = NEQR + 1
      GO TO 167
C
 180  DO 185 K = 1,MAXMPC
 185  KG(K) = 0
 190  CALL CLOSE (GEOM4,REW)
      CALL CLOSE (SCR1,REW)
C
C     PROCESS ELEMENT CARDS AND FILL UP CONNECTION TABLE IG
C
 200  IFILE = GEOM2
      CALL PRELOC (*300,Z(IBUF1),GEOM2)
      IELEM = 1 - INCR
 205  IELEM = IELEM + INCR
      IF (IELEM .GT. LAST) GO TO 250
      IF (KE(IELEM+3) .EQ. CHBDY ) GO TO 205
      IF (KE(IELEM+3) .EQ. PLOTEL) GO TO 205
      SCALAR = KE(IELEM+10)
      IF (SCALAR .EQ. -1) GO TO 205
      CALL LOCATE (*205,Z(IBUF1),KE(IELEM+3),J)
      NWDS  = KE(IELEM+ 5)
      NGPTS = KE(IELEM+ 9)
      NGPT1 = KE(IELEM+12)
      NCON  = NGPTS
 210  CALL READ (*300,*205,GEOM2,KG(1),NWDS,0,M)
      IF (SCALAR .EQ. 0) GO TO 220
      IF (KG(5).EQ.0 .OR. KG(6).EQ.0) GO TO 210
C     THE ABOVE CONDITIONS HOLD TRUE FOR CDAMPI, CELASI, AND CMASSI
C     WHERE I = 1,2
 220  NEL = NEL + 1
      CALL SCAT (KG(NGPT1),NCON,INV,II3,NORIG)
      IF (NGRID .EQ. -1) GO TO 270
      IF (NCON  .LE.  1) GO TO 240
      NGPT2 = NGPT1 + NCON - 1
      K = NGPT2 - 1
      DO 230 I = NGPT1,K
      L = I + 1
      DO 230 J = L,NGPT2
 230  CALL SETIG (KG(I),KG(J),IG,NORIG)
 240  IF (IELEM-LAST) 210,210,255
C
C     SPECIAL TREATMENT FOR GENERAL ELEM.
C     (LIMITED TO KDIM*4 GRID POINTS PER GENEL)
C
 250  XXX(1) = GENEL
      XXX(2) = XXX(1)/100
      CALL LOCATE (*270,Z(IBUF1),XXX,J)
      KDIM4  = KDIM*4
 255  NTOT   = 0
      CALL READ (*300,*270,GEOM2,K,1,0,M)
      K    = 0
      KGPV = 0
      GO TO 263
 260  IF (KG(NCON) .EQ. KGPV) GO TO 265
      KGPV = KG(NCON)
 263  NTOT = NTOT + 1
      IF (NTOT .LT. KDIM4) NCON = NTOT
 265  CALL READ (*300,*270,GEOM2,KG(NCON),2,0,M)
      IF (KG(NCON) .NE. -1) IF (KG(NCON+1)) 260,  265,  260
C                                           GRD  SCALAR GRD
C                                           PT.   PT.   PT.
      K = K + 1
      XXX(K) = KG(NCON+1)
      IF (K .LT. 2) GO TO 265
      NCON = NCON - 1
      M    = XXX(1)
      NWDS = 1 + (M*M-M)/2 + M
      CALL READ (*300,*270,GEOM2,K,-NWDS,0,M)
      CALL READ (*300,*270,GEOM2,K,   1,0,M)
      NGPT1 = 1
      IF (K .EQ. 0) GO TO 220
      NWDS  = M*XXX(2)
      CALL READ (*300,*270,GEOM2,K,-NWDS,0,M)
      GO TO 220
 270  CALL CLOSE (GEOM2,REW)
      IF (NTOT .GT. KDIM4) GO TO 330
      IF (.NOT.DEBUG) RETURN
C
      M = NN(1)
      WRITE  (NOUT,280) NN
      WRITE  (NOUT,285) ((INV(I,J),J=1,2),I=1,M)
 280  FORMAT (//21H /BANDS/ FROM BREAD =,10I8)
 285  FORMAT (/12H TABLE INV =,(/10X,2I8))
      RETURN
C
 290  IFILE = SCR1
 300  CALL MESAGE (-1,IFILE,SUB)
 320  WRITE  (NOUT,325) UWM,IZ(1),MAXMPC
 325  FORMAT (A25,', MPC SET (OR CRIGID ID)',I9,
     1        ' IS TOO LONG,  ONLY THE FIRST',I4, /5X,
     2        ' GRID POINTS ARE USED IN THE BANDIT COMPUTATION')
      GO TO 180
 330  WRITE  (NOUT,335) UFM,NTOT
 335  FORMAT (A23,', GENEL ELEMENT HAS TOO MANY GRID POINTS,',I7)
      J = NTOT/400 + 1
      IF (J .LE. 9) WRITE (NOUT,336) J
 336  FORMAT (5X,'USER NEEDS TO ADD A ''NASTRAN BANDTDIM=',I1,
     1       ''' CARD AND RERUN JOB')
      GO TO 350
 340  WRITE  (NOUT,345)
 345  FORMAT ('0*** MORE THAN 1000 INDEPENDENT GRID POINTS USED IN A ',
     1        'RIGID ELEMENT')
 350  CALL MESAGE (-61,0,0)
C
 370  NGRID = 0
      RETURN
      END
