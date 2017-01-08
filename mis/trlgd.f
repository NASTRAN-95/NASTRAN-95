      SUBROUTINE TRLGD(FCT,FCO,AP,AS,AD,AH,
     1     PPO,PSO,PDO,PDT,PHT,IFLAG1,SCR1,IFLAG      )
C
C     THE PURPOSE OF THIS SUBROUTINE IS TO COMPUTE LOAD FACTORS
C         BOTH AT APPLIED TIMES(T) AND OUTPUT TIMES(O).
C
C     INPUTS (6)
C
C       FCT --MATRIX OF TIME FUNCTIONS--ALL TIMES
C       FCO --MATRIX OF TIME FUNCTIONS--OUTPUT TIMES
C       IFLAG =-1   IMPLIES FCT = FCO AND ONLY FCT EXISTS
C         NOTE THAT ALSO IMPLIES PDO = PDT
C       AP,AS,AD,AH  ARE TRANSFORMATION MATRICIES TO P,S,D,AND H SET RES
C
C     OUTPUTS (5)
C       PPO,PSO,PDO  LOADS AT OUTPUT TIMES(ANY MAY NOT EXIST)
C       PDT,PHT      LOADS AT ALL TIMES (ANY MAY NOT EXIST)
C
C     SCR1          SCRATCH FILE FOR MPYAD
C
C     IFLAG1 =-1  IMPLIES THAT AP = AD
C
C     FCT MAY BE FCO, IN CASE OF EQUALITY THE T FILES WILL EXIST
C
      INTEGER  FCT,FCO,AP,AS,AD,AH,PPO,PSO,PDT,PHT,SCR1,MCB(7)
     1, TRNSP,SIGN,PREC,PDO
C
      COMMON /SYSTEM/ISKIP(54),IPREC
C
      SIGN = +1
      TRNSP = 0
      PREC = IPREC
C
C     FORM PPO
C
      MCB(1) = PPO
      CALL RDTRL(MCB)
      IF(MCB(1) .LE. 0) GO TO 10
      CALL SSG2B(AP,FCO,0,PPO,TRNSP,PREC,SIGN,SCR1)
      MCB(1) = PPO
      CALL RDTRL(MCB)
      MCB(2) = MCB(2) -1
      CALL WRTTRL(MCB)
   10 CONTINUE
C
C     FORM   PSO
C
      MCB(1) = PSO
      CALL RDTRL(MCB)
      IF (MCB(1) .LE. 0) GO TO 20
      MCB(1) = AS
      CALL RDTRL(MCB)
      IF (MCB(2) .LE. 0)  GO TO 20
      CALL SSG2B(AS,FCO,0,PSO,TRNSP,PREC,SIGN,SCR1)
      MCB(1) = PSO
      CALL RDTRL(MCB)
      MCB(2) = MCB(2) -1
      CALL WRTTRL(MCB)
   20 CONTINUE
C
C     BUILD PDO
C
      IF(IFLAG1 .EQ. -1) GO TO 30
      MCB(1) = PDO
      CALL RDTRL(MCB)
      IF (MCB(1) .LE. 0) GO TO 30
      CALL SSG2B(AD,FCO,0,PDO,TRNSP,PREC,SIGN,SCR1)
      MCB(1) = PDO
      CALL RDTRL(MCB)
      MCB(2) = MCB(2) -1
      CALL WRTTRL(MCB)
   30 CONTINUE
C
C     BUILD PDT
C
      MCB(1) = PDT
      CALL RDTRL(MCB)
      IF (MCB(1) .LE. 0) GO TO 40
      CALL SSG2B(AD,FCT,0,PDT,TRNSP,PREC,SIGN,SCR1)
   40 CONTINUE
C
C     BUILD PHT
C
      MCB(1) = PHT
      CALL RDTRL(MCB)
      IF(MCB(1) .LE. 0) GO TO 50
      CALL SSG2B(AH,FCT,0,PHT,TRNSP,PREC,SIGN,SCR1)
   50 CONTINUE
      RETURN
      END
