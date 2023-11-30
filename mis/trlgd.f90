
SUBROUTINE trlgd(Fct,Fco,Ap,As,Ad,Ah,Ppo,Pso,Pdo,Pdt,Pht,Iflag1,Scr1,Iflag)
   IMPLICIT NONE
   INTEGER Iprec , Iskip(54)
   COMMON /system/ Iskip , Iprec
   INTEGER Ad , Ah , Ap , As , Fco , Fct , Iflag , Iflag1 , Pdo , Pdt , Pht , Ppo , Pso , Scr1
   INTEGER mcb(7) , prec , sign , trnsp
!
!     THE PURPOSE OF THIS SUBROUTINE IS TO COMPUTE LOAD FACTORS
!         BOTH AT APPLIED TIMES(T) AND OUTPUT TIMES(O).
!
!     INPUTS (6)
!
!       FCT --MATRIX OF TIME FUNCTIONS--ALL TIMES
!       FCO --MATRIX OF TIME FUNCTIONS--OUTPUT TIMES
!       IFLAG =-1   IMPLIES FCT = FCO AND ONLY FCT EXISTS
!         NOTE THAT ALSO IMPLIES PDO = PDT
!       AP,AS,AD,AH  ARE TRANSFORMATION MATRICIES TO P,S,D,AND H SET RES
!
!     OUTPUTS (5)
!       PPO,PSO,PDO  LOADS AT OUTPUT TIMES(ANY MAY NOT EXIST)
!       PDT,PHT      LOADS AT ALL TIMES (ANY MAY NOT EXIST)
!
!     SCR1          SCRATCH FILE FOR MPYAD
!
!     IFLAG1 =-1  IMPLIES THAT AP = AD
!
!     FCT MAY BE FCO, IN CASE OF EQUALITY THE T FILES WILL EXIST
!
!
!
   sign = +1
   trnsp = 0
   prec = Iprec
!
!     FORM PPO
!
   mcb(1) = Ppo
   CALL rdtrl(mcb)
   IF ( mcb(1)>0 ) THEN
      CALL ssg2b(Ap,Fco,0,Ppo,trnsp,prec,sign,Scr1)
      mcb(1) = Ppo
      CALL rdtrl(mcb)
      mcb(2) = mcb(2) - 1
      CALL wrttrl(mcb)
   ENDIF
!
!     FORM   PSO
!
   mcb(1) = Pso
   CALL rdtrl(mcb)
   IF ( mcb(1)>0 ) THEN
      mcb(1) = As
      CALL rdtrl(mcb)
      IF ( mcb(2)>0 ) THEN
         CALL ssg2b(As,Fco,0,Pso,trnsp,prec,sign,Scr1)
         mcb(1) = Pso
         CALL rdtrl(mcb)
         mcb(2) = mcb(2) - 1
         CALL wrttrl(mcb)
      ENDIF
   ENDIF
!
!     BUILD PDO
!
   IF ( Iflag1/=-1 ) THEN
      mcb(1) = Pdo
      CALL rdtrl(mcb)
      IF ( mcb(1)>0 ) THEN
         CALL ssg2b(Ad,Fco,0,Pdo,trnsp,prec,sign,Scr1)
         mcb(1) = Pdo
         CALL rdtrl(mcb)
         mcb(2) = mcb(2) - 1
         CALL wrttrl(mcb)
      ENDIF
   ENDIF
!
!     BUILD PDT
!
   mcb(1) = Pdt
   CALL rdtrl(mcb)
   IF ( mcb(1)>0 ) CALL ssg2b(Ad,Fct,0,Pdt,trnsp,prec,sign,Scr1)
!
!     BUILD PHT
!
   mcb(1) = Pht
   CALL rdtrl(mcb)
   IF ( mcb(1)>0 ) CALL ssg2b(Ah,Fct,0,Pht,trnsp,prec,sign,Scr1)
END SUBROUTINE trlgd