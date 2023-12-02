!*==trlgd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trlgd(Fct,Fco,Ap,As,Ad,Ah,Ppo,Pso,Pdo,Pdt,Pht,Iflag1,Scr1,Iflag)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Fct
   INTEGER :: Fco
   INTEGER :: Ap
   INTEGER :: As
   INTEGER :: Ad
   INTEGER :: Ah
   INTEGER :: Ppo
   INTEGER :: Pso
   INTEGER :: Pdo
   INTEGER :: Pdt
   INTEGER :: Pht
   INTEGER :: Iflag1
   INTEGER :: Scr1
   INTEGER :: Iflag
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: mcb
   INTEGER :: prec , sign , trnsp
   EXTERNAL rdtrl , ssg2b , wrttrl
!
! End of declarations rewritten by SPAG
!
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
   prec = iprec
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
