
SUBROUTINE gfsmrg(Filea,File11,File21,File12,File22,Rpart,Cpart)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Core(1) , Ia(7) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Lcore , Lcr , Nsub0 , Nsub1 , Rule
   COMMON /parmeg/ Ia , Ia11 , Ia21 , Ia12 , Ia22 , Lcr , Rule
   COMMON /patx  / Lcore , Nsub0 , Nsub1
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Cpart , File11 , File12 , File21 , File22 , Filea , Rpart
!
! Local variable declarations
!
   INTEGER cp(7) , i , name(2) , rp(7)
   INTEGER korsz
!
! End of declarations
!
!
!     GENERAL MATRIX MERGE ROUTINE
!
!
!                  --               --
!                  I        I        I
!                  I FILE11 I FILE12 I   --     --
!                  I        I        I   I       I
!                  I-----------------I = I FILEA I
!                  I        I        I   I       I
!                  I FILE21 I FILE22 I   --     --
!                  I        I        I
!                  --               --
!
!        WHERE
!
!             RPART - ROW PARTITIONING VECTOR
!             CPART - COLUMN PARTITION VECTOR
!
!
!
!     OPEN CORE
!
!
!     CALCV COMMON BLOCK
!
!
!     PARTITION - MERGE COMMON BLOCK
!
!
   DATA name/4HGFSM , 4HRG  /
!
!
!     GET TRAILERS FOR INPUTS
!
   rp(1) = Rpart
   IF ( Rpart/=0 ) CALL rdtrl(rp)
   cp(1) = Cpart
   IF ( Cpart/=0 ) CALL rdtrl(cp)
!
   DO i = 2 , 7
      Ia(i) = 0
      Ia11(i) = 0
      Ia12(i) = 0
      Ia21(i) = 0
      Ia22(i) = 0
   ENDDO
!
   Ia11(1) = File11
   IF ( File11/=0 ) CALL rdtrl(Ia11)
   IF ( Ia11(1)<0 ) Ia11(1) = 0
   Ia12(1) = File12
   IF ( File12/=0 ) CALL rdtrl(Ia12)
   IF ( Ia12(1)<0 ) Ia12(1) = 0
   Ia21(1) = File21
   IF ( File21/=0 ) CALL rdtrl(Ia21)
   IF ( Ia21(1)<0 ) Ia21(1) = 0
   Ia22(1) = File22
   IF ( File22/=0 ) CALL rdtrl(Ia22)
   IF ( Ia22(1)<0 ) Ia22(1) = 0
!
!     SET UP MATRIX CONTROL BLOCK FOR OUTPUT
!
   Ia(1) = Filea
   Ia(4) = 2
   IF ( Rpart/=0 .AND. Cpart/=0 ) Ia(4) = 1
   Ia(5) = max0(Ia11(5),Ia12(5),Ia21(5),Ia22(5))
!
!     SET UP DUMMY PARTITION VECTOR
!
   i = 0
   Core(1) = 0
   Core(i+2) = 1
   Core(i+3) = Ia(2)
   Core(i+4) = 2
   Core(i+5) = 1
   Core(i+6) = 0
!
   Rule = 0
   Lcr = korsz(Core)
!
   IF ( Rpart==0 ) THEN
!
!     COLUMN MERGE
!
      IF ( Cpart==0 ) THEN
!
!     ILLEGAL INPUT - NO PARTITION VECTOR
!
         CALL mesage(-7,0,name)
         GOTO 99999
      ELSE
         Ia(2) = max0(Ia11(2),Ia21(2))
         Ia(3) = Nsub0 + Nsub1
         CALL merge(Core,cp,Core)
      ENDIF
   ELSEIF ( Cpart==0 ) THEN
!
!     ROW MERGE
!
      Ia(2) = Nsub0 + Nsub1
      Ia(3) = max0(Ia11(3),Ia12(3))
      CALL merge(rp,Core,Core)
   ELSE
!
!     FULL MERGE
!
      Ia(2) = Nsub0 + Nsub1
      Ia(3) = Ia(2)
      CALL merge(rp,cp,Core)
   ENDIF
!
!     WRITE TRIALER FOR OUTPUT
!
   CALL wrttrl(Ia)
!
   RETURN
99999 RETURN
END SUBROUTINE gfsmrg
