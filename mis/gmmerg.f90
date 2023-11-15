
SUBROUTINE gmmerg(Filea,File11,File21,File12,File22,Rpart,Cpart,Nsub,Mrgtyp,Core,Lcore)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ia(7) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Lcr , Rule
   COMMON /parmeg/ Ia , Ia11 , Ia21 , Ia12 , Ia22 , Lcr , Rule
!
! Dummy argument declarations
!
   INTEGER Cpart , File11 , File12 , File21 , File22 , Filea , Lcore , Mrgtyp , Rpart
   INTEGER Core(6) , Nsub(4)
!
! Local variable declarations
!
   INTEGER cp(7) , i , name(2) , rp(7)
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
!             NSUB(1) - NUMBER OF COLUMNS IN RPART 0 SUBSET
!             NSUB(2) - NUMBER OF COLUMNS IN RPART 1 SUBSET
!             NSUB(3) - NUMBER OF ROWS IN CPART 0 SUBSET
!             NSUB(4) - NUMBER OF ROWS IN CPART 1 SUBSET
!             MRGTYP - MERGE TYPE (1 .EQ. SQUARE, 2 .EQ. RECTANGULAR)
!             CPART - COLUMN PARTITION VECTOR
!
!
!
!
!
   DATA name/4HGMME , 4HRG  /
!
!***********************************************************************
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
   Ia(4) = Mrgtyp
   Ia(5) = max0(Ia11(5),Ia12(5),Ia21(5),Ia22(5))
!
!     SET UP DUMMY PARTITION VECTOR
!
   Core(1) = 0
   Core(2) = 1
   Core(3) = Ia(2)
   Core(4) = 2
   Core(5) = 1
   Core(6) = 0
   Lcr = Lcore
   Rule = 0
!
   IF ( Rpart==0 ) THEN
!
!  *  *  MERGE ROWS ONLY
!
      IF ( Cpart==0 ) THEN
!
!     ILLEGAL INPUT - NO PARTITION VECTOR
!
         CALL mesage(-7,0,name)
         GOTO 99999
      ELSE
         Ia(2) = max0(Ia11(2),Ia21(2))
         Ia(3) = Nsub(3) + Nsub(4)
         CALL merge(Core,cp,Core)
      ENDIF
   ELSEIF ( Cpart==0 ) THEN
!
!  *  *  MERGE COLUMNS ONLY
!
      Ia(2) = Nsub(1) + Nsub(2)
      Ia(3) = max0(Ia11(3),Ia12(3))
      CALL merge(rp,Core,Core)
   ELSE
!
!     FULL MERGE
!
      Ia(2) = Nsub(1) + Nsub(2)
      Ia(3) = Nsub(3) + Nsub(4)
      CALL merge(rp,cp,Core)
   ENDIF
!
!     WRITE TRIALER FOR OUTPUT
!
   CALL wrttrl(Ia)
!
   RETURN
99999 END SUBROUTINE gmmerg
