!*==gfsmrg.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsmrg(Filea,File11,File21,File12,File22,Rpart,Cpart)
   IMPLICIT NONE
   USE c_parmeg
   USE c_patx
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Filea
   INTEGER :: File11
   INTEGER :: File21
   INTEGER :: File12
   INTEGER :: File22
   INTEGER :: Rpart
   INTEGER :: Cpart
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: cp , rp
   INTEGER :: i
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
      ia(i) = 0
      ia11(i) = 0
      ia12(i) = 0
      ia21(i) = 0
      ia22(i) = 0
   ENDDO
!
   ia11(1) = File11
   IF ( File11/=0 ) CALL rdtrl(ia11)
   IF ( ia11(1)<0 ) ia11(1) = 0
   ia12(1) = File12
   IF ( File12/=0 ) CALL rdtrl(ia12)
   IF ( ia12(1)<0 ) ia12(1) = 0
   ia21(1) = File21
   IF ( File21/=0 ) CALL rdtrl(ia21)
   IF ( ia21(1)<0 ) ia21(1) = 0
   ia22(1) = File22
   IF ( File22/=0 ) CALL rdtrl(ia22)
   IF ( ia22(1)<0 ) ia22(1) = 0
!
!     SET UP MATRIX CONTROL BLOCK FOR OUTPUT
!
   ia(1) = Filea
   ia(4) = 2
   IF ( Rpart/=0 .AND. Cpart/=0 ) ia(4) = 1
   ia(5) = max0(ia11(5),ia12(5),ia21(5),ia22(5))
!
!     SET UP DUMMY PARTITION VECTOR
!
   i = 0
   core(1) = 0
   core(i+2) = 1
   core(i+3) = ia(2)
   core(i+4) = 2
   core(i+5) = 1
   core(i+6) = 0
!
   rule = 0
   lcr = korsz(core)
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
         ia(2) = max0(ia11(2),ia21(2))
         ia(3) = nsub0 + nsub1
         CALL merge(core,cp,core)
      ENDIF
   ELSEIF ( Cpart==0 ) THEN
!
!     ROW MERGE
!
      ia(2) = nsub0 + nsub1
      ia(3) = max0(ia11(3),ia12(3))
      CALL merge(rp,core,core)
   ELSE
!
!     FULL MERGE
!
      ia(2) = nsub0 + nsub1
      ia(3) = ia(2)
      CALL merge(rp,cp,core)
   ENDIF
!
!     WRITE TRIALER FOR OUTPUT
!
   CALL wrttrl(ia)
!
99999 END SUBROUTINE gfsmrg
