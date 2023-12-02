!*==gfsptn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsptn(Filea,File11,File21,File12,File22,Rpart,Cpart)
   IMPLICIT NONE
   USE C_PARMEG
   USE C_PATX
   USE C_ZZZZZZ
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
!     GENERAL MATRIX PARTION ROUTINE
!
!
!                              --               --
!                              I        I        I
!                  --     --   I FILE11 I FILE12 I
!                  I       I   I        I        I
!                  I FILEA I = I-----------------I
!                  I       I   I        I        I
!                  --     --   I FILE21 I FILE22 I
!                              I        I        I
!                              --               --
!
!        WHERE
!
!             RPART - ROW PARTITIONING VECTOR
!             CPART - COLUMN PARTITION VECTOR
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
   DATA name/4HGFSP , 4HTN  /
!
!
!     GET TRAILERS FOR INPUTS
!
   rp(1) = Rpart
   IF ( Rpart/=0 ) CALL rdtrl(rp)
   cp(1) = Cpart
   IF ( Cpart/=0 ) CALL rdtrl(cp)
   Ia(1) = Filea
   CALL rdtrl(Ia)
!
!     SET UP MATRIX CONTROL BLOCKS FOR OUTPUTS
!
   Ia11(1) = File11
   Ia12(1) = File12
   Ia21(1) = File21
   Ia22(1) = File22
!
   DO i = 2 , 5
      Ia11(i) = Ia(i)
      Ia12(i) = Ia(i)
      Ia21(i) = Ia(i)
      Ia22(i) = Ia(i)
   ENDDO
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
!     COLUMN PARTITION
!
      IF ( Cpart==0 ) THEN
!
!     ILLEGAL INPUT - NO PARTITION VECTOR
!
         CALL mesage(-7,0,name)
         GOTO 99999
      ELSE
         Ia11(3) = Nsub0
         Ia12(3) = Nsub0
         Ia21(3) = Nsub1
         Ia22(3) = Nsub1
         CALL partn(Core,cp,Core)
      ENDIF
   ELSEIF ( Cpart==0 ) THEN
!
!     ROW PARTITION
!
      CALL partn(rp,Core,Core)
   ELSE
!
!     FULL PARTITION
!
      Ia11(3) = Nsub0
      Ia12(3) = Nsub0
      Ia21(3) = Nsub1
      Ia22(3) = Nsub1
      CALL partn(rp,cp,Core)
   ENDIF
!
!     WRITE TRAILERS FOR OUTPUTS
!
   IF ( Ia11(1)/=0 ) CALL wrttrl(Ia11)
   IF ( Ia12(1)/=0 ) CALL wrttrl(Ia12)
   IF ( Ia21(1)/=0 ) CALL wrttrl(Ia21)
   IF ( Ia22(1)/=0 ) CALL wrttrl(Ia22)
!
   RETURN
99999 END SUBROUTINE gfsptn
