!*==gfsptn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsptn(Filea,File11,File21,File12,File22,Rpart,Cpart)
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
   ia(1) = Filea
   CALL rdtrl(ia)
!
!     SET UP MATRIX CONTROL BLOCKS FOR OUTPUTS
!
   ia11(1) = File11
   ia12(1) = File12
   ia21(1) = File21
   ia22(1) = File22
!
   DO i = 2 , 5
      ia11(i) = ia(i)
      ia12(i) = ia(i)
      ia21(i) = ia(i)
      ia22(i) = ia(i)
   ENDDO
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
!     COLUMN PARTITION
!
      IF ( Cpart==0 ) THEN
!
!     ILLEGAL INPUT - NO PARTITION VECTOR
!
         CALL mesage(-7,0,name)
         GOTO 99999
      ELSE
         ia11(3) = nsub0
         ia12(3) = nsub0
         ia21(3) = nsub1
         ia22(3) = nsub1
         CALL partn(core,cp,core)
      ENDIF
   ELSEIF ( Cpart==0 ) THEN
!
!     ROW PARTITION
!
      CALL partn(rp,core,core)
   ELSE
!
!     FULL PARTITION
!
      ia11(3) = nsub0
      ia12(3) = nsub0
      ia21(3) = nsub1
      ia22(3) = nsub1
      CALL partn(rp,cp,core)
   ENDIF
!
!     WRITE TRAILERS FOR OUTPUTS
!
   IF ( ia11(1)/=0 ) CALL wrttrl(ia11)
   IF ( ia12(1)/=0 ) CALL wrttrl(ia12)
   IF ( ia21(1)/=0 ) CALL wrttrl(ia21)
   IF ( ia22(1)/=0 ) CALL wrttrl(ia22)
!
99999 END SUBROUTINE gfsptn
