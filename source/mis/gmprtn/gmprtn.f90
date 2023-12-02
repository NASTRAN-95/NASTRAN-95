!*==gmprtn.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gmprtn(Filea,File11,File21,File12,File22,Rpart,Cpart,Nsub0,Nsub1,Core,Lcore)
   USE c_parmeg
   IMPLICIT NONE
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
   INTEGER :: Nsub0
   INTEGER :: Nsub1
   INTEGER , DIMENSION(6) :: Core
   INTEGER :: Lcore
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: cp , rp
   INTEGER :: i
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL mesage , partn , rdtrl , wrttrl
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
!
   DATA name/4HGMPR , 4HTN  /
!
!***********************************************************************
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
   Core(1) = 0
   Core(2) = 1
   Core(3) = ia(2)
   Core(4) = 2
   Core(5) = 1
   Core(6) = 0
!
   rule = 0
   lcr = Lcore
!
   IF ( Rpart==0 ) THEN
!
!  *  *  PARTITION ROWS ONLY
!
      IF ( Cpart==0 ) THEN
!
!     ILLEGAL INPUT - NO PARTITION VECTOR
!
         CALL mesage(-7,0,name)
         RETURN
      ELSE
         ia11(3) = Nsub0
         ia12(3) = Nsub0
         ia21(3) = Nsub1
         ia22(3) = Nsub1
         CALL partn(Core,cp,Core)
      ENDIF
   ELSEIF ( Cpart==0 ) THEN
!
!  *  *  PARTITION COLUMNS ONLY
!
      CALL partn(rp,Core,Core)
   ELSE
!
!     FULL PARTITION
!
      ia11(3) = Nsub0
      ia12(3) = Nsub0
      ia21(3) = Nsub1
      ia22(3) = Nsub1
      CALL partn(rp,cp,Core)
   ENDIF
!
!     WRITE TRAILERS FOR OUTPUTS
!
   IF ( ia11(1)/=0 ) CALL wrttrl(ia11)
   IF ( ia12(1)/=0 ) CALL wrttrl(ia12)
   IF ( ia21(1)/=0 ) CALL wrttrl(ia21)
   IF ( ia22(1)/=0 ) CALL wrttrl(ia22)
!
END SUBROUTINE gmprtn
