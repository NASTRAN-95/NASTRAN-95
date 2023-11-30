
SUBROUTINE gmprtn(Filea,File11,File21,File12,File22,Rpart,Cpart,Nsub0,Nsub1,Core,Lcore)
   IMPLICIT NONE
   INTEGER Ia(7) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Lcr , Rule
   COMMON /parmeg/ Ia , Ia11 , Ia21 , Ia12 , Ia22 , Lcr , Rule
   INTEGER Cpart , File11 , File12 , File21 , File22 , Filea , Lcore , Nsub0 , Nsub1 , Rpart
   INTEGER Core(6)
   INTEGER cp(7) , i , name(2) , rp(7)
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
   Core(1) = 0
   Core(2) = 1
   Core(3) = Ia(2)
   Core(4) = 2
   Core(5) = 1
   Core(6) = 0
!
   Rule = 0
   Lcr = Lcore
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
!  *  *  PARTITION COLUMNS ONLY
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
99999 RETURN
END SUBROUTINE gmprtn