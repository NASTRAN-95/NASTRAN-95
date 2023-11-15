
SUBROUTINE gfstrn(A,At,I,Scr1)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Irow , Sysbuf , Z(1)
   REAL Val(4)
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ Val , Irow
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER A , At , I , Scr1
!
! Local variable declarations
!
   INTEGER ibuf , ir , mcb(7) , name(2) , nz
   INTEGER korsz
!
! End of declarations
!
!
!     MATRIX TRANSPOSE ROUTINE
!
!     MORE EFFICIENT THEN TRANSPOSE FOR SPARSE MATRICES
!
!
!     TRANSPOSE IS SOLVED BY THE FOLLOWING EQUATION
!
!                                    T
!                  --    --   --   --  --   --
!                  I      I   I     I  I     I
!                  I  AT  I = I  A  I  I  I  I
!                  I      I   I     I  I     I
!                  --    --   --   --  --   --
!
!     WHERE I IS AN IDENITY MATRIX
!
!
!
!     SYSTEM PARAMETERS
!
!
!     PACK COMMON
!
!
!     OPEN CORE
!
!
   DATA name/4HGFST , 4HRN  /
!
!***********************************************************************
!
   nz = korsz(Z)
   ibuf = nz - Sysbuf
   IF ( ibuf<0 ) CALL mesage(-8,0,name)
!
!     GET MATRIX TRAILER
!
   mcb(1) = A
   CALL rdtrl(mcb)
   IF ( mcb(1)<0 ) RETURN
   ir = mcb(3)
!
!     GENERATE A SQUARE IDENITY MATRIX   IR BY IR
!
   Val(1) = 1.0
   CALL makmcb(mcb,I,ir,2,2)
   CALL gopen(I,Z(ibuf),1)
!
   DO Irow = 1 , ir
      CALL bldpk(1,2,I,0,0)
      CALL zblpki
      CALL bldpkn(I,0,mcb)
   ENDDO
   CALL close(I,1)
   CALL wrttrl(mcb)
!
!     PERFORM MULTIPLY
!
   CALL ssg2b(A,I,0,At,1,2,1,Scr1)
!
END SUBROUTINE gfstrn
