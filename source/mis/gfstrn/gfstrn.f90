!*==gfstrn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfstrn(A,At,I,Scr1)
   IMPLICIT NONE
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: At
   INTEGER :: I
   INTEGER :: Scr1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibuf , ir , nz
   INTEGER , DIMENSION(7) :: mcb
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
   nz = korsz(z)
   ibuf = nz - sysbuf
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
   val(1) = 1.0
   CALL makmcb(mcb,I,ir,2,2)
   CALL gopen(I,z(ibuf),1)
!
   DO irow = 1 , ir
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
