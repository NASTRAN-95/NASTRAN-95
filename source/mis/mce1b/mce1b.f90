!*==mce1b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1b
USE C_BLANK
USE C_DCOMPX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL decomp , korsz , mesage , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     MCE1B DECOMPOSES RM INTO LOWER AND UPPER TRIANGULAR FACTORS
!
   DATA nam/4HMCE1 , 4HB   /
!
!     INITIALIZE MATRIX CONTROL BLOCKS
!
   Nz = korsz(Z)
   A(1) = Rm
   CALL rdtrl(A)
   Lx(1) = L
   Lx(3) = A(3)
   Lx(4) = 4
   Lx(5) = A(5)
   Ux(1) = U
   Ux(3) = A(3)
   Ux(4) = 5
   Ux(5) = A(5)
   Scrx1 = Scr1
   Scrx2 = Scr2
   Scrx3 = Scr3
!
!     PERFORM DECOMPOSITION
!
   CALL decomp(*100,Z,Z,Z)
!
!     WRITE TRAILERS
!
   CALL wrttrl(Lx)
   CALL wrttrl(Ux)
   RETURN
!
!     FATAL ERROR MESSAGE FOR SINGULAR MATRIX
!
 100  CALL mesage(-5,Rm,nam)
END SUBROUTINE mce1b
