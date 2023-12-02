!*==mce1b.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1b
   USE c_blank
   USE c_dcompx
   USE c_zzzzzz
   USE iso_fortran_env
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
   nz = korsz(z)
   a(1) = rm
   CALL rdtrl(a)
   lx(1) = l
   lx(3) = a(3)
   lx(4) = 4
   lx(5) = a(5)
   ux(1) = u
   ux(3) = a(3)
   ux(4) = 5
   ux(5) = a(5)
   scrx1 = scr1
   scrx2 = scr2
   scrx3 = scr3
!
!     PERFORM DECOMPOSITION
!
   CALL decomp(*100,z,z,z)
!
!     WRITE TRAILERS
!
   CALL wrttrl(lx)
   CALL wrttrl(ux)
   RETURN
!
!     FATAL ERROR MESSAGE FOR SINGULAR MATRIX
!
 100  CALL mesage(-5,rm,nam)
END SUBROUTINE mce1b
