!*==mce1a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1a
   USE c_bitpos
   USE c_blank
   USE c_parmeg
   USE c_patx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: i , rect , square
   EXTERNAL calcv , korsz , partn , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     MCE1A PARTITIONS RG INTO RM AND RN
!
   DATA rect/2/ , square/1/ , i/1/
!
!     GENERATE ROW PARTITIONING VECTOR
!
   nz = korsz(z)
   usetxx = uset
   CALL calcv(scr1,ug,un,um,z)
!
!     GENERATE NULL COLUMN PARTITIONING VECTOR
!
   z(i) = 0
   z(i+2) = nsub2
   z(i+7) = 1
   z(i+8) = 2
   z(i+9) = -16777215
!
!     INITIALIZE MATRIX CONTROL BLOCKS
!
   n = nz
   rule = 0
   a(1) = rg
   CALL rdtrl(a)
   a11(1) = rn
   a11(2) = nsub1
   a11(3) = nsub2
   a11(4) = rect
   a11(5) = a(5)
   a12(1) = rm
   a12(2) = nsub2
   a12(3) = nsub2
   a12(4) = square
   a12(5) = a(5)
   mcb(1) = scr1
   CALL rdtrl(mcb)
   a21(1) = 0
   a22(1) = 0
!
!     PARTITION RG INTO RM AND RN
!
   CALL partn(mcb,z,z)
!
!     WRITE TRAILERS FOR RM AND RN
!
   CALL wrttrl(a12)
   CALL wrttrl(a11)
END SUBROUTINE mce1a
