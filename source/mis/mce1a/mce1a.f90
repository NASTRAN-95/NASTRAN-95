!*==mce1a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1a
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PARMEG
   USE C_PATX
   USE C_ZZZZZZ
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
   Nz = korsz(Z)
   Usetxx = Uset
   CALL calcv(Scr1,Ug,Un,Um,Z)
!
!     GENERATE NULL COLUMN PARTITIONING VECTOR
!
   Z(i) = 0
   Z(i+2) = Nsub2
   Z(i+7) = 1
   Z(i+8) = 2
   Z(i+9) = -16777215
!
!     INITIALIZE MATRIX CONTROL BLOCKS
!
   N = Nz
   Rule = 0
   A(1) = Rg
   CALL rdtrl(A)
   A11(1) = Rn
   A11(2) = Nsub1
   A11(3) = Nsub2
   A11(4) = rect
   A11(5) = A(5)
   A12(1) = Rm
   A12(2) = Nsub2
   A12(3) = Nsub2
   A12(4) = square
   A12(5) = A(5)
   Mcb(1) = Scr1
   CALL rdtrl(Mcb)
   A21(1) = 0
   A22(1) = 0
!
!     PARTITION RG INTO RM AND RN
!
   CALL partn(Mcb,Z,Z)
!
!     WRITE TRAILERS FOR RM AND RN
!
   CALL wrttrl(A12)
   CALL wrttrl(A11)
END SUBROUTINE mce1a
