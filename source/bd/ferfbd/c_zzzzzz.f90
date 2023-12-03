!*==/home/marcusmae/nasa/nastran/SPAGged/C_ZZZZZZ.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_ZZZZZZ
   INTEGER, DIMENSION(400000000), TARGET :: Icore
   DOUBLE PRECISION, POINTER :: Dcore(:)

CONTAINS

  SUBROUTINE ZZZZZZ_INIT() bind(C)
  USE iso_c_binding
  IMPLICIT NONE

  ! Declare a temporary c_ptr variable
  TYPE(c_ptr) :: cptr

  ! Assign the address of the integer array to the c_ptr variable
  cptr = C_LOC(Icore)

  ! Associate the real pointer with the c_ptr variable
  CALL C_F_POINTER(cptr, Dcore, [1])
  
  END SUBROUTINE ZZZZZZ_INIT

END MODULE C_ZZZZZZ
