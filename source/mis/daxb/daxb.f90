!*==daxb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE daxb(A,B,C)
USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3) :: A
   REAL(REAL64) , DIMENSION(3) :: B
   REAL(REAL64) , DIMENSION(3) :: C
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3) :: d
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
!*****
!  DOUBLE PRECISION VERSION
!
!  THIS ROUTINE PERFORMS A X B INTO C  (C MAY OVERLAP A OR B IN CORE)
!*****
   d(1) = A(2)*B(3) - A(3)*B(2)
   d(2) = A(3)*B(1) - A(1)*B(3)
   d(3) = A(1)*B(2) - A(2)*B(1)
   C(1) = d(1)
   C(2) = d(2)
   C(3) = d(3)
END SUBROUTINE daxb
