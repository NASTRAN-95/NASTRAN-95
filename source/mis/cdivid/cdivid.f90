!*==cdivid.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cdivid(A,B,D,Ncol)
USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: A
   REAL(REAL64) , DIMENSION(1) :: B
   REAL(REAL64) , DIMENSION(2) :: D
   INTEGER :: Ncol
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: denm , dtemp
   INTEGER :: i
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
!     THIS ROUTINE DIVIDES THE VECTOR A BY D AND STORE RESULT IN B
!
   denm = D(1)**2 + D(2)**2
   DO i = 1 , Ncol , 2
      dtemp = (A(i)*D(1)+A(i+1)*D(2))/denm
      B(i+1) = (A(i+1)*D(1)-A(i)*D(2))/denm
      B(i) = dtemp
   ENDDO
END SUBROUTINE cdivid
