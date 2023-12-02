!*==dvmag.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION dvmag(V1,Eps)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: dvmag
   REAL(REAL64) , DIMENSION(3) :: V1
   REAL(REAL64) :: Eps
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     RETURNS DOUBLE PRECISION MAGNITUDE OF VECTOR V1
!        DVMAG= 0.D0 WHEN .LE. EPS
!
!
!
   dvmag = 0.D0
   a = dadotb(V1,V1)
   IF ( a>0.D0 ) dvmag = dsqrt(a)
   IF ( dvmag<=Eps ) dvmag = 0.D0
END FUNCTION dvmag
