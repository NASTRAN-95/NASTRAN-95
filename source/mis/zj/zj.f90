!*==zj.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION zj(Arg)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: zj
   REAL :: Arg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , an , dbslj , pf
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!     ZERO ORDER BESSEL FUNCTION OF FIRST KIND
!
   dbslj = 1.0E-10
   a = -(Arg/2.0)**2
   zj = 1.0
   pf = 1.0
   an = 1.0
   DO i = 1 , 20
      an = an*a/pf**2
      pf = pf + 1.0
      IF ( abs(an)<=dbslj ) RETURN
      zj = zj + an
   ENDDO
END FUNCTION zj
