!*==tspl2d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tspl2d(Ts7)
USE C_SMA1IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(60) :: Ts7
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   REAL :: x2 , x2y , x3 , xy , xy2 , y2 , y3
!
! End of declarations rewritten by SPAG
!
!
!    TRANSVERSE SHEAR ROUTINE2 FOR CTRPLT1 - DOUBLE PRECISION VERSION
!
   DO i = 1 , 60
      Ts7(i) = 0.0D0
   ENDDO
   x2 = X*X
   xy = X*Y
   y2 = Y*Y
   x3 = x2*X
   x2y = x2*Y
   xy2 = X*y2
   y3 = y2*Y
   Ts7(4) = 2.0
   Ts7(7) = 6.0*X
   Ts7(8) = 2.0*Y
   Ts7(11) = 12.0*x2
   Ts7(12) = 6.0*xy
   Ts7(13) = 2.0*y2
   Ts7(16) = 20.0*x3
   Ts7(17) = 6.0*xy2
   Ts7(18) = 2.0*y3
   Ts7(26) = 2.0
   Ts7(29) = 2.0*X
   Ts7(30) = 6.0*Y
   Ts7(33) = 2.0*x2
   Ts7(34) = Ts7(12)
   Ts7(35) = 12.0*y2
   Ts7(37) = 2.0*x3
   Ts7(38) = 6.0*x2y
   Ts7(39) = 12.0*xy2
   Ts7(40) = 20.0*y3
   Ts7(45) = 2.0
   Ts7(48) = 4.0*X
   Ts7(49) = 4.0*Y
   Ts7(52) = 6.0*x2
   Ts7(53) = 8.0*xy
   Ts7(54) = 6.0*y2
   Ts7(57) = 12.0*x2y
   Ts7(58) = Ts7(39)
   Ts7(59) = 8.0*y3
END SUBROUTINE tspl2d
