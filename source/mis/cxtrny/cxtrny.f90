!*==cxtrny.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cxtrny(X,Y,Alpha)
USE C_CINVPX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: X
   REAL(REAL64) , DIMENSION(1) :: Y
   REAL(REAL64) , DIMENSION(2) :: Alpha
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ncol2
!
! End of declarations rewritten by SPAG
!
!*******
!     CX TRN Y FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA WHERE
!     X AND Y ARE COMPLEX
!*******
   ncol2 = Ncol + Ncol
   Alpha(1) = 0.D0
   Alpha(2) = 0.D0
   DO i = 1 , ncol2 , 2
      Alpha(1) = Alpha(1) + X(i)*Y(i) - X(i+1)*Y(i+1)
      Alpha(2) = Alpha(2) + X(i)*Y(i+1) + X(i+1)*Y(i)
   ENDDO
END SUBROUTINE cxtrny
