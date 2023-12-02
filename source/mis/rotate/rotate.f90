!*==rotate.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rotate(Da,Row,Row1,Row2,O,Sin,Cos)
USE C_GIVN
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: Da
   INTEGER :: Row
   INTEGER :: Row1
   INTEGER :: Row2
   REAL(REAL64) , DIMENSION(1) :: O
   REAL(REAL64) , DIMENSION(1) :: Sin
   REAL(REAL64) , DIMENSION(1) :: Cos
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: cosine , sine , x , y , z
   INTEGER :: i , j , jp1 , m
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED
!     ONLY BY VALVEC
!
!    1,                CHECK
!
!     O     = 2ND ROW OF THE COMPLETE MATRIX.
!     SIN   = SINES.
!     COS   = COSINES.
!     DA = MATRIX PARTITION (TRIANGULAR) - DOUBLE PRECISION
!
   m = 0
   DO j = Row1 , Row2
      sine = Sin(j)
      cosine = Cos(j)
      m = m + 1
      IF ( sine/=0.0D0 ) THEN
         x = O(Row+1)*cosine + O(j)*sine
         y = Da(m)*sine + O(j)*cosine
         z = x*cosine + y*sine
         O(j) = y*cosine - x*sine
         Da(m) = O(Row+1) + Da(m) - z
         O(Row+1) = z
      ENDIF
      IF ( j/=N ) THEN
         jp1 = j + 1
         DO i = jp1 , N
            m = m + 1
            x = Da(m)*cosine - O(i)*sine
            O(i) = O(i)*cosine + Da(m)*sine
            y = Cos(i)*O(j) + Sin(i)*x
            Da(m) = Cos(i)*x - Sin(i)*O(j)
            O(j) = y
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE rotate
