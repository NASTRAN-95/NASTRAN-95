!*==rotate1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rotate1(A,Row,Row1,Row2,O,Sin,Cos)
   USE c_givn
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: A
   INTEGER :: Row
   INTEGER :: Row1
   INTEGER :: Row2
   REAL , DIMENSION(1) :: O
   REAL , DIMENSION(1) :: Sin
   REAL , DIMENSION(1) :: Cos
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cosine , sine , x , y , z
   INTEGER :: i , j , jp1 , m
!
! End of declarations rewritten by SPAG
!
!
!     ROTATION OF A MATRIX PARTITION.
!     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED
!     ONLY BY VALVEC
!
!
!     O     = 2ND ROW OF THE COMPLETE MATRIX.
!     SIN   = SINES.
!     COS   = COSINES.
!     A  = MATRIX PARTITION (TRIANGULAR) - SINGLE PRECISION
!
   m = 0
   DO j = Row1 , Row2
      sine = Sin(j)
      cosine = Cos(j)
      m = m + 1
      IF ( sine/=0. ) THEN
         x = O(Row+1)*cosine + O(j)*sine
         y = A(m)*sine + O(j)*cosine
         z = x*cosine + y*sine
         O(j) = y*cosine - x*sine
         A(m) = O(Row+1) + A(m) - z
         O(Row+1) = z
      ENDIF
      IF ( j/=n ) THEN
         jp1 = j + 1
         DO i = jp1 , n
            m = m + 1
            x = A(m)*cosine - O(i)*sine
            O(i) = O(i)*cosine + A(m)*sine
            y = Cos(i)*O(j) + Sin(i)*x
            A(m) = Cos(i)*x - Sin(i)*O(j)
            O(j) = y
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE rotate1
