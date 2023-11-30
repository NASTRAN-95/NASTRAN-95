
SUBROUTINE rotate1(A,Row,Row1,Row2,O,Sin,Cos)
   IMPLICIT NONE
   INTEGER N
   REAL Title(100)
   COMMON /givn  / Title , N
   INTEGER Row , Row1 , Row2
   REAL A(1) , Cos(1) , O(1) , Sin(1)
   REAL cosine , sine , x , y , z
   INTEGER i , j , jp1 , m
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
      IF ( j/=N ) THEN
         jp1 = j + 1
         DO i = jp1 , N
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
