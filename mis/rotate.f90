
SUBROUTINE rotate(Da,Row,Row1,Row2,O,Sin,Cos)
   IMPLICIT NONE
   INTEGER N
   REAL Title(100)
   COMMON /givn  / Title , N
   INTEGER Row , Row1 , Row2
   DOUBLE PRECISION Cos(1) , Da(1) , O(1) , Sin(1)
   DOUBLE PRECISION cosine , sine , x , y , z
   INTEGER i , j , jp1 , m
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
