
SUBROUTINE cxtrny(X,Y,Alpha)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Aaa
   INTEGER Ncol
   COMMON /cinvpx/ Aaa , Ncol
!
! Dummy argument declarations
!
   DOUBLE PRECISION Alpha(2) , X(1) , Y(1)
!
! Local variable declarations
!
   INTEGER i , ncol2
!
! End of declarations
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
