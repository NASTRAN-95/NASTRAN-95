
SUBROUTINE sinc0s(Row,Sick,D,O,Cos)
   IMPLICIT NONE
   INTEGER N
   REAL Title(100)
   COMMON /givn  / Title , N
   INTEGER Row , Sick
   DOUBLE PRECISION Cos(1) , D(1) , O(1)
   INTEGER i , rowp2
   DOUBLE PRECISION z
!                    =
!     SUBROUTINE SICOX (D,O,COS)
!
!     THIS ROUTINE WAS CALLED SICOX BEFORE, WITH ENTRY POINT SINCAS
!                                                                =
!     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED
!     ONLY BY VALVEC
!
!     IT CALCULATES SINES AND COSINES FOR GIVENS TRIDIAGONALIZATION
!
!
!     D   = DIAGONAL AND SINES.
!     O   = OFF-DIAGONAL.
!     COS = COSINES.
!
!     RETURN
!
!
!     ENTRY SINCAS (ROW,SICK)
!     =======================
!
!     CALCULATE THE SINES AND COSINES OF ROW -ROW-.
!
   Sick = 0
   rowp2 = Row + 2
   DO i = rowp2 , N
      IF ( D(i)==0.0D0 ) THEN
!
!     NO ROTATION.
!
         Cos(i) = 1.0D0
      ELSE
!
!     CALCULATE THE ROTATION.
!
         Sick = 1
         z = dsqrt(D(i)**2+D(Row+1)**2)
         D(i) = D(i)/z
         Cos(i) = D(Row+1)/z
         D(Row+1) = z
         IF ( Cos(i)<0.0D0 ) THEN
            Cos(i) = dabs(Cos(i))
            D(i) = -D(i)
            D(Row+1) = -D(Row+1)
         ENDIF
      ENDIF
   ENDDO
   O(Row) = D(Row+1)
END SUBROUTINE sinc0s
