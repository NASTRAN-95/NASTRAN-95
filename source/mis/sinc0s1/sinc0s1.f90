!*==sinc0s1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sinc0s1(Row,Sick,D,O,Cos)
   USE c_givn
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Row
   INTEGER :: Sick
   REAL , DIMENSION(1) :: D
   REAL , DIMENSION(1) :: O
   REAL , DIMENSION(1) :: Cos
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , rowp2
   REAL :: z
!
! End of declarations rewritten by SPAG
!
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
   DO i = rowp2 , n
      IF ( D(i)==0.0 ) THEN
!
!     NO ROTATION.
!
         Cos(i) = 1.0
      ELSE
!
!     CALCULATE THE ROTATION.
!
         Sick = 1
         z = sqrt(D(i)**2+D(Row+1)**2)
         D(i) = D(i)/z
         Cos(i) = D(Row+1)/z
         D(Row+1) = z
         IF ( Cos(i)<0.0 ) THEN
            Cos(i) = abs(Cos(i))
            D(i) = -D(i)
            D(Row+1) = -D(Row+1)
         ENDIF
      ENDIF
   ENDDO
   O(Row) = D(Row+1)
END SUBROUTINE sinc0s1
