!*==gauss2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gauss2(A,N,N2)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(20,1) :: A
   INTEGER :: N
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX(REAL64) , DIMENSION(20,30) :: da
   INTEGER :: i , j , k , l , m
!
! End of declarations rewritten by SPAG
!
!
!
   DO i = 1 , N
      DO j = 1 , N2
         da(i,j) = A(i,j)
      ENDDO
   ENDDO
   DO i = 1 , N
      k = i + 1
      DO j = k , N2
         da(i,j) = da(i,j)/da(i,i)
      ENDDO
      DO m = 1 , N
         IF ( m/=i ) THEN
            DO l = k , N2
               da(m,l) = da(m,l) - da(m,i)*da(i,l)
            ENDDO
         ENDIF
      ENDDO
   ENDDO
   DO i = 1 , N
      DO j = 1 , N2
         A(i,j) = da(i,j)
      ENDDO
   ENDDO
END SUBROUTINE gauss2
