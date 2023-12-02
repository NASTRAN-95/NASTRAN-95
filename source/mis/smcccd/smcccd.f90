!*==smcccd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE smcccd(Dtemp,Zil,Ilim,Zol)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ilim
   COMPLEX(REAL64) , DIMENSION(Ilim) :: Dtemp
   COMPLEX(REAL64) , DIMENSION(Ilim) :: Zil
   COMPLEX(REAL64) :: Zol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
   DO i = 1 , Ilim
      Dtemp(i) = Dtemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smcccd
