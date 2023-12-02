!*==tktztk.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tktztk(Tk,Z,Nz,L,M,N)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3,3) :: Tk
   REAL(REAL64) , DIMENSION(1) :: Z
   INTEGER :: Nz
   INTEGER :: L
   INTEGER :: M
   INTEGER :: N
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE  PERFORMS A COORDINATE TRANSFORMATION ON THE
!     SYMMETRIC HALF OF A 3 BY 3 MATRIX
!
   Tk(1,1) = Z(Nz)*Z(L) + Z(Nz+3)*Z(L+1) + Z(Nz+6)*Z(L+2)
   Tk(2,1) = Z(Nz+1)*Z(L) + Z(Nz+4)*Z(L+1) + Z(Nz+7)*Z(L+2)
   Tk(3,1) = Z(Nz+2)*Z(L) + Z(Nz+5)*Z(L+1) + Z(Nz+8)*Z(L+2)
   Tk(1,2) = Z(Nz)*Z(L+1) + Z(Nz+3)*Z(M) + Z(Nz+6)*Z(M+1)
   Tk(2,2) = Z(Nz+1)*Z(L+1) + Z(Nz+4)*Z(M) + Z(Nz+7)*Z(M+1)
   Tk(3,2) = Z(Nz+2)*Z(L+1) + Z(Nz+5)*Z(M) + Z(Nz+8)*Z(M+1)
   Tk(1,3) = Z(Nz)*Z(L+2) + Z(Nz+3)*Z(M+1) + Z(Nz+6)*Z(N)
   Tk(2,3) = Z(Nz+1)*Z(L+2) + Z(Nz+4)*Z(M+1) + Z(Nz+7)*Z(N)
   Tk(3,3) = Z(Nz+2)*Z(L+2) + Z(Nz+5)*Z(M+1) + Z(Nz+8)*Z(N)
   Z(L) = Z(Nz)*Tk(1,1) + Z(Nz+3)*Tk(1,2) + Z(Nz+6)*Tk(1,3)
   Z(L+1) = Z(Nz)*Tk(2,1) + Z(Nz+3)*Tk(2,2) + Z(Nz+6)*Tk(2,3)
   Z(L+2) = Z(Nz)*Tk(3,1) + Z(Nz+3)*Tk(3,2) + Z(Nz+6)*Tk(3,3)
   Z(M) = Z(Nz+1)*Tk(2,1) + Z(Nz+4)*Tk(2,2) + Z(Nz+7)*Tk(2,3)
   Z(M+1) = Z(Nz+1)*Tk(3,1) + Z(Nz+4)*Tk(3,2) + Z(Nz+7)*Tk(3,3)
   Z(N) = Z(Nz+2)*Tk(3,1) + Z(Nz+5)*Tk(3,2) + Z(Nz+8)*Tk(3,3)
END SUBROUTINE tktztk
