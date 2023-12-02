!*==smccrs.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
 
SUBROUTINE smccrs(Temp,Zil,Ilim,Zol)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ilim
   REAL , DIMENSION(Ilim) :: Temp
   REAL , DIMENSION(Ilim) :: Zil
   REAL :: Zol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   DO i = 1 , Ilim
      Temp(i) = Temp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smccrs
