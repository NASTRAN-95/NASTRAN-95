!*==smcccs.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
 
SUBROUTINE smcccs(Ctemp,Zil,Ilim,Zol)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ilim
   COMPLEX , DIMENSION(Ilim) :: Ctemp
   COMPLEX , DIMENSION(Ilim) :: Zil
   COMPLEX :: Zol
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
      Ctemp(i) = Ctemp(i) + Zil(i)*Zol
   ENDDO
END SUBROUTINE smcccs
