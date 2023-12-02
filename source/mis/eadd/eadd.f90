!*==eadd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eadd(P,Prec)
USE C_BLANK
USE C_REGEAN
USE C_SADDX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: P
   INTEGER :: Prec
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: alpha , beta
   INTEGER :: i , ialp , ibeta , kprec
   INTEGER , DIMENSION(7) :: ia , ib , ic
   EXTERNAL korsz , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Mcbs(1),Ia(1)) , (Mcbs(8),Ialp) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Ib(1)) , (Mcbs(20),Ibeta) , (Mcbs(21),Beta(1)) ,   &
!>>>>    & (Mcbs(61),Ic(1))
!
   Nz = (korsz(Core)/2)*2 - Lc
   DO i = 1 , 7
      ia(i) = Im(i)
      ib(i) = Ik(i)
      ic(i) = Ik(i)
   ENDDO
   ic(1) = Ka(1)
   kprec = Ik(5)
   IF ( Prec>=1 .AND. Prec<=4 ) kprec = Prec
   ialp = kprec
   alpha(1) = P(1)
   ibeta = kprec
   beta(1) = 1.0D0
   Nomat = 2
   CALL sadd(Core,Core)
   CALL wrttrl(ic)
END SUBROUTINE eadd
