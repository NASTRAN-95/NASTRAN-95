!*==ssg2a.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg2a(Pg,Pnbar,Pm,Pvact)
   IMPLICIT NONE
   USE C_PARMEG
   USE C_PATX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Pg
   INTEGER :: Pnbar
   INTEGER :: Pm
   INTEGER :: Pvact
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: core
   INTEGER :: i
   INTEGER , DIMENSION(7) :: pvect
   EXTERNAL korsz , partn , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Icore(1),Core(1))
!
!
   pvect(1) = Pvact
   CALL rdtrl(pvect)
   Ia1(1) = Pg
   CALL rdtrl(Ia1)
   Ia11(1) = Pnbar
   Ia12(1) = Pm
   DO i = 2 , 5
      Ia11(i) = Ia1(i)
      Ia12(i) = Ia1(i)
   ENDDO
   Ia11(3) = N
   Ia12(3) = No(1)
   Ia21(1) = 0
   Ia22(1) = 0
   Rule = 0
   Lcr = korsz(core)
   core(1) = 0
   core(2) = 1
   core(3) = Ia1(2)
   core(4) = 2
   core(5) = 1
   core(6) = 0
   CALL partn(core,pvect,core)
   IF ( Ia11(1)/=0 ) CALL wrttrl(Ia11)
   IF ( Ia12(1)/=0 ) CALL wrttrl(Ia12)
END SUBROUTINE ssg2a
