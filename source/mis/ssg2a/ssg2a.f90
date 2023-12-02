!*==ssg2a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg2a(Pg,Pnbar,Pm,Pvact)
   USE c_parmeg
   USE c_patx
   USE c_zzzzzz
   IMPLICIT NONE
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
   ia1(1) = Pg
   CALL rdtrl(ia1)
   ia11(1) = Pnbar
   ia12(1) = Pm
   DO i = 2 , 5
      ia11(i) = ia1(i)
      ia12(i) = ia1(i)
   ENDDO
   ia11(3) = n
   ia12(3) = no(1)
   ia21(1) = 0
   ia22(1) = 0
   rule = 0
   lcr = korsz(core)
   core(1) = 0
   core(2) = 1
   core(3) = ia1(2)
   core(4) = 2
   core(5) = 1
   core(6) = 0
   CALL partn(core,pvect,core)
   IF ( ia11(1)/=0 ) CALL wrttrl(ia11)
   IF ( ia12(1)/=0 ) CALL wrttrl(ia12)
END SUBROUTINE ssg2a
