!*==factru.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE factru(A,Lll,Ull,Scr1,Scr2,Scr3) !HIDESTARS (*,A,Lll,Ull,Scr1,Scr2,Scr3)
USE C_DCOMPX
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: Lll
   INTEGER :: Ull
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   EXTERNAL decomp , korsz , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!
!
! ----------------------------------------------------------------------
!
   Ib = 0
   Ibb = 0
   Ia(1) = A
   CALL rdtrl(Ia)
   Il(1) = Lll
   Iu(1) = Ull
   Iscr1 = Scr1
   Iscr2 = Scr2
   Iscr3 = Scr3
   Nz = korsz(Xx)
   Il(3) = Ia(3)
   Iu(3) = Ia(3)
   Il(4) = 4
   Iu(4) = 5
   Iu(5) = Iprec
   Il(5) = Iprec
   CALL decomp(*100,Xx,Xx,Xx)
   CALL wrttrl(Il)
   CALL wrttrl(Iu)
   RETURN
 100  RETURN 1
!
END SUBROUTINE factru
