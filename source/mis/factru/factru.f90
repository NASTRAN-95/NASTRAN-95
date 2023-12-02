!*==factru.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE factru(A,Lll,Ull,Scr1,Scr2,Scr3) !HIDESTARS (*,A,Lll,Ull,Scr1,Scr2,Scr3)
   USE c_dcompx
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
   ib = 0
   ibb = 0
   ia(1) = A
   CALL rdtrl(ia)
   il(1) = Lll
   iu(1) = Ull
   iscr1 = Scr1
   iscr2 = Scr2
   iscr3 = Scr3
   nz = korsz(xx)
   il(3) = ia(3)
   iu(3) = ia(3)
   il(4) = 4
   iu(4) = 5
   iu(5) = iprec
   il(5) = iprec
   CALL decomp(*100,xx,xx,xx)
   CALL wrttrl(il)
   CALL wrttrl(iu)
   RETURN
 100  RETURN 1
!
END SUBROUTINE factru
