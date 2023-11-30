
SUBROUTINE factru(*,A,Lll,Ull,Scr1,Scr2,Scr3)
   IMPLICIT NONE
   DOUBLE PRECISION Dett , Mindia
   INTEGER Ia(7) , Ib , Ibb , Il(7) , Ipow , Iprec , Iscr1 , Iscr2 , Iscr3 , Iu(7) , Nz
   REAL Sys(54) , Xx(1)
   COMMON /dcompx/ Ia , Il , Iu , Iscr1 , Iscr2 , Iscr3 , Dett , Ipow , Nz , Mindia , Ib , Ibb
   COMMON /system/ Sys , Iprec
   COMMON /zzzzzz/ Xx
   INTEGER A , Lll , Scr1 , Scr2 , Scr3 , Ull
   INTEGER korsz
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