
SUBROUTINE mce1c
   IMPLICIT NONE
   INTEGER Gm , Gmx(7) , Iprec , Ksystm(65) , L , Lx(7) , Mcb(7) , Nz , Prec , Rg , Rm , Rn , Rnx(7) , Scr1 , Scr2 , Scr3 , Sign ,  &
         & U , Uset , Ux(7)
   REAL Z(1)
   COMMON /blank / Uset , Rg , Gm , Scr1 , Scr2 , Scr3 , Rm , Rn , L , U , Mcb
   COMMON /gfbsx / Lx , Ux , Rnx , Gmx , Nz , Prec , Sign
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER korsz
!
!     MCE1C PERFORMS A FORWARD-BACKWARD SUBSTITUTION WITH THE
!     TRIANGULAR FACTORS OF RM TO SOLVE FOR GM IN THE EQUATION
!     RM*GM = -RN.
!
!
   EQUIVALENCE (Ksystm(55),Iprec)
!
!     INITIALIZE MATRIX CONTROL BLOCKS
!
   Nz = korsz(Z)
   Lx(1) = L
   CALL rdtrl(Lx)
   Ux(1) = U
   CALL rdtrl(Ux)
   Rnx(1) = Rn
   CALL rdtrl(Rnx)
   Gmx(1) = Gm
   Gmx(3) = Rnx(3)
   Gmx(4) = Rnx(4)
   Gmx(5) = Iprec
   Prec = Iprec
   Sign = -1
!
!     PERFORM SOLUTION
!
   CALL gfbs(Z,Z)
!
!     WRITE TRAILER
!
   CALL wrttrl(Gmx)
END SUBROUTINE mce1c
