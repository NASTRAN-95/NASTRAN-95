!*==mce1c.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1c
   IMPLICIT NONE
   USE C_BLANK
   USE C_GFBSX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iprec
   EXTERNAL gfbs , korsz , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     MCE1C PERFORMS A FORWARD-BACKWARD SUBSTITUTION WITH THE
!     TRIANGULAR FACTORS OF RM TO SOLVE FOR GM IN THE EQUATION
!     RM*GM = -RN.
!
!
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
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
   Gmx(5) = iprec
   Prec = iprec
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
