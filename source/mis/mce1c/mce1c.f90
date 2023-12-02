!*==mce1c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1c
   USE c_blank
   USE c_gfbsx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   nz = korsz(z)
   lx(1) = l
   CALL rdtrl(lx)
   ux(1) = u
   CALL rdtrl(ux)
   rnx(1) = rn
   CALL rdtrl(rnx)
   gmx(1) = gm
   gmx(3) = rnx(3)
   gmx(4) = rnx(4)
   gmx(5) = iprec
   prec = iprec
   sign = -1
!
!     PERFORM SOLUTION
!
   CALL gfbs(z,z)
!
!     WRITE TRAILER
!
   CALL wrttrl(gmx)
END SUBROUTINE mce1c
