!*==mce1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1
   IMPLICIT NONE
   USE C_BLANK
   EXTERNAL mce1a , mce1b , mce1c , mce1d , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!     MCE1 PARTITIONS RG INTO RM AND RN
!     THEN SOLVES THE MATRIX EQUATION RM * GM = -RN.
!
!
!
!     SET INPUT, OUTPUT AND SCRATCH FILES
!
   Uset = 101
   Rg = 102
   Gm = 201
   Scr1 = 304
   Scr2 = 305
   Scr3 = 301
   Rm = 302
   Rn = 303
   L = 306
   U = 307
!
!     PARTITION RG INTO RM AND RN
!
   CALL mce1a
!
!     TEST FOR RM DIAGONAL
!
   Mcb(1) = Rm
   CALL rdtrl(Mcb)
   IF ( Mcb(5)==1 .AND. Mcb(6)==1 ) THEN
!
!     RM IS DIAGONAL, COMPUTE GM = -RM(-1) * RN
!
      CALL mce1d
   ELSEIF ( Mcb(5)==2 .AND. Mcb(6)==2 ) THEN
      CALL mce1d
   ELSE
!
!     RM IS NOT DIAGONAL, DECOMPOSE RM THEN SOLVE FOR GM
!     BY FORWARD-BACKWARD SUBSTITUTION.
!
      CALL mce1b
      CALL mce1c
      RETURN
   ENDIF
END SUBROUTINE mce1
