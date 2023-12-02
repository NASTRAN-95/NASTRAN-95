!*==mce1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1
   USE c_blank
   IMPLICIT NONE
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
   uset = 101
   rg = 102
   gm = 201
   scr1 = 304
   scr2 = 305
   scr3 = 301
   rm = 302
   rn = 303
   l = 306
   u = 307
!
!     PARTITION RG INTO RM AND RN
!
   CALL mce1a
!
!     TEST FOR RM DIAGONAL
!
   mcb(1) = rm
   CALL rdtrl(mcb)
   IF ( mcb(5)==1 .AND. mcb(6)==1 ) THEN
!
!     RM IS DIAGONAL, COMPUTE GM = -RM(-1) * RN
!
      CALL mce1d
   ELSEIF ( mcb(5)==2 .AND. mcb(6)==2 ) THEN
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
