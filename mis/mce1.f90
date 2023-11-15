
SUBROUTINE mce1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Gm , L , Mcb(7) , Rg , Rm , Rn , Scr1 , Scr2 , Scr3 , U , Uset
   COMMON /blank / Uset , Rg , Gm , Scr1 , Scr2 , Scr3 , Rm , Rn , L , U , Mcb
!
! End of declarations
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
