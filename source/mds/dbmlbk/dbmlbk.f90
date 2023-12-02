!*==dbmlbk.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmlbk(Lasblk)
   USE I_DSIOF
   USE I_ZZZZZZ
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Lasblk
   INTEGER index
!
! THIS SUBROUTINE WILL RETURN THE LAST BLOCK NUMBER ALLOCATED TO THE
! UNIT "IFILEX"
!
   Lasblk = fcb(6,ifilex)
   IF ( Lasblk==0 ) THEN
      index = fcb(10,ifilex)
      IF ( index==0 ) THEN
         Lasblk = 0
      ELSE
         Lasblk = mem(index+3)
      ENDIF
   ENDIF
END SUBROUTINE dbmlbk
