
SUBROUTINE dbmlbk(Lasblk)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Lasblk
   INTEGER index
!
! THIS SUBROUTINE WILL RETURN THE LAST BLOCK NUMBER ALLOCATED TO THE
! UNIT "IFILEX"
!
   Lasblk = Fcb(6,Ifilex)
   IF ( Lasblk==0 ) THEN
      index = Fcb(10,Ifilex)
      IF ( index==0 ) THEN
         Lasblk = 0
      ELSE
         Lasblk = Mem(index+3)
      ENDIF
   ENDIF
END SUBROUTINE dbmlbk
