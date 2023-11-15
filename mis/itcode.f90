
FUNCTION itcode(Itemx)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ifrst , Item(7,1) , Nitem
   REAL Sys(5)
   COMMON /itemdt/ Nitem , Item
   COMMON /sys   / Sys , Ifrst
!
! Dummy argument declarations
!
   INTEGER Itemx
   INTEGER itcode
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
!
!     THE FUNCTION RETURNS AN INTEGER CODE NUMBER FOR ITEM.  THE CODE
!     NUMBER IS USED IN UPDATING THE MDI.  IF AN INCORRECT ITEM NAME IS
!     USED, THE VALUE RETURNED WILL BE -1.
!
!
   DO i = 1 , Nitem
      IF ( Itemx==Item(1,i) ) GOTO 100
   ENDDO
!
!     INVALID ITEM - RETURN -1
!
   itcode = -1
   RETURN
!
!     ITEM FOUND - RETURN MDI POSITION POINTER
!
 100  itcode = i + Ifrst - 1
END FUNCTION itcode
