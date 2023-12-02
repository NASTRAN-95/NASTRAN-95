!*==itcode.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION itcode(Itemx)
   IMPLICIT NONE
   USE C_ITEMDT
   USE C_SYS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: itcode
   INTEGER :: Itemx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!     THE FUNCTION RETURNS AN INTEGER CODE NUMBER FOR ITEM.  THE CODE
!     NUMBER IS USED IN UPDATING THE MDI.  IF AN INCORRECT ITEM NAME IS
!     USED, THE VALUE RETURNED WILL BE -1.
!
!
   DO i = 1 , Nitem
      IF ( Itemx==Item(1,i) ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     INVALID ITEM - RETURN -1
!
   itcode = -1
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     ITEM FOUND - RETURN MDI POSITION POINTER
!
      itcode = i + Ifrst - 1
   END SUBROUTINE spag_block_1
END FUNCTION itcode
