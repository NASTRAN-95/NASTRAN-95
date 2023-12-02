!*==ittype.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION ittype(Itemx)
   IMPLICIT NONE
   USE C_ITEMDT
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: ittype
   INTEGER :: Itemx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!*****
!
!     THIS FUNCTION RETURNS AN INTEGER CODE NUMBER TO INDICATE
!     WHETHER A PARTICULAR SOF ITEM IS A MATRIX OR TABLE.
!     THE RETURN CODES ARE
!
!          1 - MATRIX ITEM
!          0 - TABLE ITEM
!         -1 - ILLEGAL ITEM NAME
!
!*****
!
!
   DO i = 1 , Nitem
      IF ( Itemx==Item(1,i) ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     ILLIGAL ITEM - RETURN -1
!
   ittype = -1
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     ITEM FOUND - RETURN TYPE
!
      IF ( Item(2,i)<=0 ) ittype = 0
      IF ( Item(2,i)>0 ) ittype = 1
   END SUBROUTINE spag_block_1
END FUNCTION ittype
