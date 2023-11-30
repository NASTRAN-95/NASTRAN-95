
FUNCTION ittype(Itemx)
   IMPLICIT NONE
   INTEGER Item(7,1) , Nitem
   COMMON /itemdt/ Nitem , Item
   INTEGER Itemx
   INTEGER ittype
   INTEGER i
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
      IF ( Itemx==Item(1,i) ) GOTO 100
   ENDDO
!
!     ILLIGAL ITEM - RETURN -1
!
   ittype = -1
   RETURN
!
!     ITEM FOUND - RETURN TYPE
!
 100  IF ( Item(2,i)<=0 ) ittype = 0
   IF ( Item(2,i)>0 ) ittype = 1
END FUNCTION ittype
