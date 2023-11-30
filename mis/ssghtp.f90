
SUBROUTINE ssghtp(Order,Z,Lz)
   IMPLICIT NONE
   INTEGER Lz
   INTEGER Order(Lz) , Z(Lz)
   INTEGER i , isave , jptr , ptr , save1 , save2
!*****
!  SPECIAL IN-PLACE PARTITIONING ROUTINE USED ONLY BY SSGHT MODULE.
!*****
   i = 1
   isave = 1
!
!     CHECK TO SEE THAT POINTER TO NEXT SLOT HAS NOT BEEN USED YET.
!
 100  ptr = Order(i)
   IF ( ptr>1000000 ) THEN
      i = i + 1
   ELSE
      Order(i) = ptr + 1000000
!
!     IF THE MOVE-TO LOCATION IS THE SAME, THEN DO NOTHING.
!
      IF ( ptr==i ) THEN
         i = i + 1
      ELSE
!
!     SAVE VALUE CURRENTLY IN SLOT WE ARE MOVING TO.
!
         save1 = Z(ptr)
!
!     MOVE ITEM INTO SLOT
!
         Z(ptr) = Z(i)
         DO
!
!     SET POINTER TO WHERE -SAVE1- IS TO BE MOVED.
!
            jptr = Order(ptr)
            IF ( jptr>1000000 ) THEN
!
!     END OF CHAIN.  GO BACK AND LOOK FOR ANOTHER.
!
               i = isave + 1
               EXIT
            ELSE
               Order(ptr) = jptr + 1000000
               save2 = Z(jptr)
               Z(jptr) = save1
               save1 = save2
               ptr = jptr
            ENDIF
         ENDDO
      ENDIF
   ENDIF
   isave = i
   IF ( i<=Lz ) GOTO 100
!
!     CLEAR OUT FLAGS AND RETURN.
!
   DO i = 1 , Lz
      Order(i) = Order(i) - 1000000
   ENDDO
END SUBROUTINE ssghtp