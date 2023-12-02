!*==ssghtp.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssghtp(Order,Z,Lz)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lz
   INTEGER , DIMENSION(Lz) :: Order
   INTEGER , DIMENSION(Lz) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , isave , jptr , ptr , save1 , save2
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!  SPECIAL IN-PLACE PARTITIONING ROUTINE USED ONLY BY SSGHT MODULE.
!*****
         i = 1
         isave = 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     CHECK TO SEE THAT POINTER TO NEXT SLOT HAS NOT BEEN USED YET.
!
         ptr = Order(i)
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
               SPAG_Loop_1_1: DO
!
!     SET POINTER TO WHERE -SAVE1- IS TO BE MOVED.
!
                  jptr = Order(ptr)
                  IF ( jptr>1000000 ) THEN
!
!     END OF CHAIN.  GO BACK AND LOOK FOR ANOTHER.
!
                     i = isave + 1
                     EXIT SPAG_Loop_1_1
                  ELSE
                     Order(ptr) = jptr + 1000000
                     save2 = Z(jptr)
                     Z(jptr) = save1
                     save1 = save2
                     ptr = jptr
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ENDIF
         ENDIF
         isave = i
         IF ( i<=Lz ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CLEAR OUT FLAGS AND RETURN.
!
         DO i = 1 , Lz
            Order(i) = Order(i) - 1000000
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ssghtp
