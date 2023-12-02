!*==ifpdco.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION ifpdco(Ic)
   IMPLICIT NONE
   USE C_IFPDTA
   USE C_SYSTEM
!
! Function and Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: ifpdco
   INTEGER :: Ic
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dg , lc
!
! End of declarations rewritten by SPAG
!
!
!     DECODE D.O.F. INTO LL SPACE.
!     RETURN WITH IFPDCO=.TRUE. IF ERROR ENCOUNTERED
!     FOR EXAMPLE - GIVEN IC=124, THEN
!                   LL(1)=1,   LL(2)=2,  LL(4)=4, LL(3)=LL(5)=LL(6)=0
!                   GC(1)=124, GC(2)=12, GC(3)=1, GC(4)=GC(5),GC(6)=0
!                   IFPDCO=.FALSE.
!
!
   Gc(1) = Ic
   DO lc = 1 , 6
      Ll(lc) = 0
   ENDDO
   IF ( Ic<0 ) THEN
      CALL spag_block_3
      RETURN
   ENDIF
   IF ( Ic/=0 ) THEN
      DO lc = 1 , 6
         Gc(lc+1) = Gc(lc)/10
         dg = Gc(lc) - 10*Gc(lc+1)
         IF ( Ithrml/=1 .AND. dg>6 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Ithrml==1 .AND. dg>1 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( dg==0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Ll(dg)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         Ll(dg) = dg
      ENDDO
      IF ( Gc(7)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      ifpdco = .FALSE.
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( Gc(lc)==0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      ifpdco = .TRUE.
   END SUBROUTINE spag_block_3
END FUNCTION ifpdco
