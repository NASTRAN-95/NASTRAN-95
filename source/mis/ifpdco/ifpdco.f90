!*==ifpdco.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION ifpdco(Ic)
   USE c_ifpdta
   USE c_system
   IMPLICIT NONE
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
   gc(1) = Ic
   DO lc = 1 , 6
      ll(lc) = 0
   ENDDO
   IF ( Ic<0 ) THEN
      CALL spag_block_3
      RETURN
   ENDIF
   IF ( Ic/=0 ) THEN
      DO lc = 1 , 6
         gc(lc+1) = gc(lc)/10
         dg = gc(lc) - 10*gc(lc+1)
         IF ( ithrml/=1 .AND. dg>6 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( ithrml==1 .AND. dg>1 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( dg==0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( ll(dg)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         ll(dg) = dg
      ENDDO
      IF ( gc(7)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      ifpdco = .FALSE.
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( gc(Lc)==0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      ifpdco = .TRUE.
   END SUBROUTINE spag_block_3
END FUNCTION ifpdco
