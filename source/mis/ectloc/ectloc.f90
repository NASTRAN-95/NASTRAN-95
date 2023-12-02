!*==ectloc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ectloc(*,Ect,Buf,Ielem)
   USE c_gpta1
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ect
   INTEGER , DIMENSION(3) :: Buf
   INTEGER :: Ielem
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nread
   INTEGER , SAVE :: plotel
   EXTERNAL close , fwdrec , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!*****
! ECTLOC IS A SPECIAL PURPOSE VERSION OF SUBROUTINE LOCATE.  ITS
! PURPOSE IS TO PASS THE ECT FILE SEQUENTIALLY POSITIONING EACH LOGICAL
! RECORD AFTER THE 3-WORD HEADER AND PROVIDING A POINTER TO THE
! APPROPRIATE ENTRY IN THE ELEM TABLE IN /GPTA1/. PLOTEL
! ELEMENTS ARE IGNORED.
!     NOTE---THE ECT FILE MUST BE OPEN ON EACH CALL.
!
!  ARGUMENTS
!
!     ECT   ---INPUT ---EINO FILE NAME OF THE ECT
!     BUF   ---IN/OUT---ADDRESS OF A 3-WORD ARRAY INTO WHICH
!                       THE FIRST 3 WORDS OF THE RECORD ARE READ
!     IELEM ---OUTPUT---POINTER TO 1ST WORD OF ENTRY IN ELEM
!                       TABLE IN /GPTA1/
!
! NON-STANDARD RETURN---GIVEN WHEN EOF HIT. ECT IS CLOSED BEFORE RETURN.
!*****
!
!
   DATA plotel/4HPLOT/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! READ A 3-WORD RECORD HEADER. IF NOT 3 WORDS, TRY NEXT RECORD
!
 20      CALL read(*40,*20,Ect,Buf,3,0,nread)
!
! SEARCH FOR MATCH OF FIRST WORD OF RECORD WITH ECT-ID WORD IN /GPTA1/
! IF FOUND AND NOT PLOTEL, RETURN POINTER.
!
         DO i = 1 , last , incr
            IF ( Buf(1)==elem(i+3) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         CALL fwdrec(*40,Ect)
         spag_nextblock_1 = 1
      CASE (3)
         IF ( elem(i)==plotel ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Ielem = i
         RETURN
!
! EOF ENCOUNTERED--CLOSE FILE AND RETURN.
!
 40      CALL close(Ect,1)
         Ielem = 0
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ectloc
