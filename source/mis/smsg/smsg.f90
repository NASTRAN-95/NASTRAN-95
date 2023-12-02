!*==smsg.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smsg(No,P1,P2)
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: No
   INTEGER :: P1
   INTEGER , DIMENSION(2) :: P2
   *0() :: 
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: l , msgno
   INTEGER , DIMENSION(2) , SAVE :: neg , pos
   INTEGER , SAVE :: nmsg , nmsg1
   INTEGER , DIMENSION(2) :: P3
   INTEGER , DIMENSION(2) :: png
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MESSAGE WRITER FOR SUBSTRUCTURE DIAGNOSTICS, 61XX SERIES
!
   DATA pos , neg/4HWARN , 4HING  , 4HFATA , 4HL   /
   DATA nmsg/8/ , nmsg1/11/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         l = iabs(No)
         msgno = l + 6100
         IF ( l<1 .OR. l>nmsg ) THEN
            WRITE (k,99014) No , P1 , P2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( No<=0 ) THEN
               png(1) = neg(1)
               png(2) = neg(2)
            ELSE
               png(1) = pos(1)
               png(2) = pos(2)
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!
         ENTRY smsg1(No,P1,P2,P3)
!     ========================
!
         l = iabs(No)
         IF ( l<=nmsg .OR. l>nmsg1 ) THEN
            WRITE (k,99014) No , P1 , P2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( l==2 ) THEN
            WRITE (k,99016) png , msgno
            WRITE (k,99001) P1 , P2
99001       FORMAT (' REQUESTED SUBSTRUCTURE DOES NOT EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4)
         ELSEIF ( l==3 ) THEN
            WRITE (k,99016) png , msgno
            WRITE (k,99002) P1 , P2
99002       FORMAT (' REQUESTED SOF ITEM HAS INVALID NAME.  ITEM ',A4,', SUBSTRUCTURE ',2A4)
         ELSEIF ( l==4 ) THEN
            WRITE (k,99016) png , msgno
            WRITE (k,99003) P2
99003       FORMAT (' ATTEMPT TO CREATE DUPLICATE SUBSTRUCTURE NAME ',2A4)
         ELSEIF ( l==5 ) THEN
            WRITE (k,99016) png , msgno
            WRITE (k,99004) P2
99004       FORMAT (' ATTEMPT TO RE-USE SUBSTRUCTURE ',2A4,' IN A REDUCE ',' OR COMBINE OPERATION.  USE EQUIV SUBSTRUCTURE COMMAND')
         ELSEIF ( l==6 ) THEN
            WRITE (k,99015) png , msgno
            WRITE (k,99005) P1 , P2
99005       FORMAT (' UNEXPECTED END OF GROUP ENCOUNTERED WHILE READING ITEM ',A4,', SUBSTRUCTURE ',2A4)
         ELSEIF ( l==7 ) THEN
            WRITE (k,99015) png , msgno
            WRITE (k,99006) P1 , P2
99006       FORMAT (' UNEXPECTED END OF ITEM ENCOUNTERED WHILE READING ITEM ',A4,', SUBSTRUCTURE ',2A4)
         ELSEIF ( l==8 ) THEN
            WRITE (k,99015) png , msgno
            WRITE (k,99007) P1 , P2
99007       FORMAT (' INSUFFICIENT SPACE ON SOF FOR ITEM ',A4,', SUBSTRUCTURE',1X,2A4)
         ELSEIF ( l==9 ) THEN
            WRITE (k,99008) P3 , P1 , P2
99008       FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
         ELSEIF ( l==10 ) THEN
            WRITE (k,99009) P3 , P1 , P2
99009       FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,             &
                   &', IS PURGED.')
         ELSEIF ( l==11 ) THEN
            WRITE (k,99010) P3 , P1 , P2
99010       FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
         ELSEIF ( l==12 ) THEN
            WRITE (k,99011)
99011       FORMAT (' ')
         ELSE
            WRITE (k,99016) png , msgno
            WRITE (k,99012) P1 , P2
!
99012       FORMAT (' REQUESTED SOF ITEM DOES NOT EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( No>0 ) RETURN
         IF ( l<=nmsg ) CALL sofcls
         WRITE (k,99013)
99013    FORMAT (//,' FATAL ERROR')
         CALL errtrc('SMSG    ',130)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99014 FORMAT (' NO MESSAGE FOR MESSAGE NO.',I5,' PARAMETERS = ',2I10,10X,2A10)
99015 FORMAT (' *** SYSTEM ',2A4,' MESSAGE',I5)
99016 FORMAT (' *** USER ',2A4,' MESSAGE',I5)
END SUBROUTINE smsg
