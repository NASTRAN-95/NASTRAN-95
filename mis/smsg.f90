
SUBROUTINE smsg(No,P1,P2)
   IMPLICIT NONE
   INTEGER K
   REAL Sysbuf
   CHARACTER*23 Ufm
   COMMON /system/ Sysbuf , K
   COMMON /xmssg / Ufm
   INTEGER No , P1
   INTEGER P2(2) , P3(2)
   INTEGER l , msgno , neg(2) , nmsg , nmsg1 , png(2) , pos(2)
!
!     MESSAGE WRITER FOR SUBSTRUCTURE DIAGNOSTICS, 61XX SERIES
!
   DATA pos , neg/4HWARN , 4HING  , 4HFATA , 4HL   /
   DATA nmsg/8/ , nmsg1/11/
!
   l = iabs(No)
   msgno = l + 6100
   IF ( l<1 .OR. l>nmsg ) THEN
      WRITE (K,99001) No , P1 , P2
99001 FORMAT (' NO MESSAGE FOR MESSAGE NO.',I5,' PARAMETERS = ',2I10,10X,2A10)
      GOTO 200
   ELSE
      IF ( No<=0 ) THEN
         png(1) = neg(1)
         png(2) = neg(2)
      ELSE
         png(1) = pos(1)
         png(2) = pos(2)
      ENDIF
      GOTO 100
   ENDIF
!
!
   ENTRY smsg1(No,P1,P2,P3)
!     ========================
!
   l = iabs(No)
   IF ( l<=nmsg .OR. l>nmsg1 ) THEN
      WRITE (K,99001) No , P1 , P2
      GOTO 200
   ENDIF
!
 100  IF ( l==2 ) THEN
      WRITE (K,99016) png , msgno
      WRITE (K,99002) P1 , P2
99002 FORMAT (' REQUESTED SUBSTRUCTURE DOES NOT EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4)
   ELSEIF ( l==3 ) THEN
      WRITE (K,99016) png , msgno
      WRITE (K,99003) P1 , P2
99003 FORMAT (' REQUESTED SOF ITEM HAS INVALID NAME.  ITEM ',A4,', SUBSTRUCTURE ',2A4)
   ELSEIF ( l==4 ) THEN
      WRITE (K,99016) png , msgno
      WRITE (K,99004) P2
99004 FORMAT (' ATTEMPT TO CREATE DUPLICATE SUBSTRUCTURE NAME ',2A4)
   ELSEIF ( l==5 ) THEN
      WRITE (K,99016) png , msgno
      WRITE (K,99005) P2
99005 FORMAT (' ATTEMPT TO RE-USE SUBSTRUCTURE ',2A4,' IN A REDUCE ',' OR COMBINE OPERATION.  USE EQUIV SUBSTRUCTURE COMMAND')
   ELSEIF ( l==6 ) THEN
      WRITE (K,99015) png , msgno
      WRITE (K,99006) P1 , P2
99006 FORMAT (' UNEXPECTED END OF GROUP ENCOUNTERED WHILE READING ITEM ',A4,', SUBSTRUCTURE ',2A4)
   ELSEIF ( l==7 ) THEN
      WRITE (K,99015) png , msgno
      WRITE (K,99007) P1 , P2
99007 FORMAT (' UNEXPECTED END OF ITEM ENCOUNTERED WHILE READING ITEM ',A4,', SUBSTRUCTURE ',2A4)
   ELSEIF ( l==8 ) THEN
      WRITE (K,99015) png , msgno
      WRITE (K,99008) P1 , P2
99008 FORMAT (' INSUFFICIENT SPACE ON SOF FOR ITEM ',A4,', SUBSTRUCTURE',1X,2A4)
   ELSEIF ( l==9 ) THEN
      WRITE (K,99009) P3 , P1 , P2
99009 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
   ELSEIF ( l==10 ) THEN
      WRITE (K,99010) P3 , P1 , P2
99010 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
   ELSEIF ( l==11 ) THEN
      WRITE (K,99011) P3 , P1 , P2
99011 FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
   ELSEIF ( l==12 ) THEN
      WRITE (K,99012)
99012 FORMAT (' ')
   ELSE
      WRITE (K,99016) png , msgno
      WRITE (K,99013) P1 , P2
!
99013 FORMAT (' REQUESTED SOF ITEM DOES NOT EXIST.  ITEM ',A4,', SUBSTRUCTURE ',2A4)
   ENDIF
 200  IF ( No>0 ) RETURN
   IF ( l<=nmsg ) CALL sofcls
   WRITE (K,99014)
99014 FORMAT (//,' FATAL ERROR')
   CALL errtrc('SMSG    ',130)
   RETURN
99015 FORMAT (' *** SYSTEM ',2A4,' MESSAGE',I5)
99016 FORMAT (' *** USER ',2A4,' MESSAGE',I5)
END SUBROUTINE smsg
