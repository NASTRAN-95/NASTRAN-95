
SUBROUTINE preloc(Buf,File) !HIDESTARS (*,Buf,File)
   IMPLICIT NONE
   INTEGER Two(32)
   COMMON /two   / Two
   INTEGER File , Flg
   INTEGER Bff(1) , Buf(2) , Id(2)
   INTEGER andf
   INTEGER flag , icheck , j , k , l , last , nam(2) , nm1(2) , ret , trl(7)
   EXTERNAL andf
!
!     PRELOC OPENS AND POSITIONS REQUESTED FILE TO FIRST DATA RECORD.
!     LOCATE POSITIONS FILE TO REQUESTED DATA RECORD WITHIN FILE.
!
   DATA nam , nm1/4HPREL , 4HOC   , 4HLOCA , 4HTE  /
!
!     OPEN FILE. IF PURGED, GIVE ALTERNATE RETURN.
!     OTHERWISE SKIP HEADER RECORD
!
   trl(1) = File
   CALL rdtrl(trl)
   IF ( trl(1)>=0 ) THEN
      IF ( trl(2)+trl(3)+trl(4)+trl(5)+trl(6)+trl(7)/=0 ) THEN
         CALL open(*100,File,Buf(2),0)
         CALL fwdrec(*200,File)
         Buf(1) = File
         icheck = 123456789
         RETURN
      ENDIF
   ENDIF
 100  RETURN 1
!
!     FATAL FILE ERRORS
!
 200  CALL mesage(-2,File,nam)
 300  CALL mesage(-3,trl,nm1)
!
!
   ENTRY locate(Bff,Id,Flg) !HIDESTARS (*,Bff,Id,Flg)
!     ===========================
!
!     ENTRY TO POSITION DATA RECORD.
!
!     READ TRAILER FOR FILE. IF BIT NOT ON OR FILE PURGED,
!     GIVE ALTERNATE RETURN.
!
!WKBD IF (ICHECK .NE. 123456789) CALL ERRTRC ('LOCATE  ',10)
   trl(1) = Bff(1)
   CALL rdtrl(trl)
   IF ( trl(1)<0 ) RETURN 1
   k = (Id(2)-1)/16
   l = Id(2) - 16*k
   IF ( andf(trl(k+2),Two(l+16))==0 ) RETURN 1
!
!     READ THREE ID WORDS FROM DATA RECORD.
!     IF END-OF-FILE, REPOSITION FILE TO FIRST DATA RECORD AND RETRY.
!     IF ID WORD MATCHES USER, RETURN.
!
   last = 0
   ASSIGN 400 TO ret
 400  CALL read(*700,*400,trl(1),trl(2),3,0,flag)
   IF ( trl(2)/=Id(1) ) THEN
!
!     SKIP RECORD. READ ID WORDS FROM NEXT RECORD. IF MATCH,RETURN.
!     IF END-OF FILE, REPOSITION TO FIRST DATA RECORD AND RETRY.
!     IF NO MATCH, TEST FOR RETURN TO ORIGINAL FILE POSITION. IF SO,
!     QUEUE MESSAGE AND GIVE ALTERNATE RETURN. IF NOT, CONTINUE SEARCH.
!
      ASSIGN 500 TO ret
      CALL fwdrec(*200,trl(1))
   ELSE
      Flg = trl(4)
      RETURN
   ENDIF
 500  DO
      CALL read(*700,*300,trl(1),trl(5),3,0,flag)
      IF ( trl(5)/=Id(1) ) THEN
!
         IF ( trl(5)==trl(2) ) EXIT
         CALL fwdrec(*200,trl(1))
      ELSE
         Flg = trl(7)
         RETURN
      ENDIF
   ENDDO
 600  CALL sswtch(40,j)
!WKBD IF (J .NE. 0) CALL ERRTRC ('LOCATE  ',35)
   CALL mesage(30,72,Id)
   CALL fwdrec(*200,trl(1))
   RETURN 1
!
!     CODE TO POSITION FILE TO FIRST DATA RECORD.
!
 700  CALL rewind(trl(1))
   IF ( last/=0 ) GOTO 600
   last = 1
   CALL fwdrec(*200,trl(1))
   GOTO ret
END SUBROUTINE preloc