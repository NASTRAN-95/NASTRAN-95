
SUBROUTINE ifp4b(File,Scrt,Any,Space,Lspace,Recid,Eof)
   IMPLICIT NONE
   REAL Cls , Clsrew , Rd , Rdrew , Wrt , Wrtrew
   LOGICAL Nogo
   INTEGER Output , Sysbuf
   CHARACTER*23 Ufm
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Output , Nogo
   COMMON /xmssg / Ufm
   LOGICAL Any , Eof
   INTEGER File , Lspace , Scrt
   INTEGER Recid(2) , Space(5)
   LOGICAL bit
   INTEGER buf1 , buf2 , eor , flag , i , ifile , ilimit(3) , j , name(2) , noeor , rec(3)
!
!     THIS ROUTINE, CALLED BY IFP4, COPIES DATA FROM -FILE- TO -SCRT-
!     UP TO THE -RECID- SPECIFIED IF IT EXISTS AND COPIES THE -RECID-
!     IN ANY EVENT.  -ANY- IS SET TRUE IF THE -RECID- WAS FOUND.
!     -EOF- IS SET TRUE AS SOON AS AN END OF FILE IS HIT ON -FILE-.
!     -SPACE- IS A WORKING AREA OF LENGTH -LSPACE-.  IF RECID(1) = -1,
!     THE BALANCE OF -FILE- IS COPIED TO -SCRT- AND THEN -FILE- IS
!     REWOUND AND -SCRT- IS COPIED BACK ONTO -FILE-.  BOTH FILES ARE
!     THEN CLOSED.
!
   DATA name , noeor , eor/4HIFP4 , 4HB    , 0 , 1/ , ilimit/3*2147483647/
!
   IF ( Recid(1)==-1 ) THEN
!
!     WRAP UP FILES
!
      IF ( .NOT.(Eof) ) GOTO 300
      CALL write(Scrt,ilimit,3,eor)
      GOTO 500
   ELSE
      Any = .FALSE.
!
!     CHECK TRAILER BIT TO SEE IF RECORD EXISTS
!
      CALL ifp4f(Recid(2),File,bit)
      IF ( .NOT.bit ) THEN
!
!     RECORD DOES NOT CURRENTLY EXIST, THUS START ONE
!
         CALL write(Scrt,Recid,2,noeor)
         CALL write(Scrt,0,1,noeor)
!
!     PUT BIT IN TRAILER
!
         CALL ifp4g(Recid(2),File)
         RETURN
      ELSE
         IF ( Eof ) GOTO 900
!
!     READ A 3-WORD RECORD ID
!
         Any = .TRUE.
         ifile = File
      ENDIF
   ENDIF
 100  CALL read(*900,*1100,File,rec(1),3,noeor,flag)
   CALL write(Scrt,rec,3,noeor)
   IF ( rec(1)==Recid(1) ) RETURN
   DO
!
!     NOT THE CORRECT RECORD, THUS COPY BALANCE OF RECORD OVER.
!
      CALL read(*1000,*200,File,Space,Lspace,noeor,flag)
      CALL write(Scrt,Space,Lspace,noeor)
   ENDDO
 200  CALL write(Scrt,Space,flag,eor)
   GOTO 100
 300  DO
      CALL read(*500,*400,File,Space,Lspace,noeor,flag)
      CALL write(Scrt,Space,Lspace,noeor)
   ENDDO
 400  CALL write(Scrt,Space,flag,eor)
   GOTO 300
!
!     FILE IS ALL COPIED TO SCRT.  REWIND AND RETURN.
!
 500  Eof = .TRUE.
   CALL close(Scrt,Clsrew)
   CALL close(File,Clsrew)
!
!     COPY DATA FROM SCRT TO FILE.
!
   buf1 = 1
   buf2 = Sysbuf + 2
   i = 2*Sysbuf + 4
   j = Lspace - i
   IF ( i>Lspace ) CALL mesage(-8,0,name)
   ifile = File
   CALL open(*1200,File,Space(buf1),Wrtrew)
   ifile = Scrt
   CALL open(*1200,Scrt,Space(buf2),Rdrew)
 600  DO
      CALL read(*800,*700,Scrt,Space(i),j,noeor,flag)
      CALL write(File,Space(i),j,noeor)
   ENDDO
 700  CALL write(File,Space(i),flag,eor)
   GOTO 600
 800  CALL close(Scrt,Clsrew)
   CALL close(File,Clsrew)
   RETURN
!
!     ERROR CONDITIONS
!
 900  Nogo = .TRUE.
   WRITE (Output,99001) Ufm , Recid(1) , Recid(2) , File
99001 FORMAT (A23,' 4056, RECORD ID =',2I10,' IS OUT OF SYNC ON DATA ','BLOCK NUMBER',I10,/5X,'AN IFP4 SYSTEM ERROR.')
   Eof = .TRUE.
   RETURN
!
 1000 CALL mesage(-2,ifile,name)
 1100 CALL mesage(-3,ifile,name)
 1200 CALL mesage(-1,ifile,name)
END SUBROUTINE ifp4b