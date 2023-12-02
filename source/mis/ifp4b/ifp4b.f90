!*==ifp4b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp4b(File,Scrt,Any,Space,Lspace,Recid,Eof)
   IMPLICIT NONE
   USE C_NAMES
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Scrt
   LOGICAL :: Any
   INTEGER , DIMENSION(5) :: Space
   INTEGER :: Lspace
   INTEGER , DIMENSION(2) :: Recid
   LOGICAL :: Eof
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: bit
   INTEGER :: buf1 , buf2 , flag , i , ifile , j
   INTEGER , SAVE :: eor , noeor
   INTEGER , DIMENSION(3) , SAVE :: ilimit
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(3) :: rec
   EXTERNAL close , ifp4f , ifp4g , mesage , open , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Recid(1)==-1 ) THEN
!
!     WRAP UP FILES
!
            IF ( .NOT.(Eof) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(Scrt,ilimit,3,eor)
            GOTO 60
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
               IF ( Eof ) GOTO 120
!
!     READ A 3-WORD RECORD ID
!
               Any = .TRUE.
               ifile = File
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*120,*160,File,rec(1),3,noeor,flag)
         CALL write(Scrt,rec,3,noeor)
         IF ( rec(1)==Recid(1) ) RETURN
         DO
!
!     NOT THE CORRECT RECORD, THUS COPY BALANCE OF RECORD OVER.
!
            CALL read(*140,*20,File,Space,Lspace,noeor,flag)
            CALL write(Scrt,Space,Lspace,noeor)
         ENDDO
 20      CALL write(Scrt,Space,flag,eor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         DO
            CALL read(*60,*40,File,Space,Lspace,noeor,flag)
            CALL write(Scrt,Space,Lspace,noeor)
         ENDDO
 40      CALL write(Scrt,Space,flag,eor)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     FILE IS ALL COPIED TO SCRT.  REWIND AND RETURN.
!
 60      Eof = .TRUE.
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
         CALL open(*180,File,Space(buf1),Wrtrew)
         ifile = Scrt
         CALL open(*180,Scrt,Space(buf2),Rdrew)
         spag_nextblock_1 = 4
      CASE (4)
         DO
            CALL read(*100,*80,Scrt,Space(i),j,noeor,flag)
            CALL write(File,Space(i),j,noeor)
         ENDDO
 80      CALL write(File,Space(i),flag,eor)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     CALL close(Scrt,Clsrew)
         CALL close(File,Clsrew)
         RETURN
!
!     ERROR CONDITIONS
!
 120     Nogo = .TRUE.
         WRITE (Output,99001) Ufm , Recid(1) , Recid(2) , File
99001    FORMAT (A23,' 4056, RECORD ID =',2I10,' IS OUT OF SYNC ON DATA ','BLOCK NUMBER',I10,/5X,'AN IFP4 SYSTEM ERROR.')
         Eof = .TRUE.
         RETURN
!
 140     CALL mesage(-2,ifile,name)
 160     CALL mesage(-3,ifile,name)
 180     CALL mesage(-1,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifp4b
