
SUBROUTINE exio1
   IMPLICIT NONE
   INTEGER Avblks , Blksiz , Buf(10) , Cor(1) , Date(3) , Datype(2) , Device(2) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , &
         & Ditsiz , Dry , Eofnrw , Filnam(10) , Filsiz(10) , Hdrec(10) , Head1(96) , Head2(96) , Hours , Ifrst , Ihalf , Io ,       &
         & Ioblk , Ioitcd , Iolbn , Iomode , Iopbn , Ioptr , Iosind , Items(7,1) , Jhalf , Line , Mach , Mdi , Mdibl , Mdilbn ,     &
         & Mdipbn , Min , Mode(2) , Names(10) , Nbpc , Nbpw , Ncpw , Nfiles , Nitem , Nlpp , Noblks , Norew , Nout , Nxt , Nxtcur , &
         & Nxtfsz(10) , Nxtlbn , Nxtpbn , Nxttsz , Passwd(2) , Pdate , Pos(2) , Ptime , Rd , Rdrew , Rew , Savrec(9) , Sec ,        &
         & Ssname(2) , Status , Supsiz , Sysbuf , Time(3) , Uname(2) , Wrt , Wrtrew , Z(1)
   LOGICAL Ditup , First , Mdiup , Nxtrst , Nxtup , Opnsof
   REAL Formt(2) , X1(6) , X2(2) , X3(2) , X4(21) , Xmach
   CHARACTER*25 Sfm , Uwm
   CHARACTER*31 Sim
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Xmach , Device , Uname , Formt , Mode , Pos , Datype , Names , Pdate , Ptime , Time , Ssname , Savrec ,    &
                 & Hdrec , Buf
   COMMON /itemdt/ Nitem , Items
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /output/ Head1 , Head2
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Io , Iopbn , Iolbn , Iomode , Ioptr , Iosind , Ioitcd , Ioblk ,&
                 & Mdi , Mdipbn , Mdilbn , Mdibl , Nxt , Nxtpbn , Nxtlbn , Nxttsz , Nxtfsz , Nxtcur , Ditup , Mdiup , Nxtup , Nxtrst
   COMMON /sofcom/ Nfiles , Filnam , Filsiz , Status , Passwd , First , Opnsof
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks , Noblks , Ifrst
   COMMON /system/ Sysbuf , Nout , X1 , Nlpp , X2 , Line , X3 , Date , X4 , Nbpc , Nbpw , Ncpw
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm , Sim
   COMMON /zzzzzz/ Z
   INTEGER all , append , blank , buf1 , buf2 , buf3 , buf4 , buf5 , check , comprs , disk , dump , eoi , eqf , flag , hdr , i ,    &
         & icount , id , idm , ifile , inblk(15) , incblk , ipos , irec , iss , item , itype , iz2 , j , jcopy , jmdi , jrmdi ,     &
         & jss , k , kdh , kdit , kmdi , knxt , lcore , lrec1 , mask , matric , n , ncopy , ncore , nitems , nold , norewi , nos ,  &
         & nos4 , nsave , nw , nwds , oldtsz , outblk(15) , phase3 , q , qqqq , rc , restor , rewi , rewi2 , scr1 , scr2 , sof ,    &
         & sofin , sofout , srd , subr(2) , swrt , tables , tape , unit , whole(2) , xitems(50) , xxxx
   INTEGER andf , ittype , korsz , lshift , orf , rshift , sofsiz
   LOGICAL tapbit
   EXTERNAL andf , lshift , orf , rshift
!
!     EXIO1 SERVICES INTERNAL FORMAT FUNCTIONS FOR EXIO.
!
   EQUIVALENCE (Cor(1),Z(1))
   EQUIVALENCE (Time(1),Hours) , (Time(2),Min) , (Time(3),Sec)
   DATA tape , disk , sofin , sofout , check/4HTAPE , 4HDISK , 4HSOFI , 4HSOFO , 4HCHEC/ , append , comprs , norewi , rewi ,        &
       &eqf/4HAPPE , 4HCOMP , 4HNORE , 4HREWI , 4HEOF / , all , matric , tables , phase3 , dump/4HALL  , 4HMATR , 4HTABL , 4HPHAS , &
       &4HDUMP/ , restor , whole , rewi2/4HREST , 4HWHOL , 4HESOF , 4HND  / , subr , blank , sof , eoi/4HEXIO , 4H1    , 4H     ,   &
       &4HSOF  , 4HEOI / , id , hdr , q , qqqq , xxxx/4H$ID$ , 4H$HD$ , 4HQ    , 4HQQQQ , 4HXXXX/
   DATA scr1 , scr2 , srd , swrt , iz2/301 , 302 , 1 , 2 , 2/
!
!     INITIALIZE
!
   IF ( Nitem>50 ) CALL errmkn(25,10)
   lcore = korsz(Z)
   buf1 = lcore - Sysbuf + 1
   buf2 = buf1 - Sysbuf - 1
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   lcore = buf5 - 1
   ncore = lcore
   nos = 0
   idm = 1
   IF ( lcore<=0 ) CALL mesage(-8,0,subr)
   IF ( Mode(1)/=restor ) CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
   unit = Uname(1)
!
!     CHECK TAPE BIT IF DEVICE=TAPE
!
   IF ( Device(1)/=disk .AND. Mode(1)/=comprs .AND. Mode(1)/=append ) THEN
      IF ( Device(1)/=tape ) THEN
         WRITE (Nout,99001) Uwm , Device
99001    FORMAT (A25,' 6335, ',2A4,' IS AN INVALID DEVICE FOR MODULE EXIO')
         GOTO 4900
      ELSEIF ( .NOT.tapbit(unit) ) THEN
!
!     ERRORS CAUSING MODULE AND/OR JOB TERMINATION
!
         WRITE (Nout,99002) Uwm , Uname
!
!     TEXT OF ERROR MESSAGES
!
99002    FORMAT (A25,' 6334, EXIO DEVICE PARAMETER SPECIFIES TAPE, BUT ','UNIT ',2A4,' IS NOT A PHYSICAL TAPE')
         GOTO 4900
      ENDIF
   ENDIF
!
!     SET REWIND VARIABLE
!
!     IF SOFOUT COMMAND POSITION TO END-OF-FILE IF REQUESTED
!
!     IF POSITION = REWIND AND WE ARE WRITING THEN BCKREC OVER LAST EOF
!
!     IF POSITION = EOF AND WE ARE WRITING THEN BCKREC FIRST TO INSURE
!     WE ARE INFRONT OF AND EOF AND THEN SEARCH FOR EOF
!
   ipos = -1
   IF ( Pos(1)==norewi .OR. Pos(1)==eqf ) ipos = 2
   IF ( Pos(1)==rewi ) ipos = 0
   IF ( ipos<0 ) THEN
      WRITE (Nout,99003) Uwm , Pos
99003 FORMAT (A25,' 6339, ',2A4,' IS AN INVALID FILE POSITIONING ','PARAMETER FOR MODULE EXIO')
      GOTO 4900
   ELSE
      IF ( Mode(1)==dump .OR. Mode(1)==restor ) ipos = 0
      IF ( ipos==0 ) THEN
         Head2(13) = rewi
         Head2(14) = rewi2
      ENDIF
      IF ( Mode(1)/=sofout ) GOTO 300
      IF ( ipos==0 ) GOTO 300
      CALL open(*4500,unit,Z(buf4),Rd)
      CALL bckrec(unit)
      IF ( Pos(1)==norewi ) GOTO 200
      DO
         CALL fwdrec(*100,unit)
      ENDDO
   ENDIF
 100  CALL bckrec(unit)
 200  CALL close(unit,Norew)
!
!     BRANCH ON MODE OF OPERATION
!
 300  IF ( Mode(1)==sofout .OR. Mode(1)==dump ) THEN
!
!
!     **********************   W R I T E   **********************
!
!     OPEN FILE AND WRITE 9 WORD ID RECORD
!
      CALL open(*4500,unit,Z(buf4),Wrtrew+ipos)
      CALL waltim(Sec)
      Hours = Sec/3600
      Sec = mod(Sec,3600)
      Min = Sec/60
      Sec = mod(Sec,60)
      Hdrec(1) = id
      Hdrec(2) = Passwd(1)
      Hdrec(3) = Passwd(2)
      DO i = 1 , 3
         Hdrec(i+3) = Date(i)
         Hdrec(i+6) = Time(i)
      ENDDO
      CALL write(unit,Hdrec,9,1)
      CALL page
      WRITE (Nout,99023) Uim , Passwd , Date , Time
      Line = Line + 1
!
!     WRITE DIT AND MDI CONTROL WORDS
!
      n = Ditsiz/2
      CALL write(unit,n,1,0)
      DO i = 1 , n
         CALL fdit(i,j)
         CALL write(unit,Cor(j),2,0)
         CALL fmdi(i,j)
         CALL write(unit,Cor(j+1),2,0)
      ENDDO
      CALL write(unit,0,0,1)
      CALL write(unit,eoi,1,1)
      IF ( Mode(1)/=dump ) THEN
!
!     STANDARD FORM --
!
!     COPY OUT EACH SUBSTRUCTURE/ITEM WITH ITS DATA IN THE CORRECT
!     SEQUENCE.
!
!     SETUP THE ARRAY XITEMS OF NAMES OF ITEMS TO BE COPIED.
!
         IF ( Datype(1)==all ) THEN
            nitems = Nitem
            DO i = 1 , Nitem
               xitems(i) = Items(1,i)
            ENDDO
         ELSEIF ( Datype(1)==tables ) THEN
            nitems = 0
            DO i = 1 , Nitem
               IF ( Items(2,i)<=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = Items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( Datype(1)==matric ) THEN
            nitems = 0
            DO i = 1 , Nitem
               IF ( Items(2,i)>0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = Items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( Datype(1)/=phase3 ) THEN
            nitems = 2
            xitems(1) = Datype(1)
            xitems(2) = Datype(2)
            IF ( xitems(2)==blank ) nitems = 1
         ELSE
            nitems = 0
            DO i = 1 , Nitem
               IF ( andf(Items(7,i),8)/=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = Items(1,i)
               ENDIF
            ENDDO
         ENDIF
!
!     LOOP OVER SUBSTRUCTURE NAMES.  FOR EACH SUBSTRUCTURE, WRITE OUT
!     THE NITEMS IN XITEMS.
!
         iss = 0
      ELSE
!
!
!     DUMP FORM --
!
!     COPY OUT ALL SOF SUPERBLOCKS WHICH HAVE BEEN USED WITHOUT REGARD
!     TO THE DATA SEQUENCE OR CONTENT.
!
!
         DO i = 1 , Noblks
            CALL sofio(srd,i,Z(buf1))
            CALL write(unit,Z(buf1+3),Blksiz,0)
         ENDDO
         CALL write(unit,0,0,1)
         CALL close(unit,Rew)
         WRITE (Nout,99004) Uim , Noblks , Nxttsz , Uname
99004    FORMAT (A29,' 6337,',I6,' BLOCKS (',I4,' SUPERBLOCKS) OF THE SOF',' SUCCESSFULLY DUMPED TO EXTERNAL FILE ',2A4)
         GOTO 4200
      ENDIF
   ELSEIF ( Mode(1)==sofin .OR. Mode(1)==restor ) THEN
!
!     ***********************   R E A D  ************************
!
!     BRANCH FOR RESTORE OR STANDARD READ
!
      IF ( Mode(1)/=restor ) THEN
!
!     STANDARD FORM -
!
!     COPY IN EACH INDIVIDUAL SUBSTRUCTURE/ITEM.
!
         iss = 0
!
!     SETUP ARRAY OF NAMES OF ITEMS TO BE COPIED.
!
         IF ( Datype(1)==all ) THEN
            nitems = Nitem
            DO i = 1 , Nitem
               xitems(i) = Items(1,i)
            ENDDO
         ELSEIF ( Datype(1)==tables ) THEN
            nitems = 0
            DO i = 1 , Nitem
               IF ( Items(2,i)<=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = Items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( Datype(1)==matric ) THEN
            nitems = 0
            DO i = 1 , Nitem
               IF ( Items(2,i)>0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = Items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( Datype(1)/=phase3 ) THEN
            nitems = 2
            xitems(1) = Datype(1)
            xitems(2) = Datype(2)
            IF ( xitems(2)==blank ) nitems = 1
         ELSE
            nitems = 0
            DO i = 1 , Nitem
               IF ( andf(Items(7,i),8)/=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = Items(1,i)
               ENDIF
            ENDDO
         ENDIF
!
!     DETERMINE NUMBER OF SUBSTRUCTURE/ITEMS TO BE COPIED AND INITIALIZE
!     COUNTER.
!
         jcopy = 0
         ncopy = 0
         IF ( Names(1)/=whole(1) .OR. Names(2)/=whole(2) ) THEN
            DO i = 1 , 5
               IF ( Names(2*i-1)/=xxxx ) ncopy = ncopy + 1
            ENDDO
            ncopy = ncopy*nitems
            IF ( Pdate/=0 ) ncopy = 1
         ENDIF
!
!     OPEN THE EXTERNAL FILE AND READ THE IDENTIFICATION OR HEADER
!     RECORD.
!     REMEMBER IT IN CASE THE USER HAS REQUESTED A SUBSTRUCTURE/ITEM
!     WHICH IS NOT PRESENT ON THE FILE.
!
         CALL page
         CALL open(*4500,unit,Z(buf4),Rdrew+ipos)
         GOTO 800
!
!     RESTORE FORM --
!
!     COPY EACH LOGICAL RECORD ON THE EXTERNAL FILE INTO CONSEQUTIVE,
!     CONTIGUOUS BLOCKS ON THE RESIDENT SOF.
!
!     MAKE SURE THE RESIDENT SOF IS EMPTY.
!
      ELSEIF ( Status/=0 ) THEN
         WRITE (Nout,99005) Uwm
99005    FORMAT (A25,' 6342, SOF RESTORE OPERATION FAILED.  THE RESIDENT ','SOF IS NOT EMPTY')
         GOTO 4900
      ELSE
         CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
         CALL sofcls
!
!     OPEN FILE AND READ THE ID RECORD
!
         CALL open(*4500,unit,Z(buf4),Rdrew)
         CALL read(*4400,*4400,unit,Hdrec,9,1,flag)
         IF ( Hdrec(1)/=id ) GOTO 4400
         CALL page
         Line = Line + 1
         WRITE (Nout,99023) Uim , (Hdrec(i),i=2,9)
         CALL fwdrec(*4600,unit)
         CALL fwdrec(*4600,unit)
!
!     BEGIN DATA TRANSFER
!
         i = 1
         DO
            CALL read(*4600,*700,unit,Z(buf1+3),Blksiz,0,flag)
            CALL sofio(swrt,i,Z(buf1))
            i = i + 1
         ENDDO
      ENDIF
   ELSEIF ( Mode(1)==check ) THEN
!
!     *********************   C H E C K   ***************************
!
!     REWIND THE EXTERNAL FILE AND PRINT A LIST OF ALL SUBSTRUCTURE/
!     ITEMS ON IT WITH THE DATE AND TIME WHEN THEY WERE WRITTEN THERE.
!
      CALL open(*4500,unit,Z(buf4),Rdrew)
      CALL page
      WRITE (Nout,99006) Uim , Uname
99006 FORMAT (A29,' 6349, CONTENTS OF EXTERNAL SOF FILE ',2A4,' FOLLOW')
      Line = Line + 1
      CALL read(*4600,*4700,unit,Buf,9,1,flag)
      GOTO 2900
   ELSEIF ( Mode(1)==append ) THEN
!
!     ********************   A P P E N D   ***************************
!
!     ADD AN EXISTING SOF IN ITS RANDOM ACCESS FORM TO THE RESIDENT SOF.
!     THE MDI AND DIT OF THE EXTERNAL SOF ARE MERGED INTO THOSE OF THE
!     RESIDENT SOF.  THE NXT OF THE EXTERNAL SOF IS INCREMENTED BY THE
!     NUMBER OF BLOCKS IN THE RESIDENT SOF.  THE COMMON BLOCKS /SYS/,
!     /SOF/, AND /SOFCOM/ ARE UPDATED AND WRITTEN TO THE FIRST PHYSICAL
!     BLOCK ON EACH FILE OF THE RESIDENT SOF BY SOFCLS.  NOTE THAT NO
!     USER DATA IS ACTUALLY MOVED.
!
!     FIRST, ADD THE EXTERNAL SOF TO /SOFCOM/ SO THAT SOFIO CAN BE USED
!     TO READ IT.
!
      IF ( Nfiles<10 ) THEN
         Nfiles = Nfiles + 1
         Filnam(Nfiles) = unit
         Filsiz(Nfiles) = 4
         nsave = Noblks + 1
!
!     READ THE FIRST PHYSICAL BLOCK OF THE EXTERNAL SOF AND SEE THAT IT
!     IS COMPATIBLE WITH THE RESIDENT SOF.
         incblk = -4
         DO i = 1 , Nfiles
            incblk = incblk + Filsiz(i)
         ENDDO
         CALL sofio(srd,incblk+1,Z(buf4))
!
!     PASSWORD CHECK
!
         IF ( Z(buf4+3)/=Datype(1) .OR. Z(buf4+4)/=Datype(2) ) THEN
            WRITE (Nout,99027) Uwm , Uname
            WRITE (Nout,99007)
99007       FORMAT (32X,17HINVALID PASSWORD.)
            incblk = -1
         ENDIF
!
!     FILE SEQUENCE NUMBER CHECK
!
         IF ( Z(buf4+5)/=1 ) THEN
            WRITE (Nout,99027) Uwm , Uname
            WRITE (Nout,99008)
99008       FORMAT (32X,'THE SEQUENCE NUMBER OF THE EXTERNAL SOF FILE IS NOT',' 1')
            incblk = -1
         ENDIF
!
!     NUMBER OF EXTERNAL FILES CHECK
!
         IF ( Z(buf4+6)/=1 ) THEN
            WRITE (Nout,99027) Uwm , Uname
            WRITE (Nout,99009)
99009       FORMAT (32X,'THE EXTERNAL SOF FILE MUST CONSIST OF ONLY ONLY ONE',' PHYSICAL UNIT')
            incblk = -1
         ENDIF
!
!     BLOCKSIZE CHECK
!
         IF ( Z(buf4+27)/=Blksiz ) THEN
            WRITE (Nout,99027) Uwm , Uname
            WRITE (Nout,99010) Blksiz , Z(buf4+27)
99010       FORMAT (32X,45HTHE EXTERNAL SOF HAS INCOMPATIBLE BLOCK SIZE.,/32X,32HBLOCK SIZE OF THE RESIDENT SOF =,I5,/32X,          &
                   &32HBLOCK SIZE OF THE EXTERNAL SOF =,I5)
            incblk = -1
         ENDIF
         IF ( incblk<0 ) THEN
!
!     APPEND OPERATION ABORTED.  RESTORE THE COMMON BLOCKS FOR THE
!     RESIDENT SOF.
!
            First = .TRUE.
            Opnsof = .FALSE.
            CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
            GOTO 4800
         ELSE
!
!     COMPLETE THE UPDATING OF THE COMMON BLOCKS
!
            Filsiz(Nfiles) = Z(buf4+17)
            Avblks = Avblks + Z(buf4+30)
            Nxtcur = 1
            Nxtrst = .TRUE.
            Nxtfsz(Nfiles) = Z(buf4+36)
            j = Nfiles - 1
            Nxttsz = 0
            DO i = 1 , j
               Nxttsz = Nxttsz + Nxtfsz(i)
            ENDDO
            oldtsz = Nxttsz + 1
            Nxttsz = Nxttsz + Z(buf4+35)
!
!     READ THE DIT OF THE EXTERNAL SOF AND ADD EACH SUBSTRUCTURE THERE
!     TO THE DIT OF THE RESIDENT SOF.  KEEP A TABLE IN OPEN CORE OF TWO
!     WORDS PER SUBSTRUCTURE -
!
!     (1)  SUBSTRUCTURE NUMBER FROM THE EXTERNAL SOF.
!     (2)  NEW SUBSTRUCTURE NUMBER ON THE RESIDENT SOF.
!
            nold = Z(buf4+32)
            IF ( 2*nold>lcore ) THEN
               n = 8
               GOTO 5000
            ELSE
               iss = 1
               k = 1
               kdit = Z(buf4+33) + incblk
               kmdi = Z(buf4+34) + incblk
               GOTO 3200
            ENDIF
         ENDIF
      ELSE
         WRITE (Nout,99027) Uwm , Uname
         WRITE (Nout,99011)
99011    FORMAT (32X,'TOO MANY PHYSICAL SOF UNITS. MAXIMUM ALLOWED IS 10')
         GOTO 4900
      ENDIF
   ELSEIF ( Mode(1)==comprs ) THEN
!
!     ********************   C O M P R E S S   **********************
!
!     FOR EACH SUBSTRUCTURE IN THE DIT, COPY EACH ITEM WHICH EXISTS OR
!     PSEUDO-EXISTS TO SCR1 AND DELETE THE ITEM ON THE SOF.  THEN COPY
!     ALL ITEMS BACK.  ALL INTERMEDIATE FREE BLOCKS WILL THUS BE
!     ELIMINATED AND THE DATA FOR ANY ONE ITEM WILL BE STORED ON
!     CONTIGUOUS BLOCKS.
!
!     THE FORMAT OF THE SCRATCH FILE IS --
!
!                                      +------------+
!     SUBSTRUCTURE NAME (2 WORDS)      I            I+
!     ITEM NAME (1 WORD)               I HEADER     I +
!     PSEUDO FLAG -- 2 FOR PSEUDO-ITEM I RECORD     I  +
!                    3 FOR REAL DATA   I            I   +   REPEATED
!                                      +------------+    +  FOR EACH
!     DATA -- 1 SOF GROUP PER          I DATA       I   +   SUBS./ITEM
!             GINO LOGICAL RECORD      I RECORDS    I  +
!                                      +------------+ +
!     END OF ITEM FLAG (1 WORD)        I EOI RECORD I+
!                                      +------------+
!
      unit = scr1
      CALL open(*4500,scr1,Z(buf4),Wrtrew)
!
!     COPY OUT DIT AND MDI INFORMATION
!
      iss = 0
      DO k = 1 , Ditsiz , 2
         iss = iss + 1
         CALL fdit(iss,j)
         CALL write(scr1,Cor(j),2,0)
         CALL fmdi(iss,j)
         CALL write(scr1,Cor(j+1),2,0)
      ENDDO
      CALL write(scr1,0,0,1)
!
!     COPY OUT SUBSTRUCTURE ITEMS
!
      iss = 0
      DO k = 1 , Ditsiz , 2
         iss = iss + 1
         CALL fdit(iss,j)
         Ssname(1) = Cor(j)
         Ssname(2) = Cor(j+1)
         IF ( Ssname(1)/=blank ) THEN
            DO item = 1 , Nitem
               kdh = Items(2,item)
               IF ( kdh==1 ) THEN
!
!     PROCESS MATRIX ITEMS
!
                  CALL mtrxi(scr2,Ssname,Items(1,item),0,rc)
                  ifile = scr2
                  IF ( rc==2 ) GOTO 305
                  IF ( rc==3 ) THEN
                  ELSEIF ( rc==4 .OR. rc==5 ) THEN
                     CALL smsg(rc-2,Items(1,item),Ssname)
                  ELSEIF ( rc==6 ) THEN
                     GOTO 5100
                  ELSE
                     CALL write(scr1,Ssname,2,0)
                     CALL write(scr1,Items(1,item),1,0)
                     CALL write(scr1,3,1,1)
                     CALL open(*5100,scr2,Z(buf5),Rdrew)
                     Z(1) = scr2
                     CALL rdtrl(Z(1))
                     CALL write(scr1,Z(iz2),6,1)
                     CALL cpyfil(scr2,scr1,Z,lcore,icount)
                     CALL write(scr1,eoi,1,1)
                     CALL close(scr2,1)
                  ENDIF
                  CYCLE
               ELSE
                  CALL sfetch(Ssname,Items(1,item),srd,rc)
                  IF ( rc==1 ) THEN
!
!     ITEM EXISTS.  COPY IT OUT.
!
                     CALL write(scr1,Ssname,2,0)
                     CALL write(scr1,Items(1,item),1,0)
                     CALL write(scr1,3,1,1)
                     DO
                        CALL suread(Z,lcore,n,rc)
                        IF ( rc>1 ) THEN
                           CALL write(scr1,Z,n,1)
                           IF ( rc/=2 ) THEN
!
!     END OF ITEM HIT.  WRITE EOI RECORD
!
                              CALL write(scr1,eoi,1,1)
                              GOTO 310
                           ENDIF
                        ELSE
                           CALL write(scr1,Z,lcore,0)
                        ENDIF
                     ENDDO
                  ELSEIF ( rc==2 ) THEN
                  ELSEIF ( rc==3 ) THEN
                     CYCLE
                  ELSE
                     CALL smsg(rc-2,Items(1,item),Ssname)
                     CYCLE
                  ENDIF
               ENDIF
!
!     ITEM PSEUDO-EXISTS.  WRITE PSEUDO-HEADER RECORD AND EOI RECORD.
!
 305           CALL write(scr1,Ssname,2,0)
               CALL write(scr1,Items(1,item),1,0)
               CALL write(scr1,2,1,1)
               CALL write(scr1,eoi,1,1)
 310        ENDDO
         ENDIF
      ENDDO
!
!     COPY ALL ITEMS BACK TO THE SOF
!
      CALL close(scr1,Rew)
      CALL open(*4500,scr1,Z(buf4),Rdrew)
!
!     RE-INITIALIZE THE SOF, THEN RESTORE THE OLD DIT AND MDI
!
      CALL sofcls
      Status = 0
      First = .TRUE.
      CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
      CALL page
      iss = 0
      DO
         CALL read(*4600,*3500,scr1,Buf,4,0,flag)
         iss = iss + 1
         IF ( Buf(1)/=blank ) THEN
            CALL crsub(Buf,i)
            CALL fmdi(i,j)
            Cor(j+1) = Buf(3)
            Cor(j+2) = Buf(4)
            Mdiup = .TRUE.
         ENDIF
      ENDDO
   ELSE
      WRITE (Nout,99012) Uwm , Mode
99012 FORMAT (A25,' 6338, ',2A4,' IS AN INVALID MODE PARAMETER FOR ','MODULE EXIO')
      GOTO 4900
   ENDIF
 400  DO
      iss = iss + 1
      IF ( Names(1)/=whole(1) .OR. Names(2)/=whole(2) ) THEN
!
!     WRITE ONLY THOSE SUBSTRUCTURES IN THE PARAMETER LIST
!
         IF ( iss>5 ) GOTO 600
         IF ( Names(2*iss-1)/=xxxx ) THEN
            Ssname(1) = Names(2*iss-1)
            Ssname(2) = Names(2*iss)
            EXIT
         ENDIF
      ELSE
!
!     WRITE ALL SUBSTRUCTURES IN THE RESIDENT SOF.
!
         IF ( iss>Ditsiz/2 ) GOTO 600
         CALL fdit(iss,i)
         IF ( Cor(i)/=blank ) THEN
            Ssname(1) = Cor(i)
            Ssname(2) = Cor(i+1)
            EXIT
         ENDIF
      ENDIF
   ENDDO
!
!     LOOP OVER ALL ITEMS OF THIS SUBSTRUCTURE.
!
   DO item = 1 , nitems
      kdh = ittype(xitems(item))
      IF ( kdh==1 ) GOTO 550
      CALL sfetch(Ssname,xitems(item),srd,rc)
      IF ( rc==1 ) GOTO 550
      IF ( rc==3 ) CYCLE
      IF ( rc==4 .OR. rc==5 ) GOTO 500
 450  Line = Line + 2
      IF ( Line>Nlpp ) CALL page
      WRITE (Nout,99013) Uwm , Ssname , xitems(item)
99013 FORMAT (A25,' 6340, SUBSTRUCTURE ',2A4,' ITEM ',A4,' PSEUDOEXISTS ONLY AND CANNOT BE COPIED OUT BY EXIO')
      CYCLE
 500  Line = Line + 2
      IF ( Line>Nlpp ) CALL page
      CALL smsg(rc-2,xitems(item),Ssname)
      CYCLE
!
!     WRITE SUBSTRUCTURE/ITEM HEADER RECORD
!
 550  CALL waltim(Sec)
      Hours = Sec/3600
      Sec = mod(Sec,3600)
      Min = Sec/60
      Sec = mod(Sec,60)
      Hdrec(1) = hdr
      Hdrec(2) = Ssname(1)
      Hdrec(3) = Ssname(2)
      Hdrec(4) = xitems(item)
      DO i = 1 , 3
         Hdrec(i+4) = Date(i)
         Hdrec(i+7) = Time(i)
      ENDDO
      IF ( kdh==1 ) THEN
!
!     COPY MATRIX DATA ITEMS
!
         ifile = scr1
         CALL mtrxi(scr1,Ssname,xitems(item),0,rc)
         IF ( rc==2 ) GOTO 450
         IF ( rc==3 ) CYCLE
         IF ( rc==4 .OR. rc==5 ) GOTO 500
         IF ( rc==6 ) GOTO 5100
         CALL write(unit,Hdrec,10,1)
         Z(1) = scr1
         CALL rdtrl(Z(1))
         CALL write(unit,Z(iz2),6,1)
         CALL open(*5100,scr1,Z(buf5),Rdrew)
         CALL cpyfil(scr1,unit,Z,lcore,icount)
         CALL close(scr1,1)
      ELSE
         CALL write(unit,Hdrec,10,1)
         DO
!
!     COPY DATA
!
            CALL suread(Z(1),lcore,nwds,rc)
            IF ( rc==2 ) THEN
               CALL write(unit,Z,nwds,1)
            ELSEIF ( rc==3 ) THEN
               EXIT
            ELSE
               CALL write(unit,Z,lcore,0)
            ENDIF
         ENDDO
      ENDIF
!
!     WRITE END-OF-ITEM RECORD AND USER MESSAGE
!
      CALL write(unit,eoi,1,1)
      Line = Line + 1
      IF ( Line>Nlpp ) CALL page
      WRITE (Nout,99024) Uim , Ssname , xitems(item) , sof , unit , Date , Time
   ENDDO
!
!     BOTTOM OF LOOP OVER SUBSTRUCTURES
!
   GOTO 400
!
!     ALL SUBSTRUCTURE/ITEMS HAVE NOW BEEN COPIED.  CLOSE WITH EOF AND
!     NO REWIND (IN CASE MORE DATA TO FOLLOW).
!
!     WRITE EOF FOR NOU BECAUSE LEVEL 16 GINO OPT=3 DOESN T AS
!     ADVERTISED
!
 600  CALL eof(unit)
   CALL close(unit,Eofnrw)
   GOTO 4200
!
!     RESTORE COMPLETE.  CLOSE FILE AND GIVE USER THE NEWS.
!
 700  CALL close(unit,Rew)
   i = i - 1
   WRITE (Nout,99014) Uim , i
99014 FORMAT (A29,' 6344, SOF RESTORE OF ',I6,' BLOCKS SUCCESSFULLY ','COMPLETED')
   GOTO 4300
 800  CALL read(*900,*1000,unit,Hdrec,10,1,lrec1)
   lrec1 = 10
   GOTO 1000
 900  CALL rewind(unit)
   GOTO 800
 1000 DO i = 1 , lrec1
      Buf(i) = Hdrec(i)
   ENDDO
   IF ( Hdrec(1)==id ) GOTO 1300
   IF ( Hdrec(1)==hdr ) GOTO 1300
   GOTO 4400
 1100 IF ( Names(1)==whole(1) .AND. Names(2)==whole(2) ) THEN
!
!     READ OPERATION COMPLETE
!
      CALL close(unit,Norew)
      GOTO 4200
   ELSE
      CALL rewind(unit)
!
!     SCAN THROUGH THE EXTERNAL TAPE.  FOR EACH SUBSTRUCTURE/ITEM
!     ENCOUNTERED, CHECK TO SEE IF IT SHOULD BE READ.  THEN, EITHER
!     READ OR SKIP IT.
!
!     FOR EACH SUBSTRUCTURE/ITEM WHICH IS READ, SAVE THE HEADER RECORD
!     IN OPEN CORE.  WHEN DUPLICATES ARE FOUND, AND THE DATE AND TIME
!     PARAMETERS HAVE NOT BEEN SET, ISSUE A WARNING AND USE THE MOST
!     RECENT.
!
!     IF THE DATE AND TIME PARAMETERS ARE NON-ZERO, READ ONLY THE
!     SUBSTRUCTURE/ITEM WHICH HAS MATCHING VALUES AND IGNORE THE
!     SUBSTRUCTURE AND ITEM NAME PARAMETERS.
!
!     READ AN IDENTIFICATION OR HEADER RECORD
!
      CALL read(*1100,*1200,unit,Buf,10,1,flag)
   ENDIF
!
!     CHECK IT AGAINST THE FIRST RECORD READ.  IF IT MATCHES, THE ENTIRE
!     TAPE HAS BEEN SCANNED, BUT NOT ALL ITEMS WERE FOUND.
!
 1200 DO i = 1 , lrec1
      IF ( Buf(i)/=Hdrec(i) ) GOTO 1300
   ENDDO
!
!     THE ENTIRE EXTERNAL FILE HAS NOW BEEN SCANNED, BUT NOT ALL ITEMS
!     WERE FOUND.  WARN USER OF EACH ITEM NOT FOUND.
!
!     SKIP REMAINDER OF CURRENT ITEM SO FILE IS PROPERLY POSITIONED
!     FOR NEXT EXECUTION OF MODULE.
!
   DO i = 1 , 9 , 2
      IF ( Names(i)/=xxxx ) THEN
         DO item = 1 , nitems
            IF ( iss/=0 ) THEN
               DO j = 1 , iss
                  jss = 10*(j-1)
                  IF ( Names(i)==Z(jss+1) .AND. Names(i+1)==Z(jss+2) .AND. xitems(item)==Z(jss+3) ) GOTO 1220
               ENDDO
            ENDIF
            Line = Line + 2
            IF ( Line>Nlpp ) CALL page
            WRITE (Nout,99015) Uwm , Names(i) , Names(i+1) , xitems(item) , Uname
99015       FORMAT (A25,' 6348, SUBSTRUCTURE ',2A4,' ITEM ',A4,' NOT FOUND ON EXTERNAL FILE ',2A4)
 1220    ENDDO
      ENDIF
   ENDDO
   GOTO 2800
!
!     IF THAT WAS AN ID RECORD, ISSUE MESSAGE AND GO BACK TO READ THE
!     IMMEDIATELY FOLLOWING HEADER RECORD.
!
 1300 IF ( Buf(1)==id ) THEN
!
!     READ OLD DIT AND MDI DATA
!
      CALL read(*4600,*4700,unit,nos,1,0,flag)
      lcore = ncore - 4*nos
      idm = lcore + 1
      IF ( lcore<=0 ) THEN
         n = 8
         GOTO 5000
      ELSE
         nos4 = nos*4
         CALL read(*4600,*4700,unit,Z(idm),nos4,1,flag)
         CALL fwdrec(*4600,unit)
         Line = Line + 1
         IF ( Line>Nlpp ) CALL page
         WRITE (Nout,99023) Uim , (Buf(i),i=2,9)
         CALL read(*1100,*1200,unit,Buf,10,1,flag)
         GOTO 1200
      ENDIF
!
!     READ OR SKIP THE SUBSTRUCTURE/ITEM DATA.
!
   ELSEIF ( Pdate/=0 ) THEN
!
!     IF DATE AND TIME PARAMETERS WERE INVOKED, CHECK THEM.
!
      IF ( mod(Pdate,100)/=Buf(7) .OR. Pdate/10000/=Buf(5) .OR. mod(Pdate,10000)/100/=Buf(6) .OR. Ptime/10000/=Buf(8) .OR.          &
         & mod(Ptime,10000)/100/=Buf(9) .OR. mod(Ptime,100)/=Buf(10) ) GOTO 1900
      GOTO 2000
   ELSE
      IF ( Names(1)==whole(1) .AND. Names(2)==whole(2) ) GOTO 1500
      DO i = 1 , 5
         IF ( Names(2*i-1)/=xxxx ) THEN
            IF ( Buf(2)==Names(2*i-1) .AND. Buf(3)==Names(2*i) ) GOTO 1350
         ENDIF
      ENDDO
      GOTO 1400
 1350 DO i = 1 , nitems
         IF ( Buf(4)==xitems(i) ) GOTO 1500
      ENDDO
   ENDIF
 1400 DO
!
!     SKIP -
!
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4600,*1400,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,Buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4600,unit)
      ENDIF
   ENDDO
!
!     READ -
!
!     CHECK HEADER RECORDS SAVED IN CORE FOR DUPLICATE
!
 1500 IF ( iss/=0 ) THEN
      DO i = 1 , iss
         jss = 10*(i-1)
         DO j = 1 , 3
            IF ( Buf(j+1)/=Z(jss+j) ) GOTO 1550
         ENDDO
         GOTO 1600
 1550 ENDDO
   ENDIF
!
!     NO DUPLICATE.  ADD THIS HEADER TO THOSE IN CORE.
!
   IF ( 10*(iss+1)>lcore ) THEN
      n = 8
      GOTO 5000
   ELSE
      DO i = 1 , 9
         Z(10*iss+i) = Buf(i+1)
      ENDDO
      Z(10*iss+10) = 0
      iss = iss + 1
      GOTO 2000
   ENDIF
!
!     DUPLICATE SUBSTRUCTURE/ITEM ENCOUNTER.  USE MOST RECENT.
!
 1600 IF ( Z(jss+10)==0 ) THEN
      Line = Line + 3
      IF ( Line>Nlpp ) CALL page
!
!     CHECK YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
!
      IF ( Z(jss+6)<Buf(7) ) GOTO 1800
      IF ( Z(jss+6)==Buf(7) ) THEN
         IF ( Z(jss+4)<Buf(5) ) GOTO 1800
         IF ( Z(jss+4)==Buf(5) ) THEN
            IF ( Z(jss+5)<Buf(6) ) GOTO 1800
            IF ( Z(jss+5)==Buf(6) ) THEN
               IF ( Z(jss+7)<Buf(8) ) GOTO 1800
               IF ( Z(jss+7)==Buf(8) ) THEN
                  IF ( Z(jss+8)<Buf(9) ) GOTO 1800
                  IF ( Z(jss+8)==Buf(9) ) THEN
                     IF ( Z(jss+9)<Buf(10) ) GOTO 1800
!
!     MOST RECENT VERSION IS THE ONE ALREADY READ.  THEREFORE, SKIP THE
!     ONE ON TAPE.
!
                     IF ( Z(jss+9)/=Buf(10) ) WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Buf(i),i=5,10)
                  ELSE
                     WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Buf(i),i=5,10)
                  ENDIF
               ELSE
                  WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Buf(i),i=5,10)
               ENDIF
            ELSE
               WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Buf(i),i=5,10)
            ENDIF
         ELSE
            WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Buf(i),i=5,10)
         ENDIF
      ELSE
         WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Buf(i),i=5,10)
      ENDIF
   ENDIF
 1700 DO
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4600,*1700,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,Buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4600,unit)
      ENDIF
   ENDDO
!
!     MOST RECENT VERSION IS ON TAPE.  REPLACE OLDER VERSION ALREADY
!     READ.
!
 1800 WRITE (Nout,99025) Uwm , Buf(2) , Buf(3) , Buf(4) , Uname , (Z(jss+i),i=4,9)
   DO i = 1 , 9
      Z(jss+i) = Buf(i+1)
   ENDDO
   jcopy = jcopy - 1
   CALL delete(Buf(2),Buf(4),rc)
   GOTO 2000
 1900 DO
!
!     DATE AND TIME DONT MATCH. SKIP THIS SUBSTRUCTURE/ITEM.
!
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4600,*1900,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,Buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4600,unit)
      ENDIF
   ENDDO
 2000 DO
!
!     FETCH THE ITEM ON THE SOF.
!
      rc = 3
      kdh = ittype(Buf(4))
      IF ( kdh==1 ) THEN
!
!     COPY MATRIX DATA FROM THE GINO FILE TO THE SOF.
!
         ifile = scr2
         i = 10*iss + 1
         j = lcore - i + 1
         IF ( j<7 ) THEN
            n = 8
            GOTO 5000
         ELSE
            CALL read(*5200,*5300,unit,Z(i+1),6,1,nw)
            Z(i) = scr2
            CALL wrttrl(Z(i))
            inblk(1) = unit
            outblk(1) = scr2
            CALL open(*5100,scr2,Z(buf5),Wrtrew)
            GOTO 2500
         ENDIF
      ELSE
         CALL sfetch(Buf(2),Buf(4),swrt,rc)
         IF ( rc==3 ) GOTO 2200
         Line = Line + 2
         IF ( Line>Nlpp ) CALL page
         IF ( rc==2 .OR. rc==3 ) GOTO 2200
         IF ( rc==4 ) THEN
!
!     SUBSTRUCTURE DOES NOT EXIST.  ADD IT TO THE SOF HIERARCHY.
!
            CALL exlvl(nos,Z(idm),Buf(2),Z(10*iss+1),lcore-10*iss)
            CYCLE
         ELSEIF ( rc==5 ) THEN
!
!     INVALID ITEM NAME
!
            CALL smsg(3,Buf(4),Buf(2))
         ELSE
!
!     ITEM ALREADY EXISTS.
!
            WRITE (Nout,99026) Uwm , Buf(2) , Buf(3) , Buf(4)
            Z(10*iss) = 1
         ENDIF
         EXIT
      ENDIF
   ENDDO
 2100 DO
!
!     BECAUSE OF ERRORS, NO COPY.  SKIP DATA.
!
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4600,*2100,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,Buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4600,unit)
      ENDIF
   ENDDO
!
!     COPY THE DATA FROM THE GINO FILE TO THE SOF.
!
 2200 i = 10*iss + 1
   j = lcore - i + 1
   IF ( j<2 ) THEN
      n = 8
      GOTO 5000
   ENDIF
 2300 DO
      CALL read(*4600,*2400,unit,Z(i),j,0,flag)
      rc = 1
      CALL suwrt(Z(i),j,rc)
   ENDDO
 2400 IF ( Z(i)==eoi ) THEN
      rc = 3
      CALL suwrt(0,0,rc)
      GOTO 2700
   ELSE
      rc = 2
      CALL suwrt(Z(i),flag,rc)
      GOTO 2300
   ENDIF
 2500 DO
      CALL rectyp(unit,itype)
      IF ( itype/=0 ) THEN
         CALL cpystr(inblk,outblk,0,0)
      ELSE
         DO
            CALL read(*5100,*2600,unit,Z(i),j,0,nw)
            CALL write(scr2,Z(i),j,0)
         ENDDO
      ENDIF
   ENDDO
 2600 IF ( Z(i)==eoi ) THEN
      CALL close(scr2,1)
      DO
         CALL mtrxo(scr2,Buf(2),Buf(4),0,rc)
         IF ( rc==2 .OR. rc==3 ) EXIT
         IF ( rc==4 ) THEN
!
!     SUBSTRUCTURE DOES NOT EXIST - ADD IT TO THE SOF HIERARCHY
!
            CALL exlvl(nos,Z(idm),Buf(2),Z(10*iss+1),lcore-10*iss)
            CYCLE
         ELSEIF ( rc==5 ) THEN
!
!     ILLEGAL ITEM NAME
!
            Line = Line + 2
            IF ( Line>Nlpp ) CALL page
            CALL smsg(3,Buf(4),Buf(2))
         ELSEIF ( rc==6 ) THEN
            GOTO 5100
         ELSE
!
!     ITEM ALREADY EXISTS
!
            Line = Line + 2
            IF ( Line>Nlpp ) CALL page
            WRITE (Nout,99026) Uwm , Buf(2) , Buf(3) , Buf(4)
            Z(10*iss) = 1
         ENDIF
         CALL read(*1100,*1200,unit,Buf,10,1,flag)
         GOTO 1200
      ENDDO
   ELSE
      CALL write(scr2,Z(i),nw,1)
      GOTO 2500
   ENDIF
!
!     ITEM COPIED - PRINT MESSAGE
!
 2700 Line = Line + 1
   IF ( Line>Nlpp ) CALL page
   WRITE (Nout,99024) Uim , Buf(2) , Buf(3) , Buf(4) , unit , sof , (Buf(i),i=5,10)
!
!     INCREMENT NUMBER OF ITEMS COPIED.  IF NOT ALL ARE COPIED, LOOP
!     BACK TO FIND NEXT SUBSTRUCTURE/ITEM ON THE EXTERNAL FILE TO BE
!     COPIED.
!
   jcopy = jcopy + 1
   IF ( ncopy/=jcopy ) THEN
      CALL read(*1100,*1200,unit,Buf,10,1,flag)
      GOTO 1200
   ELSE
      CALL close(unit,Norew)
      GOTO 4200
   ENDIF
 2800 DO
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4600,*2800,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL close(unit,Norew)
            GOTO 4200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4600,unit)
      ENDIF
   ENDDO
 2900 Line = Line + 1
   IF ( Line>Nlpp ) CALL page
   WRITE (Nout,99023) Uim , (Buf(i),i=2,9)
 3000 DO
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4600,*3000,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*3100,*2900,unit,Buf,10,1,flag)
            Line = Line + 1
            IF ( Line>Nlpp ) CALL page
            WRITE (Nout,99016) (Buf(i),i=2,10)
99016       FORMAT (5X,'SUBSTRUCTURE ',2A4,5X,'ITEM ',A4,10X,5HDATE ,I2,1H/,I2,1H/,I2,10X,5HTIME ,I2,1H.,I2,1H.,I2)
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4600,unit)
      ENDIF
   ENDDO
 3100 CALL bckrec(unit)
   CALL close(unit,Norew)
   GOTO 4200
 3200 DO
      CALL sofio(srd,kdit,Z(buf4))
      DO i = 1 , Blksiz , 2
         Ssname(1) = Z(buf4+i+2)
         Ssname(2) = Z(buf4+i+3)
         IF ( Ssname(1)/=blank ) THEN
            DO
               CALL fdsub(Ssname,j)
               IF ( j==-1 ) THEN
                  CALL crsub(Ssname,j)
                  Z(iss) = k
                  Z(iss+1) = j
                  iss = iss + 2
                  k = k + 1
                  EXIT
               ELSE
!
!     DUPLICATE NAME ON RESIDENT SOF.  PREFIX IT WITH -Q- AND TRY AGAIN.
!
                  WRITE (Nout,99017) Uwm , Ssname
99017             FORMAT (A25,' 6351, DUPLICATE SUBSTRUCTURE NAME ',2A4,' FOUND DURING SOF APPEND OF FILE ',2A4,/32X,               &
                         &'THE SUBSTRUCTURE WITH THIS NAME ON THE FILE BEING ','APPENDED WILL BE PREFIXED WITH Q')
                  CALL prefix(q,Ssname)
                  IF ( Ssname(2)==qqqq ) THEN
                     WRITE (Nout,99018)
99018                FORMAT (1H0,31X,37HPREFIX FAILED.  SUBSTRUCTURE IGNORED.)
                     Z(iss) = (i+1)/2
                     Z(iss+1) = 0
                     iss = iss + 2
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
            IF ( iss/2>=nold ) GOTO 3300
         ENDIF
      ENDDO
!
!     GET THE NEXT BLOCK OF THE DIT FROM THE EXTERNAL SOF
!
      CALL fnxt(kdit,j)
      IF ( mod(kdit,2)==1 ) THEN
         i = andf(Cor(j),Jhalf)
      ELSE
         i = andf(rshift(Cor(j),Ihalf),Jhalf)
      ENDIF
      kdit = i + incblk
   ENDDO
!
!     THE DIT OF THE EXTERNAL SOF HAS NOW BEEN MERGED WITH THE DIT OF
!     THE RESIDENT SOF.  NOW MERGE THE MDI
!
 3300 iss = 0
   DO
      CALL sofio(srd,kmdi,Z(buf4))
      DO i = 1 , Blksiz , Dirsiz
         IF ( Blksiz-i+1>=Dirsiz ) THEN
            iss = iss + 1
            jmdi = buf4 + i + 1
            CALL bisloc(*4800,iss,Z,2,nold,k)
            CALL fmdi(Z(k+1),jrmdi)
!
!     PUT THE CONVERTED SUBSTRUCTURE INDICES IN THE FIRST TWO WORDS OF
!     THE MDI OF THE RESIDENT SOF.
!
            DO j = 1 , 6
               mask = lshift(1023,10*((j-1)/2))
!                   1023 = 2*10-1, LEFT SHIFT 0, 10, AND 20 BITS
!
               k = mod(j-1,2) + 1
               jss = andf(Z(jmdi+k),mask)
               IF ( jss/=0 ) THEN
                  CALL bisloc(*4800,jss,Z,2,nold,k)
                  jss = Z(k+1)
                  Cor(jrmdi+k) = andf(Cor(jrmdi+k),lshift(jss,10*((j-1)/2)))
               ENDIF
            ENDDO
!
!     INCREMENT THE BLOCK INDICES OF THE ITEMS IN THIS MDI DIRECTORY BY
!     THE NUMBER OF BLOCKS ON THE RESIDENT SOF.
!
            DO j = Ifrst , Dirsiz
               IF ( andf(Z(jmdi+j),Jhalf)/=0 ) Cor(jrmdi+j) = Z(jmdi+j) + incblk
            ENDDO
            IF ( iss==nold ) GOTO 3400
         ENDIF
      ENDDO
!
!     GET THE NEXT BLOCK OF THE MDI FROM THE EXTERNAL SOF.
!
      CALL fnxt(kmdi,j)
      IF ( mod(kmdi,2)==1 ) THEN
         i = andf(Cor(j),Jhalf)
      ELSE
         i = andf(rshift(Cor(j),Ihalf),Jhalf)
      ENDIF
      kmdi = i + incblk
   ENDDO
!
!     THE MDI OF THE EXTERNAL SOF HAS NOW BEEN MERGED WITH THE MDI OF
!     THE RESIDENT SOF.  NOW UPDATE THE NXT OF THE EXTERNAL SOF.
!
 3400 n = Blksiz
   knxt = incblk + 2
   incblk = orf(incblk,lshift(incblk,Ihalf))
   DO i = oldtsz , Nxttsz
      CALL sofio(srd,knxt,Z(buf4))
      IF ( i-oldtsz+1==Nxtfsz(Nfiles) ) n = (mod(Filsiz(Nfiles)-2,Supsiz)+1)/2 + 1
      DO j = 1 , n
         Z(buf4+j+2) = Z(buf4+j+2) + incblk
      ENDDO
      CALL sofio(swrt,knxt,Z(buf4))
      knxt = knxt + Supsiz
   ENDDO
!
!     RELEASE THE BLOCKS USED BY THE MDI AND DIT OF THE EXTERNAL SOF.
!     (THIS WILL CAUSE THE EXTERNAL SOF TO BE UNUSEABLE IN ITS ORIGINAL
!     FORM.)
!
   incblk = andf(incblk,Jhalf)
   CALL sofio(srd,incblk+1,Z(buf4))
   kdit = Z(buf4+33) + incblk
   kmdi = Z(buf4+34) + incblk
   CALL retblk(kdit)
   CALL retblk(kmdi)
!
!     WRITE ON ALL BLOCKS BETWEEN THE HIGHEST BLOCK WRITTEN ON THE
!     ORIGINAL RESIDENT SOF AND THE FIRST BLOCK OF THE APPENDED SOF.
!     THIS IS REQUIRED TO AVOID DATA TRANSMISSION ERRORS.
!
   n = Filsiz(Nfiles-1)
   DO i = nsave , n
      CALL sofio(swrt,nsave,Z(buf4))
   ENDDO
!
!     SOFCLS WILL UPDATE THE FIRST PHYSICAL BLOCK ON EACH SOF UNIT.
!
   CALL sofcls
!
!     APPEND OPERATION COMPLETED SUCCESSFULLY.  TELL USER THE NEWS.
!
   WRITE (Nout,99019) Uim , Uname
99019 FORMAT (A29,' 6352, EXTERNAL SOF FILE ',2A4,' SUCCESSFULLY APPENDED TO THE RESIDENT SOF')
   n = sofsiz(n)
   WRITE (Nout,99020) Uim , Avblks , n
99020 FORMAT (A29,' 6354, THERE ARE',I7,' FREE BLOCKS (',I9,' WORDS) ON THE RESIDENT SOF')
   GOTO 4300
!
!     READ HEADER RECORD AND FETCH THE SOF ITEM
!
 3500 CALL read(*4100,*4700,scr1,Buf,4,1,flag)
   kdh = ittype(Buf(3))
   IF ( kdh==1 ) THEN
!
!     COPY IN MATRIX ITEMS
!
      CALL open(*5100,scr2,Z(buf5),Wrtrew)
      CALL read(*4600,*4700,scr1,Z(iz2),6,1,nw)
      Z(1) = scr2
      CALL wrttrl(Z(1))
      inblk(1) = scr1
      outblk(1) = scr2
      GOTO 3800
   ELSE
      CALL sfetch(Buf,Buf(3),2,Buf(4))
   ENDIF
 3600 DO
!
!     COPY THE DATA
!
      CALL read(*4600,*3700,scr1,Z,lcore,0,flag)
      IF ( Z(1)==eoi ) THEN
!
!     EOI FOUND
!
         CALL suwrt(0,0,3)
         GOTO 4000
      ELSE
         CALL suwrt(Z,lcore,1)
      ENDIF
   ENDDO
 3700 IF ( Z(1)==eoi ) THEN
      CALL suwrt(0,0,3)
      GOTO 4000
   ELSE
      CALL suwrt(Z,flag,2)
      GOTO 3600
   ENDIF
 3800 DO
      CALL rectyp(scr1,itype)
      IF ( itype/=0 ) THEN
         CALL cpystr(inblk,outblk,0,0)
      ELSE
         DO
            CALL read(*4600,*3900,scr1,Z,lcore,0,nw)
            CALL write(scr2,Z,lcore,0)
         ENDDO
      ENDIF
   ENDDO
 3900 IF ( Z(1)==eoi ) THEN
!
!     EOI FOUND
!
      CALL close(scr2,1)
      CALL mtrxo(scr2,Buf,Buf(3),0,rc)
   ELSE
      CALL write(scr2,Z,nw,1)
      GOTO 3800
   ENDIF
 4000 Line = Line + 1
   IF ( Line>Nlpp ) CALL page
   WRITE (Nout,99021) Uim , Buf(1) , Buf(2) , Buf(3)
99021 FORMAT (A29,' 6353, SUBSTRUCTURE ',2A4,' ITEM ',A4,' HAS BEEN SUCCESSFULLY COMPRESSED')
   GOTO 3500
!
!     COMPRESS COMPLETE
!
 4100 CALL close(scr1,Rew)
!
!     **********************   C O D A   ************************
!
!     NORMAL TERMINATION
!
 4200 CALL sofcls
 4300 RETURN
 4400 WRITE (Nout,99022) Swm , Uname
99022 FORMAT (A27,' 6343, ',2A4,' IS NOT AN EXTERNAL SOF FILE')
   CALL close(unit,Norew)
   GOTO 4900
!
 4500 n = -1
   GOTO 5000
 4600 n = -2
   GOTO 5000
 4700 n = -3
   GOTO 5000
 4800 n = -61
   GOTO 5000
 4900 CALL sofcls
   Dry = -2
   WRITE (Nout,99028) Sim
   RETURN
!
 5000 CALL sofcls
   CALL mesage(n,unit,subr)
   Dry = -2
   WRITE (Nout,99028) Sim
   RETURN
!
 5100 n = -1
   GOTO 5400
 5200 n = -2
   GOTO 5400
 5300 n = -3
 5400 CALL sofcls
   CALL mesage(n,ifile,subr)
   RETURN
99023 FORMAT (A29,' 6336, EXIO FILE IDENTIFICATION.  PASSWORD= ',2A4,'  DATE=',I3,1H/,I2,1H/,I2,7H  TIME=,I3,1H.,I2,1H.,I2)
99024 FORMAT (A29,' 6341, SUBSTRUCTURE ',2A4,' ITEM ',A4,' SUCCESSFULLY COPIED FROM ',A4,' TO ',A4,2H (,I2,1H/,I2,1H/,I2,2H, ,I2,   &
            & 1H.,I2,1H.,I2,1H))
99025 FORMAT (A25,' 6345, SUBSTRUCTURE ',2A4,' ITEM ',A4,' IS DUPLICATED ON EXTERNAL FILE ',2A4,/32X,'OLDER VERSION (',I2,1H/,I2,   &
            & 1H/,I2,2H, ,I2,1H.,I2,1H.,I2,') IS IGNORED')
99026 FORMAT (A25,' 6346, SUBSTRUCTURE ',2A4,' ITEM ',A4,' NOT COPIED.  IT ALREADY EXISTS ON THE SOF')
99027 FORMAT (A25,' 6350, SOF APPEND OF FILE ',2A4,' FAILED')
99028 FORMAT (A31,' 6355, EXIO TERMINATED WITH ERRORS.  DRY RUN MODE ','ENTERED')
END SUBROUTINE exio1
