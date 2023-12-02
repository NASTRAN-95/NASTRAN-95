!*==exio1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE exio1
   IMPLICIT NONE
   USE c_blank
   USE c_itemdt
   USE c_machin
   USE c_names
   USE c_output
   USE c_sof
   USE c_sofcom
   USE c_sys
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , append , blank , check , comprs , disk , dump , eoi , eqf , hdr , id , iz2 , matric , norewi , phase3 ,  &
                   & q , qqqq , restor , rewi , rewi2 , scr1 , scr2 , sof , sofin , sofout , srd , swrt , tables , tape , xxxx
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , flag , hours , i , icount , idm , ifile , incblk , ipos , irec , iss , item ,      &
            & itype , j , jcopy , jmdi , jrmdi , jss , k , kdh , kdit , kmdi , knxt , lcore , lrec1 , mask , min , n , ncopy ,      &
            & ncore , nitems , nold , nos , nos4 , nsave , nw , nwds , oldtsz , rc , sec , unit
   INTEGER , DIMENSION(1) :: cor
   INTEGER , DIMENSION(15) :: inblk , outblk
   INTEGER , DIMENSION(2) , SAVE :: subr , whole
   INTEGER , DIMENSION(50) :: xitems
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     EXIO1 SERVICES INTERNAL FORMAT FUNCTIONS FOR EXIO.
!
   !>>>>EQUIVALENCE (Cor(1),Z(1))
   !>>>>EQUIVALENCE (Time(1),Hours) , (Time(2),Min) , (Time(3),Sec)
   DATA tape , disk , sofin , sofout , check/4HTAPE , 4HDISK , 4HSOFI , 4HSOFO , 4HCHEC/ , append , comprs , norewi , rewi ,        &
       &eqf/4HAPPE , 4HCOMP , 4HNORE , 4HREWI , 4HEOF / , all , matric , tables , phase3 , dump/4HALL  , 4HMATR , 4HTABL , 4HPHAS , &
       &4HDUMP/ , restor , whole , rewi2/4HREST , 4HWHOL , 4HESOF , 4HND  / , subr , blank , sof , eoi/4HEXIO , 4H1    , 4H     ,   &
       &4HSOF  , 4HEOI / , id , hdr , q , qqqq , xxxx/4H$ID$ , 4H$HD$ , 4HQ    , 4HQQQQ , 4HXXXX/
   DATA scr1 , scr2 , srd , swrt , iz2/301 , 302 , 1 , 2 , 2/
!
!     INITIALIZE
!
   IF ( nitem>50 ) CALL errmkn(25,10)
   lcore = korsz(z)
   buf1 = lcore - sysbuf + 1
   buf2 = buf1 - sysbuf - 1
   buf3 = buf2 - sysbuf
   buf4 = buf3 - sysbuf
   buf5 = buf4 - sysbuf
   lcore = buf5 - 1
   ncore = lcore
   nos = 0
   idm = 1
   IF ( lcore<=0 ) CALL mesage(-8,0,subr)
   IF ( mode(1)/=restor ) CALL sofopn(z(buf1),z(buf2),z(buf3))
   unit = uname(1)
!
!     CHECK TAPE BIT IF DEVICE=TAPE
!
   IF ( device(1)/=disk .AND. mode(1)/=comprs .AND. mode(1)/=append ) THEN
      IF ( device(1)/=tape ) THEN
         WRITE (nout,99001) uwm , device
99001    FORMAT (A25,' 6335, ',2A4,' IS AN INVALID DEVICE FOR MODULE EXIO')
         GOTO 4600
      ELSEIF ( .NOT.tapbit(unit) ) THEN
!
!     ERRORS CAUSING MODULE AND/OR JOB TERMINATION
!
         WRITE (nout,99002) uwm , uname
!
!     TEXT OF ERROR MESSAGES
!
99002    FORMAT (A25,' 6334, EXIO DEVICE PARAMETER SPECIFIES TAPE, BUT ','UNIT ',2A4,' IS NOT A PHYSICAL TAPE')
         GOTO 4600
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
   IF ( pos(1)==norewi .OR. pos(1)==eqf ) ipos = 2
   IF ( pos(1)==rewi ) ipos = 0
   IF ( ipos<0 ) THEN
      WRITE (nout,99003) uwm , pos
99003 FORMAT (A25,' 6339, ',2A4,' IS AN INVALID FILE POSITIONING ','PARAMETER FOR MODULE EXIO')
      GOTO 4600
   ELSE
      IF ( mode(1)==dump .OR. mode(1)==restor ) ipos = 0
      IF ( ipos==0 ) THEN
         head2(13) = rewi
         head2(14) = rewi2
      ENDIF
      IF ( mode(1)/=sofout ) GOTO 300
      IF ( ipos==0 ) GOTO 300
      CALL open(*4200,unit,z(buf4),rd)
      CALL bckrec(unit)
      IF ( pos(1)==norewi ) GOTO 200
      DO
         CALL fwdrec(*100,unit)
      ENDDO
   ENDIF
 100  CALL bckrec(unit)
 200  CALL close(unit,norew)
!
!     BRANCH ON MODE OF OPERATION
!
 300  IF ( mode(1)==sofout .OR. mode(1)==dump ) THEN
!
!
!     **********************   W R I T E   **********************
!
!     OPEN FILE AND WRITE 9 WORD ID RECORD
!
      CALL open(*4200,unit,z(buf4),wrtrew+ipos)
      CALL waltim(sec)
      hours = sec/3600
      sec = mod(sec,3600)
      min = sec/60
      sec = mod(sec,60)
      hdrec(1) = id
      hdrec(2) = passwd(1)
      hdrec(3) = passwd(2)
      DO i = 1 , 3
         hdrec(i+3) = date(i)
         hdrec(i+6) = time(i)
      ENDDO
      CALL write(unit,hdrec,9,1)
      CALL page
      WRITE (nout,99023) uim , passwd , date , time
      line = line + 1
!
!     WRITE DIT AND MDI CONTROL WORDS
!
      n = ditsiz/2
      CALL write(unit,n,1,0)
      DO i = 1 , n
         CALL fdit(i,j)
         CALL write(unit,cor(j),2,0)
         CALL fmdi(i,j)
         CALL write(unit,cor(j+1),2,0)
      ENDDO
      CALL write(unit,0,0,1)
      CALL write(unit,eoi,1,1)
      IF ( mode(1)/=dump ) THEN
!
!     STANDARD FORM --
!
!     COPY OUT EACH SUBSTRUCTURE/ITEM WITH ITS DATA IN THE CORRECT
!     SEQUENCE.
!
!     SETUP THE ARRAY XITEMS OF NAMES OF ITEMS TO BE COPIED.
!
         IF ( datype(1)==all ) THEN
            nitems = nitem
            DO i = 1 , nitem
               xitems(i) = items(1,i)
            ENDDO
         ELSEIF ( datype(1)==tables ) THEN
            nitems = 0
            DO i = 1 , nitem
               IF ( items(2,i)<=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( datype(1)==matric ) THEN
            nitems = 0
            DO i = 1 , nitem
               IF ( items(2,i)>0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( datype(1)/=phase3 ) THEN
            nitems = 2
            xitems(1) = datype(1)
            xitems(2) = datype(2)
            IF ( xitems(2)==blank ) nitems = 1
         ELSE
            nitems = 0
            DO i = 1 , nitem
               IF ( andf(items(7,i),8)/=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = items(1,i)
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
         DO i = 1 , noblks
            CALL sofio(srd,i,z(buf1))
            CALL write(unit,z(buf1+3),blksiz,0)
         ENDDO
         CALL write(unit,0,0,1)
         CALL close(unit,rew)
         WRITE (nout,99004) uim , noblks , nxttsz , uname
99004    FORMAT (A29,' 6337,',I6,' BLOCKS (',I4,' SUPERBLOCKS) OF THE SOF',' SUCCESSFULLY DUMPED TO EXTERNAL FILE ',2A4)
         GOTO 3900
      ENDIF
   ELSEIF ( mode(1)==sofin .OR. mode(1)==restor ) THEN
!
!     ***********************   R E A D  ************************
!
!     BRANCH FOR RESTORE OR STANDARD READ
!
      IF ( mode(1)/=restor ) THEN
!
!     STANDARD FORM -
!
!     COPY IN EACH INDIVIDUAL SUBSTRUCTURE/ITEM.
!
         iss = 0
!
!     SETUP ARRAY OF NAMES OF ITEMS TO BE COPIED.
!
         IF ( datype(1)==all ) THEN
            nitems = nitem
            DO i = 1 , nitem
               xitems(i) = items(1,i)
            ENDDO
         ELSEIF ( datype(1)==tables ) THEN
            nitems = 0
            DO i = 1 , nitem
               IF ( items(2,i)<=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( datype(1)==matric ) THEN
            nitems = 0
            DO i = 1 , nitem
               IF ( items(2,i)>0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = items(1,i)
               ENDIF
            ENDDO
         ELSEIF ( datype(1)/=phase3 ) THEN
            nitems = 2
            xitems(1) = datype(1)
            xitems(2) = datype(2)
            IF ( xitems(2)==blank ) nitems = 1
         ELSE
            nitems = 0
            DO i = 1 , nitem
               IF ( andf(items(7,i),8)/=0 ) THEN
                  nitems = nitems + 1
                  xitems(nitems) = items(1,i)
               ENDIF
            ENDDO
         ENDIF
!
!     DETERMINE NUMBER OF SUBSTRUCTURE/ITEMS TO BE COPIED AND INITIALIZE
!     COUNTER.
!
         jcopy = 0
         ncopy = 0
         IF ( names(1)/=whole(1) .OR. names(2)/=whole(2) ) THEN
            DO i = 1 , 5
               IF ( names(2*i-1)/=xxxx ) ncopy = ncopy + 1
            ENDDO
            ncopy = ncopy*nitems
            IF ( pdate/=0 ) ncopy = 1
         ENDIF
!
!     OPEN THE EXTERNAL FILE AND READ THE IDENTIFICATION OR HEADER
!     RECORD.
!     REMEMBER IT IN CASE THE USER HAS REQUESTED A SUBSTRUCTURE/ITEM
!     WHICH IS NOT PRESENT ON THE FILE.
!
         CALL page
         CALL open(*4200,unit,z(buf4),rdrew+ipos)
         GOTO 800
!
!     RESTORE FORM --
!
!     COPY EACH LOGICAL RECORD ON THE EXTERNAL FILE INTO CONSEQUTIVE,
!     CONTIGUOUS BLOCKS ON THE RESIDENT SOF.
!
!     MAKE SURE THE RESIDENT SOF IS EMPTY.
!
      ELSEIF ( status/=0 ) THEN
         WRITE (nout,99005) uwm
99005    FORMAT (A25,' 6342, SOF RESTORE OPERATION FAILED.  THE RESIDENT ','SOF IS NOT EMPTY')
         GOTO 4600
      ELSE
         CALL sofopn(z(buf1),z(buf2),z(buf3))
         CALL sofcls
!
!     OPEN FILE AND READ THE ID RECORD
!
         CALL open(*4200,unit,z(buf4),rdrew)
         CALL read(*4100,*4100,unit,hdrec,9,1,flag)
         IF ( hdrec(1)/=id ) GOTO 4100
         CALL page
         line = line + 1
         WRITE (nout,99023) uim , (hdrec(i),i=2,9)
         CALL fwdrec(*4300,unit)
         CALL fwdrec(*4300,unit)
!
!     BEGIN DATA TRANSFER
!
         i = 1
         DO
            CALL read(*4300,*700,unit,z(buf1+3),blksiz,0,flag)
            CALL sofio(swrt,i,z(buf1))
            i = i + 1
         ENDDO
      ENDIF
   ELSEIF ( mode(1)==check ) THEN
!
!     *********************   C H E C K   ***************************
!
!     REWIND THE EXTERNAL FILE AND PRINT A LIST OF ALL SUBSTRUCTURE/
!     ITEMS ON IT WITH THE DATE AND TIME WHEN THEY WERE WRITTEN THERE.
!
      CALL open(*4200,unit,z(buf4),rdrew)
      CALL page
      WRITE (nout,99006) uim , uname
99006 FORMAT (A29,' 6349, CONTENTS OF EXTERNAL SOF FILE ',2A4,' FOLLOW')
      line = line + 1
      CALL read(*4300,*4400,unit,buf,9,1,flag)
      GOTO 2900
   ELSEIF ( mode(1)==append ) THEN
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
      IF ( nfiles<10 ) THEN
         nfiles = nfiles + 1
         filnam(nfiles) = unit
         filsiz(nfiles) = 4
         nsave = noblks + 1
!
!     READ THE FIRST PHYSICAL BLOCK OF THE EXTERNAL SOF AND SEE THAT IT
!     IS COMPATIBLE WITH THE RESIDENT SOF.
         incblk = -4
         DO i = 1 , nfiles
            incblk = incblk + filsiz(i)
         ENDDO
         CALL sofio(srd,incblk+1,z(buf4))
!
!     PASSWORD CHECK
!
         IF ( z(buf4+3)/=datype(1) .OR. z(buf4+4)/=datype(2) ) THEN
            WRITE (nout,99027) uwm , uname
            WRITE (nout,99007)
99007       FORMAT (32X,17HINVALID PASSWORD.)
            incblk = -1
         ENDIF
!
!     FILE SEQUENCE NUMBER CHECK
!
         IF ( z(buf4+5)/=1 ) THEN
            WRITE (nout,99027) uwm , uname
            WRITE (nout,99008)
99008       FORMAT (32X,'THE SEQUENCE NUMBER OF THE EXTERNAL SOF FILE IS NOT',' 1')
            incblk = -1
         ENDIF
!
!     NUMBER OF EXTERNAL FILES CHECK
!
         IF ( z(buf4+6)/=1 ) THEN
            WRITE (nout,99027) uwm , uname
            WRITE (nout,99009)
99009       FORMAT (32X,'THE EXTERNAL SOF FILE MUST CONSIST OF ONLY ONLY ONE',' PHYSICAL UNIT')
            incblk = -1
         ENDIF
!
!     BLOCKSIZE CHECK
!
         IF ( z(buf4+27)/=blksiz ) THEN
            WRITE (nout,99027) uwm , uname
            WRITE (nout,99010) blksiz , z(buf4+27)
99010       FORMAT (32X,45HTHE EXTERNAL SOF HAS INCOMPATIBLE BLOCK SIZE.,/32X,32HBLOCK SIZE OF THE RESIDENT SOF =,I5,/32X,          &
                   &32HBLOCK SIZE OF THE EXTERNAL SOF =,I5)
            incblk = -1
         ENDIF
         IF ( incblk<0 ) THEN
!
!     APPEND OPERATION ABORTED.  RESTORE THE COMMON BLOCKS FOR THE
!     RESIDENT SOF.
!
            first = .TRUE.
            opnsof = .FALSE.
            CALL sofopn(z(buf1),z(buf2),z(buf3))
            GOTO 4500
         ELSE
!
!     COMPLETE THE UPDATING OF THE COMMON BLOCKS
!
            filsiz(nfiles) = z(buf4+17)
            avblks = avblks + z(buf4+30)
            nxtcur = 1
            nxtrst = .TRUE.
            nxtfsz(nfiles) = z(buf4+36)
            j = nfiles - 1
            nxttsz = 0
            DO i = 1 , j
               nxttsz = nxttsz + nxtfsz(i)
            ENDDO
            oldtsz = nxttsz + 1
            nxttsz = nxttsz + z(buf4+35)
!
!     READ THE DIT OF THE EXTERNAL SOF AND ADD EACH SUBSTRUCTURE THERE
!     TO THE DIT OF THE RESIDENT SOF.  KEEP A TABLE IN OPEN CORE OF TWO
!     WORDS PER SUBSTRUCTURE -
!
!     (1)  SUBSTRUCTURE NUMBER FROM THE EXTERNAL SOF.
!     (2)  NEW SUBSTRUCTURE NUMBER ON THE RESIDENT SOF.
!
            nold = z(buf4+32)
            IF ( 2*nold>lcore ) THEN
               n = 8
               GOTO 4700
            ELSE
               iss = 1
               k = 1
               kdit = z(buf4+33) + incblk
               kmdi = z(buf4+34) + incblk
               DO
                  CALL sofio(srd,kdit,z(buf4))
                  DO i = 1 , blksiz , 2
                     ssname(1) = z(buf4+i+2)
                     ssname(2) = z(buf4+i+3)
                     IF ( ssname(1)/=blank ) THEN
                        DO
                           CALL fdsub(ssname,j)
                           IF ( j==-1 ) THEN
                              CALL crsub(ssname,j)
                              z(iss) = k
                              z(iss+1) = j
                              iss = iss + 2
                              k = k + 1
                              EXIT
                           ELSE
!
!     DUPLICATE NAME ON RESIDENT SOF.  PREFIX IT WITH -Q- AND TRY AGAIN.
!
                              WRITE (nout,99011) uwm , ssname
99011                         FORMAT (A25,' 6351, DUPLICATE SUBSTRUCTURE NAME ',2A4,' FOUND DURING SOF APPEND OF FILE ',2A4,/32X,   &
                                     &'THE SUBSTRUCTURE WITH THIS NAME ON THE FILE BEING ','APPENDED WILL BE PREFIXED WITH Q')
                              CALL prefix(q,ssname)
                              IF ( ssname(2)==qqqq ) THEN
                                 WRITE (nout,99012)
99012                            FORMAT (1H0,31X,37HPREFIX FAILED.  SUBSTRUCTURE IGNORED.)
                                 z(iss) = (i+1)/2
                                 z(iss+1) = 0
                                 iss = iss + 2
                                 EXIT
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( iss/2>=nold ) GOTO 305
                     ENDIF
                  ENDDO
!
!     GET THE NEXT BLOCK OF THE DIT FROM THE EXTERNAL SOF
!
                  CALL fnxt(kdit,j)
                  IF ( mod(kdit,2)==1 ) THEN
                     i = andf(cor(j),jhalf)
                  ELSE
                     i = andf(rshift(cor(j),ihalf),jhalf)
                  ENDIF
                  kdit = i + incblk
               ENDDO
!
!     THE DIT OF THE EXTERNAL SOF HAS NOW BEEN MERGED WITH THE DIT OF
!     THE RESIDENT SOF.  NOW MERGE THE MDI
!
 305           iss = 0
               DO
                  CALL sofio(srd,kmdi,z(buf4))
                  DO i = 1 , blksiz , dirsiz
                     IF ( blksiz-i+1>=dirsiz ) THEN
                        iss = iss + 1
                        jmdi = buf4 + i + 1
                        CALL bisloc(*4500,iss,z,2,nold,k)
                        CALL fmdi(z(k+1),jrmdi)
!
!     PUT THE CONVERTED SUBSTRUCTURE INDICES IN THE FIRST TWO WORDS OF
!     THE MDI OF THE RESIDENT SOF.
!
                        DO j = 1 , 6
                           mask = lshift(1023,10*((j-1)/2))
!                   1023 = 2*10-1, LEFT SHIFT 0, 10, AND 20 BITS
!
                           k = mod(j-1,2) + 1
                           jss = andf(z(jmdi+k),mask)
                           IF ( jss/=0 ) THEN
                              CALL bisloc(*4500,jss,z,2,nold,k)
                              jss = z(k+1)
                              cor(jrmdi+k) = andf(cor(jrmdi+k),lshift(jss,10*((j-1)/2)))
                           ENDIF
                        ENDDO
!
!     INCREMENT THE BLOCK INDICES OF THE ITEMS IN THIS MDI DIRECTORY BY
!     THE NUMBER OF BLOCKS ON THE RESIDENT SOF.
!
                        DO j = ifrst , dirsiz
                           IF ( andf(z(jmdi+j),jhalf)/=0 ) cor(jrmdi+j) = z(jmdi+j) + incblk
                        ENDDO
                        IF ( iss==nold ) GOTO 310
                     ENDIF
                  ENDDO
!
!     GET THE NEXT BLOCK OF THE MDI FROM THE EXTERNAL SOF.
!
                  CALL fnxt(kmdi,j)
                  IF ( mod(kmdi,2)==1 ) THEN
                     i = andf(cor(j),jhalf)
                  ELSE
                     i = andf(rshift(cor(j),ihalf),jhalf)
                  ENDIF
                  kmdi = i + incblk
               ENDDO
!
!     THE MDI OF THE EXTERNAL SOF HAS NOW BEEN MERGED WITH THE MDI OF
!     THE RESIDENT SOF.  NOW UPDATE THE NXT OF THE EXTERNAL SOF.
!
 310           n = blksiz
               knxt = incblk + 2
               incblk = orf(incblk,lshift(incblk,ihalf))
               DO i = oldtsz , nxttsz
                  CALL sofio(srd,knxt,z(buf4))
                  IF ( i-oldtsz+1==nxtfsz(nfiles) ) n = (mod(filsiz(nfiles)-2,supsiz)+1)/2 + 1
                  DO j = 1 , n
                     z(buf4+j+2) = z(buf4+j+2) + incblk
                  ENDDO
                  CALL sofio(swrt,knxt,z(buf4))
                  knxt = knxt + supsiz
               ENDDO
!
!     RELEASE THE BLOCKS USED BY THE MDI AND DIT OF THE EXTERNAL SOF.
!     (THIS WILL CAUSE THE EXTERNAL SOF TO BE UNUSEABLE IN ITS ORIGINAL
!     FORM.)
!
               incblk = andf(incblk,jhalf)
               CALL sofio(srd,incblk+1,z(buf4))
               kdit = z(buf4+33) + incblk
               kmdi = z(buf4+34) + incblk
               CALL retblk(kdit)
               CALL retblk(kmdi)
!
!     WRITE ON ALL BLOCKS BETWEEN THE HIGHEST BLOCK WRITTEN ON THE
!     ORIGINAL RESIDENT SOF AND THE FIRST BLOCK OF THE APPENDED SOF.
!     THIS IS REQUIRED TO AVOID DATA TRANSMISSION ERRORS.
!
               n = filsiz(nfiles-1)
               DO i = nsave , n
                  CALL sofio(swrt,nsave,z(buf4))
               ENDDO
!
!     SOFCLS WILL UPDATE THE FIRST PHYSICAL BLOCK ON EACH SOF UNIT.
!
               CALL sofcls
!
!     APPEND OPERATION COMPLETED SUCCESSFULLY.  TELL USER THE NEWS.
!
               WRITE (nout,99013) uim , uname
99013          FORMAT (A29,' 6352, EXTERNAL SOF FILE ',2A4,' SUCCESSFULLY APPENDED TO THE RESIDENT SOF')
               n = sofsiz(n)
               WRITE (nout,99014) uim , avblks , n
99014          FORMAT (A29,' 6354, THERE ARE',I7,' FREE BLOCKS (',I9,' WORDS) ON THE RESIDENT SOF')
               GOTO 4000
            ENDIF
         ENDIF
      ELSE
         WRITE (nout,99027) uwm , uname
         WRITE (nout,99015)
99015    FORMAT (32X,'TOO MANY PHYSICAL SOF UNITS. MAXIMUM ALLOWED IS 10')
         GOTO 4600
      ENDIF
   ELSEIF ( mode(1)==comprs ) THEN
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
      CALL open(*4200,scr1,z(buf4),wrtrew)
!
!     COPY OUT DIT AND MDI INFORMATION
!
      iss = 0
      DO k = 1 , ditsiz , 2
         iss = iss + 1
         CALL fdit(iss,j)
         CALL write(scr1,cor(j),2,0)
         CALL fmdi(iss,j)
         CALL write(scr1,cor(j+1),2,0)
      ENDDO
      CALL write(scr1,0,0,1)
!
!     COPY OUT SUBSTRUCTURE ITEMS
!
      iss = 0
      DO k = 1 , ditsiz , 2
         iss = iss + 1
         CALL fdit(iss,j)
         ssname(1) = cor(j)
         ssname(2) = cor(j+1)
         IF ( ssname(1)/=blank ) THEN
            DO item = 1 , nitem
               kdh = items(2,item)
               IF ( kdh==1 ) THEN
!
!     PROCESS MATRIX ITEMS
!
                  CALL mtrxi(scr2,ssname,items(1,item),0,rc)
                  ifile = scr2
                  IF ( rc/=2 ) THEN
                     IF ( rc==3 ) THEN
                     ELSEIF ( rc==4 .OR. rc==5 ) THEN
                        CALL smsg(rc-2,items(1,item),ssname)
                     ELSEIF ( rc==6 ) THEN
                        GOTO 4800
                     ELSE
                        CALL write(scr1,ssname,2,0)
                        CALL write(scr1,items(1,item),1,0)
                        CALL write(scr1,3,1,1)
                        CALL open(*4800,scr2,z(buf5),rdrew)
                        z(1) = scr2
                        CALL rdtrl(z(1))
                        CALL write(scr1,z(iz2),6,1)
                        CALL cpyfil(scr2,scr1,z,lcore,icount)
                        CALL write(scr1,eoi,1,1)
                        CALL close(scr2,1)
                     ENDIF
                     CYCLE
                  ENDIF
               ELSE
                  CALL sfetch(ssname,items(1,item),srd,rc)
                  IF ( rc==1 ) THEN
!
!     ITEM EXISTS.  COPY IT OUT.
!
                     CALL write(scr1,ssname,2,0)
                     CALL write(scr1,items(1,item),1,0)
                     CALL write(scr1,3,1,1)
                     DO
                        CALL suread(z,lcore,n,rc)
                        IF ( rc>1 ) THEN
                           CALL write(scr1,z,n,1)
                           IF ( rc/=2 ) THEN
!
!     END OF ITEM HIT.  WRITE EOI RECORD
!
                              CALL write(scr1,eoi,1,1)
                              GOTO 320
                           ENDIF
                        ELSE
                           CALL write(scr1,z,lcore,0)
                        ENDIF
                     ENDDO
                  ELSEIF ( rc==2 ) THEN
                  ELSEIF ( rc==3 ) THEN
                     CYCLE
                  ELSE
                     CALL smsg(rc-2,items(1,item),ssname)
                     CYCLE
                  ENDIF
               ENDIF
!
!     ITEM PSEUDO-EXISTS.  WRITE PSEUDO-HEADER RECORD AND EOI RECORD.
!
               CALL write(scr1,ssname,2,0)
               CALL write(scr1,items(1,item),1,0)
               CALL write(scr1,2,1,1)
               CALL write(scr1,eoi,1,1)
 320        ENDDO
         ENDIF
      ENDDO
!
!     COPY ALL ITEMS BACK TO THE SOF
!
      CALL close(scr1,rew)
      CALL open(*4200,scr1,z(buf4),rdrew)
!
!     RE-INITIALIZE THE SOF, THEN RESTORE THE OLD DIT AND MDI
!
      CALL sofcls
      status = 0
      first = .TRUE.
      CALL sofopn(z(buf1),z(buf2),z(buf3))
      CALL page
      iss = 0
      DO
         CALL read(*4300,*3200,scr1,buf,4,0,flag)
         iss = iss + 1
         IF ( buf(1)/=blank ) THEN
            CALL crsub(buf,i)
            CALL fmdi(i,j)
            cor(j+1) = buf(3)
            cor(j+2) = buf(4)
            mdiup = .TRUE.
         ENDIF
      ENDDO
   ELSE
      WRITE (nout,99016) uwm , mode
99016 FORMAT (A25,' 6338, ',2A4,' IS AN INVALID MODE PARAMETER FOR ','MODULE EXIO')
      GOTO 4600
   ENDIF
 400  DO
      iss = iss + 1
      IF ( names(1)/=whole(1) .OR. names(2)/=whole(2) ) THEN
!
!     WRITE ONLY THOSE SUBSTRUCTURES IN THE PARAMETER LIST
!
         IF ( iss>5 ) GOTO 600
         IF ( names(2*iss-1)/=xxxx ) THEN
            ssname(1) = names(2*iss-1)
            ssname(2) = names(2*iss)
            EXIT
         ENDIF
      ELSE
!
!     WRITE ALL SUBSTRUCTURES IN THE RESIDENT SOF.
!
         IF ( iss>ditsiz/2 ) GOTO 600
         CALL fdit(iss,i)
         IF ( cor(i)/=blank ) THEN
            ssname(1) = cor(i)
            ssname(2) = cor(i+1)
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
      CALL sfetch(ssname,xitems(item),srd,rc)
      IF ( rc==1 ) GOTO 550
      IF ( rc==3 ) CYCLE
      IF ( rc==4 .OR. rc==5 ) GOTO 500
 450  line = line + 2
      IF ( line>nlpp ) CALL page
      WRITE (nout,99017) uwm , ssname , xitems(item)
99017 FORMAT (A25,' 6340, SUBSTRUCTURE ',2A4,' ITEM ',A4,' PSEUDOEXISTS ONLY AND CANNOT BE COPIED OUT BY EXIO')
      CYCLE
 500  line = line + 2
      IF ( line>nlpp ) CALL page
      CALL smsg(rc-2,xitems(item),ssname)
      CYCLE
!
!     WRITE SUBSTRUCTURE/ITEM HEADER RECORD
!
 550  CALL waltim(sec)
      hours = sec/3600
      sec = mod(sec,3600)
      min = sec/60
      sec = mod(sec,60)
      hdrec(1) = hdr
      hdrec(2) = ssname(1)
      hdrec(3) = ssname(2)
      hdrec(4) = xitems(item)
      DO i = 1 , 3
         hdrec(i+4) = date(i)
         hdrec(i+7) = time(i)
      ENDDO
      IF ( kdh==1 ) THEN
!
!     COPY MATRIX DATA ITEMS
!
         ifile = scr1
         CALL mtrxi(scr1,ssname,xitems(item),0,rc)
         IF ( rc==2 ) GOTO 450
         IF ( rc==3 ) CYCLE
         IF ( rc==4 .OR. rc==5 ) GOTO 500
         IF ( rc==6 ) GOTO 4800
         CALL write(unit,hdrec,10,1)
         z(1) = scr1
         CALL rdtrl(z(1))
         CALL write(unit,z(iz2),6,1)
         CALL open(*4800,scr1,z(buf5),rdrew)
         CALL cpyfil(scr1,unit,z,lcore,icount)
         CALL close(scr1,1)
      ELSE
         CALL write(unit,hdrec,10,1)
         DO
!
!     COPY DATA
!
            CALL suread(z(1),lcore,nwds,rc)
            IF ( rc==2 ) THEN
               CALL write(unit,z,nwds,1)
            ELSEIF ( rc==3 ) THEN
               EXIT
            ELSE
               CALL write(unit,z,lcore,0)
            ENDIF
         ENDDO
      ENDIF
!
!     WRITE END-OF-ITEM RECORD AND USER MESSAGE
!
      CALL write(unit,eoi,1,1)
      line = line + 1
      IF ( line>nlpp ) CALL page
      WRITE (nout,99024) uim , ssname , xitems(item) , sof , unit , date , time
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
   CALL close(unit,eofnrw)
   GOTO 3900
!
!     RESTORE COMPLETE.  CLOSE FILE AND GIVE USER THE NEWS.
!
 700  CALL close(unit,rew)
   i = i - 1
   WRITE (nout,99018) uim , i
99018 FORMAT (A29,' 6344, SOF RESTORE OF ',I6,' BLOCKS SUCCESSFULLY ','COMPLETED')
   GOTO 4000
 800  CALL read(*900,*1000,unit,hdrec,10,1,lrec1)
   lrec1 = 10
   GOTO 1000
 900  CALL rewind(unit)
   GOTO 800
 1000 DO i = 1 , lrec1
      buf(i) = hdrec(i)
   ENDDO
   IF ( hdrec(1)==id ) GOTO 1300
   IF ( hdrec(1)==hdr ) GOTO 1300
   GOTO 4100
 1100 IF ( names(1)==whole(1) .AND. names(2)==whole(2) ) THEN
!
!     READ OPERATION COMPLETE
!
      CALL close(unit,norew)
      GOTO 3900
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
      CALL read(*1100,*1200,unit,buf,10,1,flag)
   ENDIF
!
!     CHECK IT AGAINST THE FIRST RECORD READ.  IF IT MATCHES, THE ENTIRE
!     TAPE HAS BEEN SCANNED, BUT NOT ALL ITEMS WERE FOUND.
!
 1200 DO i = 1 , lrec1
      IF ( buf(i)/=hdrec(i) ) GOTO 1300
   ENDDO
!
!     THE ENTIRE EXTERNAL FILE HAS NOW BEEN SCANNED, BUT NOT ALL ITEMS
!     WERE FOUND.  WARN USER OF EACH ITEM NOT FOUND.
!
!     SKIP REMAINDER OF CURRENT ITEM SO FILE IS PROPERLY POSITIONED
!     FOR NEXT EXECUTION OF MODULE.
!
   DO i = 1 , 9 , 2
      IF ( names(i)/=xxxx ) THEN
         DO item = 1 , nitems
            IF ( iss/=0 ) THEN
               DO j = 1 , iss
                  jss = 10*(j-1)
                  IF ( names(i)==z(jss+1) .AND. names(i+1)==z(jss+2) .AND. xitems(item)==z(jss+3) ) GOTO 1220
               ENDDO
            ENDIF
            line = line + 2
            IF ( line>nlpp ) CALL page
            WRITE (nout,99019) uwm , names(i) , names(i+1) , xitems(item) , uname
99019       FORMAT (A25,' 6348, SUBSTRUCTURE ',2A4,' ITEM ',A4,' NOT FOUND ON EXTERNAL FILE ',2A4)
 1220    ENDDO
      ENDIF
   ENDDO
   GOTO 2800
!
!     IF THAT WAS AN ID RECORD, ISSUE MESSAGE AND GO BACK TO READ THE
!     IMMEDIATELY FOLLOWING HEADER RECORD.
!
 1300 IF ( buf(1)==id ) THEN
!
!     READ OLD DIT AND MDI DATA
!
      CALL read(*4300,*4400,unit,nos,1,0,flag)
      lcore = ncore - 4*nos
      idm = lcore + 1
      IF ( lcore<=0 ) THEN
         n = 8
         GOTO 4700
      ELSE
         nos4 = nos*4
         CALL read(*4300,*4400,unit,z(idm),nos4,1,flag)
         CALL fwdrec(*4300,unit)
         line = line + 1
         IF ( line>nlpp ) CALL page
         WRITE (nout,99023) uim , (buf(i),i=2,9)
         CALL read(*1100,*1200,unit,buf,10,1,flag)
         GOTO 1200
      ENDIF
!
!     READ OR SKIP THE SUBSTRUCTURE/ITEM DATA.
!
   ELSEIF ( pdate/=0 ) THEN
!
!     IF DATE AND TIME PARAMETERS WERE INVOKED, CHECK THEM.
!
      IF ( mod(pdate,100)/=buf(7) .OR. pdate/10000/=buf(5) .OR. mod(pdate,10000)/100/=buf(6) .OR. ptime/10000/=buf(8) .OR.          &
         & mod(ptime,10000)/100/=buf(9) .OR. mod(ptime,100)/=buf(10) ) GOTO 1900
      GOTO 2000
   ELSE
      IF ( names(1)==whole(1) .AND. names(2)==whole(2) ) GOTO 1500
      DO i = 1 , 5
         IF ( names(2*i-1)/=xxxx ) THEN
            IF ( buf(2)==names(2*i-1) .AND. buf(3)==names(2*i) ) GOTO 1350
         ENDIF
      ENDDO
      GOTO 1400
 1350 DO i = 1 , nitems
         IF ( buf(4)==xitems(i) ) GOTO 1500
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
         CALL read(*4300,*1400,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4300,unit)
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
            IF ( buf(j+1)/=z(jss+j) ) GOTO 1550
         ENDDO
         GOTO 1600
 1550 ENDDO
   ENDIF
!
!     NO DUPLICATE.  ADD THIS HEADER TO THOSE IN CORE.
!
   IF ( 10*(iss+1)>lcore ) THEN
      n = 8
      GOTO 4700
   ELSE
      DO i = 1 , 9
         z(10*iss+i) = buf(i+1)
      ENDDO
      z(10*iss+10) = 0
      iss = iss + 1
      GOTO 2000
   ENDIF
!
!     DUPLICATE SUBSTRUCTURE/ITEM ENCOUNTER.  USE MOST RECENT.
!
 1600 IF ( z(jss+10)==0 ) THEN
      line = line + 3
      IF ( line>nlpp ) CALL page
!
!     CHECK YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
!
      IF ( z(jss+6)<buf(7) ) GOTO 1800
      IF ( z(jss+6)==buf(7) ) THEN
         IF ( z(jss+4)<buf(5) ) GOTO 1800
         IF ( z(jss+4)==buf(5) ) THEN
            IF ( z(jss+5)<buf(6) ) GOTO 1800
            IF ( z(jss+5)==buf(6) ) THEN
               IF ( z(jss+7)<buf(8) ) GOTO 1800
               IF ( z(jss+7)==buf(8) ) THEN
                  IF ( z(jss+8)<buf(9) ) GOTO 1800
                  IF ( z(jss+8)==buf(9) ) THEN
                     IF ( z(jss+9)<buf(10) ) GOTO 1800
!
!     MOST RECENT VERSION IS THE ONE ALREADY READ.  THEREFORE, SKIP THE
!     ONE ON TAPE.
!
                     IF ( z(jss+9)/=buf(10) ) WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (buf(i),i=5,10)
                  ELSE
                     WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (buf(i),i=5,10)
                  ENDIF
               ELSE
                  WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (buf(i),i=5,10)
               ENDIF
            ELSE
               WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (buf(i),i=5,10)
            ENDIF
         ELSE
            WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (buf(i),i=5,10)
         ENDIF
      ELSE
         WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (buf(i),i=5,10)
      ENDIF
   ENDIF
 1700 DO
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4300,*1700,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4300,unit)
      ENDIF
   ENDDO
!
!     MOST RECENT VERSION IS ON TAPE.  REPLACE OLDER VERSION ALREADY
!     READ.
!
 1800 WRITE (nout,99025) uwm , buf(2) , buf(3) , buf(4) , uname , (z(jss+i),i=4,9)
   DO i = 1 , 9
      z(jss+i) = buf(i+1)
   ENDDO
   jcopy = jcopy - 1
   CALL delete(buf(2),buf(4),rc)
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
         CALL read(*4300,*1900,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4300,unit)
      ENDIF
   ENDDO
 2000 DO
!
!     FETCH THE ITEM ON THE SOF.
!
      rc = 3
      kdh = ittype(buf(4))
      IF ( kdh==1 ) THEN
!
!     COPY MATRIX DATA FROM THE GINO FILE TO THE SOF.
!
         ifile = scr2
         i = 10*iss + 1
         j = lcore - i + 1
         IF ( j<7 ) THEN
            n = 8
            GOTO 4700
         ELSE
            CALL read(*4900,*5000,unit,z(i+1),6,1,nw)
            z(i) = scr2
            CALL wrttrl(z(i))
            inblk(1) = unit
            outblk(1) = scr2
            CALL open(*4800,scr2,z(buf5),wrtrew)
            GOTO 2500
         ENDIF
      ELSE
         CALL sfetch(buf(2),buf(4),swrt,rc)
         IF ( rc==3 ) GOTO 2200
         line = line + 2
         IF ( line>nlpp ) CALL page
         IF ( rc==2 .OR. rc==3 ) GOTO 2200
         IF ( rc==4 ) THEN
!
!     SUBSTRUCTURE DOES NOT EXIST.  ADD IT TO THE SOF HIERARCHY.
!
            CALL exlvl(nos,z(idm),buf(2),z(10*iss+1),lcore-10*iss)
            CYCLE
         ELSEIF ( rc==5 ) THEN
!
!     INVALID ITEM NAME
!
            CALL smsg(3,buf(4),buf(2))
         ELSE
!
!     ITEM ALREADY EXISTS.
!
            WRITE (nout,99026) uwm , buf(2) , buf(3) , buf(4)
            z(10*iss) = 1
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
         CALL read(*4300,*2100,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*1100,*1200,unit,buf,10,1,flag)
            GOTO 1200
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4300,unit)
      ENDIF
   ENDDO
!
!     COPY THE DATA FROM THE GINO FILE TO THE SOF.
!
 2200 i = 10*iss + 1
   j = lcore - i + 1
   IF ( j<2 ) THEN
      n = 8
      GOTO 4700
   ENDIF
 2300 DO
      CALL read(*4300,*2400,unit,z(i),j,0,flag)
      rc = 1
      CALL suwrt(z(i),j,rc)
   ENDDO
 2400 IF ( z(i)==eoi ) THEN
      rc = 3
      CALL suwrt(0,0,rc)
      GOTO 2700
   ELSE
      rc = 2
      CALL suwrt(z(i),flag,rc)
      GOTO 2300
   ENDIF
 2500 DO
      CALL rectyp(unit,itype)
      IF ( itype/=0 ) THEN
         CALL cpystr(inblk,outblk,0,0)
      ELSE
         DO
            CALL read(*4800,*2600,unit,z(i),j,0,nw)
            CALL write(scr2,z(i),j,0)
         ENDDO
      ENDIF
   ENDDO
 2600 IF ( z(i)==eoi ) THEN
      CALL close(scr2,1)
      DO
         CALL mtrxo(scr2,buf(2),buf(4),0,rc)
         IF ( rc==2 .OR. rc==3 ) EXIT
         IF ( rc==4 ) THEN
!
!     SUBSTRUCTURE DOES NOT EXIST - ADD IT TO THE SOF HIERARCHY
!
            CALL exlvl(nos,z(idm),buf(2),z(10*iss+1),lcore-10*iss)
            CYCLE
         ELSEIF ( rc==5 ) THEN
!
!     ILLEGAL ITEM NAME
!
            line = line + 2
            IF ( line>nlpp ) CALL page
            CALL smsg(3,buf(4),buf(2))
         ELSEIF ( rc==6 ) THEN
            GOTO 4800
         ELSE
!
!     ITEM ALREADY EXISTS
!
            line = line + 2
            IF ( line>nlpp ) CALL page
            WRITE (nout,99026) uwm , buf(2) , buf(3) , buf(4)
            z(10*iss) = 1
         ENDIF
         CALL read(*1100,*1200,unit,buf,10,1,flag)
         GOTO 1200
      ENDDO
   ELSE
      CALL write(scr2,z(i),nw,1)
      GOTO 2500
   ENDIF
!
!     ITEM COPIED - PRINT MESSAGE
!
 2700 line = line + 1
   IF ( line>nlpp ) CALL page
   WRITE (nout,99024) uim , buf(2) , buf(3) , buf(4) , unit , sof , (buf(i),i=5,10)
!
!     INCREMENT NUMBER OF ITEMS COPIED.  IF NOT ALL ARE COPIED, LOOP
!     BACK TO FIND NEXT SUBSTRUCTURE/ITEM ON THE EXTERNAL FILE TO BE
!     COPIED.
!
   jcopy = jcopy + 1
   IF ( ncopy/=jcopy ) THEN
      CALL read(*1100,*1200,unit,buf,10,1,flag)
      GOTO 1200
   ELSE
      CALL close(unit,norew)
      GOTO 3900
   ENDIF
 2800 DO
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4300,*2800,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL close(unit,norew)
            GOTO 3900
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4300,unit)
      ENDIF
   ENDDO
 2900 line = line + 1
   IF ( line>nlpp ) CALL page
   WRITE (nout,99023) uim , (buf(i),i=2,9)
 3000 DO
      CALL rectyp(unit,irec)
      IF ( irec==0 ) THEN
!
!     NORMAL GINO RECORD - CHECK IF EOI
!
         CALL read(*4300,*3000,unit,i,1,1,flag)
         IF ( i==eoi ) THEN
            CALL read(*3100,*2900,unit,buf,10,1,flag)
            line = line + 1
            IF ( line>nlpp ) CALL page
            WRITE (nout,99020) (buf(i),i=2,10)
99020       FORMAT (5X,'SUBSTRUCTURE ',2A4,5X,'ITEM ',A4,10X,5HDATE ,I2,1H/,I2,1H/,I2,10X,5HTIME ,I2,1H.,I2,1H.,I2)
         ENDIF
      ELSE
!
!     STRING RECORD - SKIP IT
!
         CALL fwdrec(*4300,unit)
      ENDIF
   ENDDO
 3100 CALL bckrec(unit)
   CALL close(unit,norew)
   GOTO 3900
!
!     READ HEADER RECORD AND FETCH THE SOF ITEM
!
 3200 CALL read(*3800,*4400,scr1,buf,4,1,flag)
   kdh = ittype(buf(3))
   IF ( kdh==1 ) THEN
!
!     COPY IN MATRIX ITEMS
!
      CALL open(*4800,scr2,z(buf5),wrtrew)
      CALL read(*4300,*4400,scr1,z(iz2),6,1,nw)
      z(1) = scr2
      CALL wrttrl(z(1))
      inblk(1) = scr1
      outblk(1) = scr2
      GOTO 3500
   ELSE
      CALL sfetch(buf,buf(3),2,buf(4))
   ENDIF
 3300 DO
!
!     COPY THE DATA
!
      CALL read(*4300,*3400,scr1,z,lcore,0,flag)
      IF ( z(1)==eoi ) THEN
!
!     EOI FOUND
!
         CALL suwrt(0,0,3)
         GOTO 3700
      ELSE
         CALL suwrt(z,lcore,1)
      ENDIF
   ENDDO
 3400 IF ( z(1)==eoi ) THEN
      CALL suwrt(0,0,3)
      GOTO 3700
   ELSE
      CALL suwrt(z,flag,2)
      GOTO 3300
   ENDIF
 3500 DO
      CALL rectyp(scr1,itype)
      IF ( itype/=0 ) THEN
         CALL cpystr(inblk,outblk,0,0)
      ELSE
         DO
            CALL read(*4300,*3600,scr1,z,lcore,0,nw)
            CALL write(scr2,z,lcore,0)
         ENDDO
      ENDIF
   ENDDO
 3600 IF ( z(1)==eoi ) THEN
!
!     EOI FOUND
!
      CALL close(scr2,1)
      CALL mtrxo(scr2,buf,buf(3),0,rc)
   ELSE
      CALL write(scr2,z,nw,1)
      GOTO 3500
   ENDIF
 3700 line = line + 1
   IF ( line>nlpp ) CALL page
   WRITE (nout,99021) uim , buf(1) , buf(2) , buf(3)
99021 FORMAT (A29,' 6353, SUBSTRUCTURE ',2A4,' ITEM ',A4,' HAS BEEN SUCCESSFULLY COMPRESSED')
   GOTO 3200
!
!     COMPRESS COMPLETE
!
 3800 CALL close(scr1,rew)
!
!     **********************   C O D A   ************************
!
!     NORMAL TERMINATION
!
 3900 CALL sofcls
 4000 RETURN
 4100 WRITE (nout,99022) swm , uname
99022 FORMAT (A27,' 6343, ',2A4,' IS NOT AN EXTERNAL SOF FILE')
   CALL close(unit,norew)
   GOTO 4600
!
 4200 n = -1
   GOTO 4700
 4300 n = -2
   GOTO 4700
 4400 n = -3
   GOTO 4700
 4500 n = -61
   GOTO 4700
 4600 CALL sofcls
   dry = -2
   WRITE (nout,99028) sim
   RETURN
!
 4700 CALL sofcls
   CALL mesage(n,unit,subr)
   dry = -2
   WRITE (nout,99028) sim
   RETURN
!
 4800 n = -1
   GOTO 5100
 4900 n = -2
   GOTO 5100
 5000 n = -3
 5100 CALL sofcls
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
