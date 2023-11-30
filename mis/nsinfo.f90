
SUBROUTINE nsinfo(Jump)
!
!     THIS ROUTINE READS AND PROCESSES DATA IN THE NASINFO FILE
!
!     JUMP = 2, NSINFO IS CALLED BY NASCAR TO OPEN NASINFO FILE AND
!               PROCESS THE SYSTEM PRESET PARAMETERS IN THE 2ND SECTION
!               OF THE FILE, AND THE BCD WORDS (USED ONLY BY NUMTYP
!               SUBROUTINE) IN THE 3RD SECTION
!     JUMP = 3, NSINFO IS CALLED BY TTLPGE TO PROCESS THE INSTALLATION-
!               CENTER-TO-USER MESSAGES STORED IN THE 4TH SECTION OF
!               THE NASINFO FILE
!     JUMP = 4, NSINFO IS CALLED BY XCSA TO ECHO DIAG 48 MESSAGE STORED
!               IN THE 5TH SECITON OF THE NASINFO FILE.
!
!     SINCE DIAG48 MAY NOT BE CALLED, NASINFO FILE IS CLOSED BY XCSA
!
!     WRITTEN BY G.CHAN/UNISYS    6/1990
!
   IMPLICIT NONE
   INTEGER Bcd(1) , Bndit , Dict , Dum(64) , Ehco , Hicore , Iblnk(60) , Lpch , Lprus , Lu , Mach , Mxlns , Nbcd , Nlpp , Nosbe ,   &
         & Nout , Nprus , Nt , Pghdg3(32) , Pltop , Sys(100) , Sysbuf
   CHARACTER*25 Sfm , Uwm
   REAL Time(16)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Iblnk
   COMMON /machin/ Mach
   COMMON /ntime / Nt , Time
   COMMON /numtpx/ Nbcd , Bcd
   COMMON /output/ Dum , Pghdg3
   COMMON /system/ Sys
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   INTEGER Jump
   INTEGER bnd , bnk , bsz , card(20) , cardx(4) , cod , code , count , dd , diag48(4) , dic , ech , echo , end , eq , equ ,        &
         & equals , hic , i , j , jb , je , key , l20 , line , lpp , machx , mxl , name(2) , nos , npr , one , pch , pop , pru ,    &
         & relse , s , s3s , s88 , s89 , s90 , s92 , s94 , s96 , s97 , s98 , s99 , skp3 , symb1 , symbol , tim , tpg , ttpg , value
   CHARACTER*144 ifile
   INTEGER khrfn1
!WKBR 8/94 SUN INTEGER         NAME(2),NTAB(5),CARDX(4),CARD(20),DIAG48(4)
!WKBR CHARACTER*167   IFILE
   !>>>>EQUIVALENCE (cardx(1),card(1))
   !>>>>EQUIVALENCE (Sys(1),Sysbuf) , (Sys(2),Nout) , (Sys(9),Nlpp) , (Sys(14),Mxlns) , (Sys(19),Ehco) , (Sys(31),Hicore) ,              &
!>>>>    & (Sys(35),Lprus) , (Sys(37),Lu) , (Sys(36),Nprus) , (Sys(20),Pltop) , (Sys(92),Dict) , (Sys(76),Nosbe) , (Sys(77),Bndit) ,     &
!>>>>    & (Sys(91),Lpch)
   DATA ttpg/0/
!WKBR 8/94 SUN DATA    EQU   , R  ,  S  ,  BNK      ,   EQUALS   , NAME         /
!WKBR 8/94 SUN1        1H=   , 1HR,  1HS,  4H       ,   4H====   , 4HNSIN,2HFO  /
   DATA equ , s , bnk , equals , name/1H= , 1HS , 4H     , 4H==== , 4HNSIN , 2HFO/
 
   DATA relse , tpg , pop , tim , mxl , bsz , s3s , skp3/4HELEA , 3HTPG , 3HPOP , 3HTIM , 3HMXL , 3HBSZ , 3HS3S , 0/
   DATA lpp , hic , bnd , ech , nos , pru , npr , pch , end/3HLPP , 3HHIC , 3HBND , 3HECH , 3HNOS , 3HPRU , 3HNPR , 3HPCH , 3HEND/
   DATA s88 , s89 , s90 , s92 , s94 , s96 , s97 , s98 , s99/3HS88 , 3HS89 , 3HS90 , 3HS92 , 3HS94 , 3HS96 , 3HS97 , 3HS98 , 3HS99/
   DATA diag48 , dd , dic , cod , key/4H D I , 4H A G , 4H   4 , 2H 8 , 3H$.  , 3HDIC , 3HCOD , 3HKEY/
!
   IF ( Jump==1 ) GOTO 99999
   IF ( Jump==3 ) THEN
!
!     JUMP = 3
!     ========
!
!     READ AND ECHO OUT INSTALLATION-CENTER-TO-USER MESSAGES, SAVED IN
!     THE 4TH SECTION OF NASINFO FILE
!     TERMINATE MESSAGES BY THE LAST EQUAL-LINE.
!
!     IN THIS MESSAGE SECTION ONLY, SKIP INPUT LINE IF A '$.  ' SYMBOL
!     IS IN FIRST 4 COLUMNS.
!
      IF ( Lu==0 .OR. skp3==1 ) GOTO 99999
      CALL page1
      DO
         READ (Lu,99009,END=500) card
         IF ( card(1)/=dd ) THEN
            IF ( card(1)/=equals ) THEN
               WRITE (Nout,99007) card
            ELSE
               IF ( card(2)==equals ) GOTO 99999
               CALL page1
               WRITE (Nout,99008)
            ENDIF
         ENDIF
      ENDDO
   ELSEIF ( Jump==4 ) THEN
!
!     JUMP = 4
!     ========
!
!     PROCESS DIAG48 MESSAGE, SAVED IN THE 5TH SECTION OF NASINFO FILE
!
      CALL sswtch(20,l20)
      IF ( Lu==0 ) THEN
!
         WRITE (Nout,99001) Uim
99001    FORMAT (A29,', DIAG48 MESSAGES ARE NOT AVAILABLE DUE TO ABSENCE ','OF THE NASINFO FILE')
         GOTO 600
      ELSE
         DO i = 10 , 20
            Pghdg3(i) = bnk
         ENDDO
         Pghdg3(6) = diag48(1)
         Pghdg3(7) = diag48(2)
         Pghdg3(8) = diag48(3)
         Pghdg3(9) = diag48(4)
         line = Nlpp + 1
         count = 0
!
!     READ AND PRINT RELEASE NEWS
!     PRINT LAST TWO YEARS OF NEWS ONLY, IF DIAG 20 IS ON
!     (MECHANISM - GEAR TO THE 'nn RELEASE' LINE AND '========' LINES)
!
         one = 1
         IF ( l20==1 ) one = 0
         GOTO 300
      ENDIF
   ELSE
!
!     JUMP = 2
!     ========
!
!     OPEN NASINFO FILE, AND SET LU, THE 37TH WORD OF /SYSTEM/
!
!     CURRENTLY 'NASINFO' IS USED FOR ALL MACHINES OF TYPE 5 AND HIGHER
!
      Lu = 99
      CALL nasopn(*200,Lu,ifile)
      DO
!
!     SEARCH FOR FIRST EQUAL-LINE
!
         READ (Lu,99009,ERR=100,END=100) cardx
         IF ( card(1)==equals .OR. card(2)==equals ) THEN
            DO
!
!     READ AND PROCESS THE 2ND SECTION OF NASINFO FILE
!
               READ (Lu,99010,END=500) symbol , eq , value
               IF ( symbol/=bnk ) THEN
                  IF ( eq/=equ ) THEN
                     WRITE (Nout,99002) symbol , eq , value
99002                FORMAT ('0*** ERROR IN NASINFO FILE - LINE - ',A4,A1,I7)
                  ELSEIF ( symbol==tim ) THEN
!
!     READ IN 16 GINO TIME CONSTANTS (NT=16)
!
                     IF ( value/=Nt ) THEN
                        READ (Lu,99010,END=500) symbol
                        READ (Lu,99010,END=500) symbol
                     ELSE
                        READ (Lu,99003,END=500) Time
99003                   FORMAT (12X,8F7.2,/12X,8F7.2)
                     ENDIF
                  ELSEIF ( symbol==end ) THEN
                     DO
!
!     READ PASS THE 2ND EQUAL-LINE. CONTINUE INTO 3RD SECTION
!
                        READ (Lu,99009,END=500) cardx
                        IF ( card(1)==equals .OR. card(2)==equals ) THEN
!
!
!     THIS 3RD SECTION CONTAINS BCD WORDS WHICH ARE REALLY REAL NUMBERS.
!     (THE BINARY REPRESENTATIONS OF SOME REAL NUMBERS AND THEIR
!     CORRESPONDING BCD WORDS ARE EXACTLY THE SAME. SUBROUTINE NUMTYP
!     MAY IDENTIFY THEM AS TYPE BCD. ANY WORD ON THE BCD LIST WILL BE
!     REVERTED BACK TO AS TYPE REAL. THE LIST IS MACHINE DEPENDENT)
!
!     SKIP FIRST 5 COMMENT LINES
!
                           READ (Lu,99009,END=500)
                           READ (Lu,99009)
                           READ (Lu,99009)
                           READ (Lu,99009)
                           READ (Lu,99009)
                           DO
!
                              READ (Lu,99004,END=500) machx , Nbcd
99004                         FORMAT (I2,I3)
                              IF ( machx==Mach ) THEN
                                 IF ( Nbcd/=0 ) THEN
                                    jb = 1
                                    DO i = 1 , Nbcd , 19
                                       je = jb + 18
                                       READ (Lu,99011) (Bcd(j),j=jb,je)
                                    ENDDO
                                 ENDIF
                                 DO
!
!     READ PASS THE 3RD EQUAL-LINE, THEN RETURN
!
                                    READ (Lu,99009,END=500) cardx
                                    IF ( card(1)==equals .OR. card(2)==equals ) THEN
                                       IF ( ttpg/=0 ) Jump = ttpg
                                       GOTO 99999
                                    ENDIF
                                 ENDDO
                              ELSEIF ( Nbcd/=0 ) THEN
                                 DO i = 1 , Nbcd , 19
                                    READ (Lu,99011)
                                 ENDDO
                              ENDIF
                           ENDDO
                        ENDIF
                     ENDDO
                  ELSEIF ( value/=-99 ) THEN
                     IF ( symbol==s3s ) THEN
!
!     SKIP JUMP 3 PRINTOUT
!
                        skp3 = 1
                     ELSE
                        IF ( symbol==bsz ) Sysbuf = value
                        IF ( symbol==lpp ) Nlpp = value
                        IF ( symbol==hic ) Hicore = value
                        IF ( symbol==mxl ) Mxlns = value
                        IF ( symbol==tpg ) ttpg = value
                        IF ( symbol==ech ) echo = value
                        IF ( symbol==pch ) Lpch = value
                        IF ( symbol==dic ) Dict = value
                        IF ( symbol==bnd ) Bndit = value
                        IF ( symbol==pop ) Pltop = value
                        IF ( symbol==pru ) Lprus = value
                        IF ( symbol==npr ) Nprus = value
                        IF ( symbol==nos ) Nosbe = value
                        IF ( symbol==cod ) code = value
                        IF ( symbol==key ) key = value
                        symb1 = khrfn1(bnk,1,symbol,1)
                        IF ( symb1==s ) THEN
                           IF ( symbol==s88 ) Sys(88) = value
                           IF ( symbol==s89 ) Sys(89) = value
                           IF ( symbol==s90 ) Sys(90) = value
                           IF ( symbol==s92 ) Sys(92) = value
                           IF ( symbol==s94 ) Sys(94) = value
                           IF ( symbol==s96 ) Sys(96) = value
                           IF ( symbol==s97 ) Sys(97) = value
                           IF ( symbol==s98 ) Sys(98) = value
                           IF ( symbol==s99 ) Sys(99) = value
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDIF
!
!     NASINFO DOES NOT EXIST (or IS WRITE-PROTECTED), SET LU TO ZERO
!
 100  CLOSE (UNIT=Lu)
   CALL mesage(2,0,name)
 200  WRITE (Nout,99005) ifile
!WKBR*        1X, A167/)
99005 FORMAT ('0*** USER WARNING MESSAGE, UNABLE TO OPEN ','THE FOLLOWING NASINFO FILE -- '//1X,A44/)
   Lu = 0
   GOTO 99999
 300  DO
      READ (Lu,99009,END=600) card
      IF ( card(1)==equals .AND. card(2)==equals ) EXIT
      IF ( one/=-1 ) THEN
         IF ( card(2)==relse .AND. card(4)==bnk ) THEN
            count = count + one
            IF ( count>2 ) THEN
               DO
                  READ (Lu,99009,END=600) cardx
                  IF ( card(1)==equals .AND. card(2)==equals ) GOTO 400
               ENDDO
            ENDIF
         ENDIF
      ENDIF
      IF ( line>=Nlpp ) THEN
         CALL page1
         IF ( line==Nlpp ) THEN
            WRITE (Nout,99008)
            line = 5
         ELSE
            line = 3
         ENDIF
      ENDIF
      line = line + 1
      WRITE (Nout,99007) card
   ENDDO
!
!     READ AND PRINT THE REST OF SECTION 5
!
 400  IF ( one==-1 ) GOTO 600
   one = -1
   line = Nlpp + 1
   GOTO 300
!
!     ERROR
!
 500  WRITE (Nout,99006) Sfm
99006 FORMAT (A25,' 3002, EOF ENCOUNTERED WHILE READING NASINFO FILE')
   STOP 'JOB TERMINATED IN SUBROUTINE NSINFO'
!
 600  IF ( l20/=0 ) THEN
      CLOSE (UNIT=Lu)
      CALL pexit
   ENDIF
99007 FORMAT (25X,20A4)
99008 FORMAT (//)
99009 FORMAT (20A4)
99010 FORMAT (A4,A1,I7)
99011 FORMAT (5X,19(A4,1X))
99999 RETURN
END SUBROUTINE nsinfo