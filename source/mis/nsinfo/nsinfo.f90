!*==nsinfo.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE C_BLANK
   USE C_MACHIN
   USE C_NTIME
   USE C_NUMTPX
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Jump
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bnd , bnk , bsz , cod , dd , dic , ech , end , equ , equals , hic , key , lpp , mxl , nos , npr , pch , pop ,  &
                   & pru , relse , s , s3s , s88 , s89 , s90 , s92 , s94 , s96 , s97 , s98 , s99 , skp3 , tim , tpg , ttpg
   INTEGER :: bndit , code , count , dict , echo , ehco , eq , hicore , i , j , jb , je , l20 , line , lpch , lprus , lu , machx ,  &
            & mxlns , nlpp , nosbe , nout , nprus , one , pltop , symb1 , symbol , sysbuf , value
   INTEGER , DIMENSION(20) :: card
   INTEGER , DIMENSION(4) :: cardx
   INTEGER , DIMENSION(4) , SAVE :: diag48
   CHARACTER(144) :: ifile
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL khrfn1 , mesage , nasopn , page1 , pexit , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Jump==1 ) RETURN
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
            IF ( lu==0 .OR. skp3==1 ) RETURN
            CALL page1
            DO
               READ (lu,99009,END=60) card
               IF ( card(1)/=dd ) THEN
                  IF ( card(1)/=equals ) THEN
                     WRITE (nout,99007) card
                  ELSE
                     IF ( card(2)==equals ) RETURN
                     CALL page1
                     WRITE (nout,99008)
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
            IF ( lu==0 ) THEN
!
               WRITE (nout,99001) Uim
99001          FORMAT (A29,', DIAG48 MESSAGES ARE NOT AVAILABLE DUE TO ABSENCE ','OF THE NASINFO FILE')
               GOTO 80
            ELSE
               DO i = 10 , 20
                  Pghdg3(i) = bnk
               ENDDO
               Pghdg3(6) = diag48(1)
               Pghdg3(7) = diag48(2)
               Pghdg3(8) = diag48(3)
               Pghdg3(9) = diag48(4)
               line = nlpp + 1
               count = 0
!
!     READ AND PRINT RELEASE NEWS
!     PRINT LAST TWO YEARS OF NEWS ONLY, IF DIAG 20 IS ON
!     (MECHANISM - GEAR TO THE 'nn RELEASE' LINE AND '========' LINES)
!
               one = 1
               IF ( l20==1 ) one = 0
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
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
            lu = 99
            CALL nasopn(*40,lu,ifile)
            DO
!
!     SEARCH FOR FIRST EQUAL-LINE
!
               READ (lu,99009,ERR=20,END=20) cardx
               IF ( card(1)==equals .OR. card(2)==equals ) THEN
                  DO
!
!     READ AND PROCESS THE 2ND SECTION OF NASINFO FILE
!
                     READ (lu,99010,END=60) symbol , eq , value
                     IF ( symbol/=bnk ) THEN
                        IF ( eq/=equ ) THEN
                           WRITE (nout,99002) symbol , eq , value
99002                      FORMAT ('0*** ERROR IN NASINFO FILE - LINE - ',A4,A1,I7)
                        ELSEIF ( symbol==tim ) THEN
!
!     READ IN 16 GINO TIME CONSTANTS (NT=16)
!
                           IF ( value/=Nt ) THEN
                              READ (lu,99010,END=60) symbol
                              READ (lu,99010,END=60) symbol
                           ELSE
                              READ (lu,99003,END=60) Time
99003                         FORMAT (12X,8F7.2,/12X,8F7.2)
                           ENDIF
                        ELSEIF ( symbol==end ) THEN
                           DO
!
!     READ PASS THE 2ND EQUAL-LINE. CONTINUE INTO 3RD SECTION
!
                              READ (lu,99009,END=60) cardx
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
                                 READ (lu,99009,END=60)
                                 READ (lu,99009)
                                 READ (lu,99009)
                                 READ (lu,99009)
                                 READ (lu,99009)
                                 DO
!
                                    READ (lu,99004,END=60) machx , Nbcd
99004                               FORMAT (I2,I3)
                                    IF ( machx==Mach ) THEN
                                       IF ( Nbcd/=0 ) THEN
                                         jb = 1
                                         DO i = 1 , Nbcd , 19
                                         je = jb + 18
                                         READ (lu,99011) (Bcd(j),j=jb,je)
                                         ENDDO
                                       ENDIF
                                       DO
!
!     READ PASS THE 3RD EQUAL-LINE, THEN RETURN
!
                                         READ (lu,99009,END=60) cardx
                                         IF ( card(1)==equals .OR. card(2)==equals ) THEN
                                         IF ( ttpg/=0 ) Jump = ttpg
                                         RETURN
                                         ENDIF
                                       ENDDO
                                    ELSEIF ( Nbcd/=0 ) THEN
                                       DO i = 1 , Nbcd , 19
                                         READ (lu,99011)
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
                              IF ( symbol==bsz ) sysbuf = value
                              IF ( symbol==lpp ) nlpp = value
                              IF ( symbol==hic ) hicore = value
                              IF ( symbol==mxl ) mxlns = value
                              IF ( symbol==tpg ) ttpg = value
                              IF ( symbol==ech ) echo = value
                              IF ( symbol==pch ) lpch = value
                              IF ( symbol==dic ) dict = value
                              IF ( symbol==bnd ) bndit = value
                              IF ( symbol==pop ) pltop = value
                              IF ( symbol==pru ) lprus = value
                              IF ( symbol==npr ) nprus = value
                              IF ( symbol==nos ) nosbe = value
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
 20      CLOSE (UNIT=lu)
         CALL mesage(2,0,name)
 40      WRITE (nout,99005) ifile
!WKBR*        1X, A167/)
99005    FORMAT ('0*** USER WARNING MESSAGE, UNABLE TO OPEN ','THE FOLLOWING NASINFO FILE -- '//1X,A44/)
         lu = 0
         RETURN
      CASE (2)
         SPAG_Loop_1_1: DO
            READ (lu,99009,END=80) card
            IF ( card(1)==equals .AND. card(2)==equals ) EXIT SPAG_Loop_1_1
            IF ( one/=-1 ) THEN
               IF ( card(2)==relse .AND. card(4)==bnk ) THEN
                  count = count + one
                  IF ( count>2 ) THEN
                     DO
                        READ (lu,99009,END=80) cardx
                        IF ( card(1)==equals .AND. card(2)==equals ) EXIT SPAG_Loop_1_1
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
            IF ( line>=nlpp ) THEN
               CALL page1
               IF ( line==nlpp ) THEN
                  WRITE (nout,99008)
                  line = 5
               ELSE
                  line = 3
               ENDIF
            ENDIF
            line = line + 1
            WRITE (nout,99007) card
         ENDDO SPAG_Loop_1_1
!
!     READ AND PRINT THE REST OF SECTION 5
!
         IF ( one==-1 ) GOTO 80
         one = -1
         line = nlpp + 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ERROR
!
 60      WRITE (nout,99006) Sfm
99006    FORMAT (A25,' 3002, EOF ENCOUNTERED WHILE READING NASINFO FILE')
         STOP 'JOB TERMINATED IN SUBROUTINE NSINFO'
!
 80      IF ( l20/=0 ) THEN
            CLOSE (UNIT=lu)
            CALL pexit
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (25X,20A4)
99008 FORMAT (//)
99009 FORMAT (20A4)
99010 FORMAT (A4,A1,I7)
99011 FORMAT (5X,19(A4,1X))
END SUBROUTINE nsinfo
