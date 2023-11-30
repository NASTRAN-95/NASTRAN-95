
SUBROUTINE rcovo
   IMPLICIT NONE
   INTEGER Buf(1) , Buf1 , Energy , Fss(2) , Icore , Iopt , Ireq , Lbasic , Lcore , Loop , Lreq , Lui , Neigv , Norew , Nosort ,    &
         & Nout , Rd , Rdrew , Rew , Rfno , Rss(2) , Sof1 , Sof2 , Sof3 , Step , Sysbuf , Uimpro , Wrt , Wrtrew , Z(1)
   REAL Buf2 , Buf3 , Buf4 , Dry , Eofnrw , Pa , Pthres , Qa , Qthres , Rang(2) , Rbuf(1) , Ua , Uinms(2,5) , Uthres
   LOGICAL Mrecvr
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Rang , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER all , casess , comds(13) , eqss , freq , i , i1 , i2 , idit , iloc , imdi , irange , iset , ll , maskll , mode , mrecov ,&
         & n , nbs , ncomds , none , np , nreq , nss1 , nss2 , nwds , print , rc , rec(3) , recovr , save , srd , subc , subnam(2) ,&
         & subs , time
   INTEGER andf , korsz , lshift
   LOGICAL basic
   REAL rrec(3) , rset
   EXTERNAL andf , lshift
!
!     RCOVO READS THE CASESS RECOVER RECORD AND PROCESSES ANY
!     OUTPUT REQUESTS FOR THE CURRENT SAVE OR PRINT REQUEST
!
!     THE OUTPUT REQUESTS ARE STORED AT THE BOTTOM OF OPEN CORE IN
!     A TABLE WITH THE FOLLOWING FORM
!
!     BUF(IREQ) -  UVEC  -I
!                  PVEC   I- NONZERO IF ANY REQUEST PRESENT
!                  QVEC  -I
!                  NO. OF POINTS
!                  NO. OF BASICS
!                  BASIC NAME(2) -I
!                  DISP SET       I
!                  OLOAD SET      I
!                  SPCF SET       I - REPEATED FOR EACH BASIC
!                  SUBCASES SET   I   SUBSTRUCTURE
!                  MODES SET      I
!                  RANGE(2)       I
!                  VELO SET       I
!                  ACCE SET       I
!                  STEPS SET      I
!                  GRID OR MODAL -I
!
   !>>>>EQUIVALENCE (Buf(1),Z(1))
   !>>>>EQUIVALENCE (Buf(1),Rbuf(1)) , (rec(1),rrec(1)) , (iset,rset)
   DATA casess/101/ , subnam/4HRCOV , 4HO   /
   DATA eqss/4HEQSS/
   DATA recovr/4HRECO/ , mrecov/4HMREC/
   DATA print/4HPRIN/ , save/4HSAVE/
   DATA srd/1/
   DATA ll/2/
   DATA ncomds/13/
   DATA comds/4HDISP , 4HOLOA , 4HSPCF , 4HMODE , 4HRANG , 4HSUBC , 4HSORT , 4HBASI , 4HVELO , 4HACCE , 4HENER , 4HUIMP , 4HSTEP/
   DATA subc , subs , mode , all , none , time , freq/4HSUBC , 4HSUBS , 4HMODE , 4HALL  , 4HNONE , 4HTIME , 4HFREQ/
!
!     SET UP BUFFERS
!
   Sof1 = 1
   Sof2 = Sof1 + Sysbuf
   Sof3 = Sof2 + Sysbuf + 1
   Buf1 = Sof3 + Sysbuf
   Icore = Buf1 + Sysbuf
   Lcore = korsz(Z(1)) - Icore + 1
   IF ( Lcore<=0 ) THEN
      n = -8
      GOTO 1400
   ELSE
!
!     FIND RECOVER RECORD IN CASESS
!
      CALL gopen(casess,Z(Buf1),Rdrew)
      IF ( Step/=1 ) THEN
         DO i = 2 , Step
            CALL fwdrec(*1300,casess)
         ENDDO
      ENDIF
      CALL fread(casess,rec,2,0)
      IF ( rec(1)/=recovr .AND. rec(1)/=mrecov ) THEN
!
         WRITE (Nout,99001) Swm , Step , rec(1)
!
!     FORMATS
!
99001    FORMAT (A27,' 6305, RECORD NUMBER',I5,' IS NOT A RECOVER RECORD.','  IT IS A ',A4,' RECORD.')
         GOTO 1200
      ELSE
         Mrecvr = .FALSE.
         IF ( rec(1)==mrecov ) Mrecvr = .TRUE.
!
!     GET PRINT OR SAVE OPTION FOR THIS PASS
!
         i = 0
         DO
            CALL read(*1300,*900,casess,rec,3,0,nwds)
            IF ( rec(1)==print .OR. rec(1)==save ) THEN
               IF ( Loop==i ) THEN
!
!     GET NAME OF SUBSTRUCTURE TO BE OPERATED ON
!
                  Rss(1) = rec(2)
                  Rss(2) = rec(3)
                  Loop = Loop + 1
                  IF ( rec(1)==save ) THEN
                     DO
!
!     THIS LOOP IS A SAVE COMMAND - SEE IF ANY OTHER COMMANDS FOLLOW
!
                        CALL read(*1300,*1000,casess,rec,3,0,nwds)
                        IF ( rec(1)==print .OR. rec(1)==save ) GOTO 1100
                     ENDDO
                  ELSE
                     Iopt = 1
!
!     OPEN SOF AND FETCH EQSS FOR SUBSTRUCTURE TO BE PRINTED
!
!
                     CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                     CALL sfetch(Rss,eqss,srd,rc)
                     IF ( rc==1 ) THEN
!
!     READ GROUP 0 OF EQSS INTO CORE
!
                        CALL suread(Z(Icore),Lcore,nwds,rc)
                        IF ( rc==1 ) THEN
                           n = -8
                           GOTO 1400
                        ELSEIF ( rc==2 ) THEN
!
!     DETERMINE SIZE OF OUTPUT REQUEST BLOCK AND ALLOCATE SPACE
!     AT BOTTOM OF OPEN CORE
!
                           nbs = Z(Icore+2)
                           np = Z(Icore+3)
                           Lbasic = 13
                           Lreq = 5 + Lbasic*nbs + 2
                           IF ( Lreq>Lcore-nwds ) THEN
                              n = -8
                              GOTO 1400
                           ELSE
                              nreq = korsz(Buf(1))
                              Ireq = nreq - Lreq + 1
                              DO i = Ireq , nreq
                                 Buf(i) = 0
                              ENDDO
!
!     MOVE NAMES OF BASICS INTO OUTPUT AREA
!
                              Buf(Ireq+3) = np
                              Buf(Ireq+4) = nbs
                              DO i = 1 , nbs
                                 i1 = Ireq + (i-1)*Lbasic + 5
                                 i2 = Icore + (i-1)*2 + 4
                                 Buf(i1) = Z(i2)
                                 Buf(i1+1) = Z(i2+1)
                              ENDDO
!
!     INSERT DEFAULTS INTO OUTPUT BLOCK
!
!     MODES    = ALL
!     SUBCASES = ALL
!     RANGE    = -1.0E+35,1.0E+35
!     STEPS    = ALL
!
                              Energy = 0
                              Uimpro = 0
                              Rang(1) = -1.0E+35
                              Rang(2) = 1.0E+35
                              Buf(Ireq) = -2
                              Buf(Ireq+1) = -2
                              Buf(Ireq+2) = -2
                              DO i = 1 , nbs
                                 i1 = Ireq + (i-1)*Lbasic + 5
                                 Buf(i1+2) = -2
                                 Buf(i1+3) = -2
                                 Buf(i1+4) = -2
                                 Buf(i1+5) = -1
                                 Buf(i1+6) = -1
                                 Rbuf(i1+7) = -1.0E+35
                                 Rbuf(i1+8) = 1.0E+35
                                 Buf(i1+9) = -2
                                 Buf(i1+10) = -2
                                 Buf(i1+11) = -1
                              ENDDO
!
!     READ NEXT COMMAND AND PROCESS OUTPUT REQUEST
!
                              nss1 = 1
                              nss2 = nbs
                              irange = 0
                              basic = .FALSE.
                              EXIT
                           ENDIF
                        ELSEIF ( rc==4 ) THEN
                           GOTO 1200
                        ELSE
                           CALL smsg(7,eqss,Rss)
                           GOTO 1200
                        ENDIF
                     ELSEIF ( rc==4 ) THEN
                        WRITE (Nout,99002) Uwm , Rss
99002                   FORMAT (A25,' 6306, ATTEMPT TO RECOVER DISPLACEMENTS FOR NON-','EXISTANT SUBSTRUCTURE ',2A4)
                        GOTO 1200
                     ELSE
!
!     FETCH ON EQSS WAS UNSUCCESSFUL
!
                        IF ( rc==2 ) rc = 3
                        CALL smsg(rc-2,eqss,Rss)
                        GOTO 1200
                     ENDIF
                  ENDIF
               ELSE
                  i = i + 1
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDIF
 100  DO
      CALL read(*1300,*700,casess,rec,3,0,nwds)
      IF ( rec(1)==print .OR. rec(1)==save ) GOTO 800
      DO i = 1 , ncomds
         IF ( rec(1)==comds(i) ) GOTO 200
      ENDDO
   ENDDO
 200  IF ( i==2 ) THEN
!
!     OLOAD REQUEST
!
      IF ( rec(2)/=none ) Buf(Ireq+1) = 1
      IF ( rec(2)==none .AND. .NOT.basic ) Buf(Ireq+1) = 0
      iloc = 3
      GOTO 500
   ELSEIF ( i==3 ) THEN
!
!     SPCF REQUEST
!
      IF ( rec(2)/=none ) Buf(Ireq+2) = 1
      IF ( rec(2)==none .AND. .NOT.basic ) Buf(Ireq+2) = 0
      iloc = 4
      GOTO 500
   ELSEIF ( i==4 ) THEN
!
!     MODES REQUEST
!
      iloc = 6
      GOTO 500
   ELSEIF ( i==5 ) THEN
!
!     RANGE REQUEST (IF BEFORE A BASIC COMMAND SAVE IT FOR ENERGY
!                    PROCESSING ALSO)
!
      iloc = 7
      IF ( mod(irange,2)==1 ) iloc = 8
      irange = irange + 1
      IF ( rec(2)/=-2 .AND. rec(3)/=0 ) THEN
!
!     ILLEGAL COMMAND FORMAT
!
         WRITE (Nout,99003) Uwm , rec(1)
99003    FORMAT (A25,' 6367, ILLEGAL FORMAT ON THE RECOVER OUTPUT COMMAND',1X,A4,', COMMAND IGNORED.')
         GOTO 100
      ELSE
         IF ( .NOT.(basic) ) Rang(iloc-6) = rrec(3)
         GOTO 600
      ENDIF
   ELSEIF ( i==6 ) THEN
!
!     SUBCASES REQUEST
!
      iloc = 5
      GOTO 500
   ELSEIF ( i==7 ) THEN
!
!     SORT COMMAND - IGNORE COMMAND IF AFTER A BASIC DESIGNATOR
!
      IF ( basic ) THEN
         WRITE (Nout,99004) Uwm
99004    FORMAT (A25,' 6366, THE RECOVER OUTPUT COMMAND SORT MUST APPEAR ','BEFORE THE FIRST BASIC SUBCOMMAND.',/32X,               &
                &'ANY OTHER SORT COMMANDS ARE IGNORED.')
      ELSE
         i = 0
         IF ( rec(2)==subc ) i = 1
         IF ( rec(2)==subs ) i = 2
         IF ( rec(2)==mode ) i = 1
         IF ( rec(2)==time ) i = 1
         IF ( rec(2)==freq ) i = 1
         IF ( i==0 ) THEN
            WRITE (Nout,99003) Uwm , rec(1)
         ELSE
            Iopt = i
         ENDIF
      ENDIF
      GOTO 100
   ELSEIF ( i==8 ) THEN
   ELSEIF ( i==9 ) THEN
!
!     VELOCITY REQUEST
!
      IF ( Rfno/=8 .AND. Rfno/=9 ) GOTO 100
      IF ( rec(2)/=none ) Buf(Ireq) = 1
      iloc = 9
      GOTO 500
   ELSEIF ( i==10 ) THEN
!
!     ACCELERATION REQUEST
!
      IF ( Rfno/=8 .AND. Rfno/=9 ) GOTO 100
      IF ( rec(2)/=none ) Buf(Ireq) = 1
      iloc = 10
      GOTO 500
   ELSEIF ( i==11 ) THEN
!
!     ENERGY REQUEST
!
      iloc = -1
      GOTO 500
   ELSEIF ( i==12 ) THEN
!
!     UIMPROVED REQUEST
!
      Uimpro = 1
      GOTO 100
   ELSEIF ( i==13 ) THEN
!
!     STEPS REQUEST
!
      iloc = 11
      GOTO 500
   ELSE
!
!     DISP REQUEST
!
      IF ( rec(2)/=none ) Buf(Ireq) = 1
      iloc = 2
      GOTO 500
   ENDIF
!
!     BASIC COMMAND - VERIFY SUBSTRUCTURE NAME
!
 300  DO i = 1 , nbs
      i1 = Ireq + (i-1)*Lbasic + 5
      IF ( Buf(i1)==rec(2) .AND. Buf(i1+1)==rec(3) ) GOTO 400
   ENDDO
!
!     NAME NOT A BASIC - SKIP TO NEXT BASIC, PRINT OR SAVE COMMAND
!
   WRITE (Nout,99005) Uwm , rec(2) , rec(3) , Rss
99005 FORMAT (A25,' 6368, THE SUBSTRUCTURE ',2A4,' APPEARING ON A ','BASIC COMMAND IS NOT A COMPONENT OF ',2A4,/32X,                &
             &'ALL OUTPUT REQUESTS UNTIL THE NEXT BASIC, PRINT OR SAVE ','COMMAND ARE IGNORED.')
   DO
      CALL read(*1300,*700,casess,rec,3,0,nwds)
      IF ( rec(1)==print .OR. rec(1)==save ) GOTO 800
      IF ( rec(1)==comds(8) ) GOTO 300
   ENDDO
 400  nss1 = i
   nss2 = i
   basic = .TRUE.
   GOTO 100
!
!     CHECK VALIDITY OF SET REQUEST
!
 500  IF ( rec(2)==-2 ) THEN
      WRITE (Nout,99003) Uwm , rec(1)
      GOTO 100
   ENDIF
 600  iset = 1
   IF ( rec(2)==all ) iset = -1
   IF ( rec(2)==none ) iset = 0
   IF ( iset>0 ) THEN
      IF ( rec(2)==-2 ) THEN
!
!     REAL VALUE
!
         rset = rrec(3)
      ELSEIF ( rec(2)/=-1 ) THEN
         WRITE (Nout,99003) Uwm , rec(1)
         GOTO 100
      ELSE
!
!     INTEGER VALUE
!
         iset = rec(3)
      ENDIF
   ENDIF
!
!     LOOP OVER APPROPRIATE BASIC AREA AND INSERT REQUEST
!
   IF ( iloc<0 ) THEN
!
      Energy = iset
   ELSE
      DO i = nss1 , nss2
         i1 = Ireq + (i-1)*Lbasic + 5 + iloc
         Buf(i1) = iset
      ENDDO
   ENDIF
   GOTO 100
!
!
!     END OF RECORD READING CASESS - THIS IS THEREFORE THE LAST
!     SAVE OR PRINT COMMAND
!
 700  Loop = -1
!
!     END OF PROCESSING FO THIS PRINT COMMAND
!
 800  CALL close(casess,Rew)
!
!     DETERMINE IF EACH BASIC IS REALLY A BASIC.  IF NOT THEN THESE
!     WILL BE MODAL POINTS
!
!     BASIC   POINT TYPE = 1
!     MODAL   POINT TYPE = 4
!
   maskll = lshift(1023,20)
   DO i = 1 , nbs
      i1 = Ireq + (i-1)*Lbasic + 5
      Buf(i1+12) = 1
      CALL fdsub(Buf(i1),idit)
      IF ( idit>=0 ) THEN
         CALL fmdi(idit,imdi)
         IF ( andf(Buf(imdi+ll),maskll)/=0 ) Buf(i1+12) = 4
      ENDIF
   ENDDO
   CALL sofcls
   RETURN
!
!
!     NO PRINT OR SAVE COMMAND SPECIFIED - GENERATE A SAVE ON
!     THE SOLUTION SUBSTRUCTURE
!
 900  Rss(1) = Fss(1)
   Rss(2) = Fss(2)
   Loop = -1
   GOTO 1100
 1000 Loop = -1
!
!     NO OUTPUT BLOCK IS REQUIRED FOR A SAVE COMMAND
!
 1100 CALL close(casess,Rew)
   Ireq = 0
   Lreq = 0
   Iopt = 0
   Energy = 0
   Uimpro = 0
   RETURN
!
!     ERROR RETURNS
!
 1200 CALL sofcls
   Iopt = -1
   Loop = -1
   CALL close(casess,Rew)
   RETURN
 1300 n = -2
 1400 CALL sofcls
   CALL mesage(n,casess,subnam)
   RETURN
END SUBROUTINE rcovo