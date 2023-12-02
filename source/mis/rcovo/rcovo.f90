!*==rcovo.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovo
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , casess , eqss , freq , ll , mode , mrecov , ncomds , none , print , recovr , save , srd , subc , subs ,  &
                   & time
   LOGICAL :: basic
   INTEGER , DIMENSION(1) :: buf
   INTEGER , DIMENSION(13) , SAVE :: comds
   INTEGER :: i , i1 , i2 , idit , iloc , imdi , irange , iset , maskll , n , nbs , np , nreq , nss1 , nss2 , nwds , rc
   REAL , DIMENSION(1) :: rbuf
   INTEGER , DIMENSION(3) :: rec
   REAL , DIMENSION(3) :: rrec
   REAL :: rset
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL andf , close , fdsub , fmdi , fread , fwdrec , gopen , korsz , lshift , mesage , read , sfetch , smsg , sofcls ,        &
          & sofopn , suread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     FIND RECOVER RECORD IN CASESS
!
            CALL gopen(casess,Z(Buf1),Rdrew)
            IF ( Step/=1 ) THEN
               DO i = 2 , Step
                  CALL fwdrec(*80,casess)
               ENDDO
            ENDIF
            CALL fread(casess,rec,2,0)
            IF ( rec(1)/=recovr .AND. rec(1)/=mrecov ) THEN
!
               WRITE (Nout,99001) Swm , Step , rec(1)
!
!     FORMATS
!
99001          FORMAT (A27,' 6305, RECORD NUMBER',I5,' IS NOT A RECOVER RECORD.','  IT IS A ',A4,' RECORD.')
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Mrecvr = .FALSE.
               IF ( rec(1)==mrecov ) Mrecvr = .TRUE.
!
!     GET PRINT OR SAVE OPTION FOR THIS PASS
!
               i = 0
               SPAG_Loop_1_1: DO
                  CALL read(*80,*40,casess,rec,3,0,nwds)
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
                              CALL read(*80,*60,casess,rec,3,0,nwds)
                              IF ( rec(1)==print .OR. rec(1)==save ) THEN
                                 spag_nextblock_1 = 6
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
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
                                 spag_nextblock_1 = 8
                                 CYCLE SPAG_DispatchLoop_1
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
                                    spag_nextblock_1 = 8
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSE
                                    nreq = korsz(buf(1))
                                    Ireq = nreq - Lreq + 1
                                    DO i = Ireq , nreq
                                       buf(i) = 0
                                    ENDDO
!
!     MOVE NAMES OF BASICS INTO OUTPUT AREA
!
                                    buf(Ireq+3) = np
                                    buf(Ireq+4) = nbs
                                    DO i = 1 , nbs
                                       i1 = Ireq + (i-1)*Lbasic + 5
                                       i2 = Icore + (i-1)*2 + 4
                                       buf(i1) = Z(i2)
                                       buf(i1+1) = Z(i2+1)
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
                                    buf(Ireq) = -2
                                    buf(Ireq+1) = -2
                                    buf(Ireq+2) = -2
                                    DO i = 1 , nbs
                                       i1 = Ireq + (i-1)*Lbasic + 5
                                       buf(i1+2) = -2
                                       buf(i1+3) = -2
                                       buf(i1+4) = -2
                                       buf(i1+5) = -1
                                       buf(i1+6) = -1
                                       rbuf(i1+7) = -1.0E+35
                                       rbuf(i1+8) = 1.0E+35
                                       buf(i1+9) = -2
                                       buf(i1+10) = -2
                                       buf(i1+11) = -1
                                    ENDDO
!
!     READ NEXT COMMAND AND PROCESS OUTPUT REQUEST
!
                                    nss1 = 1
                                    nss2 = nbs
                                    irange = 0
                                    basic = .FALSE.
                                    EXIT SPAG_Loop_1_1
                                 ENDIF
                              ELSEIF ( rc==4 ) THEN
                                 spag_nextblock_1 = 7
                                 CYCLE SPAG_DispatchLoop_1
                              ELSE
                                 CALL smsg(7,eqss,Rss)
                                 spag_nextblock_1 = 7
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                           ELSEIF ( rc==4 ) THEN
                              WRITE (Nout,99002) Uwm , Rss
99002                         FORMAT (A25,' 6306, ATTEMPT TO RECOVER DISPLACEMENTS FOR NON-','EXISTANT SUBSTRUCTURE ',2A4)
                              spag_nextblock_1 = 7
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
!
!     FETCH ON EQSS WAS UNSUCCESSFUL
!
                              IF ( rc==2 ) rc = 3
                              CALL smsg(rc-2,eqss,Rss)
                              spag_nextblock_1 = 7
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                     ELSE
                        i = i + 1
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_2: DO
            CALL read(*80,*20,casess,rec,3,0,nwds)
            IF ( rec(1)==print .OR. rec(1)==save ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = 1 , ncomds
               IF ( rec(1)==comds(i) ) EXIT SPAG_Loop_1_2
            ENDDO
         ENDDO SPAG_Loop_1_2
         IF ( i==2 ) THEN
!
!     OLOAD REQUEST
!
            IF ( rec(2)/=none ) buf(Ireq+1) = 1
            IF ( rec(2)==none .AND. .NOT.basic ) buf(Ireq+1) = 0
            iloc = 3
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==3 ) THEN
!
!     SPCF REQUEST
!
            IF ( rec(2)/=none ) buf(Ireq+2) = 1
            IF ( rec(2)==none .AND. .NOT.basic ) buf(Ireq+2) = 0
            iloc = 4
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==4 ) THEN
!
!     MODES REQUEST
!
            iloc = 6
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( .NOT.(basic) ) Rang(iloc-6) = rrec(3)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( i==6 ) THEN
!
!     SUBCASES REQUEST
!
            iloc = 5
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==7 ) THEN
!
!     SORT COMMAND - IGNORE COMMAND IF AFTER A BASIC DESIGNATOR
!
            IF ( basic ) THEN
               WRITE (Nout,99004) Uwm
99004          FORMAT (A25,' 6366, THE RECOVER OUTPUT COMMAND SORT MUST APPEAR ','BEFORE THE FIRST BASIC SUBCOMMAND.',/32X,         &
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
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==8 ) THEN
         ELSEIF ( i==9 ) THEN
!
!     VELOCITY REQUEST
!
            IF ( Rfno/=8 .AND. Rfno/=9 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( rec(2)/=none ) buf(Ireq) = 1
            iloc = 9
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==10 ) THEN
!
!     ACCELERATION REQUEST
!
            IF ( Rfno/=8 .AND. Rfno/=9 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( rec(2)/=none ) buf(Ireq) = 1
            iloc = 10
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==11 ) THEN
!
!     ENERGY REQUEST
!
            iloc = -1
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==12 ) THEN
!
!     UIMPROVED REQUEST
!
            Uimpro = 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==13 ) THEN
!
!     STEPS REQUEST
!
            iloc = 11
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     DISP REQUEST
!
            IF ( rec(2)/=none ) buf(Ireq) = 1
            iloc = 2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_3: DO
!
!     BASIC COMMAND - VERIFY SUBSTRUCTURE NAME
!
            DO i = 1 , nbs
               i1 = Ireq + (i-1)*Lbasic + 5
               IF ( buf(i1)==rec(2) .AND. buf(i1+1)==rec(3) ) EXIT SPAG_Loop_1_3
            ENDDO
!
!     NAME NOT A BASIC - SKIP TO NEXT BASIC, PRINT OR SAVE COMMAND
!
            WRITE (Nout,99005) Uwm , rec(2) , rec(3) , Rss
99005       FORMAT (A25,' 6368, THE SUBSTRUCTURE ',2A4,' APPEARING ON A ','BASIC COMMAND IS NOT A COMPONENT OF ',2A4,/32X,          &
                   &'ALL OUTPUT REQUESTS UNTIL THE NEXT BASIC, PRINT OR SAVE ','COMMAND ARE IGNORED.')
            DO
               CALL read(*80,*20,casess,rec,3,0,nwds)
               IF ( rec(1)==print .OR. rec(1)==save ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( rec(1)==comds(8) ) CYCLE SPAG_Loop_1_3
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
         nss1 = i
         nss2 = i
         basic = .TRUE.
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     CHECK VALIDITY OF SET REQUEST
!
         IF ( rec(2)==-2 ) THEN
            WRITE (Nout,99003) Uwm , rec(1)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         iset = 1
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
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
               buf(i1) = iset
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!
!     END OF RECORD READING CASESS - THIS IS THEREFORE THE LAST
!     SAVE OR PRINT COMMAND
!
 20      Loop = -1
         spag_nextblock_1 = 5
      CASE (5)
!
!     END OF PROCESSING FO THIS PRINT COMMAND
!
         CALL close(casess,Rew)
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
            buf(i1+12) = 1
            CALL fdsub(buf(i1),idit)
            IF ( idit>=0 ) THEN
               CALL fmdi(idit,imdi)
               IF ( andf(buf(imdi+ll),maskll)/=0 ) buf(i1+12) = 4
            ENDIF
         ENDDO
         CALL sofcls
         RETURN
!
!
!     NO PRINT OR SAVE COMMAND SPECIFIED - GENERATE A SAVE ON
!     THE SOLUTION SUBSTRUCTURE
!
 40      Rss(1) = Fss(1)
         Rss(2) = Fss(2)
         Loop = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 60      Loop = -1
         spag_nextblock_1 = 6
      CASE (6)
!
!     NO OUTPUT BLOCK IS REQUIRED FOR A SAVE COMMAND
!
         CALL close(casess,Rew)
         Ireq = 0
         Lreq = 0
         Iopt = 0
         Energy = 0
         Uimpro = 0
         RETURN
      CASE (7)
!
!     ERROR RETURNS
!
         CALL sofcls
         Iopt = -1
         Loop = -1
         CALL close(casess,Rew)
         RETURN
 80      n = -2
         spag_nextblock_1 = 8
      CASE (8)
         CALL sofcls
         CALL mesage(n,casess,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99003 FORMAT (A25,' 6367, ILLEGAL FORMAT ON THE RECOVER OUTPUT COMMAND',1X,A4,', COMMAND IGNORED.')
END SUBROUTINE rcovo
