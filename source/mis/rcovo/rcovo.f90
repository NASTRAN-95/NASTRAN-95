!*==rcovo.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovo
   USE c_blank
   USE c_names
   USE c_rcovcm
   USE c_rcovcr
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         sof1 = 1
         sof2 = sof1 + sysbuf
         sof3 = sof2 + sysbuf + 1
         buf1 = sof3 + sysbuf
         icore = buf1 + sysbuf
         lcore = korsz(z(1)) - icore + 1
         IF ( lcore<=0 ) THEN
            n = -8
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     FIND RECOVER RECORD IN CASESS
!
            CALL gopen(casess,z(buf1),rdrew)
            IF ( step/=1 ) THEN
               DO i = 2 , step
                  CALL fwdrec(*80,casess)
               ENDDO
            ENDIF
            CALL fread(casess,rec,2,0)
            IF ( rec(1)/=recovr .AND. rec(1)/=mrecov ) THEN
!
               WRITE (nout,99001) swm , step , rec(1)
!
!     FORMATS
!
99001          FORMAT (A27,' 6305, RECORD NUMBER',I5,' IS NOT A RECOVER RECORD.','  IT IS A ',A4,' RECORD.')
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               mrecvr = .FALSE.
               IF ( rec(1)==mrecov ) mrecvr = .TRUE.
!
!     GET PRINT OR SAVE OPTION FOR THIS PASS
!
               i = 0
               SPAG_Loop_1_1: DO
                  CALL read(*80,*40,casess,rec,3,0,nwds)
                  IF ( rec(1)==print .OR. rec(1)==save ) THEN
                     IF ( loop==i ) THEN
!
!     GET NAME OF SUBSTRUCTURE TO BE OPERATED ON
!
                        rss(1) = rec(2)
                        rss(2) = rec(3)
                        loop = loop + 1
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
                           iopt = 1
!
!     OPEN SOF AND FETCH EQSS FOR SUBSTRUCTURE TO BE PRINTED
!
!
                           CALL sofopn(z(sof1),z(sof2),z(sof3))
                           CALL sfetch(rss,eqss,srd,rc)
                           IF ( rc==1 ) THEN
!
!     READ GROUP 0 OF EQSS INTO CORE
!
                              CALL suread(z(icore),lcore,nwds,rc)
                              IF ( rc==1 ) THEN
                                 n = -8
                                 spag_nextblock_1 = 8
                                 CYCLE SPAG_DispatchLoop_1
                              ELSEIF ( rc==2 ) THEN
!
!     DETERMINE SIZE OF OUTPUT REQUEST BLOCK AND ALLOCATE SPACE
!     AT BOTTOM OF OPEN CORE
!
                                 nbs = z(icore+2)
                                 np = z(icore+3)
                                 lbasic = 13
                                 lreq = 5 + lbasic*nbs + 2
                                 IF ( lreq>lcore-nwds ) THEN
                                    n = -8
                                    spag_nextblock_1 = 8
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSE
                                    nreq = korsz(buf(1))
                                    ireq = nreq - lreq + 1
                                    DO i = ireq , nreq
                                       buf(i) = 0
                                    ENDDO
!
!     MOVE NAMES OF BASICS INTO OUTPUT AREA
!
                                    buf(ireq+3) = np
                                    buf(ireq+4) = nbs
                                    DO i = 1 , nbs
                                       i1 = ireq + (i-1)*lbasic + 5
                                       i2 = icore + (i-1)*2 + 4
                                       buf(i1) = z(i2)
                                       buf(i1+1) = z(i2+1)
                                    ENDDO
!
!     INSERT DEFAULTS INTO OUTPUT BLOCK
!
!     MODES    = ALL
!     SUBCASES = ALL
!     RANGE    = -1.0E+35,1.0E+35
!     STEPS    = ALL
!
                                    energy = 0
                                    uimpro = 0
                                    rang(1) = -1.0E+35
                                    rang(2) = 1.0E+35
                                    buf(ireq) = -2
                                    buf(ireq+1) = -2
                                    buf(ireq+2) = -2
                                    DO i = 1 , nbs
                                       i1 = ireq + (i-1)*lbasic + 5
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
                                 CALL smsg(7,eqss,rss)
                                 spag_nextblock_1 = 7
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                           ELSEIF ( rc==4 ) THEN
                              WRITE (nout,99002) uwm , rss
99002                         FORMAT (A25,' 6306, ATTEMPT TO RECOVER DISPLACEMENTS FOR NON-','EXISTANT SUBSTRUCTURE ',2A4)
                              spag_nextblock_1 = 7
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
!
!     FETCH ON EQSS WAS UNSUCCESSFUL
!
                              IF ( rc==2 ) rc = 3
                              CALL smsg(rc-2,eqss,rss)
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
            IF ( rec(2)/=none ) buf(ireq+1) = 1
            IF ( rec(2)==none .AND. .NOT.basic ) buf(ireq+1) = 0
            iloc = 3
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==3 ) THEN
!
!     SPCF REQUEST
!
            IF ( rec(2)/=none ) buf(ireq+2) = 1
            IF ( rec(2)==none .AND. .NOT.basic ) buf(ireq+2) = 0
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
               WRITE (nout,99005) uwm , rec(1)
               spag_nextblock_1 = 2
            ELSE
               IF ( .NOT.(basic) ) rang(iloc-6) = rrec(3)
               spag_nextblock_1 = 4
            ENDIF
            CYCLE
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
               WRITE (nout,99003) uwm
99003          FORMAT (A25,' 6366, THE RECOVER OUTPUT COMMAND SORT MUST APPEAR ','BEFORE THE FIRST BASIC SUBCOMMAND.',/32X,         &
                      &'ANY OTHER SORT COMMANDS ARE IGNORED.')
            ELSE
               i = 0
               IF ( rec(2)==subc ) i = 1
               IF ( rec(2)==subs ) i = 2
               IF ( rec(2)==mode ) i = 1
               IF ( rec(2)==time ) i = 1
               IF ( rec(2)==freq ) i = 1
               IF ( i==0 ) THEN
                  WRITE (nout,99005) uwm , rec(1)
               ELSE
                  iopt = i
               ENDIF
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==8 ) THEN
         ELSEIF ( i==9 ) THEN
!
!     VELOCITY REQUEST
!
            IF ( rfno/=8 .AND. rfno/=9 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( rec(2)/=none ) buf(ireq) = 1
            iloc = 9
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( i==10 ) THEN
!
!     ACCELERATION REQUEST
!
            IF ( rfno/=8 .AND. rfno/=9 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( rec(2)/=none ) buf(ireq) = 1
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
            uimpro = 1
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
            IF ( rec(2)/=none ) buf(ireq) = 1
            iloc = 2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_3: DO
!
!     BASIC COMMAND - VERIFY SUBSTRUCTURE NAME
!
            DO i = 1 , nbs
               i1 = ireq + (i-1)*lbasic + 5
               IF ( buf(i1)==rec(2) .AND. buf(i1+1)==rec(3) ) EXIT SPAG_Loop_1_3
            ENDDO
!
!     NAME NOT A BASIC - SKIP TO NEXT BASIC, PRINT OR SAVE COMMAND
!
            WRITE (nout,99004) uwm , rec(2) , rec(3) , rss
99004       FORMAT (A25,' 6368, THE SUBSTRUCTURE ',2A4,' APPEARING ON A ','BASIC COMMAND IS NOT A COMPONENT OF ',2A4,/32X,          &
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
      CASE (3)
!
!     CHECK VALIDITY OF SET REQUEST
!
         IF ( rec(2)==-2 ) THEN
            WRITE (nout,99005) uwm , rec(1)
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
               WRITE (nout,99005) uwm , rec(1)
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
            energy = iset
         ELSE
            DO i = nss1 , nss2
               i1 = ireq + (i-1)*lbasic + 5 + iloc
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
 20      loop = -1
         spag_nextblock_1 = 5
      CASE (5)
!
!     END OF PROCESSING FO THIS PRINT COMMAND
!
         CALL close(casess,rew)
!
!     DETERMINE IF EACH BASIC IS REALLY A BASIC.  IF NOT THEN THESE
!     WILL BE MODAL POINTS
!
!     BASIC   POINT TYPE = 1
!     MODAL   POINT TYPE = 4
!
         maskll = lshift(1023,20)
         DO i = 1 , nbs
            i1 = ireq + (i-1)*lbasic + 5
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
 40      rss(1) = fss(1)
         rss(2) = fss(2)
         loop = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 60      loop = -1
         spag_nextblock_1 = 6
      CASE (6)
!
!     NO OUTPUT BLOCK IS REQUIRED FOR A SAVE COMMAND
!
         CALL close(casess,rew)
         ireq = 0
         lreq = 0
         iopt = 0
         energy = 0
         uimpro = 0
         RETURN
      CASE (7)
!
!     ERROR RETURNS
!
         CALL sofcls
         iopt = -1
         loop = -1
         CALL close(casess,rew)
         RETURN
 80      n = -2
         spag_nextblock_1 = 8
      CASE (8)
         CALL sofcls
         CALL mesage(n,casess,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (A25,' 6367, ILLEGAL FORMAT ON THE RECOVER OUTPUT COMMAND',1X,A4,', COMMAND IGNORED.')
END SUBROUTINE rcovo
