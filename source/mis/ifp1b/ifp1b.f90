!*==ifp1b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1b
   IMPLICIT NONE
   USE C_IFP1A
   USE C_IFPX0
   USE C_SYSTEM
   USE C_TWO
   USE C_XIFP1
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: case , cc , optp , ss
   INTEGER , DIMENSION(2) :: core
   LOGICAL , SAVE :: debug , new
   INTEGER :: i , ib1 , ib11 , ib2 , ib21 , ibuf1 , ibuf2 , icrq , iecase , ieoptp , ifcase , ifirod , ifirst , iflag , ifoptp ,    &
            & ifreq , ifreq1 , ihop , ik1 , ik11 , ik2 , ik21 , iloop , im1 , im11 , im2 , im21 , impc , impc1 , imtd , imtd1 , in ,&
            & ioloop , iopn , ip1 , ip2 , ipcase , ipoptp , iqcase , iqoptp , ispc , ispc1 , itfl , itfl1 , ix , iy , j , jump , k ,&
            & l , leng1 , leng2 , lsym , m , m0 , m1 , m2 , mm , nz
   INTEGER , DIMENSION(200) , SAVE :: ibit , iword
   INTEGER , DIMENSION(200,2) :: icase
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL andf , close , fwdrec , korsz , mesage , open , orf , read , rewind , skpfil
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS ROUTINE DETERMINES THE LOOP CONDITIONS AND CASE CONTROL
!     REQUEST CHNGES
!     LOOP$ -- THE CURRENT PROBLEM WILL LOOP
!
!     LOOP1$-- THE OLD PROBLEM WAS A LOOP AND CASE CONTROL IS CHANGED
!     IN LENGTH
!
!     COMMENTS FROM G.C.  10/92
!     IWORD AND IBIT 200 WORDS EACH CORRESPOND TO 200 WORDS IN CASECC
!     ZERO IN IWORD MEANS NO FURTHER CHECKING
!     INTEGER VALUE IN IBIT POINTS TO RESTART BIT POSITION, AND WILL BE
!     SAVED IN BITS(17) AND BITS(18), BITS FOR LCC. (LBD = 16)
!
!     LAST REVISED  7/91, BY G.CHAN/UNISYS, TO ALLOW HUGE THRU-RANGE ON
!     SET IN CASE CONTROL SECTION FOR PRINTOUT OR PLOTTING
!
   !>>>>EQUIVALENCE (Corex(1),Corey(1),Icase(1,1)) , (Core(1),Corey(401))
   DATA name/4HIFP1 , 4HB   /
   DATA case , cc/4HCASE , 4HCC  /
   DATA ss/4HSS  /
   DATA optp/4HOPTP/
   DATA iword/ - 1 , 01 , 01 , 01 , 01 , 01 , 01 , 01 , 01 , 00 , 01 , 01 , 01 , 01 , 01 , -1 , 00 , 01 , 01 , 00 , 01 , 01 , 00 ,  &
      & 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 01 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 00 , -1 , -1 , 01 , 01 , 01 , 01 , 01 , 01 , 01 , 00 , 00 , -1 , 01 ,&
      & 01 , -1 , 00 , 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 01 , -1 , -1 , 01 , 01 , 01 , -1 , 00 , 01 , 01 , 00 , 01 , 01 , 00 ,&
      & 01 , 01 , 00 , 01 , 01 , 01 , 00 , 01 , 01 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1/
   DATA ibit/00 , 02 , 03 , 04 , 05 , 06 , 07 , 08 , 09 , 10 , 10 , 10 , 13 , 14 , 15 , 00 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 ,&
      & 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 17 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 16 , 00 , 00 , 20 , 21 , 21 , 22 , 22 , 23 , 23 , 18 , 18 , 00 , 24 , 25 ,&
      & 00 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 27 , 00 , 00 , 30 , 26 , 29 , 00 , 18 , 18 , 18 , 18 , 18 , 18 , 10 , 10 ,&
      & 10 , 10 , 10 , 10 , 33 , 18 , 18 , 18 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00/
   DATA new , debug/2*.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         k = Lbd + 1
         ifirod = 0
         ioloop = 0
         iloop = 0
         ifirst = 0
         ieoptp = 0
!
!     ALLOCATE GINO BUFFERS
!
         nz = korsz(core)
         ibuf1 = nz - Ibuf + 1
         ibuf2 = ibuf1 - Ibuf
         nz = nz - 2*Ibuf
         icrq = -nz
         IF ( nz<=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iecase = 0
!
!     TRY TO FIND CASECC ON OPTP - TRY TO ASSUME PROPER POSITION
!
         CALL open(*140,optp,core(ibuf1),2)
         iopn = 0
         SPAG_Loop_1_1: DO
!
!     FIND CASECC
!
            CALL read(*160,*180,optp,core(1),2,1,iflag)
            IF ( core(1)==case .AND. core(2)==cc ) THEN
!
!     CASECC FOUND ON OLD PROB TAPE
!
!     OPEN CASECC AND SKIP CASESS IF PRESENT
!
               CALL open(*200,Casecc,core(ibuf2),0)
               EXIT SPAG_Loop_1_1
            ELSEIF ( core(1)==case .AND. core(2)==ss ) THEN
               DO
!
!     CASESS FOUND ON OPTP - SKIP TO CASECC
!
                  CALL read(*160,*180,optp,core(1),2,1,iflag)
                  IF ( core(1)==case .AND. core(2)==cc ) THEN
                     CALL open(*200,Casecc,core(ibuf2),0)
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ENDDO
            ELSE
               IF ( iopn==0 ) CALL rewind(optp)
               iopn = 1
               CALL skpfil(optp,1)
            ENDIF
         ENDDO SPAG_Loop_1_1
         SPAG_Loop_1_2: DO
            CALL read(*220,*240,Casecc,core(1),2,1,iflag)
            IF ( core(1)==case .AND. core(2)==cc ) THEN
               ASSIGN 20 TO ihop
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
 20      DO
            CALL read(*120,*180,optp,icase(1,2),Lencc,0,iflag)
            IF ( icase(16,2)==0 ) THEN
               IF ( icase(Lencc,2)/=0 ) THEN
                  lsym = icase(Lencc,2)
                  CALL read(*160,*180,optp,core(1),-lsym,0,iflag)
               ENDIF
               CALL read(*160,*40,optp,core(1),nz,1,ifoptp)
               icrq = nz
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL fwdrec(*160,optp)
            ENDIF
         ENDDO
 40      DO
            CALL read(*80,*240,Casecc,icase(1,1),Lencc,0,iflag)
            IF ( icase(16,1)==0 ) THEN
               IF ( icase(Lencc,1)/=0 ) THEN
                  lsym = icase(Lencc,1)
                  CALL read(*220,*240,Casecc,core(ifoptp+1),-lsym,0,iflag)
               ENDIF
               CALL read(*220,*60,Casecc,core(ifoptp+1),nz-ifoptp,1,ifcase)
               icrq = nz - ifoptp
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL fwdrec(*220,Casecc)
            ENDIF
         ENDDO
!
!     CHECK FOR LOOPING PROBLEM
!
 60      IF ( ifirst==0 ) THEN
            ifirst = 1
            ispc = icase(3,1)
            impc = icase(2,1)
            imtd = icase(5,1)
            ifreq = icase(14,1)
            itfl = icase(15,1)
            ik1 = icase(139,1)
            ik2 = icase(140,1)
            im1 = icase(141,1)
            im2 = icase(142,1)
            ib1 = icase(143,1)
            ib2 = icase(144,1)
            IF ( icase(165,1)<=0 .AND. icase(164,1)<=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( icase(3,1)==ispc ) THEN
            IF ( icase(2,1)==impc ) THEN
               IF ( icase(5,1)==imtd ) THEN
                  IF ( icase(139,1)==ik1 .AND. icase(140,1)==ik2 ) THEN
                     IF ( icase(141,1)==im1 .AND. icase(142,1)==im2 ) THEN
                        IF ( icase(143,1)==ib1 .AND. icase(144,1)==ib2 ) THEN
                           IF ( icase(15,1)==itfl ) THEN
                              IF ( icase(14,1)==ifreq ) THEN
                                 IF ( icase(138,1)<=0 ) THEN
                                    IF ( icase(38,1)==0 ) THEN
                                       spag_nextblock_1 = 2
                                       CYCLE SPAG_DispatchLoop_1
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
!     SET LOOP$
!
         Bits(k) = orf(Bits(k),Two1(11))
         iloop = 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     DETERMINE IF OLD PROBLEM WOULD HAVE LOOPED
!
         IF ( ifirod==0 ) THEN
            ifirod = 1
            ispc1 = icase(3,2)
            impc1 = icase(2,2)
            imtd1 = icase(5,2)
            ik11 = icase(139,2)
            ik21 = icase(140,2)
            im11 = icase(141,2)
            im21 = icase(142,2)
            ib11 = icase(143,2)
            ib21 = icase(144,2)
            itfl1 = icase(15,2)
            ifreq1 = icase(14,2)
            IF ( icase(164,2)>0 .OR. icase(165,2)>0 ) ioloop = 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     SECOND RECORD APPLY LOOP RULES
!
         IF ( icase(3,2)/=ispc1 ) THEN
            ioloop = 1
         ELSEIF ( icase(2,2)/=impc1 ) THEN
            ioloop = 1
         ELSEIF ( icase(5,2)/=imtd1 ) THEN
            ioloop = 1
         ELSEIF ( icase(139,2)/=ik11 .OR. icase(140,2)/=ik21 ) THEN
            ioloop = 1
         ELSEIF ( icase(141,2)/=im11 .OR. icase(142,2)/=im21 ) THEN
            ioloop = 1
         ELSEIF ( icase(143,2)/=ib11 .OR. icase(144,2)/=ib21 ) THEN
            ioloop = 1
         ELSEIF ( icase(138,2)>0 ) THEN
            ioloop = 1
         ELSEIF ( icase(38,2)/=0 ) THEN
            ioloop = 1
         ELSEIF ( icase(15,2)/=itfl1 ) THEN
            ioloop = 1
         ELSEIF ( icase(14,2)/=ifreq1 ) THEN
            ioloop = 1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( iecase/=1 ) THEN
!
!     CHECK FOR CHANGES -
!
            IF ( ieoptp==1 ) ieoptp = 2
            DO i = 1 , Lencc
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( ibit(i)==0 ) CYCLE
                     l = ibit(i)
                     IF ( l<=32 .AND. andf(Bits(k),Two1(l))/=0 ) CYCLE
                     IF ( iword(i)==0 ) THEN
!
!     CHECK FOR PRESENCE OF PRINT AND PLOT REQUESTS
!
                        IF ( i/=135 .AND. icase(i,1)==0 ) CYCLE
                        IF ( i/=135 ) THEN
                           IF ( ibit(i)==18 ) Bits(k+1) = orf(Bits(k+1),Two1(3))
                           IF ( ibit(i)==10 ) Bits(k+1) = orf(Bits(k+1),Two1(4))
                           IF ( ieoptp/=2 ) THEN
                              IF ( icase(i,1)<0 .AND. icase(i,2)<0 ) CYCLE
                              IF ( icase(i,1)>=0 .OR. icase(i,2)<0 ) THEN
                                 IF ( icase(i,1)<=0 .OR. icase(i,2)>0 ) THEN
                                    ipcase = ifoptp + 1
                                    DO WHILE ( ipcase<=ifoptp+ifcase )
                                       IF ( core(ipcase)==icase(i,1) ) THEN
                                         ipoptp = 1
                                         DO WHILE ( ipoptp<=ifoptp )
                                         IF ( core(ipoptp)==icase(i,2) ) THEN
                                         spag_nextblock_2 = 3
                                         CYCLE SPAG_DispatchLoop_2
                                         ENDIF
                                         ipoptp = ipoptp + core(ipoptp+1) + 2
                                         ENDDO
                                         spag_nextblock_1 = 7
                                         CYCLE SPAG_DispatchLoop_1
                                       ELSE
                                         ipcase = ipcase + core(ipcase+1) + 2
                                       ENDIF
                                    ENDDO
                                    spag_nextblock_1 = 10
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                           ENDIF
                        ELSEIF ( icase(i,1)==0 .AND. icase(i,2)==0 ) THEN
                           CYCLE
                        ENDIF
                     ELSEIF ( icase(i,1)==icase(i,2) ) THEN
                        CYCLE
                     ENDIF
                     spag_nextblock_2 = 2
                  CASE (2)
                     IF ( l>32 ) THEN
!
!     SECOND CASECC WORD
!
                        l = l - 31
                        Bits(k+1) = orf(Bits(k+1),Two1(l))
                     ELSE
                        Bits(k) = orf(Bits(k),Two1(l))
                     ENDIF
                     CYCLE
                  CASE (3)
                     iqcase = ifoptp + ifcase + 1
                     ix = ipcase
                     iy = iqcase
                     ASSIGN 62 TO jump
                     IF ( debug ) WRITE (Nout,99001)
99001                FORMAT (/,' ------ NPTP PASS ------')
                     spag_nextblock_2 = 5
                     CYCLE SPAG_DispatchLoop_2
 62                  iqoptp = iy
                     ix = ipoptp
                     ASSIGN 64 TO jump
                     IF ( debug ) WRITE (Nout,99002)
99002                FORMAT (/,' ------ OPTP PASS ------')
                     spag_nextblock_2 = 5
                     CYCLE SPAG_DispatchLoop_2
 64                  leng1 = iqoptp - iqcase
                     leng2 = iy - iqoptp
                     IF ( debug ) WRITE (Nout,99003) core(ipcase) , leng1 , leng2 , iy , iqoptp , iqcase
99003                FORMAT (//,' IFP1B/@310  CHECKING SETS',I9,' FROM NPTP AND OPTP',/5X,'LENG1,LENG2, IY,IQOPTP,IQCASE =',2I5,3I7)
                     IF ( leng1==leng2 ) THEN
                        DO mm = 1 , leng1
                           IF ( core(iqcase+mm-1)/=core(iqoptp+mm-1) ) THEN
                              spag_nextblock_2 = 4
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                        IF ( debug ) WRITE (Nout,99004) core(ipcase)
99004                   FORMAT (' ... NO DIFFERENCES IN SET',I8)
                        CYCLE
                     ENDIF
                     spag_nextblock_2 = 4
                  CASE (4)
                     WRITE (Nout,99005) Uim , core(ipcase)
99005                FORMAT (A29,', SET',I9,' DEFINITION HAS BEEN CHANGED IN RESTART')
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  CASE (5)
!
!     A NEW NON-EXPANDING METHOD IS IMPLEMENTED HERE BY  G.CAHN/UNISYS
!     8/91, IN CASE THE ORIGINAL LOGIC RUNS OUT OF CORE SPACE
!
!     THE NEW METHOD WILL CONCATINATE VARIATIONS OF SET DEFINITION TO
!     THE SIMPLEST FORM.  E.G. THE NEXT 3 LINES SPECIFY THE SAME SET
!     10 THRU 400000  (THIS IS THE SIMPLEST FORM)
!     10, 11, 12 THRU 400008, 400009, 400000
!     10 THRU 20, 21, 22, 23 THRU 200, 201 THRU 500, 501 502 THRU 400000
!
                     IF ( new ) THEN
!
!     NEW LOGIC WITHOUT THRU RANGE EXPANSION
!
                        in = core(ix+1)
                        ix = ix + 2
                        m0 = iy
                        core(iy) = core(ix)
                        iy = iy + 1
                        IF ( in==1 ) THEN
                           spag_nextblock_2 = 7
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        core(iy) = core(ix+1)
                        IF ( core(iy)==core(iy-1)+1 ) core(iy) = -core(iy)
                        iy = iy + 1
                        m = 1
                        DO
                           m = m + 1
                           IF ( m>=in ) THEN
                              icrq = iy - nz
                              IF ( iy>nz ) THEN
                                 spag_nextblock_1 = 11
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              m1 = iy - 1
                              IF ( debug ) WRITE (Nout,99006) core(ix-2) , (core(j),j=m0,m1)
99006                         FORMAT (/,' IFP1B/@480    SET',I8,/,(2X,15I8))
                              spag_nextblock_2 = 7
                              CYCLE SPAG_DispatchLoop_2
                           ELSE
                              m1 = core(ix+m)
                              m2 = iabs(m1)
                              IF ( debug ) WRITE (Nout,99007) m , in , ix , iy , m1 , core(iy-1)
99007                         FORMAT (' @440   M,IN,IX,IY,M1,CORE(IY-1) =',6I8)
                              IF ( m1<0 ) THEN
                                 IF ( core(iy-1)<=0 ) THEN
                                    core(iy-1) = -m2
                                    CYCLE
                                 ENDIF
                              ELSEIF ( m1==1-core(iy-1) ) THEN
                                 core(iy-1) = -m2
                                 CYCLE
                              ENDIF
                              core(iy) = m1
                              IF ( m1==core(iy-1)+1 ) core(iy) = -m2
                              iy = iy + 1
                           ENDIF
                        ENDDO
                     ELSE
                        in = core(ix+1)
                        ix = ix + 2
                        m = 0
                     ENDIF
                     spag_nextblock_2 = 6
                  CASE (6)
                     SPAG_Loop_2_3: DO
                        m = m + 1
                        IF ( m<in ) THEN
                           IF ( core(ix+m)>0 ) EXIT SPAG_Loop_2_3
                           m1 = core(ix+m-1)
                           m2 = -core(ix+m)
                           icrq = iy + m2 - m1 - nz
                           IF ( icrq>0 ) THEN
!
!     INSUFFICIENT CORE SPACE, SWITCH TO NEW METHOD
!
                              new = .TRUE.
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           ELSE
                              DO mm = m1 , m2
                                 core(iy) = mm
                                 iy = iy + 1
                              ENDDO
                              m = m + 1
                           ENDIF
                        ELSEIF ( m==in ) THEN
                           EXIT SPAG_Loop_2_3
                        ELSE
                           spag_nextblock_2 = 7
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO SPAG_Loop_2_3
                     icrq = iy - nz
                     IF ( iy>nz ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     core(iy) = core(ix+m-1)
                     iy = iy + 1
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  CASE (7)
!
                     GOTO jump
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
            ENDDO
            GOTO ihop
         ELSE
            IF ( ioloop==1 ) GOTO 100
            DO
               CALL read(*100,*180,optp,icase(1,2),Lencc,1,iflag)
               IF ( icase(16,2)==0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     EOF ON CASECC
!
 80      CALL close(Casecc,1)
         IF ( ieoptp==0 ) THEN
            iecase = 1
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 100     CALL close(optp,2)
!
!     SET LOOP1  THIS SHOULD REEXECUTE THE ENTIRE LOOP
!
         IF ( ieoptp/=1 .AND. ioloop/=0 ) Bits(k) = orf(Bits(k),Two1(12))
!
!     CHECK FOR LOOP$ IF NOT ON SET NOLOOP$
!
         IF ( iloop==0 ) Bits(k) = orf(Bits(k),Two1(32))
         RETURN
!
!     EOF ON  OPTP
!
 120     ASSIGN 40 TO ihop
         ieoptp = 1
         GOTO 40
!
!     ERROR MESSAGES
!
 140     ip1 = -1
         spag_nextblock_1 = 5
      CASE (5)
         ip2 = optp
         spag_nextblock_1 = 6
      CASE (6)
         CALL mesage(ip1,ip2,name)
         RETURN
!
 160     ip1 = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 180     ip1 = -3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         core(1) = optp
         core(2) = Iblank
         spag_nextblock_1 = 8
      CASE (8)
         WRITE (Nout,99008) Sfm , core(1) , core(2)
99008    FORMAT (A25,' 651, LOGIC ERROR IN SUBROUTINE IFP1B WHILE ','PROCESSING SET DATA ON ',2A4,' FILE.')
         ip1 = -37
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 200     ip1 = -1
         spag_nextblock_1 = 9
      CASE (9)
         ip2 = Casecc
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 220     ip1 = -2
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 240     ip1 = -3
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         core(1) = case
         core(2) = cc
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         ip1 = -8
         ip2 = icrq
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifp1b
