!*==curv1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curv1
   USE c_blank
   USE c_curvc1
   USE c_curvc2
   USE c_curvc3
   USE c_curvtb
   USE c_names
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(5,4) , SAVE :: elem
   INTEGER :: eor , icrq , idum , kid , noeor
   REAL :: flag
   INTEGER , DIMENSION(6) , SAVE :: mat
   REAL , DIMENSION(1) :: z
   EXTERNAL bckrec , bisloc , close , fwdrec , gopen , korsz , locate , open , preloc , pretrs , read , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!*****
!  INITIALIZATION OVERLAY. ALL LOGIC INDEPENDENT OF PROCESSING
!  THE SUBCASE DATA ON OES1 IS HANDLED IN THIS INITIALIZATION
!  ROUTINE OF THE -CURV- MODULE
!
!     OPEN CORE MAP DURING -CURV1- EXECUTION.
!     =======================================
!       INITIAL               AFTER CURV1 RETURNS
!     +-----------+             +----------------+
!     I Z(IELTYP) I             I  Z(IELTYP)     I
!     I    .      I             I     .          I
!     I  ELEMENT  I             I  REDUCED       I
!     I  TYPES    I             I  ELEMENT-TYPES I
!     I  BEING    I             I  LIST          I
!     I  PLACED   I             I     .          I
!     I  IN SCR1  I             I  Z(NELTYP)     I
!     I    .      I             +----------------+
!     I Z(NELTYP) I             I  Z(IMCSID)     I
!     +-----------+             I     .          I
!     I Z(IMID)   I             I  MCSID LIST    I
!     I    .      I             I  OF MCSIDS     I
!     I MATID-    I             I  ACTUALLY      I
!     I MCSID-    I             I  REFERENCED    I
!     I FLAG-     I             I     .          I
!     I ENTRIES   I             I  Z(NMCSID)     I
!     I    .      I             +----------------+
!     I Z(NMID)   I             I  Z(ICSTM)      I
!     +-----------+             I     .          I
!     I Z(ISIL)   I             I  CSTMS IN      I
!     I    .      I             I  EXISTENCE     I
!     I SILS IN   I             I  FOR MCSIDS    I
!     I INTERNAL  I             I  IN ABOVE      I
!     I SORT      I             I  TABLE         I
!     I    .      I             I     .          I
!     I Z(NSIL)   I             I  Z(NCSTM)      I
!     +-----------+             +----------------+
!     I Z(IEXT)   I             I     .          I
!     I    .      I             I  AVAILABLE     I
!     I EXTERNAL  I             I  CORE          I
!     I IDS IN    I             I     .          I
!     I INTERNAL  I             I     .          I
!     I SORT      I             I     .          I
!     I    .      I             I     .          I
!     I Z(NEXT)   I             I     .          I
!     +-----------+             I     .          I
!     I   .       I             I     .          I
!     I AVAILABLE I             I     .          I
!     I CORE      I             I     .          I
!     I   .       I             I     .          I
!     I Z(JCORE)  I             I  Z(JCORE)      I
!     +-----------+             +----------------+
!     I Z(IBUF4)  I             I  Z(IBUF4)      I
!     I Z(IBUF3)  I             I  Z(IBUF3)      I
!     I Z(IBUF2)  I             I  Z(IBUF2)      I
!     I Z(IBUF1)  I             I  Z(IBUF1)      I
!     I GINO-BUFS I             I  GINO-BUFS     I
!     I Z(LCORE)  I             I  Z(LCORE)      I
!     +-----------+             +----------------+
!
!*****
!
!
!
!
!
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Buf(1),Rbuf(1))
   !>>>>EQUIVALENCE (Noeor,Rdrew) , (Eor,Cls)
!
   DATA mat/103 , 1 , 12 , 203 , 2 , 17/
!
!        - - - - - - - - CURV-MODULE ELEMENTS DATA - - - - - - - -
!
!                   ELEMENT   EST       CONNECT.  MATID     BGPDT
!                   TYPE      WORDS     POINTS    INDEX     INDEX
!                   =======   =======   =======   =======   =======
!  TRIA1
!  TRIA2
!  QUAD1
!  QUAD2
   DATA elem/6 , 27 , 3 , 6 , 15 , 17 , 21 , 3 , 6 , 9 , 19 , 32 , 4 , 7 , 16 , 18 , 26 , 4 , 7 , 10/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!  IF EITHER OF THESE PARAMS IS EXCEEDED RESET AND RE-DIMENSION
!  SBUF OR BUF.
!
         lsbuf = 10
         lbuf = 100
         nelems = 4
         logerr = 37
!*****
!  INITIALIZATION OF CORE AND FLAGS
!*****
         foes1g = .TRUE.
         IF ( ip1>0 ) foes1g = .FALSE.
         any1m = .FALSE.
         any1g = .FALSE.
         lmcsid = 0
!
         lcore = korsz(iz(1))
         DO i = 1 , lcore
            iz(i) = 0
         ENDDO
         ibuf1 = lcore - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
!
!  SET FILE NUMBERS EXPLICITYLY.  ALL OVERLAYS REFERENCE /CURVTB/
!
         oes1 = 101
         mpt = 102
         cstm = 103
         est = 104
         sil = 105
         gpl = 106
         oes1m = 201
         oes1g = 202
         scr1 = 301
         scr2 = 302
         scr3 = 303
         scr4 = 304
         scr5 = 305
         jcore = ibuf4 - 1
         file = 0
         loc = 300
         icrq = -ibuf4
         IF ( ibuf4<=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!*****
!  ALLOCATE TABLE OF ELEMENT TYPES PLACED ON ESTX(SCR1).  MAXIMUM
!  SIZE NOW AND REDUCED LATER TO ACTUAL SIZE.
!*****
         ieltyp = 1
         jeltyp = ieltyp
         neltyp = nelems
!*****
!  CONSTRUCTION OF TABLE CONTAINING ENTRIES OF,
!
!     MID   = MATERIAL-ID
!     MCSID = MATERIAL-COORDINATE-SYSTEM-ID
!     FLAG  = REFERENCE-FLAG
!
!  ALL MAT1 AND MAT2 BULK DATA CARDS CONTAINING A NON-ZERO -MCSID-
!  RESULT IN AN ENTRY BEING ADDED TO THIS TABLE. TABLE IS THEN SORTED
!  ON -MID-.
!*****
         imid = neltyp + 1
         nmid = imid - 1
!
!  OPEN MPT USING -PRELOC- FUNCTION.
!
         file = mpt
         loc = 400
         CALL preloc(*200,iz(ibuf1),mpt)
!
!  PASS MAT1 AND MAT2 DATA IF ANY.
!
         DO i = 1 , 6 , 3
            iwords = mat(i+2)
            IF ( iwords>lbuf ) GOTO 180
            CALL locate(*20,iz(ibuf1),mat(i),idum)
            DO
               CALL read(*220,*20,mpt,buf(1),iwords,noeor,nwords)
               IF ( buf(iwords)>0 ) THEN
                  icrq = nmid + 3 - jcore
                  IF ( nmid+3>jcore ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  iz(nmid+1) = buf(1)
                  iz(nmid+2) = buf(iwords)
                  iz(nmid+3) = 0
                  nmid = nmid + 3
               ENDIF
            ENDDO
!
!  EOR HIT READING MAT1 OR MAT2 CARDS
!
 20      ENDDO
!
!  TABLE COMPLETE, THUS NOW SORT IT. IF TABLE IS EMPTY WE ARE THROUGH
!
         CALL close(mpt,clsrew)
         lmid = nmid - imid + 1
         nmids = lmid/3
         loc = 570
         IF ( lmid<0 ) GOTO 180
         IF ( lmid==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sort(0,0,3,1,iz(imid),lmid)
!*****
!  LOAD LIST OF SILS INTO CORE, FOLLOWED BY LIST OF EXTERNAL IDS.
!  THIS IS REQUIRED ONLY IF OES1G IS TO BE FORMED.
!*****
         IF ( .NOT.foes1g ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = sil
         loc = 580
         isil = nmid + 1
         CALL gopen(sil,iz(ibuf1),0)
         CALL read(*220,*40,sil,iz(isil),jcore-isil,noeor,lsil)
         icrq = jcore - isil
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
 40      nsil = isil + lsil - 1
         CALL close(sil,clsrew)
!
         file = gpl
         loc = 590
         iext = nsil + 1
         CALL gopen(gpl,iz(ibuf1),0)
         CALL read(*220,*60,gpl,iz(iext),jcore-iext,noeor,lext)
         icrq = jcore - iext
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
 60      next = iext + lext - 1
         CALL close(gpl,clsrew)
         IF ( lsil/=lext ) GOTO 180
         spag_nextblock_1 = 2
      CASE (2)
!*****
!  EST IS NOW READ. ANY ELEMENTS IN THE EST WHOSE MATERIAL ID REFERENCES
!  A MAT1 OR MAT2 ENTRY WHICH CONTAINS A NON-ZERO MATERIAL-COORDINATE-
!  SYSTEM-ID, WILL BE PLACED IN AN ABBREVIATED EST ON SCRATCH1.
!
!  FORMAT OF EACH ELEMENT TYPE RECORD.
!
!         ELEMENT TYPE NUMBER
!         NUMBER OF WORDS PER EACH OF THE FOLLOWING ENTRIES.
!         NUMBER OF POINTS PER THIS ELEMENT TYPE.
!
!        * ELEMENT-ID
!       *  MCSID = MATERIAL-COORDINATE-SYSTEM-ID
! ENTRY*
!       *  EXTERNAL-GRID-IDS THIS ELEMENT CONNECTS (1 OR MORE)
!        * X,Y,Z BASIC COORDINATE SETS OF EACH CONNECTED POINT(1 OR MORE
!
!           ( ABOVE ELEMENT ENTRY REPEATS FOR EACH ELEMENT
!             REFERENCING A MAT1 OR MAT2 CARD HAVING A NON-ZERO MCSID.)
!
!*****
         loc = 630
         file = scr1
         CALL open(*200,scr1,iz(ibuf2),wrtrew)
         file = est
         CALL gopen(est,iz(ibuf1),0)
!
         oldid = -99999998
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO
!
!  READ ELEMENT TYPE OF NEXT EST RECORD AND DETERMINE IF IT IS
!  AMONG ELEMENT TYPES TO BE EVEN CONSIDERED.
!
            loc = 645
            CALL read(*120,*240,est,eltype,1,noeor,nwords)
            DO i = 1 , nelems
               IF ( eltype==elem(1,i) ) EXIT SPAG_Loop_1_1
            ENDDO
            CALL fwdrec(*220,est)
         ENDDO SPAG_Loop_1_1
!
!  OK THIS  ELEMENT TYPE RECORD IS TO BE CONSIDERED.
!
         estwds = elem(2,i)
         loc = 670
         IF ( estwds>lbuf ) GOTO 180
         any = .FALSE.
         npts = elem(3,i)
         imatid = elem(4,i)
         ixyz1 = elem(5,i)
         ixyz2 = ixyz1 + 4*npts - 1
         k1 = 2 + npts
         loc = 680
         IF ( k1>lsbuf ) GOTO 180
 80      DO
!
!  READ AN ELEMENT ENTRY AND CHECK TO DETERMINE IF IT IS TO BE USED.
!
            loc = 690
            CALL read(*220,*100,est,buf(1),estwds,noeor,nwords)
            matid = buf(imatid)
            IF ( matid/=oldid ) THEN
               CALL bisloc(*80,matid,iz(imid),3,nmids,jp)
               mcsid = iz(imid+jp)
               oldid = matid
               iz(imid+jp+1) = 7
            ENDIF
!
!  DEVELOP AND OUTPUT ABBREVIATED ENTRY TO SCRATCH1.
!  (INITIALIZE RECORD WITH THREE-WORD-HEADER ENTRY.)
!
            IF ( .NOT.(any) ) THEN
               sbuf(1) = eltype
               sbuf(2) = 4*npts + 2
               sbuf(3) = npts
               CALL write(scr1,sbuf(1),3,noeor)
               iz(jeltyp) = eltype
               jeltyp = jeltyp + 1
               any = .TRUE.
            ENDIF
!
            sbuf(1) = buf(1)
            sbuf(2) = mcsid
!
!  CONVERT SILS TO EXTERNAL-IDS IF OES1G IS TO BE BUILT
!
            IF ( foes1g ) THEN
!
               jsil = 2
               loc = 740
               DO i = 3 , k1
                  CALL bisloc(*180,buf(jsil),iz(isil),1,lsil,jp)
                  sbuf(i) = iz(iext+jp-1)
                  jsil = jsil + 1
               ENDDO
            ELSE
               DO i = 3 , k1
                  sbuf(i) = 0
               ENDDO
            ENDIF
!
!  OUTPUT THIS PORTION OF ENTRY AND THEN XYZ COMPONENTS OF CONNECTED
!  POINTS
!
            CALL write(scr1,sbuf(1),npts+2,noeor)
!
            DO i = ixyz1 , ixyz2 , 4
               CALL write(scr1,buf(i+1),3,noeor)
!
!  GO FOR NEXT ELEMENT OF THIS TYPE
!
            ENDDO
         ENDDO
!
!  END OF ENTRIES FUR CURRENT ELEMENT TYPE.
!
 100     loc = 780
         IF ( nwords/=0 ) GOTO 180
         IF ( any ) CALL write(scr1,0,0,eor)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!  END OF ALL ELEMENT TYPES IN EST
!
 120     CALL close(est,clsrew)
         CALL close(scr1,clsrew)
!*****
!  REDUCTION OF MATERIAL-ID AND COORDINATE-SYSTEM-ID TO THOSE
!  ACTUALLY REFERENCED BY ELEMENTS BEING CONSIDERED.
!*****
         neltyp = jeltyp - 1
!
!  RESORT MID-MCSID TABLE ON MCSID.
!
         CALL sort(0,0,3,2,iz(imid),lmid)
         imcsid = neltyp + 1
         nmcsid = neltyp
         loc = 820
         oldid = 0
         DO i = imid , nmid , 3
            IF ( iz(i+2)<0 ) GOTO 180
            IF ( iz(i+2)/=0 ) THEN
!
!  ELIMINATE DUPLICATE MCSIDS.
!
               IF ( iz(i+1)/=oldid ) THEN
                  oldid = iz(i+1)
                  nmcsid = nmcsid + 2
                  iz(nmcsid-1) = iz(i+1)
                  iz(nmcsid) = 0
               ENDIF
            ENDIF
         ENDDO
         lmcsid = nmcsid - imcsid + 1
         mcsids = lmcsid/2
!
!  IF TABLE IS NOW EMPTY THERE IS NOTHING MORE TO DO
!
         loc = 860
         IF ( lmcsid<0 ) GOTO 180
         IF ( lmcsid==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!*****
!  COORDINATE SYSTEMS WHICH MAY BE REFERENCED ARE AT THIS TIME
!  PULLED INTO CORE FROM THE -CSTM- DATA BLOCK. (SILS AND EXTERNAL-IDS
!  TABLES IF IN CORE ARE NO LONGER REQUIRED.)
!*****
         icstm = nmcsid + 1
         ncstm = nmcsid
         file = cstm
         CALL gopen(cstm,iz(ibuf1),0)
         spag_nextblock_1 = 4
      CASE (4)
         icrq = ncstm + 14 - jcore
         IF ( ncstm+14>jcore ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 140     CALL read(*220,*160,cstm,iz(ncstm+1),14,noeor,nwords)
         kid = iz(ncstm+1)
         CALL bisloc(*140,kid,iz(imcsid),2,mcsids,jp)
         ncstm = ncstm + 14
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!  END OF COORDINATE SYSTEM DATA
!
 160     CALL close(cstm,clsrew)
         lcstm = ncstm - icstm + 1
         cstms = lcstm/14
         CALL sort(0,0,14,1,z(icstm),lcstm)
         CALL pretrs(iz(icstm),lcstm)
         spag_nextblock_1 = 5
      CASE (5)
!*****
!  INITIALIZE INPUT AND OUTPUT FILE POSITIONS.
!*****
         CALL gopen(oes1,iz(ibuf1),0)
!
!  CHECK FOR STRAIN OPTION
!
         file = oes1
         loc = 910
         CALL read(*220,*240,oes1,idrec(1),2,0,flag)
         i = idrec(2)
         IF ( i/=5 .AND. i/=21 .AND. i/=1005 ) GOTO 180
         strain = .FALSE.
         IF ( i==21 ) strain = .TRUE.
         icmplx = 0
         IF ( i==1005 ) icmplx = 1
         CALL bckrec(oes1)
!
         CALL close(oes1,cls)
         eofos1 = .FALSE.
!
         CALL gopen(oes1m,iz(ibuf1),1)
         CALL close(oes1m,cls)
!
         IF ( foes1g ) THEN
            CALL gopen(oes1g,iz(ibuf1),1)
            CALL close(oes1g,cls)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!*****
!  END OF INITIALIZATION
!*****
         RETURN
!*****
!  ERROR CONDITION ENCOUNTERED.
!*****
 180     imsg = -logerr
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 200     imsg = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 220     imsg = -2
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 240     imsg = -3
         spag_nextblock_1 = 6
      CASE (7)
         imsg = -8
         lcore = icrq
         spag_nextblock_1 = 6
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curv1
