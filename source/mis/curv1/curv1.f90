!*==curv1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curv1
   IMPLICIT NONE
   USE C_BLANK
   USE C_CURVC1
   USE C_CURVC2
   USE C_CURVC3
   USE C_CURVTB
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
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
         Lsbuf = 10
         Lbuf = 100
         Nelems = 4
         Logerr = 37
!*****
!  INITIALIZATION OF CORE AND FLAGS
!*****
         Foes1g = .TRUE.
         IF ( Ip1>0 ) Foes1g = .FALSE.
         Any1m = .FALSE.
         Any1g = .FALSE.
         Lmcsid = 0
!
         Lcore = korsz(Iz(1))
         DO I = 1 , Lcore
            Iz(I) = 0
         ENDDO
         Ibuf1 = Lcore - Sysbuf
         Ibuf2 = Ibuf1 - Sysbuf
         Ibuf3 = Ibuf2 - Sysbuf
         Ibuf4 = Ibuf3 - Sysbuf
!
!  SET FILE NUMBERS EXPLICITYLY.  ALL OVERLAYS REFERENCE /CURVTB/
!
         Oes1 = 101
         Mpt = 102
         Cstm = 103
         Est = 104
         Sil = 105
         Gpl = 106
         Oes1m = 201
         Oes1g = 202
         Scr1 = 301
         Scr2 = 302
         Scr3 = 303
         Scr4 = 304
         Scr5 = 305
         Jcore = Ibuf4 - 1
         File = 0
         Loc = 300
         icrq = -Ibuf4
         IF ( Ibuf4<=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!*****
!  ALLOCATE TABLE OF ELEMENT TYPES PLACED ON ESTX(SCR1).  MAXIMUM
!  SIZE NOW AND REDUCED LATER TO ACTUAL SIZE.
!*****
         Ieltyp = 1
         Jeltyp = Ieltyp
         Neltyp = Nelems
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
         Imid = Neltyp + 1
         Nmid = Imid - 1
!
!  OPEN MPT USING -PRELOC- FUNCTION.
!
         File = Mpt
         Loc = 400
         CALL preloc(*200,Iz(Ibuf1),Mpt)
!
!  PASS MAT1 AND MAT2 DATA IF ANY.
!
         DO I = 1 , 6 , 3
            Iwords = mat(I+2)
            IF ( Iwords>Lbuf ) GOTO 180
            CALL locate(*20,Iz(Ibuf1),mat(I),idum)
            DO
               CALL read(*220,*20,Mpt,Buf(1),Iwords,noeor,Nwords)
               IF ( Buf(Iwords)>0 ) THEN
                  icrq = Nmid + 3 - Jcore
                  IF ( Nmid+3>Jcore ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Iz(Nmid+1) = Buf(1)
                  Iz(Nmid+2) = Buf(Iwords)
                  Iz(Nmid+3) = 0
                  Nmid = Nmid + 3
               ENDIF
            ENDDO
!
!  EOR HIT READING MAT1 OR MAT2 CARDS
!
 20      ENDDO
!
!  TABLE COMPLETE, THUS NOW SORT IT. IF TABLE IS EMPTY WE ARE THROUGH
!
         CALL close(Mpt,Clsrew)
         Lmid = Nmid - Imid + 1
         Nmids = Lmid/3
         Loc = 570
         IF ( Lmid<0 ) GOTO 180
         IF ( Lmid==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sort(0,0,3,1,Iz(Imid),Lmid)
!*****
!  LOAD LIST OF SILS INTO CORE, FOLLOWED BY LIST OF EXTERNAL IDS.
!  THIS IS REQUIRED ONLY IF OES1G IS TO BE FORMED.
!*****
         IF ( .NOT.Foes1g ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         File = Sil
         Loc = 580
         Isil = Nmid + 1
         CALL gopen(Sil,Iz(Ibuf1),0)
         CALL read(*220,*40,Sil,Iz(Isil),Jcore-Isil,noeor,Lsil)
         icrq = Jcore - Isil
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
 40      Nsil = Isil + Lsil - 1
         CALL close(Sil,Clsrew)
!
         File = Gpl
         Loc = 590
         Iext = Nsil + 1
         CALL gopen(Gpl,Iz(Ibuf1),0)
         CALL read(*220,*60,Gpl,Iz(Iext),Jcore-Iext,noeor,Lext)
         icrq = Jcore - Iext
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
 60      Next = Iext + Lext - 1
         CALL close(Gpl,Clsrew)
         IF ( Lsil/=Lext ) GOTO 180
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
         Loc = 630
         File = Scr1
         CALL open(*200,Scr1,Iz(Ibuf2),Wrtrew)
         File = Est
         CALL gopen(Est,Iz(Ibuf1),0)
!
         Oldid = -99999998
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO
!
!  READ ELEMENT TYPE OF NEXT EST RECORD AND DETERMINE IF IT IS
!  AMONG ELEMENT TYPES TO BE EVEN CONSIDERED.
!
            Loc = 645
            CALL read(*120,*240,Est,Eltype,1,noeor,Nwords)
            DO I = 1 , Nelems
               IF ( Eltype==elem(1,I) ) EXIT SPAG_Loop_1_1
            ENDDO
            CALL fwdrec(*220,Est)
         ENDDO SPAG_Loop_1_1
!
!  OK THIS  ELEMENT TYPE RECORD IS TO BE CONSIDERED.
!
         Estwds = elem(2,I)
         Loc = 670
         IF ( Estwds>Lbuf ) GOTO 180
         Any = .FALSE.
         Npts = elem(3,I)
         Imatid = elem(4,I)
         Ixyz1 = elem(5,I)
         Ixyz2 = Ixyz1 + 4*Npts - 1
         K1 = 2 + Npts
         Loc = 680
         IF ( K1>Lsbuf ) GOTO 180
 80      DO
!
!  READ AN ELEMENT ENTRY AND CHECK TO DETERMINE IF IT IS TO BE USED.
!
            Loc = 690
            CALL read(*220,*100,Est,Buf(1),Estwds,noeor,Nwords)
            Matid = Buf(Imatid)
            IF ( Matid/=Oldid ) THEN
               CALL bisloc(*80,Matid,Iz(Imid),3,Nmids,Jp)
               Mcsid = Iz(Imid+Jp)
               Oldid = Matid
               Iz(Imid+Jp+1) = 7
            ENDIF
!
!  DEVELOP AND OUTPUT ABBREVIATED ENTRY TO SCRATCH1.
!  (INITIALIZE RECORD WITH THREE-WORD-HEADER ENTRY.)
!
            IF ( .NOT.(Any) ) THEN
               Sbuf(1) = Eltype
               Sbuf(2) = 4*Npts + 2
               Sbuf(3) = Npts
               CALL write(Scr1,Sbuf(1),3,noeor)
               Iz(Jeltyp) = Eltype
               Jeltyp = Jeltyp + 1
               Any = .TRUE.
            ENDIF
!
            Sbuf(1) = Buf(1)
            Sbuf(2) = Mcsid
!
!  CONVERT SILS TO EXTERNAL-IDS IF OES1G IS TO BE BUILT
!
            IF ( Foes1g ) THEN
!
               Jsil = 2
               Loc = 740
               DO I = 3 , K1
                  CALL bisloc(*180,Buf(Jsil),Iz(Isil),1,Lsil,Jp)
                  Sbuf(I) = Iz(Iext+Jp-1)
                  Jsil = Jsil + 1
               ENDDO
            ELSE
               DO I = 3 , K1
                  Sbuf(I) = 0
               ENDDO
            ENDIF
!
!  OUTPUT THIS PORTION OF ENTRY AND THEN XYZ COMPONENTS OF CONNECTED
!  POINTS
!
            CALL write(Scr1,Sbuf(1),Npts+2,noeor)
!
            DO I = Ixyz1 , Ixyz2 , 4
               CALL write(Scr1,Buf(I+1),3,noeor)
!
!  GO FOR NEXT ELEMENT OF THIS TYPE
!
            ENDDO
         ENDDO
!
!  END OF ENTRIES FUR CURRENT ELEMENT TYPE.
!
 100     Loc = 780
         IF ( Nwords/=0 ) GOTO 180
         IF ( Any ) CALL write(Scr1,0,0,eor)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!  END OF ALL ELEMENT TYPES IN EST
!
 120     CALL close(Est,Clsrew)
         CALL close(Scr1,Clsrew)
!*****
!  REDUCTION OF MATERIAL-ID AND COORDINATE-SYSTEM-ID TO THOSE
!  ACTUALLY REFERENCED BY ELEMENTS BEING CONSIDERED.
!*****
         Neltyp = Jeltyp - 1
!
!  RESORT MID-MCSID TABLE ON MCSID.
!
         CALL sort(0,0,3,2,Iz(Imid),Lmid)
         Imcsid = Neltyp + 1
         Nmcsid = Neltyp
         Loc = 820
         Oldid = 0
         DO I = Imid , Nmid , 3
            IF ( Iz(I+2)<0 ) GOTO 180
            IF ( Iz(I+2)/=0 ) THEN
!
!  ELIMINATE DUPLICATE MCSIDS.
!
               IF ( Iz(I+1)/=Oldid ) THEN
                  Oldid = Iz(I+1)
                  Nmcsid = Nmcsid + 2
                  Iz(Nmcsid-1) = Iz(I+1)
                  Iz(Nmcsid) = 0
               ENDIF
            ENDIF
         ENDDO
         Lmcsid = Nmcsid - Imcsid + 1
         Mcsids = Lmcsid/2
!
!  IF TABLE IS NOW EMPTY THERE IS NOTHING MORE TO DO
!
         Loc = 860
         IF ( Lmcsid<0 ) GOTO 180
         IF ( Lmcsid==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!*****
!  COORDINATE SYSTEMS WHICH MAY BE REFERENCED ARE AT THIS TIME
!  PULLED INTO CORE FROM THE -CSTM- DATA BLOCK. (SILS AND EXTERNAL-IDS
!  TABLES IF IN CORE ARE NO LONGER REQUIRED.)
!*****
         Icstm = Nmcsid + 1
         Ncstm = Nmcsid
         File = Cstm
         CALL gopen(Cstm,Iz(Ibuf1),0)
         spag_nextblock_1 = 4
      CASE (4)
         icrq = Ncstm + 14 - Jcore
         IF ( Ncstm+14>Jcore ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 140     CALL read(*220,*160,Cstm,Iz(Ncstm+1),14,noeor,Nwords)
         kid = Iz(Ncstm+1)
         CALL bisloc(*140,kid,Iz(Imcsid),2,Mcsids,Jp)
         Ncstm = Ncstm + 14
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!  END OF COORDINATE SYSTEM DATA
!
 160     CALL close(Cstm,Clsrew)
         Lcstm = Ncstm - Icstm + 1
         Cstms = Lcstm/14
         CALL sort(0,0,14,1,z(Icstm),Lcstm)
         CALL pretrs(Iz(Icstm),Lcstm)
         spag_nextblock_1 = 5
      CASE (5)
!*****
!  INITIALIZE INPUT AND OUTPUT FILE POSITIONS.
!*****
         CALL gopen(Oes1,Iz(Ibuf1),0)
!
!  CHECK FOR STRAIN OPTION
!
         File = Oes1
         Loc = 910
         CALL read(*220,*240,Oes1,Idrec(1),2,0,flag)
         I = Idrec(2)
         IF ( I/=5 .AND. I/=21 .AND. I/=1005 ) GOTO 180
         Strain = .FALSE.
         IF ( I==21 ) Strain = .TRUE.
         Icmplx = 0
         IF ( I==1005 ) Icmplx = 1
         CALL bckrec(Oes1)
!
         CALL close(Oes1,Cls)
         Eofos1 = .FALSE.
!
         CALL gopen(Oes1m,Iz(Ibuf1),1)
         CALL close(Oes1m,Cls)
!
         IF ( Foes1g ) THEN
            CALL gopen(Oes1g,Iz(Ibuf1),1)
            CALL close(Oes1g,Cls)
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
 180     Imsg = -Logerr
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 200     Imsg = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 220     Imsg = -2
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 240     Imsg = -3
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         Imsg = -8
         Lcore = icrq
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curv1
