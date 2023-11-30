
SUBROUTINE curv1
   IMPLICIT NONE
   LOGICAL Any , Any1g , Any1m , Anyout , Eofos1 , First , Foes1g , Strain
   INTEGER Buf(100) , Cls , Clsrew , Cstm , Cstms , Cstype , Depts , Device , Eltype , Eor , Est , Estwds , Ewords , File , Gpl ,   &
         & I , Ibuf1 , Ibuf2 , Ibuf3 , Ibuf4 , Icmplx , Icomp , Icstm , Ictype , Idep , Idoes1 , Idrec(146) , Idscr1 , Ieltyp ,     &
         & Iestx , Iext , Igmat , Iindep , Imatid , Imcsid , Imid , Imsg , Indpts , Ioes1m , Ioutpt , Ip1 , Ip2 , Isig1 , Isig2 ,   &
         & Isigma , Isil , Ising , Itran , Ivmat , Iwords , Ixyz1 , Ixyz2 , Iz(1) , J , Jcore , Jeltyp , Jindep , Jmcsid , Jp ,     &
         & Jsil , K , K1 , K2 , Kmcsid , Kount , L , Lbuf , Lcore , Lcstm , Lext , Lmcsid , Lmid , Loc , Loes1m , Logerr , Lsbuf ,  &
         & Lsil , Lx1 , Lx2 , Matid
   INTEGER Mcsid , Mcsids , Mpt , Ncstm , Ndep , Nelems , Neltyp , Nestx , Next , Ngmat , Nindep , Nmcsid , Nmid , Nmids , Noeor ,  &
         & Noes1m , Npts , Npts4 , Nsigma , Nsil , Nwords , Oes1 , Oes1g , Oes1m , Oldid , Owords , Rd , Rdrew , Sbuf(10) , Scr1 ,  &
         & Scr2 , Scr3 , Scr4 , Scr5 , Sil , Subcas , Sysbuf , Wrt , Wrtrew
   REAL Rbuf(100) , Vec(3) , Vmax(3) , Vmin(3) , Z(1) , Zdum(3)
   COMMON /blank / Ip1 , Ip2 , Icmplx , Zdum
   COMMON /curvc1/ Lsbuf , Sbuf
   COMMON /curvc2/ Lbuf , Buf
   COMMON /curvc3/ Vec , Vmax , Vmin , Idrec
   COMMON /curvtb/ Imid , Nmid , Lmid , Nmids , Ieltyp , Neltyp , Jeltyp , Icstm , Ncstm , Cstms , Lcstm , Iestx , Nestx , Imcsid , &
                 & Nmcsid , Lmcsid , Mcsids , Jmcsid , Kmcsid , Isil , Nsil , Lsil , Jsil , Ioes1m , Noes1m , Loes1m , Idep , Ndep ,&
                 & Iindep , Nindep , Jindep , Isigma , Nsigma , Igmat , Ngmat , Iext , Next , Lext , Scr1 , Scr2 , Scr3 , Scr4 ,    &
                 & Oes1m , Oes1g , Oes1 , Mpt , Cstm , Est , Sil , Gpl , Jcore , Lcore , Ibuf1 , Ibuf2 , Ibuf3 , Ibuf4 , I , J , K ,&
                 & L , K1 , K2 , Ixyz1 , Ixyz2 , Lx1 , Lx2 , Eltype , Mcsid , Idscr1 , Idoes1 , Npts , Npts4 , Iwords , Nwords ,    &
                 & Subcas , Kount , Isig1 , Isig2 , Loc , File , Imsg , Nelems , Imatid , Icomp , Estwds , Ewords , Jp , Owords ,   &
                 & Matid , Depts , Indpts , Ictype , Ivmat , Itran , Cstype , Ising , Device , Oldid , Any , Eofos1 , First ,       &
                 & Anyout , Foes1g , Strain , Logerr , Any1m , Any1g , Scr5
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Ioutpt
   COMMON /zzzzzz/ Iz
   INTEGER elem(5,4) , icrq , idum , kid , mat(6)
   REAL flag
   INTEGER korsz
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
   EQUIVALENCE (Z(1),Iz(1)) , (Buf(1),Rbuf(1))
   EQUIVALENCE (Noeor,Rdrew) , (Eor,Cls)
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
   IF ( Ibuf4<=0 ) GOTO 1900
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
   CALL preloc(*1600,Iz(Ibuf1),Mpt)
!
!  PASS MAT1 AND MAT2 DATA IF ANY.
!
   DO I = 1 , 6 , 3
      Iwords = mat(I+2)
      IF ( Iwords>Lbuf ) GOTO 1500
      CALL locate(*100,Iz(Ibuf1),mat(I),idum)
      DO
         CALL read(*1700,*100,Mpt,Buf(1),Iwords,Noeor,Nwords)
         IF ( Buf(Iwords)>0 ) THEN
            icrq = Nmid + 3 - Jcore
            IF ( Nmid+3>Jcore ) GOTO 1900
            Iz(Nmid+1) = Buf(1)
            Iz(Nmid+2) = Buf(Iwords)
            Iz(Nmid+3) = 0
            Nmid = Nmid + 3
         ENDIF
      ENDDO
!
!  EOR HIT READING MAT1 OR MAT2 CARDS
!
 100  ENDDO
!
!  TABLE COMPLETE, THUS NOW SORT IT. IF TABLE IS EMPTY WE ARE THROUGH
!
   CALL close(Mpt,Clsrew)
   Lmid = Nmid - Imid + 1
   Nmids = Lmid/3
   Loc = 570
   IF ( Lmid<0 ) GOTO 1500
   IF ( Lmid==0 ) GOTO 1300
   CALL sort(0,0,3,1,Iz(Imid),Lmid)
!*****
!  LOAD LIST OF SILS INTO CORE, FOLLOWED BY LIST OF EXTERNAL IDS.
!  THIS IS REQUIRED ONLY IF OES1G IS TO BE FORMED.
!*****
   IF ( .NOT.Foes1g ) GOTO 400
   File = Sil
   Loc = 580
   Isil = Nmid + 1
   CALL gopen(Sil,Iz(Ibuf1),0)
   CALL read(*1700,*200,Sil,Iz(Isil),Jcore-Isil,Noeor,Lsil)
   icrq = Jcore - Isil
   GOTO 1900
!
 200  Nsil = Isil + Lsil - 1
   CALL close(Sil,Clsrew)
!
   File = Gpl
   Loc = 590
   Iext = Nsil + 1
   CALL gopen(Gpl,Iz(Ibuf1),0)
   CALL read(*1700,*300,Gpl,Iz(Iext),Jcore-Iext,Noeor,Lext)
   icrq = Jcore - Iext
   GOTO 1900
!
 300  Next = Iext + Lext - 1
   CALL close(Gpl,Clsrew)
   IF ( Lsil/=Lext ) GOTO 1500
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
 400  Loc = 630
   File = Scr1
   CALL open(*1600,Scr1,Iz(Ibuf2),Wrtrew)
   File = Est
   CALL gopen(Est,Iz(Ibuf1),0)
!
   Oldid = -99999998
 500  DO
!
!  READ ELEMENT TYPE OF NEXT EST RECORD AND DETERMINE IF IT IS
!  AMONG ELEMENT TYPES TO BE EVEN CONSIDERED.
!
      Loc = 645
      CALL read(*900,*1800,Est,Eltype,1,Noeor,Nwords)
      DO I = 1 , Nelems
         IF ( Eltype==elem(1,I) ) GOTO 600
      ENDDO
      CALL fwdrec(*1700,Est)
   ENDDO
!
!  OK THIS  ELEMENT TYPE RECORD IS TO BE CONSIDERED.
!
 600  Estwds = elem(2,I)
   Loc = 670
   IF ( Estwds>Lbuf ) GOTO 1500
   Any = .FALSE.
   Npts = elem(3,I)
   Imatid = elem(4,I)
   Ixyz1 = elem(5,I)
   Ixyz2 = Ixyz1 + 4*Npts - 1
   K1 = 2 + Npts
   Loc = 680
   IF ( K1>Lsbuf ) GOTO 1500
 700  DO
!
!  READ AN ELEMENT ENTRY AND CHECK TO DETERMINE IF IT IS TO BE USED.
!
      Loc = 690
      CALL read(*1700,*800,Est,Buf(1),Estwds,Noeor,Nwords)
      Matid = Buf(Imatid)
      IF ( Matid/=Oldid ) THEN
         CALL bisloc(*700,Matid,Iz(Imid),3,Nmids,Jp)
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
         CALL write(Scr1,Sbuf(1),3,Noeor)
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
            CALL bisloc(*1500,Buf(Jsil),Iz(Isil),1,Lsil,Jp)
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
      CALL write(Scr1,Sbuf(1),Npts+2,Noeor)
!
      DO I = Ixyz1 , Ixyz2 , 4
         CALL write(Scr1,Buf(I+1),3,Noeor)
!
!  GO FOR NEXT ELEMENT OF THIS TYPE
!
      ENDDO
   ENDDO
!
!  END OF ENTRIES FUR CURRENT ELEMENT TYPE.
!
 800  Loc = 780
   IF ( Nwords/=0 ) GOTO 1500
   IF ( Any ) CALL write(Scr1,0,0,Eor)
   GOTO 500
!
!  END OF ALL ELEMENT TYPES IN EST
!
 900  CALL close(Est,Clsrew)
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
      IF ( Iz(I+2)<0 ) GOTO 1500
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
   IF ( Lmcsid<0 ) GOTO 1500
   IF ( Lmcsid==0 ) GOTO 1300
!*****
!  COORDINATE SYSTEMS WHICH MAY BE REFERENCED ARE AT THIS TIME
!  PULLED INTO CORE FROM THE -CSTM- DATA BLOCK. (SILS AND EXTERNAL-IDS
!  TABLES IF IN CORE ARE NO LONGER REQUIRED.)
!*****
   Icstm = Nmcsid + 1
   Ncstm = Nmcsid
   File = Cstm
   CALL gopen(Cstm,Iz(Ibuf1),0)
 1000 icrq = Ncstm + 14 - Jcore
   IF ( Ncstm+14>Jcore ) GOTO 1900
 1100 CALL read(*1700,*1200,Cstm,Iz(Ncstm+1),14,Noeor,Nwords)
   kid = Iz(Ncstm+1)
   CALL bisloc(*1100,kid,Iz(Imcsid),2,Mcsids,Jp)
   Ncstm = Ncstm + 14
   GOTO 1000
!
!  END OF COORDINATE SYSTEM DATA
!
 1200 CALL close(Cstm,Clsrew)
   Lcstm = Ncstm - Icstm + 1
   Cstms = Lcstm/14
   CALL sort(0,0,14,1,Z(Icstm),Lcstm)
   CALL pretrs(Iz(Icstm),Lcstm)
!*****
!  INITIALIZE INPUT AND OUTPUT FILE POSITIONS.
!*****
 1300 CALL gopen(Oes1,Iz(Ibuf1),0)
!
!  CHECK FOR STRAIN OPTION
!
   File = Oes1
   Loc = 910
   CALL read(*1700,*1800,Oes1,Idrec(1),2,0,flag)
   I = Idrec(2)
   IF ( I/=5 .AND. I/=21 .AND. I/=1005 ) GOTO 1500
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
!*****
!  END OF INITIALIZATION
!*****
 1400 RETURN
!*****
!  ERROR CONDITION ENCOUNTERED.
!*****
 1500 Imsg = -Logerr
   GOTO 1400
 1600 Imsg = -1
   GOTO 1400
 1700 Imsg = -2
   GOTO 1400
 1800 Imsg = -3
   GOTO 1400
 1900 Imsg = -8
   Lcore = icrq
   GOTO 1400
END SUBROUTINE curv1
