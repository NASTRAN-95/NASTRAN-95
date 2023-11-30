
SUBROUTINE dpd5
   IMPLICIT NONE
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , Eqdyn ,   &
         & Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Ibuf , Ifile(6) , Ihalf , Ineq , Jhalf , Kn , L , Loads(32) , Luset , Lusetd ,   &
         & Mach , Mcb(7) , Msg(3) , Nam(2) , Neqdyn , Ngrid , Nlft , Nodlt , Noeed , Nofrl , Nogo , Nolin(21) , Nonlft , Nopsdl ,   &
         & Nosdt , Notfl , Notrl , Noue , Nout , Psd(2) , Psdl , Scr1 , Scr2 , Scr3 , Scr4 , Sdt , Seqep(2) , Sil , Sild , Tf(2) ,  &
         & Tfl , Tic(2) , Trl , Tstep(2) , Two(32) , Ua , Ud , Uset , Usetd , Z(1)
   REAL Bufr(20) , Clsrew , Rd , Rdrew , Ue , Uf , Ufe , Ug , Ul , Um , Un , Une , Uo , Up , Ur , Us , Usb , Usg , Wrt , Wrtrew ,   &
      & Zz(1)
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /blank / Luset , Lusetd , Notfl , Nodlt , Nopsdl , Nofrl , Nonlft , Notrl , Noeed , Nosdt , Noue
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineq
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /setup / Ifile
   COMMON /system/ Ibuf , Nout
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
   INTEGER andf , lshift , orf
   INTEGER eigp(2) , file , flag , i , i23 , i45 , id , imsg(2) , in , irow , isw , itfl , iusetd , j , k , last , mask , n ,       &
         & nback , nusetd
   LOGICAL pack
   EXTERNAL andf , lshift , orf
!
!     DPD5 ASSEMBLEMS
!     (1) THE EIGENVALUE EXTRACTION DATA BLOCK (EED), AND
!     (2) THE TRANSFER FUNCTION LIST (TFL).
!
!     REVISED  9/1989, BY G.C./UNISYS
!     NO COLUMN AND ROW WORD PACKING IN TFL FILE FOR MACHINES WITH 32
!     BIT WORD SIZE, OR LESS
!
   EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
   DATA eigp/257 , 4/
!
!     (1) PROCESS EDD
!     ===============
!
!     OPEN EED AND WRITE HEADER.
!     INITIALIZE TO LOOP THROUGH EIG CARDS.
!
!     OPEN DYNAMICS POOL.
!
   file = Dpool
   CALL preloc(*2400,Z(Buf1),Dpool)
!
   file = Eed
   CALL open(*1800,Eed,Z(Buf2),Wrtrew)
   file = Dpool
   CALL fname(Eed,Buf)
   CALL write(Eed,Buf,2,1)
   in = 0
   DO j = 2 , 7
      Mcb(j) = 0
   ENDDO
   L = 12
   Msg(1) = 75
!
!     LOCATE EIGB CARDS IN DYNAMICS POOL. IF PRESENT, TURN NOEED FLAG
!     OFF, WRITE ID ON EED AND TURN ON TRAILER BIT.
!
   CALL locate(*400,Z(Buf1),Eigb,flag)
   Noeed = 1
   CALL write(Eed,Eigb,2,0)
   CALL write(Eed,0,1,0)
   j = (Eigb(2)-1)/16
   k = Eigb(2) - 16*j
   Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
   ASSIGN 200 TO nback
   L = 12
   mask = Two(Ua)
!
!     READ EIGB CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
 100  CALL read(*2500,*300,Dpool,Buf,18,0,flag)
   GOTO 1400
 200  CALL write(Eed,Buf,12,0)
   CALL write(Eed,Buf(14),6,0)
   GOTO 100
 300  CALL write(Eed,0,0,1)
!
!     LOCATE EIGC CARDS IN DYNAMICS POOL. IF PRESENT, TURN OFF NOEED
!     FLAG, WRITE ID ON EED AND TURN ON TRL BIT.
!
 400  CALL locate(*800,Z(Buf1),Eigc,flag)
   Noeed = 1
   CALL write(Eed,Eigc,2,0)
   CALL write(Eed,0,1,0)
   j = (Eigc(2)-1)/16
   k = Eigc(2) - 16*j
   Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
   ASSIGN 600 TO nback
   L = 6
   mask = Two(Ud)
!
!     READ EIGC CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
 500  CALL read(*2500,*700,Dpool,Buf,10,0,flag)
   GOTO 1400
 600  CALL write(Eed,Buf,7,0)
   CALL write(Eed,Buf(8),3,0)
   DO
      CALL read(*2500,*2500,Dpool,Buf,7,0,flag)
      CALL write(Eed,Buf,7,0)
      IF ( Buf(1)==-1 ) GOTO 500
   ENDDO
 700  CALL write(Eed,0,0,1)
!
!     LOCATE EIGP CARDS. IF PRESENT, TURN NOEED FLAG OFF,
!     WRITE ID ON EED AND TURN ON TRAILER BIT. COPY DATA ON EED.
!
 800  CALL locate(*1000,Z(Buf1),eigp,flag)
   Noeed = 1
   CALL write(Eed,eigp,2,0)
   CALL write(Eed,0,1,0)
   j = (eigp(2)-1)/16
   k = eigp(2) - 16*j
   Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
   DO
      CALL read(*2500,*900,Dpool,Buf,4,0,flag)
      CALL write(Eed,Buf,4,0)
   ENDDO
 900  CALL write(Eed,0,0,1)
!
!     LOCATE EIGR CARDS IN DYNAMICS POOL. IF PRESENT, TURN OFF NOEED
!     FLAG, WRITE ID ON EED AND TURN ON TRL BIT.
!
 1000 CALL locate(*1700,Z(Buf1),Eigr,flag)
   Noeed = 1
   CALL write(Eed,Eigr,2,0)
   CALL write(Eed,0,1,0)
   j = (Eigr(2)-1)/16
   k = Eigr(2) - 16*j
   Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
   ASSIGN 1200 TO nback
   L = 12
   mask = Two(Ua)
!
!     READ EIGR CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
 1100 CALL read(*2500,*1300,Dpool,Buf,18,0,flag)
   GOTO 1400
 1200 CALL write(Eed,Buf,12,0)
   CALL write(Eed,Buf(14),6,0)
   GOTO 1100
 1300 CALL write(Eed,0,0,1)
   GOTO 1700
!
!     CODE TO CONVERT GRID NO. AND COMPOIENT CODE TO SIL NO.
!     SIL NO. IS IN A SET FOR EIGR, EIGB - IN D SET FOR EIGC.
!     WRITE DATA ON EED.
!
 1400 IF ( Buf(L)==0 ) GOTO nback
   IF ( in/=0 ) GOTO 1600
   file = Usetd
   CALL open(*2400,Usetd,Z(Buf3),Rdrew)
   CALL fwdrec(*2500,Usetd)
   iusetd = Neqdyn + 2
   CALL read(*2500,*1500,Usetd,Z(iusetd),Buf3-iusetd,1,n)
   CALL mesage(-8,0,Nam)
 1500 CALL close(Usetd,Clsrew)
   in = 1
 1600 imsg(1) = Buf(1)
   imsg(2) = Buf(L)
   CALL dpdaa
   nusetd = iusetd + Buf(L) - 1
   Buf(L) = 0
   DO j = iusetd , nusetd
      IF ( andf(Z(j),mask)/=0 ) Buf(L) = Buf(L) + 1
   ENDDO
   IF ( andf(Z(nusetd),mask)/=0 ) GOTO nback
   Nogo = 1
   CALL mesage(30,107,imsg)
   GOTO nback
!
!     COMPLETE EIG CARD PROCESSING.
!
 1700 CALL close(Eed,Clsrew)
   Mcb(1) = Eed
   CALL wrttrl(Mcb)
!
!
!     (2) PRECESS TFL FILE
!     ====================
!
!     SELECT PACK OR NO-PACK LOGIC
!
 1800 pack = .TRUE.
   i45 = 4
   i23 = 3
   IF ( Ihalf<=16 ) THEN
      pack = .FALSE.
      i45 = 5
      i23 = 2
   ENDIF
!
!     OPEN TFL. WRITE HEADER. INITIALIZE TO READ TF CARDS.
!
   DO j = 2 , 7
      Mcb(j) = 0
   ENDDO
   CALL locate(*2300,Z(Buf1),Tf,flag)
   Notfl = 0
   file = Tfl
   CALL open(*2300,Tfl,Z(Buf2),Wrtrew)
   CALL fname(Tfl,Buf)
   CALL write(Tfl,Buf,2,1)
   Msg(1) = 68
   L = 2
   id = 0
   itfl = Neqdyn + 2
   i = itfl
   isw = 0
   last = 0
!
!     READ FIXED SECTION OF TF CARD. CONVERT GRID POINT AND COMP. TO
!     SIL NO. TEST FOR NEW TRANSFER FUNCTION SET.
!
 1900 CALL read(*2500,*2000,Dpool,Buf,6,0,flag)
   Msg(3) = Buf(1)
   CALL dpdaa
   irow = Buf(2)
   IF ( Buf(1)==id ) GOTO 2200
   Notfl = Notfl + 1
   IF ( id/=0 ) GOTO 2100
   id = Buf(1)
   GOTO 2200
!
!     SORT TRANSFER EQUATIONS AND WRITE ON TFL ONE RECORD PER TRANSFER
!     FUNCTION SET. FIRST WORD OF RECORD IS SETID.
!
 2000 last = 1
 2100 CALL write(Tfl,id,1,0)
   IF ( isw==0 ) THEN
      n = i - itfl
      IF ( pack ) CALL sorti(0,0,4,1,Z(itfl),n)
      IF ( .NOT.pack ) CALL sorti2(0,0,5,1,Z(itfl),n)
      CALL write(Tfl,Z(itfl),n,1)
   ELSE
      CALL write(Scr1,0,0,1)
      CALL close(Scr1,Clsrew)
      CALL open(*2400,Scr1,Z(Buf2),Rdrew)
      Ifile(1) = Scr2
      Ifile(2) = Scr3
      Ifile(3) = Scr4
      n = Buf3 - itfl
      IF ( pack ) CALL sorti(Scr1,Tfl,4,1,Z(itfl),n)
      IF ( .NOT.pack ) CALL sorti2(Scr1,Tfl,5,1,Z(itfl),n)
      CALL close(Scr1,Clsrew)
   ENDIF
   i = itfl
   id = Buf(1)
   isw = 0
   IF ( last/=0 ) THEN
!
!     HERE WHEN ALL TRANSFER FUNCTION SETS COMPLETE.
!     CLOSE FILE AND WRITE TRAILER.
!
      CALL close(Tfl,Clsrew)
      Mcb(2) = Notfl
      Mcb(1) = Tfl
      CALL wrttrl(Mcb)
      GOTO 2300
   ENDIF
!
!     INTEGER PACKING LOGIC (MACHINES WITH 36 BITS WORDS, OR MORE) -
!     PACK COLN AND ROW INTO ONE WORD IF BOTH CAN BE STORED IN HALF WORD
!     THEN FOLLOWED BY 3 COEFFICIENTS, TOTALLY 4 WORDS
!
!     NON-INTEGER PACKING LOGIC (MACHINES WITH 32 BITS WORDS) -
!     THE COLUMN AND ROW ARE NOT PACKED, AND THEREFORE NOT BOUNED TO
!     65536 SIZE LIMIT. 1ST WORD IS COLUMN, 2ND WORD IS ROW, THEN
!     FOLLOWED BY 3 COEFFICIENTS, TOTALLY 5 WORDS
!     THE SUBROUTINE SORTI2 IS USED WHEN SORTING TFL BY 2 KEY WORDS
!
 2200 IF ( .NOT.pack ) THEN
      Buf(3) = irow
   ELSE
      IF ( Buf(2)>=Jhalf .OR. irow>=Jhalf ) GOTO 2700
      Buf(3) = lshift(Buf(2),Ihalf)
      Buf(3) = orf(Buf(3),irow)
   ENDIF
   IF ( isw/=0 ) THEN
      CALL write(Scr1,Buf(i23),i45,0)
   ELSEIF ( i+i45>Buf3 ) THEN
      isw = 1
      file = Scr1
      CALL open(*2400,Scr1,Z(Buf3),Wrtrew)
      n = i - itfl
      CALL write(Scr1,Z(itfl),n,0)
      CALL write(Scr1,Buf(i23),i45,0)
   ELSE
      DO j = i23 , 6
         Z(i) = Buf(j)
         i = i + 1
      ENDDO
   ENDIF
!
!     READ GRID POINT, COMP AND VALUES. CONVERT POINT AND COMP. TO SIL
!     NO. STORE IN CORE. IF SPILL, WRITE ON SCR1.
!
   CALL read(*2500,*2400,Dpool,Buf(2),5,0,flag)
   IF ( Buf(2)==-1 ) GOTO 1900
   CALL dpdaa
   GOTO 2200
 2300 CALL close(Dpool,Clsrew)
   RETURN
!
!     FATAL ERRORS
!
 2400 n = -1
   GOTO 2600
 2500 n = -2
 2600 CALL mesage(n,file,Nam)
 2700 WRITE (Nout,99001) Ihalf , Buf(2) , irow
99001 FORMAT ('0*** COLUMN OR ROW SIL NO. EXCEEDS',I3,' BITS WORD ','PACKING LIMIT',2I9)
   CALL mesage(-37,Nam,Nam)
END SUBROUTINE dpd5
