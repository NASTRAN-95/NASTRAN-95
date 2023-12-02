!*==dpd5.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd5
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_DPDCOM
   USE C_MACHIN
   USE C_NAMES
   USE C_SETUP
   USE C_SYSTEM
   USE C_TWO
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: eigp
   INTEGER :: file , flag , i , i23 , i45 , id , in , irow , isw , itfl , iusetd , j , k , last , mask , n , nback , nusetd
   INTEGER , DIMENSION(2) :: imsg
   LOGICAL :: pack
   EXTERNAL andf , close , dpdaa , fname , fwdrec , locate , lshift , mesage , open , orf , preloc , read , sorti , sorti2 , write ,&
          & wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DPD5 ASSEMBLEMS
!     (1) THE EIGENVALUE EXTRACTION DATA BLOCK (EED), AND
!     (2) THE TRANSFER FUNCTION LIST (TFL).
!
!     REVISED  9/1989, BY G.C./UNISYS
!     NO COLUMN AND ROW WORD PACKING IN TFL FILE FOR MACHINES WITH 32
!     BIT WORD SIZE, OR LESS
!
   !>>>>EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
   DATA eigp/257 , 4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL preloc(*320,Z(Buf1),Dpool)
!
         file = Eed
         CALL open(*260,Eed,Z(Buf2),Wrtrew)
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
         CALL locate(*60,Z(Buf1),Eigb,flag)
         Noeed = 1
         CALL write(Eed,Eigb,2,0)
         CALL write(Eed,0,1,0)
         j = (Eigb(2)-1)/16
         k = Eigb(2) - 16*j
         Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
         ASSIGN 20 TO nback
         L = 12
         mask = Two(Ua)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ EIGB CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
         CALL read(*340,*40,Dpool,Buf,18,0,flag)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 20      CALL write(Eed,Buf,12,0)
         CALL write(Eed,Buf(14),6,0)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL write(Eed,0,0,1)
!
!     LOCATE EIGC CARDS IN DYNAMICS POOL. IF PRESENT, TURN OFF NOEED
!     FLAG, WRITE ID ON EED AND TURN ON TRL BIT.
!
 60      CALL locate(*120,Z(Buf1),Eigc,flag)
         Noeed = 1
         CALL write(Eed,Eigc,2,0)
         CALL write(Eed,0,1,0)
         j = (Eigc(2)-1)/16
         k = Eigc(2) - 16*j
         Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
         ASSIGN 80 TO nback
         L = 6
         mask = Two(Ud)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ EIGC CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
         CALL read(*340,*100,Dpool,Buf,10,0,flag)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      CALL write(Eed,Buf,7,0)
         CALL write(Eed,Buf(8),3,0)
         DO
            CALL read(*340,*340,Dpool,Buf,7,0,flag)
            CALL write(Eed,Buf,7,0)
            IF ( Buf(1)==-1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 100     CALL write(Eed,0,0,1)
!
!     LOCATE EIGP CARDS. IF PRESENT, TURN NOEED FLAG OFF,
!     WRITE ID ON EED AND TURN ON TRAILER BIT. COPY DATA ON EED.
!
 120     CALL locate(*160,Z(Buf1),eigp,flag)
         Noeed = 1
         CALL write(Eed,eigp,2,0)
         CALL write(Eed,0,1,0)
         j = (eigp(2)-1)/16
         k = eigp(2) - 16*j
         Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
         DO
            CALL read(*340,*140,Dpool,Buf,4,0,flag)
            CALL write(Eed,Buf,4,0)
         ENDDO
 140     CALL write(Eed,0,0,1)
!
!     LOCATE EIGR CARDS IN DYNAMICS POOL. IF PRESENT, TURN OFF NOEED
!     FLAG, WRITE ID ON EED AND TURN ON TRL BIT.
!
 160     CALL locate(*240,Z(Buf1),Eigr,flag)
         Noeed = 1
         CALL write(Eed,Eigr,2,0)
         CALL write(Eed,0,1,0)
         j = (Eigr(2)-1)/16
         k = Eigr(2) - 16*j
         Mcb(j+2) = orf(Mcb(j+2),Two(k+16))
         ASSIGN 180 TO nback
         L = 12
         mask = Two(Ua)
         spag_nextblock_1 = 4
      CASE (4)
!
!     READ EIGR CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
         CALL read(*340,*200,Dpool,Buf,18,0,flag)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 180     CALL write(Eed,Buf,12,0)
         CALL write(Eed,Buf(14),6,0)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 200     CALL write(Eed,0,0,1)
         GOTO 240
      CASE (5)
!
!     CODE TO CONVERT GRID NO. AND COMPOIENT CODE TO SIL NO.
!     SIL NO. IS IN A SET FOR EIGR, EIGB - IN D SET FOR EIGC.
!     WRITE DATA ON EED.
!
         IF ( Buf(L)==0 ) GOTO nback
         IF ( in/=0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = Usetd
         CALL open(*320,Usetd,Z(Buf3),Rdrew)
         CALL fwdrec(*340,Usetd)
         iusetd = Neqdyn + 2
         CALL read(*340,*220,Usetd,Z(iusetd),Buf3-iusetd,1,n)
         CALL mesage(-8,0,Nam)
 220     CALL close(Usetd,Clsrew)
         in = 1
         spag_nextblock_1 = 6
      CASE (6)
         imsg(1) = Buf(1)
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
 240     CALL close(Eed,Clsrew)
         Mcb(1) = Eed
         CALL wrttrl(Mcb)
!
!
!     (2) PRECESS TFL FILE
!     ====================
!
!     SELECT PACK OR NO-PACK LOGIC
!
 260     pack = .TRUE.
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
         CALL locate(*300,Z(Buf1),Tf,flag)
         Notfl = 0
         file = Tfl
         CALL open(*300,Tfl,Z(Buf2),Wrtrew)
         CALL fname(Tfl,Buf)
         CALL write(Tfl,Buf,2,1)
         Msg(1) = 68
         L = 2
         id = 0
         itfl = Neqdyn + 2
         i = itfl
         isw = 0
         last = 0
         spag_nextblock_1 = 7
      CASE (7)
!
!     READ FIXED SECTION OF TF CARD. CONVERT GRID POINT AND COMP. TO
!     SIL NO. TEST FOR NEW TRANSFER FUNCTION SET.
!
         CALL read(*340,*280,Dpool,Buf,6,0,flag)
         Msg(3) = Buf(1)
         CALL dpdaa
         irow = Buf(2)
         IF ( Buf(1)==id ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Notfl = Notfl + 1
         IF ( id/=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         id = Buf(1)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     SORT TRANSFER EQUATIONS AND WRITE ON TFL ONE RECORD PER TRANSFER
!     FUNCTION SET. FIRST WORD OF RECORD IS SETID.
!
 280     last = 1
         spag_nextblock_1 = 8
      CASE (8)
         CALL write(Tfl,id,1,0)
         IF ( isw==0 ) THEN
            n = i - itfl
            IF ( pack ) CALL sorti(0,0,4,1,Z(itfl),n)
            IF ( .NOT.pack ) CALL sorti2(0,0,5,1,Z(itfl),n)
            CALL write(Tfl,Z(itfl),n,1)
         ELSE
            CALL write(Scr1,0,0,1)
            CALL close(Scr1,Clsrew)
            CALL open(*320,Scr1,Z(Buf2),Rdrew)
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
            GOTO 300
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         DO
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
            IF ( .NOT.pack ) THEN
               Buf(3) = irow
            ELSE
               IF ( Buf(2)>=Jhalf .OR. irow>=Jhalf ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Buf(3) = lshift(Buf(2),Ihalf)
               Buf(3) = orf(Buf(3),irow)
            ENDIF
            IF ( isw/=0 ) THEN
               CALL write(Scr1,Buf(i23),i45,0)
            ELSEIF ( i+i45>Buf3 ) THEN
               isw = 1
               file = Scr1
               CALL open(*320,Scr1,Z(Buf3),Wrtrew)
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
            CALL read(*340,*320,Dpool,Buf(2),5,0,flag)
            IF ( Buf(2)==-1 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL dpdaa
         ENDDO
 300     CALL close(Dpool,Clsrew)
         RETURN
!
!     FATAL ERRORS
!
 320     n = -1
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 340     n = -2
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(n,file,Nam)
         spag_nextblock_1 = 11
      CASE (11)
         WRITE (Nout,99001) Ihalf , Buf(2) , irow
99001    FORMAT ('0*** COLUMN OR ROW SIL NO. EXCEEDS',I3,' BITS WORD ','PACKING LIMIT',2I9)
         CALL mesage(-37,Nam,Nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd5
