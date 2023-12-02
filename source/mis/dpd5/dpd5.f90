!*==dpd5.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd5
   USE c_bitpos
   USE c_blank
   USE c_dpdcom
   USE c_machin
   USE c_names
   USE c_setup
   USE c_system
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
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
         file = dpool
         CALL preloc(*320,z(buf1),dpool)
!
         file = eed
         CALL open(*260,eed,z(buf2),wrtrew)
         file = dpool
         CALL fname(eed,buf)
         CALL write(eed,buf,2,1)
         in = 0
         DO j = 2 , 7
            mcb(j) = 0
         ENDDO
         l = 12
         msg(1) = 75
!
!     LOCATE EIGB CARDS IN DYNAMICS POOL. IF PRESENT, TURN NOEED FLAG
!     OFF, WRITE ID ON EED AND TURN ON TRAILER BIT.
!
         CALL locate(*60,z(buf1),eigb,flag)
         noeed = 1
         CALL write(eed,eigb,2,0)
         CALL write(eed,0,1,0)
         j = (eigb(2)-1)/16
         k = eigb(2) - 16*j
         mcb(j+2) = orf(mcb(j+2),two(k+16))
         ASSIGN 20 TO nback
         l = 12
         mask = two(ua)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ EIGB CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
         CALL read(*340,*40,dpool,buf,18,0,flag)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 20      CALL write(eed,buf,12,0)
         CALL write(eed,buf(14),6,0)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL write(eed,0,0,1)
!
!     LOCATE EIGC CARDS IN DYNAMICS POOL. IF PRESENT, TURN OFF NOEED
!     FLAG, WRITE ID ON EED AND TURN ON TRL BIT.
!
 60      CALL locate(*120,z(buf1),eigc,flag)
         noeed = 1
         CALL write(eed,eigc,2,0)
         CALL write(eed,0,1,0)
         j = (eigc(2)-1)/16
         k = eigc(2) - 16*j
         mcb(j+2) = orf(mcb(j+2),two(k+16))
         ASSIGN 80 TO nback
         l = 6
         mask = two(ud)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ EIGC CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
         CALL read(*340,*100,dpool,buf,10,0,flag)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      CALL write(eed,buf,7,0)
         CALL write(eed,buf(8),3,0)
         DO
            CALL read(*340,*340,dpool,buf,7,0,flag)
            CALL write(eed,buf,7,0)
            IF ( buf(1)==-1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 100     CALL write(eed,0,0,1)
!
!     LOCATE EIGP CARDS. IF PRESENT, TURN NOEED FLAG OFF,
!     WRITE ID ON EED AND TURN ON TRAILER BIT. COPY DATA ON EED.
!
 120     CALL locate(*160,z(buf1),eigp,flag)
         noeed = 1
         CALL write(eed,eigp,2,0)
         CALL write(eed,0,1,0)
         j = (eigp(2)-1)/16
         k = eigp(2) - 16*j
         mcb(j+2) = orf(mcb(j+2),two(k+16))
         DO
            CALL read(*340,*140,dpool,buf,4,0,flag)
            CALL write(eed,buf,4,0)
         ENDDO
 140     CALL write(eed,0,0,1)
!
!     LOCATE EIGR CARDS IN DYNAMICS POOL. IF PRESENT, TURN OFF NOEED
!     FLAG, WRITE ID ON EED AND TURN ON TRL BIT.
!
 160     CALL locate(*240,z(buf1),eigr,flag)
         noeed = 1
         CALL write(eed,eigr,2,0)
         CALL write(eed,0,1,0)
         j = (eigr(2)-1)/16
         k = eigr(2) - 16*j
         mcb(j+2) = orf(mcb(j+2),two(k+16))
         ASSIGN 180 TO nback
         l = 12
         mask = two(ua)
         spag_nextblock_1 = 4
      CASE (4)
!
!     READ EIGR CARDS. IF GRID NO. IS PRESENT, CONVERT TO SIL VALUE.
!     WRITE DATA ON EED.
!
         CALL read(*340,*200,dpool,buf,18,0,flag)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 180     CALL write(eed,buf,12,0)
         CALL write(eed,buf(14),6,0)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 200     CALL write(eed,0,0,1)
         GOTO 240
      CASE (5)
!
!     CODE TO CONVERT GRID NO. AND COMPOIENT CODE TO SIL NO.
!     SIL NO. IS IN A SET FOR EIGR, EIGB - IN D SET FOR EIGC.
!     WRITE DATA ON EED.
!
         IF ( buf(l)==0 ) GOTO nback
         IF ( in/=0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = usetd
         CALL open(*320,usetd,z(buf3),rdrew)
         CALL fwdrec(*340,usetd)
         iusetd = neqdyn + 2
         CALL read(*340,*220,usetd,z(iusetd),buf3-iusetd,1,n)
         CALL mesage(-8,0,nam)
 220     CALL close(usetd,clsrew)
         in = 1
         spag_nextblock_1 = 6
      CASE (6)
         imsg(1) = buf(1)
         imsg(2) = buf(l)
         CALL dpdaa
         nusetd = iusetd + buf(l) - 1
         buf(l) = 0
         DO j = iusetd , nusetd
            IF ( andf(z(j),mask)/=0 ) buf(l) = buf(l) + 1
         ENDDO
         IF ( andf(z(nusetd),mask)/=0 ) GOTO nback
         nogo = 1
         CALL mesage(30,107,imsg)
         GOTO nback
!
!     COMPLETE EIG CARD PROCESSING.
!
 240     CALL close(eed,clsrew)
         mcb(1) = eed
         CALL wrttrl(mcb)
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
         IF ( ihalf<=16 ) THEN
            pack = .FALSE.
            i45 = 5
            i23 = 2
         ENDIF
!
!     OPEN TFL. WRITE HEADER. INITIALIZE TO READ TF CARDS.
!
         DO j = 2 , 7
            mcb(j) = 0
         ENDDO
         CALL locate(*300,z(buf1),tf,flag)
         notfl = 0
         file = tfl
         CALL open(*300,tfl,z(buf2),wrtrew)
         CALL fname(tfl,buf)
         CALL write(tfl,buf,2,1)
         msg(1) = 68
         l = 2
         id = 0
         itfl = neqdyn + 2
         i = itfl
         isw = 0
         last = 0
         spag_nextblock_1 = 7
      CASE (7)
!
!     READ FIXED SECTION OF TF CARD. CONVERT GRID POINT AND COMP. TO
!     SIL NO. TEST FOR NEW TRANSFER FUNCTION SET.
!
         CALL read(*340,*280,dpool,buf,6,0,flag)
         msg(3) = buf(1)
         CALL dpdaa
         irow = buf(2)
         IF ( buf(1)==id ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         notfl = notfl + 1
         IF ( id/=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         id = buf(1)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     SORT TRANSFER EQUATIONS AND WRITE ON TFL ONE RECORD PER TRANSFER
!     FUNCTION SET. FIRST WORD OF RECORD IS SETID.
!
 280     last = 1
         spag_nextblock_1 = 8
      CASE (8)
         CALL write(tfl,id,1,0)
         IF ( isw==0 ) THEN
            n = i - itfl
            IF ( pack ) CALL sorti(0,0,4,1,z(itfl),n)
            IF ( .NOT.pack ) CALL sorti2(0,0,5,1,z(itfl),n)
            CALL write(tfl,z(itfl),n,1)
         ELSE
            CALL write(scr1,0,0,1)
            CALL close(scr1,clsrew)
            CALL open(*320,scr1,z(buf2),rdrew)
            ifile(1) = scr2
            ifile(2) = scr3
            ifile(3) = scr4
            n = buf3 - itfl
            IF ( pack ) CALL sorti(scr1,tfl,4,1,z(itfl),n)
            IF ( .NOT.pack ) CALL sorti2(scr1,tfl,5,1,z(itfl),n)
            CALL close(scr1,clsrew)
         ENDIF
         i = itfl
         id = buf(1)
         isw = 0
         IF ( last/=0 ) THEN
!
!     HERE WHEN ALL TRANSFER FUNCTION SETS COMPLETE.
!     CLOSE FILE AND WRITE TRAILER.
!
            CALL close(tfl,clsrew)
            mcb(2) = notfl
            mcb(1) = tfl
            CALL wrttrl(mcb)
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
               buf(3) = irow
            ELSE
               IF ( buf(2)>=jhalf .OR. irow>=jhalf ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               buf(3) = lshift(buf(2),ihalf)
               buf(3) = orf(buf(3),irow)
            ENDIF
            IF ( isw/=0 ) THEN
               CALL write(scr1,buf(i23),i45,0)
            ELSEIF ( i+i45>buf3 ) THEN
               isw = 1
               file = scr1
               CALL open(*320,scr1,z(buf3),wrtrew)
               n = i - itfl
               CALL write(scr1,z(itfl),n,0)
               CALL write(scr1,buf(i23),i45,0)
            ELSE
               DO j = i23 , 6
                  z(i) = buf(j)
                  i = i + 1
               ENDDO
            ENDIF
!
!     READ GRID POINT, COMP AND VALUES. CONVERT POINT AND COMP. TO SIL
!     NO. STORE IN CORE. IF SPILL, WRITE ON SCR1.
!
            CALL read(*340,*320,dpool,buf(2),5,0,flag)
            IF ( buf(2)==-1 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL dpdaa
         ENDDO
 300     CALL close(dpool,clsrew)
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
         CALL mesage(n,file,nam)
         spag_nextblock_1 = 11
      CASE (11)
         WRITE (nout,99001) ihalf , buf(2) , irow
99001    FORMAT ('0*** COLUMN OR ROW SIL NO. EXCEEDS',I3,' BITS WORD ','PACKING LIMIT',2I9)
         CALL mesage(-37,nam,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd5
