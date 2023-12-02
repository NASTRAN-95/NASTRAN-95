!*==dpd4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd4
   USE c_bitpos
   USE c_blank
   USE c_dpdcom
   USE c_names
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , flag , i , id , ii , iii , incore , inolin , ipoint , iset , itic , iusetd , j , k , k1 , k2 , kk , kkk ,      &
            & kset , mskud , mskue , n , nn , nnolin , notic , notstp , nset , nusetd , nwdin
   INTEGER , SAVE :: nolinr
   EXTERNAL andf , close , dpdaa , fname , fwdrec , locate , mesage , open , preloc , read , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DPD4 ASSEMBLES THE NON-LINEAR FORCING TABLE (NLFT)
!     AND THE TRANSIENT RESPONSE LIST (TRL).
!
   !>>>>EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
   DATA nolinr/7/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE POINTERS. OPEN SCR1. OPEN DYNAMICS POOL.
!
         inolin = neqdyn + 2
         j = inolin
         mskud = two(ud)
         mskue = two(ue)
         incore = 0
         ii = 1
         i = 1
         msg(1) = 67
         CALL preloc(*260,z(buf1),dpool)
         CALL open(*260,scr1,z(buf2),wrtrew)
         ineq = 0
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOCATE NOLINI CARD. IF PRESENT, TURN NONLFT FLAG OFF,
!
         CALL locate(*20,z(buf1),nolin(i),flag)
         nonlft = 1
         nwdin = nolin(i+2)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ A NOLINI CARD. CONVERT POINTS ON CARD TO SIL NOS.
!     STORE DATA IN CORE. IF SPILL, WRITE ON SCRATCH FILE.
!
         CALL read(*280,*20,dpool,buf,nwdin,0,flag)
         msg(3) = 100000000*ii + buf(1)
         IF ( ii<5 ) THEN
!                             NOLIN5,NFTUBE,NOLIN6
            iii = ii
            IF ( buf(6)>=10 ) THEN
               iii = ii + 4
               buf(6) = buf(6) - 10
            ENDIF
            IF ( ii==2 ) THEN
               IF ( buf(8)>=10 ) THEN
                  buf(8) = buf(8) - 10
                  IF ( iii==2 ) iii = 10
                  IF ( iii==6 ) iii = 9
               ENDIF
            ENDIF
         ELSEIF ( ii<6 ) THEN
!
!     SPECIAL HANDLING OF NOLIN5 CARD
!     CARD FORMAT AS RECEIVED FROM IFP
!        SID  AA   AB   FAB  EA/TEA  EB/TEB  ALPA/TALPA  ALPB/TALPB
!        GA1  GA2  GA3  GA4  GB1     GB2     GB3         GB4
!
!     WE CONVERT THIS CARD INTO THE FOLLOWING 6-WORD ENTRY FORMAT
!
!        SID  12  SILA1  AA          SILA2  AB
!        SID  12  SILA3  FAB         SIL4   0
!        SID  12  SILB1  EA/TEA      SILB2  EB/TEB
!        SID  12  SILB3  ALPA/TALPA  SILB4  ALPB/TALPB
!
            l = 23
            kk = 16
            DO k = 1 , 8
               buf(l+1) = 0
               buf(l) = buf(kk)
               IF ( buf(l)/=0 ) CALL dpdaa
               kk = kk - 1
               l = l - 2
            ENDDO
            buf(24) = buf(8)
            buf(22) = buf(7)
            buf(18) = buf(6)
            buf(16) = buf(5)
            buf(12) = 0
            buf(10) = buf(4)
            buf(6) = buf(3)
            buf(4) = buf(2)
            buf(3) = buf(9)
            buf(5) = buf(11)
            buf(9) = buf(13)
            buf(11) = buf(15)
            buf(17) = buf(19)
            DO k = 1 , 24 , 6
               buf(k) = buf(1)
               buf(k+1) = 12
            ENDDO
            nn = 24
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ii==6 ) THEN
!
            l = 7
            buf(7) = buf(2)
            buf(8) = 1
            CALL dpdaa
            buf(3) = buf(7)
            buf(7) = buf(3)
            buf(8) = 1
            CALL dpdaa
            buf(5) = buf(7)
            buf(6) = buf(5)
            buf(2) = 11
            msg(3) = buf(1)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            iii = 13
            IF ( buf(6)>=10 ) THEN
               iii = 14
               buf(6) = buf(6) - 10
            ENDIF
         ENDIF
         l = 2
         CALL dpdaa
         buf(3) = buf(2)
         l = 5
         CALL dpdaa
         l = 7
         IF ( ii==2 ) CALL dpdaa
         buf(6) = buf(7)
         buf(2) = iii
         spag_nextblock_1 = 4
      CASE (4)
         nn = 6
         spag_nextblock_1 = 5
      CASE (5)
         IF ( incore/=0 ) THEN
            CALL write(scr1,buf,nn,0)
         ELSEIF ( j+nn>=buf2 ) THEN
            CALL write(scr1,z(inolin),j-inolin,0)
            incore = 1
            CALL write(scr1,buf,nn,0)
         ELSE
            DO k = 1 , nn
               z(j) = buf(k)
               j = j + 1
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     HERE WHEN ALL CARDS OF CURRENT TYPE HAVE BEEN READ.
!     TEST FOR ALL CARDS READ.
!
 20      i = i + 3
         ii = ii + 1
         IF ( ii<=nolinr ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL write(scr1,0,0,1)
         CALL close(scr1,clsrew)
         IF ( nonlft==-1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SORT THE DATA ON SET ID.
!
         IF ( incore/=0 ) THEN
            CALL open(*260,scr1,z(buf2),rdrew)
            CALL read(*280,*40,scr1,z,buf1,1,n)
            CALL mesage(-8,0,nam)
         ELSE
            nnolin = j - 6
            n = j - inolin
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      CALL close(scr1,clsrew)
         inolin = 1
         nnolin = n - 5
         spag_nextblock_1 = 6
      CASE (6)
         CALL sort(0,0,6,1,z(inolin),n)
!
!     READ USETD INTO CORE.
!
         file = usetd
         CALL open(*260,usetd,z(buf2),rdrew)
         CALL fwdrec(*280,usetd)
         iusetd = nnolin + 7
         CALL read(*280,*60,usetd,z(iusetd),buf2-iusetd,1,n)
         CALL mesage(-8,0,nam)
 60      CALL close(usetd,clsrew)
!
!     OPEN THE NLFT. WRITE SET IDS IN HEADER RECORD.
!
         file = nlft
         CALL open(*80,nlft,z(buf2),wrtrew)
         CALL fname(nlft,buf)
         CALL write(nlft,buf,2,0)
         z(nnolin+6) = 0
         DO i = inolin , nnolin , 6
            IF ( z(i+6)/=z(i) ) CALL write(nlft,z(i),1,0)
         ENDDO
         CALL write(nlft,0,0,1)
!
!     WRITE ONE RECORD PER SET. WITHIN EACH SET, SORT DATA ON SIL NO.
!     CONVERT SIL NOS. TO SIL NOS. IN UD AND UE SETS
!
         i = inolin
         DO
            j = i
            DO WHILE ( z(i+6)==z(i) )
               i = i + 6
            ENDDO
            n = i + 6 - j
!
! ... THE FOLLOWING SORT WAS REMOVED DUE TO THE INSTALLATION OF NOLIN5
!     CALL SORT (0,0,6,3,Z(J),N)
!
!WKBR SPR94005 6/94   DO 1387 KC = J,I,6
            DO k = j , i , 6
               buf(1) = z(k+1)
               buf(2) = z(k+2)
               buf(4) = z(k+3)
               buf(5) = z(k+4)
               buf(8) = z(k+5)
               buf(9) = 0
               DO kk = 2 , 8 , 3
                  IF ( kk<8 .OR. buf(1)==2 .OR. buf(1)==6 .OR. buf(1)==9 .OR. buf(1)==10 .OR. kk/=8 ) THEN
                     k1 = 0
                     k2 = 0
                     nusetd = iusetd + buf(kk) - 1
                     IF ( nusetd>=iusetd ) THEN
                        DO kkk = iusetd , nusetd
                           buf(10) = z(kkk)
                           IF ( andf(buf(10),mskud)/=0 ) k1 = k1 + 1
                           IF ( andf(buf(10),mskue)/=0 ) k2 = k2 + 1
                        ENDDO
                     ENDIF
                     buf(kk) = k1
                     buf(kk+1) = k2
                     IF ( nusetd>=iusetd ) THEN
                        IF ( andf(buf(10),mskue)==0 ) buf(kk+1) = 0
                        IF ( andf(buf(10),mskud)==0 ) THEN
                           nogo = 1
                           buf(1) = z(k)
                           buf(2) = k1
                           CALL mesage(30,93,buf)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
               buf(7) = buf(8)
               buf(8) = buf(9)
               CALL write(nlft,buf,8,0)
            ENDDO
            CALL write(nlft,0,0,1)
            i = i + 6
            IF ( z(i)==0 ) THEN
!
!     CLOSE FILE AND WRITE TRAILER.
!
               CALL close(nlft,clsrew)
               mcb(1) = nlft
               mcb(2) = (nnolin-inolin)/6 + 1
               CALL wrttrl(mcb)
               IF ( incore/=0 ) ineq = 0
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 80      nonlft = -1
         spag_nextblock_1 = 7
      CASE (7)
!
!     LOCATE TIC CARDS IN DYNAMICS POOL.
!
         notrl = -1
         notic = 0
         notstp = 0
         CALL locate(*240,z(buf1),tic,flag)
         notrl = 1
!
!     OPEN SCR1. INITIALIZE TO READ TIC CARDS.
!
         file = scr1
         CALL open(*260,scr1,z(buf2),wrtrew)
         itic = neqdyn + 2
         nset = buf3 - 1
         j = nset
         l = 2
         msg(1) = 69
         id = 0
         SPAG_Loop_1_1: DO
!
!     READ A TIC CARD. IF SET ID IS DIFFERENT, STORE IT IN LIST.
!     IF NOT FIRST CARD, SORT DATA ON SIL NO. AND WRITE IT IN SCR1.
!
            CALL read(*280,*100,dpool,buf,5,0,flag)
            IF ( buf(1)/=id ) THEN
               IF ( id/=0 ) THEN
                  n = i - itic
                  CALL sort(0,0,3,1,z(itic),n)
                  CALL write(scr1,z(itic),n,1)
               ENDIF
               id = buf(1)
               z(j) = id
               j = j - 1
               i = itic
               msg(3) = id
            ENDIF
!
!     CONVERT POINT AND COMPONENT TO SIL NO.
!     STORE SIL NO., UO, VO IN CORE.
!
            CALL dpdaa
            z(i) = buf(2)
            z(i+1) = buf(4)
            z(i+2) = buf(5)
            i = i + 3
            IF ( i>=j ) THEN
               CALL mesage(-8,0,nam)
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     HERE WHEN LAST CARD READ - SORT AND WRITE LAST RECORD.
!
 100     n = i - itic
         CALL sort(0,0,3,1,z(itic),n)
         CALL write(scr1,z(itic),n,1)
         CALL close(scr1,clsrew)
         iset = j + 1
!
!     OPEN TRL. WRITE SET IDS IN HEADER.
!
         file = trl
         CALL open(*220,trl,z(buf2),wrtrew)
         CALL fname(trl,buf)
         n = nset - iset + 1
         buf(3) = n
         notic = n
         CALL write(trl,buf,3,0)
         i = iset
         j = nset
         SPAG_Loop_1_2: DO
            id = z(j)
            z(j) = z(i)
            z(i) = id
            i = i + 1
            j = j - 1
            IF ( i>=j ) THEN
               CALL write(trl,z(iset),n,0)
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 8
      CASE (8)
!
!     READ USETD INTO CORE.
!     COMPUTE NO. OF POINTS UN UD SET. WRITE NO. AS LAST WORD OF HEADER.
!
         file = usetd
         CALL open(*260,usetd,z(buf3),rdrew)
         CALL fwdrec(*280,usetd)
         iusetd = 1
         ineq = 0
         CALL read(*280,*120,usetd,z(iusetd),buf3-iusetd,1,n)
         CALL mesage(-8,0,nam)
 120     CALL close(usetd,clsrew)
         nusetd = iusetd + n - 1
         k = 0
         DO i = iusetd , nusetd
            IF ( andf(z(i),mskud)/=0 ) k = k + 1
         ENDDO
         CALL write(trl,k,1,1)
         IF ( notic==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ SCR1. CONVERT SIL NO. TO AN SIL NO. IN THE D-SET.
!     WRITE TRL ONE RECORD PER SET.
!
         file = scr1
         kset = iset
         CALL open(*260,scr1,z(buf3),rdrew)
         spag_nextblock_1 = 9
      CASE (9)
         k = 0
         ipoint = iusetd
         DO
            CALL read(*160,*140,scr1,buf,3,0,flag)
            nusetd = iusetd + buf(1) - 1
            DO i = ipoint , nusetd
               IF ( andf(z(i),mskud)/=0 ) k = k + 1
            ENDDO
            buf(1) = k
            IF ( andf(z(nusetd),mskud)==0 ) THEN
               nogo = 1
               CALL mesage(30,133,z(kset))
            ENDIF
            CALL write(trl,buf,3,0)
            ipoint = nusetd + 1
         ENDDO
 140     CALL write(trl,0,0,1)
         kset = kset + 1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 160     CALL close(scr1,clsrew)
!
!     IF TSTEP CARDS PRESENT, COPY THEM ONTO TRL.
!
         CALL locate(*180,z(buf1),tstep,flag)
         spag_nextblock_1 = 10
      CASE (10)
         SPAG_Loop_1_3: DO
            CALL read(*280,*180,dpool,buf,1,0,flag)
            notstp = notstp + 1
            CALL write(trl,buf,1,0)
            DO
               CALL read(*280,*300,dpool,buf,3,0,flag)
               IF ( buf(1)==-1 ) THEN
                  CALL write(trl,0,0,1)
                  CYCLE SPAG_Loop_1_3
               ELSE
                  CALL write(trl,buf,3,0)
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
!
!     CLOSE FILES AND WRITE TRAILER.
!
 180     CALL close(trl,clsrew)
         mcb(1) = trl
         mcb(2) = notic
         mcb(3) = notstp
         CALL wrttrl(mcb)
 200     CALL close(dpool,clsrew)
         RETURN
!
 220     notrl = -1
         GOTO 200
!
!     HERE IF NO TIC CARDS - LOCATE TSTEP CARDS IN DYNAMICS POOL.
!     IF ABSENT, RETURN. OTHERWISE OPEN TRL AND WRTIE HEADER.
!
 240     CALL locate(*200,z(buf1),tstep,flag)
         notrl = 1
         file = trl
         CALL open(*220,trl,z(buf2),wrtrew)
         CALL fname(trl,buf)
         buf(3) = 0
         CALL write(trl,buf,3,0)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     FATAL FILE ERRORS
!
 260     n = -1
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 280     n = -2
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 300     n = -3
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd4
