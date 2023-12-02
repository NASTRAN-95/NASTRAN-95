!*==dpd2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd2
   USE c_blank
   USE c_dpdcom
   USE c_names
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(20) :: bufr
   INTEGER , DIMENSION(3) :: bufx
   INTEGER :: file , flag , i , ibuf , id , idarea , idload , iemf , ii , iii , itabl , itemp , j , j2 , jj , k , kk , m , n ,      &
            & ncore , ngrid , nlist , nn , nodld , nx
   INTEGER , DIMENSION(4) :: scr
   REAL , DIMENSION(1) :: zz
   EXTERNAL close , dpdaa , fname , fwdrec , locate , mesage , open , preloc , read , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DPD2 ASSEMBLES THE DYNAMIC LOADS TABLE (DLT).
!
   !>>>>EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid) , (Scr1,Scr(1)) , (Buf2,Bufx(1))
!
!     OPEN DYNAMICS POOL. SET POINTERS TO LOOP THRU DAREA, DELAY
!     AND DPHASE TABLES.
!
         file = dpool
         CALL preloc(*120,z(buf1),dpool)
         ii = 1
         iii = 1
         itabl = neqdyn + 2
         l = 2
         j = buf4 - 1
         msg(1) = 66
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOCATE CARD TYPE. IF PRESENT--
!     STORE POINTER TO 1ST TABLE NO. IN LOADS TABLE, OPEN SCRATCH FILE
!     FOR TABLES, SET ID = 0.
!
         CALL locate(*40,z(buf1),loads(ii),flag)
         loads(ii+2) = j
         file = scr(iii)
         CALL open(*120,file,z(buf2),wrtrew)
         id = 0
         SPAG_Loop_1_1: DO
!
!     READ A CARD. IF TABLE NO. IS DIFFERENT, STORE TABLE NO. IN TABLE
!     LIST.  IF NOT FIRST CARD, SORT TABLE ON SIL NO. AND WRITE ON
!     SCRATCH FILE.
!
            CALL read(*140,*20,dpool,buf,4,0,flag)
            IF ( buf(1)/=id ) THEN
               IF ( id/=0 ) THEN
                  n = i - itabl
                  CALL sort(0,0,2,1,z(itabl),n)
                  CALL write(file,z(itabl),n,1)
               ENDIF
               id = buf(1)
               z(j) = id
               j = j - 1
               i = itabl
               msg(3) = id
            ENDIF
!
!     CONVERT POINT AND COMPONENT TO SIL NO.
!     STORE SIL NO. AND VALUE IN CORE.
!
            CALL dpdaa
            z(i) = buf(2)
            z(i+1) = buf(4)
            i = i + 2
            IF ( i>=j ) THEN
               CALL mesage(-8,0,nam)
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     HERE WHEN LAST CARD OF CURRENT TYPE HAS BEEN READ--
!     SORT AND WRITE LAST RECORD. CLOSE SCRATCH FILE.  STORE
!     NUMBER OF TABLES IN TABLE LIST. TEST FOR ALL CARD TYPES PROCESSED.
!
 20      n = i - itabl
         CALL sort(0,0,2,1,z(itabl),n)
         CALL write(file,z(itabl),n,1)
         CALL close(file,clsrew)
         loads(ii+3) = loads(ii+2) - j
 40      ii = ii + 4
         iii = iii + 1
         IF ( iii<=3 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SET POINTERS TO LOOP THRU RLOAD1,2 AND TLOAD1,2 CARDS
!
         ncore = j
         j = 1
         iii = 1
         ineq = 0
         spag_nextblock_1 = 3
      CASE (3)
!
!     LOCATE A CARD TYPE. IF PRESENT--
!     READ ALL CARDS OF TYPE INTO CORE.
!
         CALL locate(*60,z(buf1),loads(ii),flag)
         m = loads(ii+2)
         SPAG_Loop_1_2: DO
            z(j) = iii
            CALL read(*140,*60,dpool,z(j+1),m,0,flag)
            j = j + 11
            IF ( j>=ncore ) THEN
               CALL mesage(-8,0,nam)
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
!
!     TEST FOR ALL CARD TYPES PROCESSED.
!     IF SO, SORT CARDS ON LOAD SET ID.
!
 60      ii = ii + 4
         iii = iii + 1
         IF ( iii<=4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = j - 1
         IF ( n/=0 ) THEN
!
            CALL sort(0,0,11,2,z,n)
            nlist = j - 11
!
!     LOCATE DLOAD CARDS ON DYNAMICS POOL.
!     IF PRESENT READ INTO CORE. SORT EACH DLOAD CARD ON REFERENCED SET
!     ID.
!
            nodld = 0
            CALL locate(*80,z(buf1),dload,flag)
            idload = j
            i = idload
            j = i
         ELSE
            CALL close(dpool,clsrew)
            RETURN
         ENDIF
         SPAG_Loop_1_3: DO
            CALL read(*140,*80,dpool,z(j+1),2,0,flag)
            j = j + 3
            nodld = nodld + 1
            DO
               CALL read(*140,*160,dpool,z(j),2,0,flag)
               IF ( z(j)==-1 ) THEN
                  n = j - (i+3)
                  CALL sort(0,0,2,2,z(i+3),n)
!
!     CHECK FOR DLOAD SET ID UNIQUENESS
!
                  DO kk = 2 , n , 2
                     jj = i + 2 + kk
                     IF ( kk<n ) THEN
                        IF ( z(jj)==z(jj+2) ) THEN
                           nogo = 1
                           msg(2) = z(i+1)
                           msg(3) = z(jj)
                           CALL mesage(30,135,msg(2))
                        ENDIF
                     ENDIF
                  ENDDO
                  z(i) = n + 2
                  i = j
                  CYCLE SPAG_Loop_1_3
               ELSE
                  j = j + 2
                  IF ( j>=ncore ) CALL mesage(-8,0,nam)
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
 80      CALL close(dpool,clsrew)
!
!     OPEN THE DLT. WRITE NAME IN HEADER RECORD.
!     THEN WRITE NO. OF DLOAD CARDS FOLLOWED BY DLOAD SET IDS.
!     THEN WRITE SET IDS FOR EACH RECORD OF THE DLT (FOLLOWING DLOAD
!     RECORD)
!
         file = dlt
         CALL open(*100,dlt,z(buf1),wrtrew)
         CALL fname(dlt,buf)
         buf(3) = nodld
         CALL write(dlt,buf,3,0)
         IF ( nodld/=0 ) THEN
            i = idload
            j = 1
            SPAG_Loop_1_4: DO
               CALL write(dlt,z(i+1),1,0)
               i = i + z(i) + 1
               j = j + 1
               IF ( j>nodld ) THEN
!
!     CHECK DLOAD SID  VS  RLOAD1,2 AND TLOAD1,2 FOR UNIQUENESS
!
                  i = idload
                  DO jj = 1 , nodld
                     itemp = z(i+1)
                     DO kk = 1 , nlist , 11
                        IF ( itemp==z(kk+1) ) THEN
                           nogo = 1
                           msg(2) = itemp
                           CALL mesage(30,136,msg(2))
                        ENDIF
                     ENDDO
                     i = i + z(i) + 1
                  ENDDO
                  EXIT SPAG_Loop_1_4
               ENDIF
            ENDDO SPAG_Loop_1_4
         ENDIF
         DO i = 1 , nlist , 11
            buf(1) = z(i+1)
!
!     CHECK FOR UNIQUE SET IDS ON TLOAD1,2 AND RLOAD1,2 CARDS  THEN WRIT
!
            IF ( i<nlist ) THEN
               IF ( z(i+1)==z(i+12) ) THEN
                  nogo = 1
                  msg(2) = itemp
                  CALL mesage(30,136,msg(2))
               ENDIF
            ENDIF
            CALL write(dlt,buf,1,0)
         ENDDO
         CALL write(dlt,0,0,1)
!
!     IF DLOAD CARDS PRESENT, WRITE THE DLOAD RECORD.
!
         IF ( nodld==0 ) THEN
!
!     INITIALIZE TO LOOP THRU ALL LOAD SETS. THE REMAINDER OF THE DLT
!     WILL CONSIST OF ONE LOGICAL RECORD PER LOAD SET.
!
            i = 1
         ELSE
            buf(1) = -1
            buf(2) = -1
            i = idload
            j = 1
            SPAG_Loop_1_5: DO
               n = z(i)
               CALL write(dlt,z(i+1),n,0)
               CALL write(dlt,buf,2,0)
               i = i + n + 1
               j = j + 1
               IF ( j>nodld ) THEN
                  CALL write(dlt,0,0,1)
                  i = 1
                  EXIT SPAG_Loop_1_5
               ENDIF
            ENDDO SPAG_Loop_1_5
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     WRITE FIXED SECTION OF DLT RECORD.
!
         buf(1) = z(i)
         buf(2) = z(i+2)
!
!     SAVE INFORCED MOTION FLAG ON TLOAD CARDS
!
         IF ( z(i)>=3 .AND. z(i)<=4 ) THEN
            iemf = z(i+4)
            z(i+4) = 0
         ENDIF
         CALL write(dlt,buf,2,0)
         CALL write(dlt,z(i+5),6,0)
!
!     POSITION SCRATCH FILES TO SELECTED TABLES.
!
         idarea = 0
         DO j = 1 , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  buf(2*j-1) = 16777215
!                  16777215 =2**24 - 1
                  k = i + j
                  buf(j+16) = z(k+1)
                  IF ( buf(j+16)/=0 ) THEN
                     jj = loads(4*j-1)
                     nn = loads(4*j)
                     IF ( nn/=0 ) THEN
                        DO nx = 1 , nn
                           IF ( z(jj)==buf(j+16) ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           jj = jj - 1
                        ENDDO
                     ENDIF
                     IF ( ithrml/=1 .OR. j/=1 ) THEN
                        buf(10) = z(i+1)
                        buf(11) = buf(j+16)
                        buf(11) = buf(11) + 100000000*j
                        nogo = 1
                        CALL mesage(30,71,buf(10))
                        buf(j+16) = 0
                     ELSE
                        idarea = -1
                        buf(17) = 0
                     ENDIF
                  ENDIF
               CASE (2)
                  nn = nx - 1
                  file = scr(j)
                  ibuf = bufx(j)
                  CALL open(*120,file,z(ibuf),rdrew)
                  IF ( nn/=0 ) THEN
                     DO nx = 1 , nn
                        CALL fwdrec(*140,file)
                     ENDDO
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     INITIALIZE TABLE READ.
!
         buf(14) = buf(17)
         buf(15) = buf(18)
         buf(16) = buf(19)
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ AN ENTRY FROM APPROPRIATE TABLE/S).
!     IF ALL ENTRIES HAVE BEEN READ, GO TO CLOSE DLT RECORD.
!
         DO j = 1 , 3
            IF ( ithrml==1 .AND. j==1 ) THEN
               IF ( idarea/=0 ) THEN
                  IF ( idarea==-2 ) GOTO 90
                  idarea = -2
                  buf(1) = 1
                  buf(2) = 0
                  buf(14) = 0
               ENDIF
            ENDIF
            IF ( buf(j+13)/=0 ) THEN
               file = scr(j)
               j2 = 2*j
               CALL read(*140,*90,file,buf(j2-1),2,0,flag)
            ENDIF
            CYCLE
 90         buf(2*j-1) = 16777215
            buf(j+13) = 0
         ENDDO
         IF ( buf(1)+buf(3)+buf(5)==3*16777215 ) THEN
!
!     CLOSE DLT RECORD,  CLOSE TABLES AND TEST FOR COMPLETION OF DLT.
!
            CALL write(dlt,0,0,1)
            DO j = 1 , 3
               IF ( buf(j+16)/=0 ) CALL close(scr(j),clsrew)
            ENDDO
            i = i + 11
            IF ( i<=nlist ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CLOSE DLT, WRITE TRAILER AND RETURN.
!
            CALL close(dlt,clsrew)
            mcb(1) = dlt
            mcb(2) = dlt
            CALL wrttrl(mcb)
            nodlt = 1
            GOTO 100
         ELSE
!
!     SELECT MINIMUM SIL NO(S) AND FORMAT OUTPUT.
!
            DO j = 1 , 6
               buf(j+10) = 0
            ENDDO
            buf(7) = 1
            buf(8) = 2
            buf(9) = 3
            IF ( buf(1)>buf(3) ) THEN
!
!     1 .GT. 2--SWITCH 1 AND 2 THEN COMPARE 2 AND 3. IF 2 .GT. 3, SWITCH
!
               k = buf(7)
               buf(7) = buf(8)
               buf(8) = k
               IF ( buf(1)<=buf(5) ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = buf(8)
               buf(8) = buf(9)
               buf(9) = k
            ELSE
!
!     1 .LE. 2--COMPARE 2 TO 3. IF 2 .GT. 3, SWITCH 2 AND 3.
!
               IF ( buf(3)<=buf(5) ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = buf(8)
               buf(8) = buf(9)
               buf(9) = k
            ENDIF
!
!     COMPARE 1 TO 2--IF 1 .GT. 2, SWITCH 1 AND 2.
!
            k = buf(7)
            l = buf(8)
            IF ( buf(2*k-1)>buf(2*l-1) ) THEN
               buf(7) = l
               buf(8) = k
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     PICK UP 1. SET TO READ 1.
!
         k = buf(7)
         buf(10) = buf(2*k-1)
         buf(k+10) = buf(2*k)
         buf(k+13) = k
!
!     IF 1 .EQ. 2, PICK UP 2 AND SET TO READ 2.
!
         l = buf(8)
         IF ( buf(2*k-1)==buf(2*l-1) ) THEN
            buf(l+10) = buf(2*l)
            buf(l+13) = l
!
!     IF 1 .EQ. 2 .EQ. 3, PICK UP 3 AND SET TO READ 3.
!
            m = buf(9)
            IF ( buf(2*l-1)==buf(2*m-1) ) THEN
               buf(m+10) = buf(2*m)
               buf(m+13) = m
            ENDIF
         ENDIF
!
!     WRITE SIL NO., A, TAU, THETA. THEN GO TO READ ANOTHER TABLE
!     ENTRY(S).
!
         IF ( z(i)>=3 .AND. z(i)<=4 ) buf(13) = iemf
         CALL write(dlt,buf(10),4,0)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     RETURN
!
!     FATAL FILE ERRORS
!
 120     n = -1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     n = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 160     n = -3
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd2
