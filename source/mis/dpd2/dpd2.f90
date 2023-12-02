!*==dpd2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd2
   IMPLICIT NONE
   USE C_BLANK
   USE C_DPDCOM
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
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
         file = Dpool
         CALL preloc(*120,Z(Buf1),Dpool)
         ii = 1
         iii = 1
         itabl = Neqdyn + 2
         L = 2
         j = Buf4 - 1
         Msg(1) = 66
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOCATE CARD TYPE. IF PRESENT--
!     STORE POINTER TO 1ST TABLE NO. IN LOADS TABLE, OPEN SCRATCH FILE
!     FOR TABLES, SET ID = 0.
!
         CALL locate(*40,Z(Buf1),Loads(ii),flag)
         Loads(ii+2) = j
         file = scr(iii)
         CALL open(*120,file,Z(Buf2),Wrtrew)
         id = 0
         SPAG_Loop_1_1: DO
!
!     READ A CARD. IF TABLE NO. IS DIFFERENT, STORE TABLE NO. IN TABLE
!     LIST.  IF NOT FIRST CARD, SORT TABLE ON SIL NO. AND WRITE ON
!     SCRATCH FILE.
!
            CALL read(*140,*20,Dpool,Buf,4,0,flag)
            IF ( Buf(1)/=id ) THEN
               IF ( id/=0 ) THEN
                  n = i - itabl
                  CALL sort(0,0,2,1,Z(itabl),n)
                  CALL write(file,Z(itabl),n,1)
               ENDIF
               id = Buf(1)
               Z(j) = id
               j = j - 1
               i = itabl
               Msg(3) = id
            ENDIF
!
!     CONVERT POINT AND COMPONENT TO SIL NO.
!     STORE SIL NO. AND VALUE IN CORE.
!
            CALL dpdaa
            Z(i) = Buf(2)
            Z(i+1) = Buf(4)
            i = i + 2
            IF ( i>=j ) THEN
               CALL mesage(-8,0,Nam)
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     HERE WHEN LAST CARD OF CURRENT TYPE HAS BEEN READ--
!     SORT AND WRITE LAST RECORD. CLOSE SCRATCH FILE.  STORE
!     NUMBER OF TABLES IN TABLE LIST. TEST FOR ALL CARD TYPES PROCESSED.
!
 20      n = i - itabl
         CALL sort(0,0,2,1,Z(itabl),n)
         CALL write(file,Z(itabl),n,1)
         CALL close(file,Clsrew)
         Loads(ii+3) = Loads(ii+2) - j
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
         Ineq = 0
         spag_nextblock_1 = 3
      CASE (3)
!
!     LOCATE A CARD TYPE. IF PRESENT--
!     READ ALL CARDS OF TYPE INTO CORE.
!
         CALL locate(*60,Z(Buf1),Loads(ii),flag)
         m = Loads(ii+2)
         SPAG_Loop_1_2: DO
            Z(j) = iii
            CALL read(*140,*60,Dpool,Z(j+1),m,0,flag)
            j = j + 11
            IF ( j>=ncore ) THEN
               CALL mesage(-8,0,Nam)
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
            CALL sort(0,0,11,2,Z,n)
            nlist = j - 11
!
!     LOCATE DLOAD CARDS ON DYNAMICS POOL.
!     IF PRESENT READ INTO CORE. SORT EACH DLOAD CARD ON REFERENCED SET
!     ID.
!
            nodld = 0
            CALL locate(*80,Z(Buf1),Dload,flag)
            idload = j
            i = idload
            j = i
         ELSE
            CALL close(Dpool,Clsrew)
            RETURN
         ENDIF
         SPAG_Loop_1_3: DO
            CALL read(*140,*80,Dpool,Z(j+1),2,0,flag)
            j = j + 3
            nodld = nodld + 1
            DO
               CALL read(*140,*160,Dpool,Z(j),2,0,flag)
               IF ( Z(j)==-1 ) THEN
                  n = j - (i+3)
                  CALL sort(0,0,2,2,Z(i+3),n)
!
!     CHECK FOR DLOAD SET ID UNIQUENESS
!
                  DO kk = 2 , n , 2
                     jj = i + 2 + kk
                     IF ( kk<n ) THEN
                        IF ( Z(jj)==Z(jj+2) ) THEN
                           Nogo = 1
                           Msg(2) = Z(i+1)
                           Msg(3) = Z(jj)
                           CALL mesage(30,135,Msg(2))
                        ENDIF
                     ENDIF
                  ENDDO
                  Z(i) = n + 2
                  i = j
                  CYCLE SPAG_Loop_1_3
               ELSE
                  j = j + 2
                  IF ( j>=ncore ) CALL mesage(-8,0,Nam)
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
 80      CALL close(Dpool,Clsrew)
!
!     OPEN THE DLT. WRITE NAME IN HEADER RECORD.
!     THEN WRITE NO. OF DLOAD CARDS FOLLOWED BY DLOAD SET IDS.
!     THEN WRITE SET IDS FOR EACH RECORD OF THE DLT (FOLLOWING DLOAD
!     RECORD)
!
         file = Dlt
         CALL open(*100,Dlt,Z(Buf1),Wrtrew)
         CALL fname(Dlt,Buf)
         Buf(3) = nodld
         CALL write(Dlt,Buf,3,0)
         IF ( nodld/=0 ) THEN
            i = idload
            j = 1
            SPAG_Loop_1_4: DO
               CALL write(Dlt,Z(i+1),1,0)
               i = i + Z(i) + 1
               j = j + 1
               IF ( j>nodld ) THEN
!
!     CHECK DLOAD SID  VS  RLOAD1,2 AND TLOAD1,2 FOR UNIQUENESS
!
                  i = idload
                  DO jj = 1 , nodld
                     itemp = Z(i+1)
                     DO kk = 1 , nlist , 11
                        IF ( itemp==Z(kk+1) ) THEN
                           Nogo = 1
                           Msg(2) = itemp
                           CALL mesage(30,136,Msg(2))
                        ENDIF
                     ENDDO
                     i = i + Z(i) + 1
                  ENDDO
                  EXIT SPAG_Loop_1_4
               ENDIF
            ENDDO SPAG_Loop_1_4
         ENDIF
         DO i = 1 , nlist , 11
            Buf(1) = Z(i+1)
!
!     CHECK FOR UNIQUE SET IDS ON TLOAD1,2 AND RLOAD1,2 CARDS  THEN WRIT
!
            IF ( i<nlist ) THEN
               IF ( Z(i+1)==Z(i+12) ) THEN
                  Nogo = 1
                  Msg(2) = itemp
                  CALL mesage(30,136,Msg(2))
               ENDIF
            ENDIF
            CALL write(Dlt,Buf,1,0)
         ENDDO
         CALL write(Dlt,0,0,1)
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
            Buf(1) = -1
            Buf(2) = -1
            i = idload
            j = 1
            SPAG_Loop_1_5: DO
               n = Z(i)
               CALL write(Dlt,Z(i+1),n,0)
               CALL write(Dlt,Buf,2,0)
               i = i + n + 1
               j = j + 1
               IF ( j>nodld ) THEN
                  CALL write(Dlt,0,0,1)
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
         Buf(1) = Z(i)
         Buf(2) = Z(i+2)
!
!     SAVE INFORCED MOTION FLAG ON TLOAD CARDS
!
         IF ( Z(i)>=3 .AND. Z(i)<=4 ) THEN
            iemf = Z(i+4)
            Z(i+4) = 0
         ENDIF
         CALL write(Dlt,Buf,2,0)
         CALL write(Dlt,Z(i+5),6,0)
!
!     POSITION SCRATCH FILES TO SELECTED TABLES.
!
         idarea = 0
         DO j = 1 , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  Buf(2*j-1) = 16777215
!                  16777215 =2**24 - 1
                  k = i + j
                  Buf(j+16) = Z(k+1)
                  IF ( Buf(j+16)/=0 ) THEN
                     jj = Loads(4*j-1)
                     nn = Loads(4*j)
                     IF ( nn/=0 ) THEN
                        DO nx = 1 , nn
                           IF ( Z(jj)==Buf(j+16) ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           jj = jj - 1
                        ENDDO
                     ENDIF
                     IF ( Ithrml/=1 .OR. j/=1 ) THEN
                        Buf(10) = Z(i+1)
                        Buf(11) = Buf(j+16)
                        Buf(11) = Buf(11) + 100000000*j
                        Nogo = 1
                        CALL mesage(30,71,Buf(10))
                        Buf(j+16) = 0
                     ELSE
                        idarea = -1
                        Buf(17) = 0
                     ENDIF
                  ENDIF
                  CYCLE
               CASE (2)
                  nn = nx - 1
                  file = scr(j)
                  ibuf = bufx(j)
                  CALL open(*120,file,Z(ibuf),Rdrew)
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
         Buf(14) = Buf(17)
         Buf(15) = Buf(18)
         Buf(16) = Buf(19)
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ AN ENTRY FROM APPROPRIATE TABLE/S).
!     IF ALL ENTRIES HAVE BEEN READ, GO TO CLOSE DLT RECORD.
!
         DO j = 1 , 3
            IF ( Ithrml==1 .AND. j==1 ) THEN
               IF ( idarea/=0 ) THEN
                  IF ( idarea==-2 ) GOTO 90
                  idarea = -2
                  Buf(1) = 1
                  Buf(2) = 0
                  Buf(14) = 0
               ENDIF
            ENDIF
            IF ( Buf(j+13)/=0 ) THEN
               file = scr(j)
               j2 = 2*j
               CALL read(*140,*90,file,Buf(j2-1),2,0,flag)
            ENDIF
            CYCLE
 90         Buf(2*j-1) = 16777215
            Buf(j+13) = 0
         ENDDO
         IF ( Buf(1)+Buf(3)+Buf(5)==3*16777215 ) THEN
!
!     CLOSE DLT RECORD,  CLOSE TABLES AND TEST FOR COMPLETION OF DLT.
!
            CALL write(Dlt,0,0,1)
            DO j = 1 , 3
               IF ( Buf(j+16)/=0 ) CALL close(scr(j),Clsrew)
            ENDDO
            i = i + 11
            IF ( i<=nlist ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CLOSE DLT, WRITE TRAILER AND RETURN.
!
            CALL close(Dlt,Clsrew)
            Mcb(1) = Dlt
            Mcb(2) = Dlt
            CALL wrttrl(Mcb)
            Nodlt = 1
            GOTO 100
         ELSE
!
!     SELECT MINIMUM SIL NO(S) AND FORMAT OUTPUT.
!
            DO j = 1 , 6
               Buf(j+10) = 0
            ENDDO
            Buf(7) = 1
            Buf(8) = 2
            Buf(9) = 3
            IF ( Buf(1)>Buf(3) ) THEN
!
!     1 .GT. 2--SWITCH 1 AND 2 THEN COMPARE 2 AND 3. IF 2 .GT. 3, SWITCH
!
               k = Buf(7)
               Buf(7) = Buf(8)
               Buf(8) = k
               IF ( Buf(1)<=Buf(5) ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = Buf(8)
               Buf(8) = Buf(9)
               Buf(9) = k
            ELSE
!
!     1 .LE. 2--COMPARE 2 TO 3. IF 2 .GT. 3, SWITCH 2 AND 3.
!
               IF ( Buf(3)<=Buf(5) ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = Buf(8)
               Buf(8) = Buf(9)
               Buf(9) = k
            ENDIF
!
!     COMPARE 1 TO 2--IF 1 .GT. 2, SWITCH 1 AND 2.
!
            k = Buf(7)
            L = Buf(8)
            IF ( Buf(2*k-1)>Buf(2*L-1) ) THEN
               Buf(7) = L
               Buf(8) = k
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     PICK UP 1. SET TO READ 1.
!
         k = Buf(7)
         Buf(10) = Buf(2*k-1)
         Buf(k+10) = Buf(2*k)
         Buf(k+13) = k
!
!     IF 1 .EQ. 2, PICK UP 2 AND SET TO READ 2.
!
         L = Buf(8)
         IF ( Buf(2*k-1)==Buf(2*L-1) ) THEN
            Buf(L+10) = Buf(2*L)
            Buf(L+13) = L
!
!     IF 1 .EQ. 2 .EQ. 3, PICK UP 3 AND SET TO READ 3.
!
            m = Buf(9)
            IF ( Buf(2*L-1)==Buf(2*m-1) ) THEN
               Buf(m+10) = Buf(2*m)
               Buf(m+13) = m
            ENDIF
         ENDIF
!
!     WRITE SIL NO., A, TAU, THETA. THEN GO TO READ ANOTHER TABLE
!     ENTRY(S).
!
         IF ( Z(i)>=3 .AND. Z(i)<=4 ) Buf(13) = iemf
         CALL write(Dlt,Buf(10),4,0)
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
         CALL mesage(n,file,Nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd2
