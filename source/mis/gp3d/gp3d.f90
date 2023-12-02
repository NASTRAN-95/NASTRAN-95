!*==gp3d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp3d
   USE c_blank
   USE c_gp3com
   USE c_gpta1
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: any , anyet , anygpt , heat , lflag
   INTEGER :: defalt , ectwds , file , flag , gptrec , i , id , ieltyp , iet1 , iet2 , igpt , igptt , igrid , iheat , ii , ilist ,  &
            & inrec , irecs , iretrn , j , jtemp , k , let1 , let2 , lgpt , list1 , list2 , move , n , net1 , net2 , ngpt , ngptt , &
            & ngrid , nlist , nsets , nwords , outpt , outwds , record , setid , sysbuf , twoi
   REAL :: deftmp , factor , fgrids , h , hover2 , rtemp , sum , t1 , t2 , tbar , tprime
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL , DIMENSION(50) :: rbuf
   REAL , DIMENSION(1) :: rz
   REAL , DIMENSION(32) :: tgrid
   EXTERNAL bisloc , close , ectloc , fname , fwdrec , gopen , locate , mesage , open , preloc , read , rewind , sort , write ,     &
          & wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GP3D CREATES THE ETT (ELEMENT TEMPERATURE TABLE)
!
!     THE GPTT AS PREPARED BY GP3B COMES TO THIS ROUTINE VIA SCRATCH
!     DATA SET 1.
!
!     DATA IN THE GPTT IS USED TOGETHER WITH DATA OBTAINED FROM TEMPP1,
!     TEMPP2, TEMPP3, AND TEMPRB CARDS WHICH RESIDE ON GEOM3.
!
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (Rbuf(1),Buf(1)) , (defalt,deftmp) , (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(56),Iheat)
   DATA nam/4HGP3D , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!                 +---------------------+
!     OPEN CORE   I                     I  Z(ILIST) = Z(1)
!                 I  ET SET-LIST        I
!     DESIGN FOR  I  2 WDS/ENTRY        I
!                 I                     I  Z(NLIST)
!     GP3D        +---------------------+
!                 I                     I  Z(IGPTT)
!                 I  GPT SET-LIST       I
!                 I  3 WDS/ENTRY        I
!                 I                     I  Z(NGPTT)
!                 +---------------------+
!                 I                     I  Z(IGPT) *
!                 I  GPTT DATA          I           *
!                 I  FOR CURRENT SETID  I            *
!                 I  2 WDS/ENTRY        I             *
!                 I                     I  Z(NGPT)     *
!                 +---------------------+               *
!                 I                     I  Z(IET1)       *
!                 I  2-DIMEN EL-TEMP    I                * THIS SPACE IS
!                 I  FOR CURRENT SETID  I                * DYNAMIC FOR
!                 I  7 WDS/ENTRY        I                * EACH SET OF
!                 I                     I  Z(NET1)       * TEMPERATURE
!                 +---------------------+                * DATA.
!                 I                     I  Z(IET2)       *
!                 I  1-DIMEN EL-TEMP    I                *
!                 I  FOR CURRENT SETID  I               *
!                 I  15 WDS/ENTRY       I              *
!                 I                     I  Z(NET2)    *
!                 +---------------------+            *
!                 I/////////////////////I           *
!                 I/////////////////////I          *
!                 +---------------------+
!                 I                     I  Z(BUF1)
!                 I  BUFFER 2           I
!                 I                     I
!                 +---------------------+
!                 I                     I  Z(BUF2)
!                 I  BUFFER 1           I
!                 I                     I  Z(KORSZ)
!                 +---------------------+
!
!
!
!     OPEN GEOM3, AND SCR1. READ IN TEMPP1, TEMPP2, TEMPP3, TEMPRB CARDS
!     CONVERT AND WRITE THEM OUT ON SCR2.
!
         heat = .FALSE.
         IF ( iheat==1 ) heat = .TRUE.
         lflag = .FALSE.
         j = -1
         nwords = 8
         ilist = 1
         nlist = 0
         file = geom3
         any = .FALSE.
         CALL preloc(*380,z(buf1),geom3)
         file = scr2
         CALL open(*380,scr2,z(buf2),wrtrew)
!
!     PICK UP TEMPP1 CARDS
!
         file = geom3
         CALL locate(*40,z(buf1),tempp1,flag)
         any = .TRUE.
         ASSIGN 20 TO iretrn
         buf(7) = 0
         buf(8) = 1
 20      CALL read(*400,*40,geom3,buf,6,0,flag)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PICK UP TEMPP2 CARDS
!
 40      CALL locate(*80,z(buf1),tempp2,flag)
         any = .TRUE.
         ASSIGN 60 TO iretrn
 60      CALL read(*400,*80,geom3,buf,8,0,flag)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PICK UP TEMPP3 CARDS (CONVERT THESE TO LOOK LIKE TEMPP1 CARDS)
!
 80      CALL locate(*120,z(buf1),tempp3,flag)
         any = .TRUE.
         ASSIGN 100 TO iretrn
 100     CALL read(*400,*120,geom3,buf,24,0,flag)
         n = 25
         SPAG_Loop_1_1: DO i = 1 , 11
            n = n - 2
            IF ( rbuf(n)/=0.0 .OR. rbuf(n+1)/=0.0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         n = n/2
         t1 = rbuf(4)
         t2 = rbuf(2*n+2)
         IF ( n==1 ) THEN
!
            tbar = rbuf(4)
            tprime = 0.0
         ELSE
            h = rbuf(2*n+1) - rbuf(3)
            sum = 0.0
            n = n - 1
            DO i = 1 , n
               twoi = 2*i
               factor = rbuf(twoi+3) - rbuf(twoi+1)
               IF ( factor<=0.0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               sum = sum + (rbuf(twoi+2)+rbuf(twoi+4))*factor
            ENDDO
            tbar = sum/(2.0*h)
            hover2 = h/2.0
            sum = 0.0
            DO i = 1 , n
               twoi = 2*i
               sum = sum + (rbuf(twoi+3)-rbuf(twoi+1))*(3.0*(rbuf(twoi+1)-rbuf(3)-hover2)*(rbuf(twoi+4)+rbuf(twoi+2))               &
                   & +(rbuf(twoi+2)+2.0*rbuf(twoi+4))*(rbuf(twoi+3)-rbuf(twoi+1)))
            ENDDO
            tprime = 2.0*sum/h**3
         ENDIF
!
         rbuf(3) = tbar
         rbuf(4) = tprime
         rbuf(5) = t1
         rbuf(6) = t2
         buf(7) = 0
         buf(8) = 1
         spag_nextblock_1 = 3
      CASE (2)
!
!     BAD DATA ON A TEMPP3 CARD
!
         WRITE (outpt,99001) ufm , buf(1) , buf(2)
99001    FORMAT (A23,' 4010, TEMPP3 BULK DATA CARD WITH SET ID =',I8,' AND ELEMENT ID =',I8,/27X,                                   &
                &'DOES NOT HAVE ASCENDING VALUES SPECIFIED FOR Z.')
         lflag = .TRUE.
         GOTO 100
!
!     END OF 8 WORD CARDS.  WRITE EOR ON SCR2 AND DO TEMPRB CARDS NOW.
!
 120     CALL write(scr2,0,0,1)
         nwords = 16
         CALL locate(*160,z(buf1),temprb,flag)
         any = .TRUE.
         ASSIGN 140 TO iretrn
 140     CALL read(*400,*160,geom3,buf,16,0,flag)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     WRITE EOR ON SCR2. SCR2 THEN WILL HAVE 2 RECORDS (1 OR BOTH EMPTY)
!
 160     CALL write(scr2,0,0,1)
         CALL close(geom3,rew)
         CALL close(scr2,rew)
!
!     READ IN GPTT HEADER RECORD FROM SCR1
!
         igptt = nlist + 1
         ngptt = igptt
         file = scr1
         IF ( notemp/=1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*380,scr1,z(buf1),rdrew)
         CALL read(*400,*200,scr1,z(igptt),buf2-igptt,1,flag)
         CALL mesage(-8,0,nam)
         GOTO 200
      CASE (3)
!
!     INTERNAL SUBROUTINE TO BUILD SET LIST FROM TEMPERATURE CARD DATA
!     FIND SET-ID OR ADD IT TO LIST IN SORT, BUMP COUNT AND WRITE CARD.
!
         IF ( j==-1 ) THEN
!
!     ADD NEW SETID TO LIST
!
            j = j + 2
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( buf(1)==z(j) ) THEN
!
!     MATCH WAS FOUND  (ILIST ASSUMED TO BE EQUAL TO 1)
!
            z(j+1) = z(j+1) + nwords
         ELSEIF ( buf(1)>z(j) .AND. j==nlist-1 ) THEN
            j = j + 2
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     LOOK FOR MATCHING SETID OR FIND WHERE NEW SETID BELONGS
!
            CALL bisloc(*180,buf(1),z(ilist),2,nlist/2,j)
            z(j+1) = z(j+1) + nwords
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     ADD THIS NEW SETID INTO LIST
!
 180     IF ( buf(1)>z(j) ) j = j + 2
!
!     PUSH Z(J) THRU Z(NLIST) DOWN TWO WORDS TO MAKE ROOM FOR NEW SETID
!
         i = nlist + 2
         DO k = j , nlist
            z(i) = z(i-2)
            i = i - 1
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         z(j) = buf(1)
         nlist = nlist + 2
         z(j+1) = nwords
         spag_nextblock_1 = 5
      CASE (5)
!
!     WRITE OUT THE DATA CARD ON THE SCRATCH FILE FOR LATER USE
!
         CALL write(scr2,buf,nwords,0)
         GOTO iretrn
 200     ngptt = nlist + flag
         igptt = igptt + 2
         nsets = (ngptt-igptt+1)/3
!
!     DETERMINE NUMBER OF RECORDS OF EXTERNAL INDEX TEMP DATA
!     FOLLOWING HEADER RECORD.
!
         irecs = 0
         IF ( nsets>0 ) THEN
            DO i = igptt , ngptt , 3
               irecs = max0(z(i+2),irecs)
            ENDDO
         ENDIF
         CALL close(scr1,norew)
         spag_nextblock_1 = 6
      CASE (6)
!
!     OPEN ETT, PUT OUT HEADER RECORD WITH THE 3 WORD SET ENTRIES.
!
         IF ( notemp/=1 .AND. .NOT.any ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         notemp = 1
         file = ett
         CALL open(*380,ett,z(buf2),wrtrew)
         CALL fname(ett,buf)
         CALL write(ett,buf,2,0)
         list1 = ilist
         list2 = igptt
         record = 0
         spag_nextblock_1 = 7
      CASE (7)
         IF ( list1<=nlist-1 .OR. list2>ngptt-2 ) THEN
            IF ( list1>nlist-1 .OR. list2<=ngptt-2 ) THEN
               IF ( list1>nlist-1 .AND. list2>ngptt-2 ) THEN
!
!     HEADER RECORD IS COMPLETE.  WRITE EOR AND CLOSE WITH NOREWIND.
!
                  CALL write(ett,0,0,1)
                  CALL close(ett,norew)
!
!     FOR EACH SET DEFINED IN THE EL-TEMP SET LIST AND OR THE GRID-TEMP
!     SET LIST PASS GEOM2 USING LOCATE FOR ALL THE ELEMENTS FOR
!     WHICH ETT TEMP DATA OUTPUT IS POSSIBLE.
!     IF ANY ELEMENTS CONCERNED ARE PRESENT THEN SELECT FROM THE TEMP
!     DATA AVAILABLE THAT WHICH IS APPLICABLE AND OUTPUT THE DATA ON THE
!     ETT IN THE FOLLOWING FORMAT.
!
!     CONTENTS OF 1 RECORD OF THE OUTPUT FILE ETT. EACH RECORD CONTAINS
!     DATA FOR 1 SET.
!
!         SET-ID
!         ELEMENT TYPE          * * * * * * * * * *
!         NUMBER OF TEMPERATURE DATA VALUES/EL-ID  *
!         EL-ID          *                          *
!         TEMP-VALUE      *                          *
!             .           * EL-ID                    *
!             .           * ENTRY                    *
!             .           *                          *  ELEMENT-TYPE
!         LAST-TEMP-VALUE*                           *     ENTRY
!               *             (1 OR MORE EL-ID       *
!               *              ENTRIES PER EL-TYPE   *   (1 OR MORE
!               *              ENTRY)                *    PER RECORD)
!         EL-ID          *                           *
!         TEMP-VALUE      *                          *
!             .           * EL-ID                    *
!             .           * ENTRY                    *
!             .           *                         *
!         LAST-TEMP-VALUE*                         *
!         0                     * * * * * * * * * *
!
!     IN THE ABOVE IF THE ELEMENT HAS NO SPECIAL DATA, A NEGATIVE
!     ELEMENT ID IS INSERTED FOLLOWED BY NO TEMPERATURE DATA.
!
!     NOW GATHER THE DATA AVAILABLE FOR A SET FROM SCR1 AND OR SCR2.
!
                  gptrec = 1
                  list1 = ilist
                  list2 = igptt
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
!
               ELSEIF ( z(list1)<z(list2) ) THEN
               ELSEIF ( z(list1)==z(list2) ) THEN
!
!     SET-ID OF LIST1 IS .EQ. SET-ID OF LIST2.
!
                  buf(1) = z(list2)
                  buf(2) = z(list2+1)
                  list1 = list1 + 2
                  list2 = list2 + 3
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     SET-ID OF LIST1 IS .LT. SET-ID OF LIST2 OR LIST2 IS ALL USED.
!
            buf(1) = z(list1)
            buf(2) = -1
            list1 = list1 + 2
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     SET-ID OF LIST2 IS .LT. SET-ID OF LIST1 OR LIST1 IS ALL USED.
!
         buf(1) = z(list2)
         buf(2) = z(list2+1)
         list2 = list2 + 3
         IF ( z(list2-1)==0 ) THEN
            buf(3) = 0
            CALL write(ett,buf,3,0)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     WRITE 3-WORD SET-ID ENTRY IN HEADER
!
         record = record + 1
         buf(3) = record
         CALL write(ett,buf,3,0)
         spag_nextblock_1 = 7
      CASE (10)
         anygpt = .FALSE.
         anyet = .FALSE.
         igpt = 0
         ngpt = 0
         iet1 = 0
         net1 = 0
         iet2 = 0
         net2 = 0
         IF ( list1<=nlist-1 ) THEN
            IF ( list2<=ngptt-2 ) THEN
!
               IF ( z(list1)<z(list2) ) THEN
               ELSEIF ( z(list1)==z(list2) ) THEN
!
!     NEXT SET-ID HAS BOTH GRID-TEMP AND EL-TEMP DATA
!
                  setid = z(list2)
                  defalt = z(list2+1)
                  anyet = .TRUE.
                  inrec = z(list2+2)
                  IF ( inrec>0 ) anygpt = .TRUE.
                  nwords = z(list1+1)
                  list1 = list1 + 2
                  list2 = list2 + 3
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     NEXT SET-ID HAS ONLY EL-TEMP DATA
!
            setid = z(list1)
            defalt = -1
            anyet = .TRUE.
            nwords = z(list1+1)
            list1 = list1 + 2
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( list2>ngptt-2 ) THEN
!
!     ETT IS COMPLETE
!
            IF ( lflag ) CALL mesage(-61,0,0)
!
!     WRITE TRAILER FOR ETT
!
            buf(1) = ett
            buf(7) = 7
            DO i = 2 , 6
               buf(i) = 0
            ENDDO
!
!     OPEN ETT AND APPEND GPTT SECTION OF TEMP DATA IN INTERNAL NOTATION
!
            file = ett
            CALL open(*380,ett,z(buf2),wrt)
            IF ( .NOT.anygpt .AND. .NOT.heat ) GOTO 360
!
!     OPEN SCR1 AND SKIP THE TEMPERATURE DATA HAVING EXTERNAL INDICES
!
            file = scr1
            CALL gopen(scr1,z(buf1),rdrew)
            IF ( irecs>0 ) THEN
               DO i = 1 , irecs
                  CALL fwdrec(*400,scr1)
               ENDDO
            ENDIF
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
!     NEXT SET-ID HAS ONLY GRID-TEMP DATA
!
         setid = z(list2)
         defalt = z(list2+1)
         inrec = z(list2+2)
         IF ( inrec>0 ) anygpt = .TRUE.
         list2 = list2 + 3
         spag_nextblock_1 = 12
      CASE (12)
!
!     AT THIS POINT READ IN ANY GRID-TEMP DATA AND/OR ANY EL-TEMP DATA.
!     SORT THE EL-TEMP DATA ON EL-ID. THE GRID-TEMP DATA IS SORTED ON
!     GRIDS
!
         igpt = ngptt + 1
         ngpt = igpt
         IF ( .NOT.anygpt ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = scr1
         CALL open(*380,scr1,z(buf1),rd)
!
!     POSITION GPTT TO DESIRED GRID-POINT-TEMP SET AND READ IT IN.
!
         move = inrec - gptrec
         IF ( move<0 ) THEN
            CALL rewind(scr1)
            move = inrec
         ELSEIF ( move==0 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , move
            CALL fwdrec(*400,scr1)
         ENDDO
         spag_nextblock_1 = 13
      CASE (13)
         gptrec = inrec + 1
         CALL read(*400,*220,scr1,z(igpt),buf2-igpt,1,flag)
         CALL mesage(-8,0,nam)
 220     ngpt = igpt + flag - 1
         CALL close(scr1,norew)
         spag_nextblock_1 = 14
      CASE (14)
!
!     READ IN EL-TEMP DATA PERTAINING TO THIS SET-ID
!
         IF ( .NOT.anyet ) GOTO 260
         IF ( ngpt+nwords>=buf2 ) CALL mesage(-8,0,nam)
         file = scr2
         CALL open(*380,scr2,z(buf1),rdrew)
         iet1 = ngpt + 1
         net1 = ngpt
         SPAG_Loop_1_2: DO
            CALL read(*400,*240,scr2,buf,8,0,flag)
            IF ( buf(1)==setid ) THEN
               DO i = 2 , 8
                  net1 = net1 + 1
                  z(net1) = buf(i)
               ENDDO
               nwords = nwords - 8
               IF ( nwords==0 ) THEN
                  CALL fwdrec(*380,scr2)
                  EXIT SPAG_Loop_1_2
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
 240     iet2 = net1 + 1
         net2 = net1
         SPAG_Loop_1_3: DO
            CALL read(*400,*260,scr2,buf,16,0,flag)
            IF ( buf(1)==setid ) THEN
               DO i = 2 , 16
                  net2 = net2 + 1
                  z(net2) = buf(i)
               ENDDO
               nwords = nwords - 16
               IF ( nwords==0 ) EXIT SPAG_Loop_1_3
            ENDIF
         ENDDO SPAG_Loop_1_3
!
!     ALL DATA IS NOW IN CORE FOR THIS SET-ID
!
 260     CALL close(scr2,rew)
         IF ( .NOT.anyet .AND. .NOT.anygpt ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SORT THE 7-WORD TEMP CARDS ON ID AND CHECK FOR DUPLICATE ID S
!     AMONG ALL THE ELEMENT TEMPERATURE DATA
!
         IF ( iet1<net1 ) CALL sort(0,0,7,1,z(iet1),net1-iet1+1)
         IF ( iet2<net2 ) CALL sort(0,0,15,1,z(iet2),net2-iet2+1)
!
         let1 = (net1-iet1+1)/7
         let2 = (net2-iet2+1)/15
         lgpt = (ngpt-igpt+1)/2
         lflag = .FALSE.
         IF ( let1>1 ) THEN
            id = z(iet1)
            j = iet1 + 7
            DO i = j , net1 , 7
               IF ( id==z(i) ) THEN
!
!     ERROR - TWO OR MORE ID-S EQUAL IN TEMPERATURE DATA WITHIN A SET.
!
                  WRITE (outpt,99003) ufm , setid , id
                  lflag = .TRUE.
               ENDIF
               id = z(i)
            ENDDO
         ENDIF
         IF ( let2>1 ) THEN
            id = z(iet2)
            j = iet2 + 15
            DO i = j , net2 , 15
               IF ( id==z(i) ) THEN
                  WRITE (outpt,99003) ufm , setid , id
                  lflag = .TRUE.
               ENDIF
               id = z(i)
            ENDDO
         ENDIF
!
!     OPEN GEOM2, PREPARE TO PASS GEOM2, AND OUTPUT A RECORD OF THE ETT.
!
         file = geom2
         CALL preloc(*380,z(buf1),geom2)
!
!     OPEN ETT TO PUT OUT DATA-RECORD FOR THIS SET AND WRITE SETID,
!
         file = ett
         CALL open(*380,ett,z(buf2),wrt)
         CALL write(ett,setid,1,0)
!
!     RUN THROUGH POSSIBLE TEMPERATURE DEPENDENT ELEMENTS ON GEOM2.
!
         file = geom2
         spag_nextblock_1 = 15
      CASE (15)
         CALL ectloc(*320,file,buf,i)
!
!     OK DATA FOR A CARD TYPE HAS BEEN FOUND.  WRITE EL-TYPE AND
!     DATA FOR A CARD TYPE FOUND.
!
         buf(1) = elem(i+2)
         buf(2) = elem(i+14) - 1
         ieltyp = buf(1)
!
!     WRITE ELEMENT TYPE HEADER
!
         CALL write(ett,buf,2,0)
         IF ( elem(i+13)==0 ) GOTO 300
         jtemp = elem(i+13)
         outwds = elem(i+14)
         ectwds = elem(i+5)
         igrid = elem(i+12)
         ngrid = igrid + elem(i+9) - 1
         fgrids = 0.0
         spag_nextblock_1 = 16
      CASE (16)
         SPAG_Loop_1_4: DO
            CALL read(*400,*300,geom2,buf,ectwds,0,flag)
!
!     ON FIRST PASS COUNT NUMBER OF NON-ZERO GRIDS
!
            IF ( fgrids==0 ) THEN
               DO j = igrid , ngrid
                  IF ( buf(j)/=0 ) fgrids = fgrids + 1.0
               ENDDO
            ENDIF
!
!     SELECT DATA TO BE OUTPUT
!
            IF ( .NOT.anyet ) EXIT SPAG_Loop_1_4
            IF ( jtemp==2 ) THEN
!
!     2 - DIMENSIONAL ELEMENT-TEMP DATA MAY BE AVAIL.
!
               IF ( let1<1 ) EXIT SPAG_Loop_1_4
               CALL bisloc(*280,buf(1),z(iet1),7,let1,j)
               j = iet1 + j
            ELSEIF ( jtemp==3 .OR. jtemp==4 ) THEN
               EXIT SPAG_Loop_1_4
            ELSE
!
!     1 - DIMENSIONAL ELEMENT-TEMP DATA MAY BE AVAIL.
!
               IF ( let2<1 ) EXIT SPAG_Loop_1_4
               CALL bisloc(*280,buf(1),z(iet2),15,let2,j)
               j = iet2 + j
!
!     AVERAGE T-BAR-A AND T-BAR-B IF THIS IS A ROD, CONROD, OR TUBE
!
               IF ( ieltyp==1 .OR. ieltyp==3 .OR. ieltyp==10 ) THEN
                  rbuf(2) = (rz(j)+rz(j+1))/2.0
!
!     OUTPUT ELEMENT-TEMPERATURE DATA FOR 1 ELEMENT OF THIS TYPE IN SET
!
                  CALL write(ett,buf,outwds,0)
                  CYCLE
               ENDIF
            ENDIF
            DO k = 2 , outwds
               buf(k) = z(j)
               j = j + 1
            ENDDO
            CALL write(ett,buf,outwds,0)
         ENDDO SPAG_Loop_1_4
!
!     CHECK FOR GRID-POINT-TEMP-DATA
!
 280     IF ( anygpt ) THEN
!
!     GRID-POINT-TEMP-DATA IS AVAILABLE FOR SOME OR ALL GRID POINTS.
!
            any = .FALSE.
            rtemp = 0.0
            ii = 0
            DO k = igrid , ngrid
               ii = ii + 1
               IF ( buf(k)/=0 ) THEN
                  CALL bisloc(*285,buf(k),z(igpt),2,lgpt,j)
                  j = igpt + j
                  rtemp = rtemp + rz(j)
                  IF ( ii>32 ) CALL mesage(-61,0,0)
                  tgrid(ii) = rz(j)
                  any = .TRUE.
               ELSE
!
!     UNDEFINED GRID-POINT
!
                  tgrid(ii) = 0
               ENDIF
               CYCLE
 285           IF ( defalt==-1 ) THEN
                  spag_nextblock_1 = 17
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               rtemp = rtemp + deftmp
               tgrid(ii) = deftmp
            ENDDO
!
!     IF NOTHING BUT DEFAULT DATA THEN WRITE NOTHING SINCE THE
!     DEFAULT IS IN THE HEADER RECORD.
!
            IF ( .NOT.any ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     IF BAR ELEMENT PUT GRID TEMPS INTO BUFFER FOR T-BAR-A AND T-BAR-B
!
            IF ( ieltyp/=34 ) THEN
!
               rbuf(2) = rtemp/fgrids
               j = 3
               IF ( jtemp==4 ) j = 2
            ELSE
               rbuf(2) = tgrid(1)
               rbuf(3) = tgrid(2)
               j = 4
            ENDIF
!
            IF ( jtemp>=3 ) THEN
               DO k = 1 , ii
                  rbuf(j) = tgrid(k)
                  j = j + 1
               ENDDO
            ENDIF
            DO WHILE ( j<=outwds )
               buf(j) = 0
               j = j + 1
            ENDDO
            CALL write(ett,buf,outwds,0)
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
!
!     NO GRID-POINT-TEMP-DATA.  VERIFY THAT THERE IS A DEFAULT TEMP.
!
         ELSEIF ( defalt/=-1 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
!
!     ERROR NO TEMP DATA OR DEFALT OF ANY KIND FOR THIS ID.
!
         lflag = .TRUE.
         WRITE (outpt,99002) ufm , setid , buf(1)
99002    FORMAT (A23,' 4012, THERE IS NO ELEMENT, GRID POINT, OR DEFAULT',' TEMPERATURE DATA FOR',/30X,'TEMPERATURE SET',I12,       &
                &', WITH RESPECT TO ELEMENT ID =',I8)
         spag_nextblock_1 = 18
      CASE (18)
!
!     OUTPUT A NEGATIVE ELEMENT ID SINCE THERE IS NO DATA AVAILABLE.
!
         id = -buf(1)
         CALL write(ett,id,1,0)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
!
!     END OF ELEMENTS FOR THIS EL-TYPE.  WRITE ZERO ON ETT
!
 300     CALL write(ett,0,1,0)
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
!
!     ETT-RECORD IS COMPLETE FOR THIS SET. WRITE EOR AND PROCESS NEXT
!     SET.
!
 320     CALL write(ett,0,0,1)
         CALL close(ett,norew)
         spag_nextblock_1 = 10
      CASE (19)
         DO
!
!     COPY BALANCE OF SCR1 TO ETT
!
            CALL read(*360,*340,scr1,z,buf2-1,0,flag)
            CALL write(ett,z,buf2-1,0)
         ENDDO
 340     CALL write(ett,z,flag,1)
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 360     CALL close(scr1,rew)
         CALL close(ett,rew)
         CALL wrttrl(buf)
         spag_nextblock_1 = 20
      CASE (20)
!
!     THERE WAS NO GPTT DATA AND ALSO NO ETT DATA. THUS RETURN HAVING
!     CREATED NO ETT DATA SET.
!
         RETURN
!
!     ERROR CONDITIONS ON FILES
!
!
!     FILE NOT IN FIST OR PURGED
!
 380     j = -1
         spag_nextblock_1 = 21
         CYCLE SPAG_DispatchLoop_1
!
!     EOF HIT WHILE READING FILE
!
 400     j = -2
         spag_nextblock_1 = 21
      CASE (21)
         CALL mesage(j,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99003 FORMAT (A23,' 4011, ELEMENT TEMPERATURE SET',I9,' CONTAINS ','MULTIPLE TEMPERATURE DATA SPECIFIED FOR ELEMENT ID',I9)
END SUBROUTINE gp3d
