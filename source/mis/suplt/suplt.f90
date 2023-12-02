!*==suplt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE suplt(Iz,Iy,X,U,Gplst,Pen,Deform)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Iz
   INTEGER , DIMENSION(1) :: Iy
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER , DIMENSION(1) :: Gplst
   INTEGER :: Pen
   INTEGER :: Deform
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) :: err
   INTEGER :: i , i1 , id1 , id2 , id3 , id4 , ih , il , ipar , iret , iret1 , j , j1 , j2 , k , kk , l , len , lim , linesp , ll , &
            & n , n3 , n4 , ntab
   INTEGER , DIMENSION(2) :: limt
   INTEGER , DIMENSION(5) , SAVE :: lm , nm
   INTEGER , DIMENSION(71) :: m
   INTEGER , DIMENSION(17) , SAVE :: m1 , m2 , m4
   INTEGER , DIMENSION(11) , SAVE :: m3
   INTEGER , DIMENSION(9) , SAVE :: m5
   REAL :: x1 , x2 , y1 , y2
   EXTERNAL line , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     TO CREATE A SET OF UNIQUE LINES TO BE PLOTTED.  IN ADDITION, TO
!     AVOID SKIPPING ALL OVER THE PLOT FOR EACH LINE.
!
!     INPUT (SIMULAR TO -GPCT-)
!       NGRID - NUMBER OF INTERNAL GRID POINTS.
!       IY    - 1 THRU NGRID - POINTERS TO FIRST CONNECTION OF THE
!                              INTERNAL GRID MATCHING THIS INDEX.
!                              IF THE GRID HAS NO ENTRIES IZ(I+1) WILL
!                              HAVE THE SAME POINTER VALUE.
!             - NGRID+1      - POINTER TO END-OF-RECORD.
!       IZ    - CONNECTING INTERNAL GRIDS.  POINTER FOR NEXT GRID
!                              DETERMINES LAST ENTRY.  ENTRIES PUSHED
!                              DOWN AND  -1  ADDED AT END AS EACH ENTRY
!                              IS USED.
!
!       NTAB  = TOTAL ENTRY COUNTER IN GPCT
!       ID1   = START OF CURRENT -LINE-
!       ID2   = END   OF CURRENT -LINE-
!       ID3   = START OF LAST -LINE-
!       ID4   = END   OF LAST -LINE-
!
   !>>>>EQUIVALENCE (m1(1),m(1)) , (m2(1),m(18)) , (m3(1),m(35)) , (m4(1),m(46)) , (m5(1),m(63))
   DATA nm/17 , 17 , 11 , 17 , 9/ , lm/1 , 18 , 35 , 46 , 63/ , m1/4H(35X , 4H,26H , 4HSUPL , 4HT RE , 4HJECT , 4HED P , 4HLOT. ,   &
       &4H PIV , 4HOT,I , 4H8,26 , 4HH IS , 4H ZER , 4HO OR , 4H SAM , 4HE AS , 4H ENT , 4HRY.)/ , m2/4H(35X , 4H,54H , 4HSUPL ,    &
       &4HT RE , 4HJECT , 4HED P , 4HLOT. , 4H NEG , 4HATIV , 4HE NU , 4HMBER , 4H ENT , 4HRIES , 4H - N , 4H3,N4 , 4H =,2 ,        &
      & 4HI10)/ , m3/4H(35X , 4H,31H , 4HUNEX , 4HPECT , 4HED E , 4HOF I , 4HN SU , 4HPLT  , 4H- PI , 4HVOT, , 4HI10)/ , m4/4H(35X ,&
       &4H,11H , 4HSUPL , 4HT-EN , 4HTRY, , 4HI10, , 4H22H  , 4HFOR  , 4HPIVO , 4HT NO , 4HT FO , 4HUND  , 4H(,2I , 4H6,8H ,        &
      & 4H) RA , 4HNGE. , 4H)   / , m5/4H(35X , 4H,24H , 4HNO E , 4HLEME , 4HNTS  , 4HIN T , 4HHIS  , 4HSET. , 4H)   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         linesp = 0
!
!     LOCATE FIRST PIVOT (ID1) WITH ODD NUMBER OF ENTRIES
!
         id2 = 0
         id1 = 0
         SPAG_Loop_1_1: DO i = 1 , Ngrid
            ll = Iy(i+1) - Iy(i)
            IF ( ll/=0 ) THEN
!
!     IN CASE AN ODD NO. ENTRIES ISN'T FOUND THE DEFAULT IS FIRST PIVOT
!
               IF ( id2==0 ) id2 = i
               IF ( mod(ll,2)/=0 ) THEN
                  id1 = i
                  i1 = Iy(i)
                  id2 = Iz(i1)
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
!
         ntab = Iy(Ngrid+1) - Iy(1)
!
!     NO ELEMENTS IN THE SET
!
         IF ( ntab==0 ) THEN
            err(1) = 0
            k = 5
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SEE IF ANY ODD ENTRIES FOUND
!
            limt(1) = 0
            limt(2) = Ngrid + 1
            IF ( id1==0 ) THEN
               id1 = id2
               i1 = Iy(id1)
               id2 = Iz(i1)
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (2)
!
!     INTERNAL SEARCH FOR FIRST GPCT ENTRY ABOVE AND BELOW THE PIVOT
!     VALUE FOR THE PIVOT           *** M I N / M A X ***
!
         i1 = Iy(id1)
         il = -100000
         ih = 100000
         j1 = Iy(id1+1) - 1
!
         SPAG_Loop_1_2: DO i = i1 , j1
            IF ( Iz(i)<0 ) EXIT SPAG_Loop_1_2
            IF ( Iz(i)==0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Iz(i)<id1 ) THEN
               il = Iz(i)
            ELSEIF ( Iz(i)==id1 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
         ih = Iz(i)
         spag_nextblock_1 = 4
      CASE (4)
!
!     DETERMINE  WHICH IS CLOSER TO PIVOT
!
         i = id1 - il
         j = ih - id1
         IF ( j<i ) THEN
!
!     ID2 IS GREATER ID
!
            id2 = ih
         ELSEIF ( j==i ) THEN
!
!     EQUAL DISTANT, GO TO SAME DIRECTION AS BEFORE
!
            IF ( id4<id3 ) THEN
!
!     ID2 IS LESSOR ID
!
               id2 = il
            ELSEIF ( id4==id3 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
               id2 = ih
            ENDIF
         ELSE
            id2 = il
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     OUTPUT THE LINE -
!     NOTE THAT ID4 MAY BE RESET AT 320 SO DONT TAKE SHORTCUTS
!
         i = iabs(Gplst(id1))
         j = iabs(Gplst(id2))
         IF ( Deform/=0 ) THEN
            x1 = U(1,i)
            y1 = U(2,i)
            x2 = U(1,j)
            y2 = U(2,j)
         ELSE
            x1 = X(2,i)
            y1 = X(3,i)
            x2 = X(2,j)
            y2 = X(3,j)
         ENDIF
         CALL line(x1,y1,x2,y2,Pen,0)
         linesp = linesp + 1
!
!     REMOVE ENTRIES FROM CORE, LEFT SHIFT AS NEEDED AND PUT -1 AT THE
!     END OF THE TABLE.  PLACE THE NUMBER OF ENTRIES LEFT IN N3 AND N4.
!     DECREMENT THE TOTAL NUMBER OF ENTRIES BY 2. SET ID3 AND ID4.
!
         IF ( ntab<=2 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kk = 0
         j1 = i1
         j2 = Iy(id1+1) - 1
         ll = id2
         spag_nextblock_1 = 6
      CASE (6)
         ipar = j1
         il = 0
!
         SPAG_Loop_1_3: DO i = j1 , j2
            IF ( Iz(i)<ll ) THEN
               IF ( Iz(i)<0 ) EXIT SPAG_Loop_1_3
               IF ( Iz(i)==0 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Iz(i)==ll ) THEN
!
!     COMPONENT TO BE ELIMINATED HAS BEEN FOUND
!
               il = 1
               CYCLE
            ELSE
               Iz(ipar) = Iz(i)
            ENDIF
            ipar = ipar + 1
         ENDDO SPAG_Loop_1_3
!
         Iz(ipar) = -ll
         IF ( il==0 ) THEN
            err(1) = 3
            err(2) = ll
            err(3) = j1
            err(4) = j2
            k = 4
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( kk/=0 ) THEN
            ntab = ntab - 2
            id4 = id2
            n4 = ipar - j1
!
!     START OF LOOP AFTER FIRST -LINE-
!
            IF ( n4<0 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( n4==0 ) THEN
!
!     CASE ID4 HAS NO MORE ENTRIES. CHECK IF ID3 CAN BE PIVOT
!
               IF ( n3<0 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( n3==0 ) THEN
!
!     ID3 AND ID4 ARE NULL.  GO TO CLOSEST END OF TABLE FROM ID4
!
                  i = Ngrid - id4
                  j = 1
                  IF ( i>id4 ) j = -1
                  l = (j+2)/2 + 1
                  lim = limt(l)
                  len = id4
!
                  ASSIGN 40 TO iret1
                  kk = id4
               ELSE
!
!     NONZERO - ID3 IS TO BE ID1
!
                  id1 = id3
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
!
!     LAST END HAS ENTRY TO CONTINUE FROM
!
               id1 = id4
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            kk = 2
            id3 = id1
            n3 = ipar - j1
            ll = id2
            j1 = Iy(ll)
            j2 = Iy(ll+1) - 1
            ll = id1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         kk = kk + j
         IF ( kk==lim ) GOTO iret1
         ipar = 2
         ASSIGN 20 TO iret
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK IF ANY ENTRIES FOUND
!
 20      IF ( ipar==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ENTRY FOUND
!
         len = kk + j
         id4 = kk
         n4 = ipar
!
!     AN ENTRY WAS FOUND - CHECK FOR ODD NUMBER OF ENTRIES FOR PIVOT
!
         IF ( mod(ipar,2)==1 ) THEN
            id1 = kk
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NOT AN ODD NUMBER OF ENTRIES FOR ID4.  CHECK GPCT ENTRIES
!     FOR ONLY ONE ENTRY.
!
            ASSIGN 60 TO iret
!
            ih = j2
            il = j1 - 1
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THAT END OF TABLE FAILED - TRY OTHER END
!
 40      j = -j
         limt(l) = len
         l = (j+2)/2 + 1
         lim = limt(l)
         kk = id4
         ASSIGN 80 TO iret1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         il = il + 1
         kk = Iz(il)
         IF ( kk<=0 ) THEN
            id1 = id4
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ipar = 1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      IF ( ipar==1 ) THEN
            id1 = kk
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( il<ih ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            id1 = id4
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
!
!     PIVOT NOW DETERMINED
!
         ENDIF
      CASE (9)
!
!
!     INTERNAL ROUTINE TO DETERMINE NUMBER OF ENTRIES FOR PIVOT
!
!     INPUT
!        IPAR = 1 -- 0,1 OR MORE THAN 1 ENTRY RETURN
!             = 2 -- ACTUAL NUMBER OF ENTRIES RETURN
!        KK   = ID OF PIVOT
!
!     OUTPUT
!        IPAR = DESIRED NUMBER OF ENTRIES
!        KK   = SAME AS INPUT
!        J1   = POINTER TO 1ST LOCATION
!        J2   = NOT NECESSARILY LAST LOCATION (I.E. IPAR INPUT AS 1)
!
         j1 = Iy(kk)
         j2 = Iy(kk+1) - 1
         IF ( ipar==1 ) j2 = min0(j1+2,j2)
         ipar = 0
         IF ( j2>=j1 ) THEN
!
            SPAG_Loop_1_4: DO i = j1 , j2
               IF ( Iz(i)<0 ) EXIT SPAG_Loop_1_4
               IF ( Iz(i)==0 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ipar = ipar + 1
            ENDDO SPAG_Loop_1_4
         ENDIF
         GOTO iret
      CASE (10)
!
!     ERROR MESSAGES
!
         err(1) = 1
         err(2) = id1
         k = 1
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         err(1) = 2
         err(2) = n3
         err(3) = n4
         k = 2
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 80      err(1) = 1
         err(2) = id4
         k = 3
         spag_nextblock_1 = 12
      CASE (12)
!
         i = lm(k)
         CALL wrtprt(Merr,err,m(i),nm(k))
         IF ( k==5 ) RETURN
         spag_nextblock_1 = 13
      CASE (13)
!
!     CONVERT TABLE TO ORIGINAL VALUES UNLESS THIS IS THE LAST CALL
!
         il = Ngrid + 1
         i = Iy(il) - 1
         IF ( Deform==0 ) THEN
            DO j = 1 , i
               Iz(j) = iabs(Iz(j))
            ENDDO
!
            DO j1 = 1 , Ngrid
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( Iy(j1)==Iy(j1+1) ) CYCLE
                     i = Iy(j1)
                     l = i
                     n = Iy(j1+1) - 1
                     IF ( i+1>n ) CYCLE
                     spag_nextblock_2 = 2
                  CASE (2)
!
!     SHUTTLE EXCHANGE
!     (NOTE FROM G.CHAN/UNISYS  10/1990
!     THERE ARE MORE THAN JUST A SHUTTLE SORTING HERE. REPLACING THE
!     SHUTTLE EXCHANGE METHOD BY SORT ROUTINE, WHICH USES A MUCH FASTER
!     TECHNEQUE, DOES NOT WORK HERE)
!
                     IF ( Iz(i)>Iz(i+1) ) THEN
                        k = Iz(i+1)
                        Iz(i+1) = Iz(i)
                        Iz(i) = k
                        j = i
                        SPAG_Loop_2_5: DO WHILE ( j/=l )
                           IF ( Iz(j)>=Iz(j-1) ) EXIT SPAG_Loop_2_5
                           k = Iz(j)
                           Iz(j) = Iz(j-1)
                           Iz(j-1) = k
                           j = j - 1
                        ENDDO SPAG_Loop_2_5
                     ENDIF
                     IF ( i<n-1 ) THEN
                        i = i + 1
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
            ENDDO
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     A NONSTANDARD RETURN COULD BE ADDED HERE.  BAD PLOT RESULTS IF
!     THIS ROUTINE FAILS.  THE FRAME WILL BE PRESENT HOWEVER
!
END SUBROUTINE suplt
