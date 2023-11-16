
SUBROUTINE suplt(Iz,Iy,X,U,Gplst,Pen,Deform)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iout , Merr , Ngrid
   REAL Skp1(19) , Sysbuf
   COMMON /blank / Ngrid , Skp1 , Merr
   COMMON /system/ Sysbuf , Iout
!
! Dummy argument declarations
!
   INTEGER Deform , Pen
   INTEGER Gplst(1) , Iy(1) , Iz(1)
   REAL U(2,1) , X(3,1)
!
! Local variable declarations
!
   INTEGER err(4) , i , i1 , id1 , id2 , id3 , id4 , ih , il , ipar , iret , iret1 , j , j1 , j2 , k , kk , l , len , lim , limt(2) &
         & , linesp , ll , lm(5) , m(71) , m1(17) , m2(17) , m3(11) , m4(17) , m5(9) , n , n3 , n4 , nm(5) , ntab
   REAL x1 , x2 , y1 , y2
!
! End of declarations
!
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
   EQUIVALENCE (m1(1),m(1)) , (m2(1),m(18)) , (m3(1),m(35)) , (m4(1),m(46)) , (m5(1),m(63))
   DATA nm/17 , 17 , 11 , 17 , 9/ , lm/1 , 18 , 35 , 46 , 63/ , m1/4H(35X , 4H,26H , 4HSUPL , 4HT RE , 4HJECT , 4HED P , 4HLOT. ,   &
       &4H PIV , 4HOT,I , 4H8,26 , 4HH IS , 4H ZER , 4HO OR , 4H SAM , 4HE AS , 4H ENT , 4HRY.)/ , m2/4H(35X , 4H,54H , 4HSUPL ,    &
       &4HT RE , 4HJECT , 4HED P , 4HLOT. , 4H NEG , 4HATIV , 4HE NU , 4HMBER , 4H ENT , 4HRIES , 4H - N , 4H3,N4 , 4H =,2 ,        &
      & 4HI10)/ , m3/4H(35X , 4H,31H , 4HUNEX , 4HPECT , 4HED E , 4HOF I , 4HN SU , 4HPLT  , 4H- PI , 4HVOT, , 4HI10)/ , m4/4H(35X ,&
       &4H,11H , 4HSUPL , 4HT-EN , 4HTRY, , 4HI10, , 4H22H  , 4HFOR  , 4HPIVO , 4HT NO , 4HT FO , 4HUND  , 4H(,2I , 4H6,8H ,        &
      & 4H) RA , 4HNGE. , 4H)   / , m5/4H(35X , 4H,24H , 4HNO E , 4HLEME , 4HNTS  , 4HIN T , 4HHIS  , 4HSET. , 4H)   /
!
   linesp = 0
!
!     LOCATE FIRST PIVOT (ID1) WITH ODD NUMBER OF ENTRIES
!
   id2 = 0
   id1 = 0
   DO i = 1 , Ngrid
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
            EXIT
         ENDIF
      ENDIF
   ENDDO
!
   ntab = Iy(Ngrid+1) - Iy(1)
!
!     NO ELEMENTS IN THE SET
!
   IF ( ntab==0 ) THEN
      err(1) = 0
      k = 5
      GOTO 1500
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
      GOTO 400
   ENDIF
!
!     INTERNAL SEARCH FOR FIRST GPCT ENTRY ABOVE AND BELOW THE PIVOT
!     VALUE FOR THE PIVOT           *** M I N / M A X ***
!
 100  i1 = Iy(id1)
   il = -100000
   ih = 100000
   j1 = Iy(id1+1) - 1
!
   DO i = i1 , j1
      IF ( Iz(i)<0 ) EXIT
      IF ( Iz(i)==0 ) GOTO 1200
      IF ( Iz(i)<id1 ) THEN
         il = Iz(i)
      ELSEIF ( Iz(i)==id1 ) THEN
         GOTO 1200
      ELSE
         GOTO 200
      ENDIF
   ENDDO
   GOTO 300
!
 200  ih = Iz(i)
!
!     DETERMINE  WHICH IS CLOSER TO PIVOT
!
 300  i = id1 - il
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
         GOTO 1200
      ELSE
         id2 = ih
      ENDIF
   ELSE
      id2 = il
   ENDIF
!
!     OUTPUT THE LINE -
!     NOTE THAT ID4 MAY BE RESET AT 320 SO DONT TAKE SHORTCUTS
!
 400  i = iabs(Gplst(id1))
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
   IF ( ntab<=2 ) GOTO 1600
   kk = 0
   j1 = i1
   j2 = Iy(id1+1) - 1
   ll = id2
 500  ipar = j1
   il = 0
!
   DO i = j1 , j2
      IF ( Iz(i)<ll ) THEN
         IF ( Iz(i)<0 ) EXIT
         IF ( Iz(i)==0 ) GOTO 1200
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
   ENDDO
!
   Iz(ipar) = -ll
   IF ( il==0 ) THEN
      err(1) = 3
      err(2) = ll
      err(3) = j1
      err(4) = j2
      k = 4
      GOTO 1500
   ELSEIF ( kk/=0 ) THEN
      ntab = ntab - 2
      id4 = id2
      n4 = ipar - j1
!
!     START OF LOOP AFTER FIRST -LINE-
!
      IF ( n4<0 ) GOTO 1300
      IF ( n4==0 ) THEN
!
!     CASE ID4 HAS NO MORE ENTRIES. CHECK IF ID3 CAN BE PIVOT
!
         IF ( n3<0 ) GOTO 1300
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
            ASSIGN 800 TO iret1
            kk = id4
         ELSE
!
!     NONZERO - ID3 IS TO BE ID1
!
            id1 = id3
            GOTO 100
         ENDIF
      ELSE
!
!     LAST END HAS ENTRY TO CONTINUE FROM
!
         id1 = id4
         GOTO 100
      ENDIF
   ELSE
      kk = 2
      id3 = id1
      n3 = ipar - j1
      ll = id2
      j1 = Iy(ll)
      j2 = Iy(ll+1) - 1
      ll = id1
      GOTO 500
   ENDIF
 600  kk = kk + j
   IF ( kk==lim ) GOTO iret1
   ipar = 2
   ASSIGN 700 TO iret
   GOTO 1100
!
!     CHECK IF ANY ENTRIES FOUND
!
 700  IF ( ipar==0 ) GOTO 600
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
      GOTO 100
   ELSE
!
!     NOT AN ODD NUMBER OF ENTRIES FOR ID4.  CHECK GPCT ENTRIES
!     FOR ONLY ONE ENTRY.
!
      ASSIGN 1000 TO iret
!
      ih = j2
      il = j1 - 1
      GOTO 900
   ENDIF
!
!     THAT END OF TABLE FAILED - TRY OTHER END
!
 800  j = -j
   limt(l) = len
   l = (j+2)/2 + 1
   lim = limt(l)
   kk = id4
   ASSIGN 1400 TO iret1
   GOTO 600
 900  il = il + 1
   kk = Iz(il)
   IF ( kk<=0 ) THEN
      id1 = id4
      GOTO 100
   ELSE
      ipar = 1
      GOTO 1100
   ENDIF
 1000 IF ( ipar==1 ) THEN
      id1 = kk
      GOTO 100
   ELSE
      IF ( il<ih ) GOTO 900
      id1 = id4
      GOTO 100
!
!     PIVOT NOW DETERMINED
!
   ENDIF
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
 1100 j1 = Iy(kk)
   j2 = Iy(kk+1) - 1
   IF ( ipar==1 ) j2 = min0(j1+2,j2)
   ipar = 0
   IF ( j2>=j1 ) THEN
!
      DO i = j1 , j2
         IF ( Iz(i)<0 ) EXIT
         IF ( Iz(i)==0 ) GOTO 1200
         ipar = ipar + 1
      ENDDO
   ENDIF
   GOTO iret
!
!     ERROR MESSAGES
!
 1200 err(1) = 1
   err(2) = id1
   k = 1
   GOTO 1500
 1300 err(1) = 2
   err(2) = n3
   err(3) = n4
   k = 2
   GOTO 1500
 1400 err(1) = 1
   err(2) = id4
   k = 3
!
 1500 i = lm(k)
   CALL wrtprt(Merr,err,m(i),nm(k))
   IF ( k==5 ) GOTO 99999
!
!     CONVERT TABLE TO ORIGINAL VALUES UNLESS THIS IS THE LAST CALL
!
 1600 il = Ngrid + 1
   i = Iy(il) - 1
   IF ( Deform==0 ) THEN
      DO j = 1 , i
         Iz(j) = iabs(Iz(j))
      ENDDO
!
      DO j1 = 1 , Ngrid
         IF ( Iy(j1)==Iy(j1+1) ) CYCLE
         i = Iy(j1)
         l = i
         n = Iy(j1+1) - 1
         IF ( i+1>n ) CYCLE
!
!     SHUTTLE EXCHANGE
!     (NOTE FROM G.CHAN/UNISYS  10/1990
!     THERE ARE MORE THAN JUST A SHUTTLE SORTING HERE. REPLACING THE
!     SHUTTLE EXCHANGE METHOD BY SORT ROUTINE, WHICH USES A MUCH FASTER
!     TECHNEQUE, DOES NOT WORK HERE)
!
 1620    IF ( Iz(i)>Iz(i+1) ) THEN
            k = Iz(i+1)
            Iz(i+1) = Iz(i)
            Iz(i) = k
            j = i
            DO WHILE ( j/=l )
               IF ( Iz(j)>=Iz(j-1) ) EXIT
               k = Iz(j)
               Iz(j) = Iz(j-1)
               Iz(j-1) = k
               j = j - 1
            ENDDO
         ENDIF
         IF ( i<n-1 ) THEN
            i = i + 1
            GOTO 1620
         ENDIF
!
      ENDDO
   ENDIF
!
!     A NONSTANDARD RETURN COULD BE ADDED HERE.  BAD PLOT RESULTS IF
!     THIS ROUTINE FAILS.  THE FRAME WILL BE PRESENT HOWEVER
!
99999 RETURN
END SUBROUTINE suplt
