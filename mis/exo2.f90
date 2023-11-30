
SUBROUTINE exo2
   IMPLICIT NONE
   INTEGER A(4) , Cor(1) , Ditsiz , Dry , Eol , Eor , Iadd , Irow , Items(7,1) , Lbuf , Line , Mach , Names(10) , Nitem , Nlpp ,    &
         & Nout , Nword(4) , Prc(2) , Sysbuf , Type(2) , Uname(2) , Unit , Z(1)
   DOUBLE PRECISION Da , Dz(1)
   REAL Rd , Rdrew , Rew , Wrt , Wrtrew , X1(3) , X2(6) , X3(6) , X4(2) , X5(3) , X6(9)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   LOGICAL Univac
   COMMON /blank / Dry , X1 , Uname , X2 , Type , Names , Unit , Univac , Lbuf , Iadd
   COMMON /itemdt/ Nitem , Items
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /sof   / X5 , Ditsiz
   COMMON /system/ Sysbuf , Nout , X3 , Nlpp , X4 , Line , X6 , Mach
   COMMON /type  / Prc , Nword
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zntpkx/ A , Irow , Eol , Eor
   COMMON /zzzzzz/ Z
   INTEGER all , bar , bdit , bgss , blank , bmdi , buf1 , buf2 , buf3 , buf4 , cstm , eltype , eog , eoi , eqss , hdr(7) , i ,     &
         & icore , idpcor , iprc , irfno , irw , iss , item , itm , itms(50) , j , jh , k , lams , lcore , loap , lods , matric ,   &
         & more , n , n2 , ncol , ncore , ngpel , nitems , ns , nss , nwds , nwds0 , offset , phase3 , plts , q4 , rc , scr1 , sof ,&
         & soln , sp , srd , subr(2) , swrt , t3 , tables , typout , whole(2) , xxxx
   INTEGER andf , ittype , korsz , rshift
   EXTERNAL andf , rshift
!
!     EXO2 PERFORMS EXTERNAL FORMAT SOFOUT OPERATIONS
!
   !>>>>EQUIVALENCE (Cor(1),Z(1))
   !>>>>EQUIVALENCE (Z(1),Dz(1)) , (A(1),Da)
   DATA all , matric , tables , phase3 , whole/4HALL  , 4HMATR , 4HTABL , 4HPHAS , 4HWHOL , 4HESOF/ , subr , blank , sof ,          &
       &xxxx/4HEXO2 , 4H     , 4H     , 4HSOF  , 4HXXXX/ , eqss , bgss , cstm , lods , loap/4HEQSS , 4HBGSS , 4HCSTM , 4HLODS ,     &
       &4HLOAP/ , plts , soln , lams , q4 , t3 , bar/4HPLTS , 4HSOLN , 4HLAMS , 2HQ4 , 2HT3 , 2HBR/ , srd , swrt , more , eog ,     &
      & eoi , sp/1 , 2 , 1 , 2 , 3 , 1/ , jh , scr1 , bdit , bmdi/1 , 301 , 4HDIT  , 4HMDI /
!
!     INITIALIZE
!
   IF ( Nitem>50 ) CALL errmkn(23,10)
   ncore = korsz(Z)
   i = ncore - Lbuf
   IF ( Mach==4 ) i = i - Lbuf
   ncore = i - 1
   irw = Iadd
   Iadd = i
   CALL exfort(3,Unit,0,0,irw,0,0)
   buf1 = ncore - Sysbuf + 1
   buf2 = buf1 - Sysbuf - 1
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   ncore = buf4 - 1
   IF ( buf4>0 ) THEN
      CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
!
!     CONSTRUCT ARRAY OF NAMES OF ITEMS TO BE COPIED
!
      IF ( Type(1)==all ) THEN
         nitems = Nitem
         DO i = 1 , Nitem
            itms(i) = Items(1,i)
         ENDDO
      ELSEIF ( Type(1)==tables ) THEN
         nitems = 0
         DO i = 1 , Nitem
            IF ( Items(2,i)<=0 ) THEN
               nitems = nitems + 1
               itms(nitems) = Items(1,i)
            ENDIF
         ENDDO
      ELSEIF ( Type(1)==matric ) THEN
         nitems = 0
         DO i = 1 , Nitem
            IF ( Items(2,i)>0 ) THEN
               nitems = nitems + 1
               itms(nitems) = Items(1,i)
            ENDIF
         ENDDO
      ELSEIF ( Type(1)/=phase3 ) THEN
         nitems = 2
         itms(1) = Type(1)
         itms(2) = Type(2)
         IF ( itms(2)==blank ) nitems = 1
      ELSE
         nitems = 0
         DO i = 1 , Nitem
            IF ( andf(Items(7,i),8)/=0 ) THEN
               nitems = nitems + 1
               itms(nitems) = Items(1,i)
            ENDIF
         ENDDO
      ENDIF
!
!     PUT NAMES OF ALL SUBSTRUCTURES TO BE COPIED AT TOP OF OPEN CORE
!
      nss = 0
      IF ( Names(1)==whole(1) .AND. Names(2)==whole(2) ) THEN
         n = Ditsiz/2
         DO i = 1 , n
            CALL fdit(i,j)
            IF ( Cor(j)/=blank ) THEN
               nss = nss + 1
               IF ( 2*nss>ncore ) GOTO 200
               Z(2*nss-1) = Cor(j)
               Z(2*nss) = Cor(j+1)
            ENDIF
         ENDDO
      ELSE
         DO i = 1 , 9 , 2
            IF ( Names(i)/=xxxx ) THEN
               nss = nss + 1
               IF ( 2*nss>ncore ) GOTO 200
               Z(2*nss-1) = Names(i)
               Z(2*nss) = Names(i+1)
            ENDIF
         ENDDO
      ENDIF
      icore = 2*nss + 3
      lcore = ncore - icore + 1
      idpcor = icore/2 + 1
      CALL page
!
!     WRITE OUT DIT AND MDI CONTROL WORDS
!
      n = Ditsiz/2
      IF ( 6*n>lcore ) GOTO 200
      hdr(1) = bdit
      hdr(2) = blank
      hdr(3) = blank
      hdr(4) = 2
      hdr(5) = Ditsiz
      hdr(6) = sp
      hdr(7) = eog
      CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
      DO i = 1 , n
         CALL fdit(i,j)
         Z(icore+2*i-2) = Cor(j)
         Z(icore+2*i-1) = Cor(j+1)
      ENDDO
      CALL exfort(swrt,Unit,2,Z(icore),Ditsiz,sp,0)
      hdr(1) = bmdi
      hdr(4) = 10
      hdr(5) = 6*n
      hdr(7) = eoi
      CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
      k = icore
      DO i = 1 , n
         CALL fmdi(i,j)
         Z(k) = rshift(Cor(j+1),20)
         Z(k+1) = andf(rshift(Cor(j+1),10),1023)
         Z(k+2) = andf(Cor(j+1),1023)
         Z(k+3) = andf(rshift(Cor(j+2),20),1023)
         Z(k+4) = andf(rshift(Cor(j+2),10),1023)
         Z(k+5) = andf(Cor(j+2),1023)
         k = k + 6
      ENDDO
      CALL exfort(swrt,Unit,10,Z(icore),6*n,sp,0)
!
!     LOOP OVER ALL SUBSTRUCTURES AND ITEMS, COPYING EACH ONE TO THE
!     EXTERNAL FILE
!
      DO iss = 1 , nss
         hdr(1) = Z(2*iss-1)
         hdr(2) = Z(2*iss)
         DO item = 1 , nitems
            hdr(3) = itms(item)
            itm = ittype(itms(item))
            IF ( itm==1 ) THEN
!
!     MATRICES
!
!     ON CDC MACHINE (NOT ANY 64-BIT MACHINE), FORCE ALL MATRIX DATA TO
!     BE DOUBLE PRECISION SO THE EXTRA DIGITS WONT BE LOST GOING TO
!     OTHER MACHINES
!
!     GROUP 0 -- MATRIX TRAILER
!
               CALL softrl(hdr,hdr(3),Z(icore-1))
               rc = Z(icore-1)
               IF ( rc==2 .OR. rc==4 .OR. rc==5 ) THEN
               ELSEIF ( rc==3 ) THEN
                  CYCLE
               ELSE
                  typout = Z(icore+3)
                  IF ( Mach==4 .AND. Prc(typout)==1 ) typout = typout + 1
                  Z(icore+3) = typout
                  ncol = Z(icore)
                  hdr(4) = 10
                  hdr(5) = 6
                  hdr(6) = sp
                  hdr(7) = eog
                  CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                  CALL exfort(swrt,Unit,10,Z(icore),6,sp,0)
!
!     MOVE MATRIX TO SCR2
!
                  CALL mtrxi(scr1,hdr,hdr(3),Z(buf4),rc)
                  CALL gopen(scr1,Z(buf4),Rdrew)
!
!     COPY MATRIX OUT ONE COLUMN AT A TIME, NON-ZEROES ONLY.
!
!                        ROW NO.  +
!                        VALUE     +
!                        ROW NO.    +
!                        VALUE       I  FORMAT OF ONE MATRIX
!                          .         I  COLUMN ON THE EXTERNAL
!                          .         I  FILE.
!                          .        +
!                        -1        +
!                        0.0      +
!
                  hdr(4) = 20 + typout
                  hdr(6) = typout
                  iprc = Prc(typout)
                  n = Nword(typout) + iprc
                  n2 = Nword(typout) + 1
                  DO j = 1 , ncol
                     nwds = 0
                     k = icore
                     CALL intpk(*2,scr1,0,typout,0)
                     DO
                        CALL zntpki
                        Z(k) = Irow
                        Z(k+iprc) = A(1)
                        IF ( typout/=1 ) THEN
                           Z(k+iprc+1) = A(2)
                           IF ( typout>3 ) THEN
                              Z(k+4) = A(3)
                              Z(k+5) = A(4)
                           ENDIF
                        ENDIF
                        nwds = nwds + n2
                        k = k + n
                        IF ( k+n>ncore ) GOTO 200
                        IF ( Eol/=0 ) EXIT
                     ENDDO
 2                   Z(k) = -1
                     Z(k+iprc) = 0
                     Z(k+iprc+1) = 0
                     Z(k+4) = 0
                     Z(k+5) = 0
                     nwds = nwds + n2
                     hdr(5) = nwds
                     IF ( j==ncol ) hdr(7) = eoi
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,20+typout,Z(icore),nwds,typout,Dz(idpcor))
                  ENDDO
                  CALL close(scr1,Rew)
                  GOTO 50
               ENDIF
            ELSE
               CALL sfetch(hdr,hdr(3),srd,rc)
               IF ( rc==1 ) THEN
                  CALL suread(Z(icore),lcore,nwds,rc)
                  IF ( rc/=2 ) GOTO 200
!
                  IF ( itms(item)==eqss ) THEN
!
!     EQSS
!
!     GROUP 0
!
                     n = nwds
                     ns = Z(icore+2)
                     IF ( ns>13 ) n = 30
                     hdr(4) = 3
                     hdr(5) = n
                     hdr(6) = sp
                     hdr(7) = eog
                     IF ( n<nwds ) hdr(7) = more
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,3,Z(icore),n,sp,0)
                     IF ( n/=nwds ) THEN
                        hdr(4) = 2
                        hdr(5) = nwds - n
                        hdr(7) = eog
                        CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                        CALL exfort(swrt,Unit,2,Z(icore+n),nwds-n,sp,0)
                     ENDIF
!
!     GROUPS 1 TO NS + 1
!
                     hdr(4) = 10
                     ns = ns + 1
                     DO j = 1 , ns
                        CALL suread(Z(icore),lcore,nwds,rc)
                        IF ( rc/=2 ) GOTO 200
                        hdr(5) = nwds
                        IF ( j==ns ) hdr(7) = eoi
                        CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                        CALL exfort(swrt,Unit,10,Z(icore),nwds,sp,0)
                     ENDDO
                     GOTO 50
                  ELSEIF ( itms(item)==bgss ) THEN
!
!     BGSS
!
!     GROUP 0
!
                     hdr(4) = 3
                     hdr(5) = 3
                     hdr(6) = sp
                     hdr(7) = eog
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,3,Z(icore),3,sp,0)
!
!     GROUP 1
!
                     CALL suread(Z(icore),lcore,nwds,rc)
                     IF ( rc/=2 ) GOTO 200
                     hdr(4) = 6
                     hdr(5) = nwds
                     hdr(7) = eoi
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,6,Z(icore),nwds,sp,0)
                     GOTO 50
                  ELSEIF ( itms(item)==cstm ) THEN
!
!     CSTM
!
!     GROUP 0
!
                     hdr(4) = 3
                     hdr(5) = 2
                     hdr(6) = sp
                     hdr(7) = eog
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,3,Z(icore),2,sp,0)
!
!     GROUP 1
!
                     IF ( icore+13>ncore ) GOTO 200
                     DO
                        CALL suread(Z(icore),14,nwds,rc)
                        IF ( rc==2 ) THEN
                           hdr(5) = 0
                           hdr(7) = eog
                           CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                           hdr(4) = 0
                           hdr(7) = eoi
                           CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                           GOTO 50
                        ELSE
                           hdr(4) = 8
                           hdr(5) = 4
                           hdr(7) = more
                           CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                           CALL exfort(swrt,Unit,8,Z(icore),4,sp,0)
                           hdr(4) = 9
                           hdr(5) = 10
                           CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                           CALL exfort(swrt,Unit,9,Z(icore+4),10,sp,0)
                        ENDIF
                     ENDDO
                  ELSE
                     IF ( itms(item)==lods ) GOTO 10
                     IF ( itms(item)==loap ) GOTO 10
                     IF ( itms(item)==plts ) THEN
!
!     PLTS
!
!     GROUP 0
!
                        n = nwds
                        ns = Z(icore+2)
                        hdr(6) = sp
                        hdr(4) = 3
                        hdr(5) = 3
                        hdr(7) = more
                        CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                        CALL exfort(swrt,Unit,3,Z(icore),3,sp,0)
                        DO j = 1 , ns
                           hdr(4) = 13
                           hdr(5) = 4
                           CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                           CALL exfort(swrt,Unit,13,Z(icore+14*j-11),4,sp,0)
                           hdr(4) = 9
                           hdr(5) = 10
                           IF ( j==ns ) hdr(7) = eog
                           CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                           CALL exfort(swrt,Unit,9,Z(icore+14*j-7),10,sp,0)
                        ENDDO
!
!     GROUP 1 -- BGPDT
!
                        CALL suread(Z(icore),lcore,nwds,rc)
                        IF ( rc==3 ) GOTO 20
                        IF ( rc/=2 ) GOTO 200
                        hdr(4) = 6
                        hdr(5) = nwds
                        CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                        CALL exfort(swrt,Unit,6,Z(icore),nwds,sp,0)
!
!     GROUP 2 -- EQEXIN
!
                        CALL suread(Z(icore),lcore,nwds,rc)
                        IF ( rc/=2 ) GOTO 200
                        hdr(4) = 10
                        hdr(5) = nwds
                        CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                        CALL exfort(swrt,Unit,10,Z(icore),nwds,sp,0)
!
!     GROUP 3 -- GPSETS
!
                        CALL suread(Z(icore),lcore,nwds,rc)
                        IF ( rc/=2 ) GOTO 200
                        hdr(4) = 10
                        hdr(5) = nwds
                        CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                        CALL exfort(swrt,Unit,10,Z(icore),nwds,sp,0)
!
!     GROUP 4 -- ELSETS
!
!     OUTPUT CHANGES MADE BY G.CHAN/UNISYS   4/91
!
!     IN 90 AND EARLIER VERSIONS, ONLY ONE ELEMENT PLOT SYMBOL WORD WAS
!     WRITTEN OUT USING FORMAT 2, AND ON NEXT ELSETS DATA LINE, FORMAT
!     10 WAS USED FOR ALL ELEMENTS. NO OFFSET DATA WAS PROVIDED FOR THE
!     BAR, QUAD4 AND TRIA3 ELEMENTS. THE NO. OF GRID POINT PER ELEMENT,
!     NGPEL, WAS THE FIRST WORD ON THE ELSETS DATA LINE.  (LINE=RECORD)
!     ALSO, THE 90 AND EARLIER VERSIONS DID NOT COUNT PROPERTY ID, PID,
!     ON THE ELSETS DATA LINE. THUS THE TOTAL NO. OF WORDS MAY BE IN
!     ERROR AND MAY CAUSE EXTRA ZEROS TO APPEAR AT THE END OF THE LINE.
!
!     IN 91 VERSION, ELEMENT PLOT SYMBOL LINE HAS 2 WORDS, SYMBOL AND
!     NGPEL, AND FORMAT 25 IS USED. ON NEXT ELSETS DATA LINE, FORMAT 10
!     IS USED FOR ALL ELEMENTS WITH NO OFFSETS. FORMAT 26 IS USED FOR
!     THE BAR WHICH HAS 6 OFFSET VALUES, AND FORMATS 27 AND 28 ARE USED
!     FOR TRIA3 AND QUAD4 WHICH HAVE 1 OFFSET VALUE EACH. NOTE THAT
!     NGPEL HAS BEEN MOVED, AND IS NO LONGER THE FIRST WORD ON THE
!     ELSETS DATA LINE.
!
                        hdr(7) = more
                        DO
!
!     READ PLOT SYMBOL, AND NO. OF GRID POINTS PER ELEMENT
!     SET UP NO. OF OFFSET DATA FOR BAR, QUAD4 AND TRIA3
!
                           CALL suread(Z(icore),2,nwds,rc)
                           IF ( rc>=2 ) THEN
!
!     WRITE END-OF-ITEM FOR PLTS
!
                              hdr(5) = 0
                              hdr(7) = eog
                              CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                              GOTO 20
                           ELSE
                              hdr(4) = 25
                              hdr(5) = 2
                              ngpel = Z(icore+1)
                              eltype = Z(icore)
                              offset = 0
                              IF ( eltype==bar ) offset = 6
                              IF ( eltype==q4 .OR. eltype==t3 ) offset = 1
                              CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                              CALL exfort(swrt,Unit,25,Z(icore),2,sp,0)
!
!     READ ELEMENT ID NUMBER, PROPERTY ID, GRID POINT CONNECTION INDICES
!     AND OFFSETS IF THEY EXIST
!     (ERROR IN 90 AND EARLIER VERSIONS, PROPERTY ID WAS LEFT OUT, AND
!     THEREFORE DATA COUNT PER ELEMENT WAS INCORRECT)
!
                              n = icore - ngpel - 2 - offset
                              DO
                                 n = n + ngpel + 2 + offset
                                 IF ( n>ncore ) GOTO 200
                                 CALL suread(Z(n),1,nwds,rc)
                                 IF ( Z(n)/=0 ) THEN
                                    IF ( n+ngpel+2+offset>ncore ) GOTO 200
                                    CALL suread(Z(n+1),ngpel+1,nwds,rc)
                                    IF ( offset/=0 ) CALL suread(Z(n+ngpel+2),offset,nwds,rc)
                                    IF ( rc==1 ) CYCLE
                                    IF ( rc==2 .OR. rc==3 ) GOTO 100
                                 ENDIF
!
!     ALL ELEMENTS OF ONE TYPE READ INTO CORE, NOW COPY OUT
!
                                 hdr(5) = n - icore + 1
                                 IF ( offset<1 ) THEN
!               REGULAR  QUAD4  BAR
!               ELEMENT  TRIA3
!
                                    hdr(4) = 10
                                 ELSEIF ( offset==1 ) THEN
                                    hdr(4) = 27
                                    IF ( eltype==q4 ) hdr(4) = 28
                                 ELSE
                                    hdr(4) = 26
                                 ENDIF
                                 CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                                 CALL exfort(swrt,Unit,hdr(4),Z(icore),hdr(5),sp,0)
                                 EXIT
                              ENDDO
                           ENDIF
                        ENDDO
                     ELSE
                        IF ( itms(item)==soln ) GOTO 30
                        IF ( itms(item)==lams ) GOTO 30
!
!     UNKNOWN TABLE ITME
!
                        Line = Line + 2
                        IF ( Line>Nlpp ) CALL page
                        WRITE (Nout,99001) Swm , itms(item)
99001                   FORMAT (A27,' 6360, SOFOUT (EXTERNAL) ENCOUNTERS A UNSUPPORTED ','TABLE ITEM ',A4,/35X,                     &
                               &'THE ITEM WILL NOT BE COPIED.')
                        CYCLE
                     ENDIF
                  ENDIF
               ELSEIF ( rc==3 ) THEN
                  CYCLE
               ENDIF
            ENDIF
            Line = Line + 2
            IF ( Line>Nlpp ) CALL page
            IF ( rc>3 ) THEN
               CALL smsg(rc-2,hdr(3),hdr)
            ELSE
               WRITE (Nout,99002) Uwm , (hdr(i),i=1,3)
!
!     MESSAGE TEXT
!
99002          FORMAT (A25,' 6340, SUBSTRUCTURE ',2A4,' ITEM ',A4,/5X,' PSEUDO-EXISTS ONLY AND CANNOT BE COPIED OUT BY EXIO.')
            ENDIF
            CYCLE
!
!     LODS AND LOAP
!
!     GROUP 0
!
 10         n = nwds
            ns = Z(icore+3)
            IF ( ns>13 ) n = 30
            hdr(4) = 3
            hdr(5) = n
            hdr(6) = sp
            hdr(7) = eog
            IF ( n<nwds ) hdr(7) = more
            CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
            CALL exfort(swrt,Unit,3,Z(icore),n,sp,0)
            IF ( n/=nwds ) THEN
               hdr(4) = 2
               hdr(5) = nwds - n
               hdr(7) = eog
               CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
               CALL exfort(swrt,Unit,2,Z(icore+n),nwds-n,sp,0)
            ENDIF
!
!     GROUP 1 TO NS
!
            hdr(4) = 10
            DO j = 1 , ns
               CALL suread(Z(icore),lcore,nwds,rc)
               IF ( rc/=2 ) GOTO 200
               hdr(5) = nwds
               IF ( j==ns ) hdr(7) = eoi
               CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
               CALL exfort(swrt,Unit,10,Z(icore),nwds,sp,0)
            ENDDO
            GOTO 50
 20         hdr(4) = 0
            hdr(5) = 0
            hdr(7) = eoi
            CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
            GOTO 50
!
!     SOLN AND LAMS
!
 30         irfno = Z(icore+2)
            IF ( irfno/=1 ) THEN
               IF ( irfno/=2 ) THEN
                  IF ( irfno==3 ) THEN
!
!     GROUP 0 -- NORMAL MODES (REAL OR COMPLEX)
!
                     ns = Z(icore+3)
                     hdr(4) = 3
                     hdr(5) = 4
                     hdr(6) = sp
                     hdr(7) = eog
                     IF ( ns<=0 ) hdr(7) = eoi
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,3,Z(icore),4,sp,0)
                     IF ( ns<=0 ) GOTO 50
!
!     GROUP 1 -- NORMAL MODES
!
                     CALL suread(Z(icore),lcore,nwds,rc)
                     IF ( rc/=2 ) GOTO 200
                     hdr(4) = 20
                     hdr(5) = nwds
                     hdr(7) = eoi
                     IF ( itms(item)==lams ) hdr(7) = eog
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,20,Z(icore),nwds,sp,0)
                     IF ( itms(item)/=lams ) GOTO 50
!
!     GROUP 2 -- NORMAL MODES (LAMS ITEM ONLY)
!
                     CALL suread(Z(icore),lcore,nwds,rc)
                     IF ( rc/=2 ) GOTO 200
                     hdr(4) = 10
                     hdr(5) = nwds
                     hdr(7) = eoi
                     CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                     CALL exfort(swrt,Unit,10,Z(icore),nwds,sp,0)
                     GOTO 50
                  ELSE
                     IF ( irfno==8 ) GOTO 40
                     IF ( irfno==9 ) GOTO 40
                     Line = Line + 2
                     IF ( Line>Nlpp ) CALL page
                     WRITE (Nout,99003) Swm , irfno , hdr(1) , hdr(2)
99003                FORMAT (A27,' 6358, ILLEGAL RIGID FORMAT NUMBER ',I5,' IN SOLN ITEM FOR SUBSTRUCTURE ',2A4,1H.,/34X,           &
                            &'THE ITEM WILL NOT BE COPIED.')
                     GOTO 50
                  ENDIF
               ENDIF
            ENDIF
!
!     GROUP 0 -- STATICS
!
            n = nwds
            ns = Z(icore+3)
            IF ( ns>6 ) n = 23
            ns = Z(icore+4)
            hdr(4) = 16
            hdr(5) = n
            hdr(6) = sp
            hdr(7) = eog
            IF ( n<nwds ) hdr(7) = more
            CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
            CALL exfort(swrt,Unit,16,Z(icore),n,sp,0)
            IF ( n/=nwds ) THEN
               hdr(4) = 17
               hdr(5) = nwds - n
               hdr(7) = eog
               CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
               CALL exfort(swrt,Unit,17,Z(icore+n),nwds-n,sp,0)
            ENDIF
!
!     GROUPS 1 TO NS (ONE PER SUBCASE) -- STATICS
!
            DO j = 1 , ns
               CALL suread(Z(icore),lcore,nwds,rc)
               IF ( rc/=2 ) GOTO 200
               n = nwds
               IF ( Z(icore)>5 ) n = 11
               hdr(4) = 18
               hdr(5) = n
               hdr(7) = eog
               IF ( j==ns ) hdr(7) = eoi
               IF ( n<nwds ) hdr(7) = more
               CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
               CALL exfort(swrt,Unit,18,Z(icore),n,sp,0)
               IF ( n/=nwds ) THEN
                  hdr(4) = 19
                  hdr(5) = nwds - n
                  hdr(7) = eog
                  IF ( j==ns ) hdr(7) = eoi
                  CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
                  CALL exfort(swrt,Unit,19,Z(icore+n),nwds-n,sp,0)
               ENDIF
            ENDDO
            GOTO 50
!
!     GROUP 0 -- DYNAMICS
!
 40         ns = Z(icore+3)
            nwds0 = 3*ns + 5
            n = nwds0
            IF ( ns>6 ) n = 23
            ns = Z(icore+4) + 1
            IF ( Z(icore+nwds0)==0 ) ns = 1
            hdr(4) = 16
            hdr(5) = n
            hdr(6) = sp
            hdr(7) = more
            CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
            CALL exfort(swrt,Unit,16,Z(icore),n,sp,0)
            IF ( n/=nwds0 ) THEN
               hdr(4) = 17
               hdr(5) = nwds0 - n
               CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
               CALL exfort(swrt,Unit,17,Z(icore+n),nwds0-n,sp,0)
            ENDIF
            hdr(4) = 10
            hdr(5) = nwds - nwds0
            hdr(7) = eog
            CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
            CALL exfort(swrt,Unit,10,Z(icore+nwds0),nwds-nwds0,sp,0)
!
!     GROUP 1 TO NS+1 -- DYNAMICS
!
            DO j = 1 , ns
               CALL suread(Z(icore),lcore,nwds,rc)
               IF ( rc/=2 ) GOTO 200
               hdr(4) = 9
               hdr(5) = nwds
               hdr(7) = eog
               IF ( j==ns ) hdr(7) = eoi
               CALL exfort(swrt,Unit,jh,hdr,7,sp,0)
               CALL exfort(swrt,Unit,9,Z(icore),nwds,sp,0)
            ENDDO
!
!     WRITE USER MESSAGE FOR SUCCESSFUL COPY
!
 50         Line = Line + 1
            IF ( Line>Nlpp ) CALL page
            WRITE (Nout,99004) Uim , hdr(1) , hdr(2) , hdr(3) , sof , Uname
99004       FORMAT (A29,' 6357, SUBSTRUCTURE ',2A4,' ITEM ',A4,' SUCCESSFULLY COPIED FROM ',A4,' TO ',2A4)
         ENDDO
      ENDDO
!
!     NORMAL MODULE COMPLETION.  WRITE LOGICAL EOF
!
      CALL exfort(4,Unit,0,0,1,0,0)
      CALL sofcls
      RETURN
!
!     ABNORMAL MODULE COMPLETION
!
 100  CALL smsg(rc+4,itms(item),hdr)
      GOTO 300
   ENDIF
 200  CALL mesage(8,0,subr)
 300  Dry = -2
   CALL sofcls
   RETURN
END SUBROUTINE exo2