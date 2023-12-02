!*==ema.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ema
   USE c_blank
   USE c_lhpwx
   USE c_ma1xx
   USE c_machin
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zblpkx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(18) :: d
   REAL :: eps , factor , yj
   LOGICAL :: first , last , piez
   INTEGER , SAVE :: gpect , kons , lbuf , mdict , scr1 , scr2 , xblock , xemd , xgg
   INTEGER :: gpectx , icode , id , indx , ipiez , itab , ivpt , jdict , jjlast , kfact , ksft , mask , maxblk , maxdof , maxel ,   &
            & maxgpe , nsil , ntab , nxtcol , openr , openw , output , ppoint , sysbuf
   INTEGER , DIMENSION(6) , SAVE :: hdr
   INTEGER , DIMENSION(180) :: ihq
   INTEGER , DIMENSION(2) :: is
   INTEGER , DIMENSION(2) , SAVE :: ma1h
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(4) :: msg
   REAL(REAL64) , DIMENSION(1) :: xd , zd
   REAL(REAL64) :: xdd
   REAL , DIMENSION(2) :: xs
   REAL , DIMENSION(1) :: y
   EXTERNAL andf , bckrec , bisloc , bldpk , bldpkn , close , decode , filpos , fname , gopen , korsz , lshift , mesage , mrge ,    &
          & open , orf , rdtrl , read , rshift , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     DMAP SEQUENCE
!
!     EMA    GPECT,XEMD,XBLOCK/XGG/C,N,NOK4/C,N,WTMASS $
!
!            WHERE NOK4 .NE. -1 TO BUILD K4GG (USE DAMPING FACTOR),
!                       .EQ. -1 TO IGNORE DAMPING FACTOR
!
!     EMA USES TWO SCRATCH FILES
!
!WKBI 1/95
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Output) , (Ksystm(78),Ipiez)
   !>>>>EQUIVALENCE (Z(1),Zd(1)) , (xs(1),xd(1),is(1)) , (Z(1),Y(1)) , (Buf(1),D(1)) , (Ipvt,Ivpt) , (Buf(1),Ihq(1))
   DATA lbuf/100/ , mcb/7*0/ , ma1h/4HEMA  , 2H  / , kons/14/ , mdict/6/ , hdr/6*0/
   DATA gpect , xemd , xblock/101 , 102 , 103/ , xgg/201/ , scr1 , scr2/301 , 302/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     RE-SET KONS IF HALF WORD IS LARGER THAN THAN 16 BITS
!
         IF ( ihalf>=18 ) kons = 16
         IF ( ihalf>=30 ) kons = 24
!
!     ALLOCATE BUFFERS. OPEN GPECT AND ALLOCATE A TABLE OF 4 WORDS
!     PER ELEMENT TYPE. OPEN SCRATCH FILE FOR GPECTX. OPEN XEMD.
!
         mcb(1) = 0
         mcb(2) = 0
         mcb(3) = 0
         mcb(4) = 0
         mcb(5) = 0
         mcb(6) = 0
         mcb(7) = 0
         maxii = 2**kons - 1
         maxblk = 0
         ksft = kshift
         mask = lshift(jhalf,ihalf)
         buf1 = korsz(z) - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         lcore = buf1 + sysbuf - 1
         k4flag = 1
         IF ( nok4==-1 ) k4flag = 0
!
!     SET LOGICAL VARIABLE TRUE IF THIS IS A PIEZOELECRRIC COUPLED
!     PROBLEM AND STRUCTURAL DAMPING FLAG IS ON
!
         piez = .FALSE.
         IF ( ipiez==1 .AND. k4flag/=0 ) piez = .TRUE.
         buf(1) = xblock
         CALL rdtrl(buf)
         IF ( buf(1)<0 ) THEN
            msg(3) = xblock
            msg(4) = 1001
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ELSE
            prec = buf(2)
            buf(1) = gpect
            CALL rdtrl(buf)
            IF ( buf(1)<0 ) THEN
               msg(3) = gpect
               msg(4) = 1002
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ELSE
               idict = 4*buf(2) + 1
               nsil = buf(3)
               maxel = buf(4)
               maxdof = buf(5)
               CALL gopen(gpect,z(buf1),rdrew)
               l = (2**(ihalf+ihalf-2)-1)*2 + 1
               maxipv = rshift(l,kons)
               scrin = gpect
               scrout = scr1
               CALL gopen(xemd,z(buf3),rdrew)
               CALL open(*160,scrout,z(buf2),wrtrew)
               CALL write(scrout,buf,3,1)
!
!     SET SWITCHES FOR MULTIPLICATION BY DAMPING
!     OR WEIGHT MASS FACTOR (OR BOTH)
!
               eps = abs(wtmass-1.0)
               IF ( eps<1.E-6 .AND. k4flag==0 ) ASSIGN 128 TO kfact
               IF ( eps<1.E-6 .AND. k4flag/=0 ) ASSIGN 122 TO kfact
               IF ( eps>1.E-6 .AND. k4flag==0 ) ASSIGN 124 TO kfact
               IF ( eps>1.E-6 .AND. k4flag/=0 ) ASSIGN 126 TO kfact
!
!     FILL CORE WITH ELEMENT MATRIX DICTIONARIES. FOR EACH ELEMENT TYPE
!     STORE POINTER TO 1ST DICT AND THE NBR OF DICTS IN TABLE AT TOP OF
!     CORE ALSO STORE LENGTH OF EACH DICTIONARY AND FORMAT CODE.
!
               l = idict
               DO i = 1 , idict
                  z(i) = 0
               ENDDO
               maxn = 0
               last = .TRUE.
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*180,xemd,hdr,3,0,nread)
         ielem = 4*hdr(1) - 3
         ldict = hdr(2)
         z(ielem) = l
         z(ielem+2) = ldict
         z(ielem+3) = hdr(3)
         spag_nextblock_1 = 3
      CASE (3)
         DO WHILE ( l+ldict<buf3 )
            jdict = ldict
            CALL read(*200,*20,xemd,z(l),ldict,0,nread)
            l = l + ldict
         ENDDO
         last = .FALSE.
         z(ielem+1) = (l-z(ielem))/ldict
         high = z(l-jdict)
         IF ( z(ielem+1)==0 ) z(ielem) = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      IF ( nread/=0 ) THEN
!
!     FATAL ERROR MESSAGES
!
            msg(1) = 1016
            spag_nextblock_1 = 19
         ELSE
            high = z(l-ldict)
            z(ielem+1) = (l-z(ielem))/ldict
            spag_nextblock_1 = 2
         ENDIF
         CYCLE
 40      last = .TRUE.
         CALL close(xemd,clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!
!     PASS GPECT (OR PARTIALLY COMPLETED GPECTX) ENTRY BY ENTRY.
!     IF ENTRY HAS BEEN COMPLETED, COPY IT OUT.  OTHERWISE, LOCATE
!     DICTIONARY FOR ELEMENT (IF IN CORE) AND ATTACH IT.
!     DETERMINE LENGTH OF LONGEST RECORD IN GPECTX.
!     DETERMINE THE MAXIMUM LENGTH OF ONE COLUMN OF AN ELEMENT MATRIX.
!
         nhdr = 2
         IF ( last ) nhdr = 5
         maxgpe = 0
         low = z(idict)
         ngps = 0
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ AND WRITE HEADER FOR RECORD (SIL, DOF, ETC.)
!
         CALL read(*100,*220,scrin,hdr,2,0,nread)
         CALL write(scrout,hdr,nhdr,0)
         gpewds = nhdr
         ngps = ngps + 1
 60      SPAG_Loop_1_1: DO
!
!     READ FIRST WORD  OF ENTRY ON GPECT. TEST FOR DICT ALREADY ATTACHED
!
            CALL read(*240,*80,scrin,buf,1,0,nread)
            IF ( iabs(buf(1))>lbuf ) THEN
               msg(1) = 1035
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( buf(1)<0 ) THEN
!
!     DICTIONARY NOT ATTACHED---TRY TO LOCATE DICT IN CORE
!
                  m = -buf(1)
                  CALL read(*300,*320,scrin,buf(2),m,0,nread)
                  IF ( buf(2)<low ) CYCLE
                  IF ( buf(2)>high ) THEN
                     IF ( last ) CYCLE
                     n = m + 1
                  ELSE
                     kelem = 4*buf(3) - 3
                     IF ( z(kelem)==0 ) CYCLE
                     l = z(kelem)
                     n = z(kelem+1)
                     ldict = z(kelem+2)
                     nlocs = z(kelem+3)
                     CALL bisloc(*60,buf(2),z(l),ldict,n,k)
                     k = k + l - 1
                     IF ( k4flag/=0 .AND. z(k+4)==0 ) CYCLE
!
!     DICTIONARY LOCATED---WRITE OUT COMPLETED ENTRY ON GPECTX
!         0             NO. OF WORDS IN ENTRY (NOT INCL THIS WORD)
!       1 - 5           ELEM ID, F, N, C, GE
!         6             LOC OF ELEMENT MATRIX COLUMNS FOR CURRENT PIVOT
!       7 - 6+NGRIDS    SIL-S OF CONNECTED GRID POINTS
!
                     ngrids = m - 2
                     indx = k + mdict - 1
                     IF ( nlocs==1 ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( ngrids>nlocs ) THEN
                        msg(1) = 1052
                        spag_nextblock_1 = 19
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        kk = 1
                        DO i = 1 , ngrids
                           IF ( kk/=1 ) THEN
!
!     CHECK FOR DUPLICATE SILS - E.G. HBDY ELEMENT WITH AMBIENT PTS
!
                              DO WHILE ( buf(kk+3)==buf(kk+2) )
                                 kk = kk + 1
                              ENDDO
                           ENDIF
                           IF ( buf(kk+3)==hdr(1) ) THEN
!
!     SIL THAT MATCHES THE PIVOT FOUND.  NOW INSURE THAT THIS SIL
!     HAS NOT BEEN ALREADY CONNECTED DUE TO A PREVIOUS ENTRY IN THIS
!     GPECT RECORD.  (CAUSED BY DUPLICATE IDS I.E. CELAS2)
!
!     GINO-LOC WILL NOW BE ZERO IF THAT IS TRUE
!
                              indx = k + mdict + i - 2
                              IF ( z(indx)/=0 ) EXIT SPAG_Loop_1_1
                           ENDIF
                           kk = kk + 1
                        ENDDO
                        CYCLE
                     ENDIF
                  ENDIF
               ELSE
!
!     DICTIONARY ALREADY ATTACHED---READ REMAINDER OF ENTRY AND
!     COPY ENTRY TO GPECTX.
!
                  CALL read(*260,*280,scrin,buf(2),buf(1),0,nread)
                  n = buf(1) + 1
               ENDIF
               CALL write(scrout,buf,n,0)
               gpewds = gpewds + n
            ENDIF
         ENDDO SPAG_Loop_1_1
         z(k+mdict-1) = z(indx)
         IF ( z(k+1)==2 ) THEN
            buf(4) = buf(i+3)
            ngrids = 1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( ldict-nlocs+1/=mdict ) THEN
            msg(1) = 1056
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSE
            n = mdict + ngrids
            CALL write(scrout,n,1,0)
            CALL write(scrout,z(k),mdict,0)
            maxblk = max0(z(indx)/ksft,maxblk)
!
!     ZERO GINO-LOC AS HAVING BEEN USED NOW.
!
            z(indx) = 0
            CALL write(scrout,buf(4),ngrids,0)
            maxn = max0(maxn,z(k+2))
            gpewds = gpewds + n + 1
            GOTO 60
         ENDIF
!
!     HERE ON END-OF-RECORD ON GPECT
!
 80      CALL write(scrout,0,0,1)
         maxgpe = max0(maxgpe,gpewds)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     HERE ON END-OF-FILE ON GPECT---TEST FOR COMPLETION OF GPECTX
!
 100     CALL close(scrin,clsrew)
         CALL close(scrout,clsrew)
         IF ( ngps/=nsil ) THEN
            msg(1) = 1110
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( last ) THEN
!
!     HERE WE GO NOW FOR THE ASSEMBLY PHASE---PREPARE BY ALLOCATING
!     STORAGE FOR ONE ELEMENT MATRIX COLUMN AND ITS ROW POSITIONS
!
            irowp = prec*maxn + 1
            igpx = irowp + maxn
            first = .TRUE.
            gpectx = scrout
            mcb(1) = xgg
            mcb(4) = 6
            mcb(5) = prec
            mcb(6) = 0
            mcb(7) = 0
            last = .FALSE.
            nrec = 0
            maxnpr = maxn*prec
            openr = rdrew
            openw = wrtrew
            oldcod = 0
            itab = buf1 - maxblk
            ntab = buf1 - 1
            IF ( itab<igpx ) THEN
               msg(1) = 1202
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
!
!     GPECTX NOT COMPLETE---SWITCH FILES AND MAKE ANOTHER PASS
!
            IF ( scrin==gpect ) scrin = scr2
            k = scrin
            scrin = scrout
            scrout = k
            CALL gopen(scrin,z(buf1),rdrew)
            CALL gopen(scrout,z(buf2),wrtrew)
            l = idict
            ldict = z(ielem+2)
            nlocs = z(ielem+3)
            DO i = 1 , idict
               z(i) = 0
            ENDDO
            last = .TRUE.
            z(ielem) = idict
            z(ielem+2) = ldict
            z(ielem+3) = nlocs
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     BEGIN A PASS - OPEN GPECTX
!
         ipvt = igpx
         jj = itab - 3
         DO indx = itab , ntab
            z(indx) = 0
         ENDDO
         CALL gopen(gpectx,z(buf1),openr)
         spag_nextblock_1 = 8
      CASE (8)
!
!     READ A RECORD FROM GPECTX INTO CORE
!
         IF ( ivpt+maxgpe>=jj .OR. ipvt>maxipv ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*340,*120,gpectx,z(ivpt),maxgpe+1,1,nread)
         msg(1) = 1220
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 120     icol = ipvt + nread
         nrec = nrec + 1
!
!     MAKE A PASS THROUGH EACH ELEMENT CONNECTED TO THE PIVOT---FORM THE
!     UNION OF ALL CODE WORDS AND STORE ELEMENT POINTERS IN LIST AT THE
!     END OF OPEN CORE
!
         ppoint = lshift(ipvt,kons)
         ii = ipvt + 5
         union = 0
         z(ipvt+2) = 0
         z(ipvt+3) = 0
         z(ipvt+4) = ipvt
         DO WHILE ( ii<icol )
            IF ( z(ii)<0 ) THEN
               msg(1) = 1231
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSE
               kk = ii - ipvt
               IF ( kk>maxii ) THEN
                  msg(1) = 1232
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( jj<=icol ) THEN
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  z(jj) = z(ii+mdict)
                  z(jj+1) = orf(ppoint,kk)
                  z(jj+2) = 0
                  indx = itab + z(jj)/ksft - 1
                  IF ( indx>ntab ) THEN
                     msg(1) = 1238
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( z(indx)/=0 ) THEN
                        jjlast = itab - andf(z(indx),jhalf)
                        z(jjlast+2) = jj
                        z(indx) = orf(andf(z(indx),mask),itab-jj)
                     ELSE
                        z(indx) = orf(lshift(itab-jj,ihalf),itab-jj)
                     ENDIF
                     jj = jj - 3
                     union = orf(union,z(ii+4))
                     ii = ii + z(ii) + 1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( ii/=icol ) THEN
            msg(1) = 1235
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     FORM THE LIST OF NON-NULL COLUMNS TO BE BUILT FOR THIS PIVOT
!
            IF ( union==0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( union/=oldcod ) THEN
               CALL decode(union,scalas,nsca)
               oldcod = union
            ENDIF
            z(ipvt+2) = icol
            IF ( icol+nsca>=jj ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ii = icol
            DO l = 1 , nsca
               z(ii) = z(ipvt) + scalas(l)
               ii = ii + 1
            ENDDO
            irow = ii
!
!     NOW MAKE A PASS AGAIN THROUGH EACH ELEMENT CONNECTED TO CURRENT
!     PIVOT AND FORM A LIST OF UNIQUE ROW INDICES.
!
            ii = ipvt + 5
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         l1 = ii + mdict + 1
         l2 = ii + z(ii)
         IF ( oldcod/=z(ii+4) ) THEN
            icode = z(ii+4)
            CALL decode(icode,scalas,nsca)
            oldcod = z(ii+4)
         ENDIF
         kk = irow
         IF ( ii/=ipvt+5 ) THEN
            j = irowp
            DO l = l1 , l2
!
!     IGNORE DUPLICATE IDS AS IN SOME CELAS2 ELEMENTS ETC.
!
               IF ( l<=l1 .OR. z(l)/=z(l-1) ) THEN
                  DO i = 1 , nsca
                     z(j) = z(l) + scalas(i)
                     j = j + 1
                  ENDDO
               ENDIF
            ENDDO
            IF ( j>igpx ) THEN
               msg(1) = 1264
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSE
               m = j - irowp
               IF ( irow+nbrwds+m>=jj ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL mrge(z(irow),nbrwds,z(irowp),m)
               nrow = irow + nbrwds - 1
               IF ( nrow>=jj ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ELSE
            DO l = l1 , l2
!
!     IGNORE DUPLICATE IDS AS IN SOME CELAS2 ELEMENTS ETC.
!
               IF ( l<=l1 .OR. z(l)/=z(l-1) ) THEN
                  DO i = 1 , nsca
                     z(kk) = z(l) + scalas(i)
                     kk = kk + 1
                     IF ( kk>=jj ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            nrow = kk - 1
            nbrwds = kk - irow
         ENDIF
         ii = l2 + 1
         IF ( ii<icol ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         z(ipvt+3) = irow
!
!     NOW ALLOCATE STORAGE FOR COLUMNS OF XGG ASSOCIATED WITH THIS PIVOT
!
         imat = nrow + 1
         nbrcol = irow - icol
         nbrrow = imat - irow
         nbrwds = prec*nbrcol*nbrrow
         nmat = imat + nbrwds - 1
         IF ( nmat>=jj ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = imat , nmat
            z(i) = 0
         ENDDO
         z(ipvt+4) = imat
         ii = nmat + 1
         spag_nextblock_1 = 10
      CASE (10)
!
!     ADVANCE POINTER AND TRY TO GET ANOTHER PIVOT ALLOCATED
!
         ilist = jj + 3
         npvt = ipvt
         IF ( nrec==ngps ) THEN
!
!     HERE WHEN LAST PIVOT POINT HAS BEEN READ AND ALLOCATED
!
            last = .TRUE.
            op = clsrew
            spag_nextblock_1 = 13
         ELSE
            ipvt = ii
            spag_nextblock_1 = 8
         ENDIF
      CASE (11)
!
!     HERE WHEN STORAGE EXCEEDED DURING PROCESSING OF A PIVOT.
!     IF FIRST PIVOT ON PASS, INSUFFICIENT CORE FOR MODULE.
!     OTHERWISE, BACKSPACE GPECTX AND PREPARE TO PROCESS ALL
!     PIVOTS IN CORE WHICH HAVE BEEN COMPLETELY ALLOCATED.
!
         IF ( ipvt==igpx ) CALL mesage(-8,0,ma1h)
         CALL bckrec(gpectx)
         nrec = nrec - 1
         spag_nextblock_1 = 12
      CASE (12)
         op = cls
         IF ( ipvt==igpx ) CALL mesage(-8,0,ma1h)
         spag_nextblock_1 = 13
      CASE (13)
!
!     CLOSE GPECTX. OPEN XBLOCK.
!
         CALL close(gpectx,op)
         nwds = buf1 - ilist
         IF ( nwds>0 ) THEN
            CALL gopen(xblock,z(buf1),rdrew)
            oldcod = 0
!
!     PASS THE LIST OF ELEMENT MATRIX POINTERS. EACH ENTRY POINTS TO THE
!     PIVOT POINT AND ELEMENT DICTIONARY IN CORE AND TO THE POSITION IN
!     THE XBLOCK FILE CONTAINING THE ASSOCIATED ELEMENT MATRIX COLUMNS.
!     WHEN PROCESSING OF ALL ENTRIES IS COMPLETE, COLUMNS OF XGG NOW IN
!     CORE ARE COMPLETE.
!
            DO indx = itab , ntab
               IF ( z(indx)/=0 ) THEN
                  jj = itab - rshift(z(indx),ihalf)
                  IF ( jj>=ilist ) THEN
                     SPAG_Loop_2_2: DO
                        CALL filpos(xblock,z(jj))
                        ipvt = rshift(z(jj+1),kons)
                        IF ( ipvt>npvt ) THEN
                           spag_nextblock_1 = 18
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        ielem = ipvt + andf(z(jj+1),maxii)
                        icol = z(ipvt+2)
                        irow = z(ipvt+3)
                        imat = z(ipvt+4)
!
!     DECODE CODE WORD FOR ELEMENT. FORM LIST OF ROW INDICES DESCRIBING
!     TERMS IN THE ELEMENT MATRIX COLUMN. THEN CONVERT THESE INDICES TO
!     RELATIVE ADDRESSES IN XGG COLUMN IN CORE (USE LIST OF ROW INDICES
!     FOR XGG COLUMN TO DO THIS).
!
                        IF ( z(ielem+4)/=oldcod ) THEN
                           icode = z(ielem+4)
                           CALL decode(icode,scalas,nsca)
                           oldcod = z(ielem+4)
                        ENDIF
                        l1 = ielem + mdict + 1
                        l2 = ielem + z(ielem)
                        k = irowp
                        DO l = l1 , l2
!
!     IGNORE DUPLICATE IDS AS IN SOME CELAS2 ELEMENTS ETC.
!
                           IF ( l<=l1 .OR. z(l)/=z(l-1) ) THEN
                              DO i = 1 , nsca
                                 z(k) = z(l) + scalas(i)
                                 k = k + 1
                              ENDDO
                           ENDIF
                        ENDDO
                        nrowp = k - 1
                        IF ( nrowp>=igpx ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        nrow = imat - 1
                        id = z(irowp)
                        CALL bisloc(*140,id,z(irow),1,(imat-irow),irowx)
                        irowx = irow + irowx - 1
                        DO k = irowp , nrowp
                           spag_nextblock_2 = 1
                           SPAG_DispatchLoop_2: DO
                              SELECT CASE (spag_nextblock_2)
                              CASE (1)
                                 DO i = irowx , nrow
                                    IF ( z(k)==z(i) ) THEN
                                       spag_nextblock_2 = 2
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
                                 ENDDO
                                 spag_nextblock_1 = 15
                                 CYCLE SPAG_DispatchLoop_1
                              CASE (2)
                                 z(k) = (i-irow)*prec
                                 irowx = i + 1
                                 EXIT SPAG_DispatchLoop_2
                              END SELECT
                           ENDDO SPAG_DispatchLoop_2
                        ENDDO
                        nbrrow = nrowp - irowp + 1
!
!     PREPARE TO READ EACH COLUMN OF ELEMENT MATRIX
!
                        ncol = irow - 1
                        icolx = icol
                        nbrwds = z(ielem+3)*prec
                        IF ( z(ielem+2)==2 ) nbrwds = prec
                        IF ( nbrwds>maxnpr ) THEN
                           spag_nextblock_1 = 16
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        DO i = 1 , nsca
                           spag_nextblock_3 = 1
                           SPAG_DispatchLoop_3: DO
                              SELECT CASE (spag_nextblock_3)
                              CASE (1)
!
!     READ A COLUMN OF THE ELEMENT MATRIX AND DETERMINE ADDRESS
!     OF FIRST WORD OF ASSOCIATED COLUMN OF XGG IN CORE.
!
                                 CALL read(*360,*380,xblock,z,nbrwds,0,nread)
                                 col = z(ipvt) + scalas(i)
                                 DO k = icolx , ncol
                                    IF ( col==z(k) ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                 ENDDO
                                 spag_nextblock_1 = 17
                                 CYCLE SPAG_DispatchLoop_1
                              CASE (2)
                                 imatn = imat + (imat-irow)*(k-icol)*prec
                                 icolx = k + 1
                                 IF ( z(ielem+2)==2 ) THEN
!
!     ELEMENT MATRIX IS DIAGONAL
!
                                    nbrrow = 1
                                    z(irowp) = z(irowp+i-1)
                                 ENDIF
!
!     IF DAMPING OR WEIGHT MASS FACTOR (OR BOTH) PRESENT, MULTIPLY
!     EACH TERM IN THE ELEMENT MATRIX COLUMN BY THE FACTOR.
!
                                 GOTO kfact
 122                             factor = y(ielem+5)
                                 spag_nextblock_3 = 3
                                 CYCLE SPAG_DispatchLoop_3
 124                             factor = wtmass
                                 spag_nextblock_3 = 3
                                 CYCLE SPAG_DispatchLoop_3
 126                             factor = y(ielem+5)*wtmass
                                 spag_nextblock_3 = 3
                              CASE (3)
                                 IF ( prec==2 ) THEN
                                    m = nbrwds/2
                                    DO k = 1 , m
                                       IF ( piez .AND. (i==nsca .OR. mod(k,4)==0) ) zd(k) = 0.D0
                                       zd(k) = factor*zd(k)
                                    ENDDO
                                    spag_nextblock_3 = 5
                                 ELSE
                                    DO k = 1 , nbrwds
!
!     FOR PIEZOELECTRIC COUPLED PROBLEMS, ANY STRUCTURAL DAMPING COEFF.
!     SHOULD MULTIPLY ONLY THE UNCOUPLED STRUCTURAL TERMS. SO, SKIP
!     EVERY 4TH TERM IN A COLUMN AND SKIP EVERY 4TH COLUMN
!
                                       IF ( piez .AND. (i==nsca .OR. mod(k,4)==0) ) y(k) = 0.
                                       y(k) = factor*y(k)
                                    ENDDO
                                    spag_nextblock_3 = 4
                                 ENDIF
                                 CYCLE
!
!     NOW ADD TERMS OF THE ELEMENT MATRIX INTO XGG
!
 128                             IF ( prec==2 ) THEN
                                    spag_nextblock_3 = 5
                                    CYCLE SPAG_DispatchLoop_3
                                 ENDIF
                                 spag_nextblock_3 = 4
                              CASE (4)
!
!     DO ARITHMETIC IN SINGLE PRECISION
!
                                 DO k = 1 , nbrrow
                                    j = imatn + z(irowp+k-1)
!WKBI 1/95
                                    yj = y(j)
                                    y(j) = y(j) + y(k)
!WKBNB 1/95  FOLLOWING CODE WILL CAUSE A TRUE ZERO WHEN SUBTRACTING SAME NO.
                                    IF ( y(k)/=0.0 ) THEN
                                       yj = yj/y(k)
                                       IF ( yj<=-.999999999998 .AND. yj>=-1.000000000001 ) y(j) = 0.0
                                    ENDIF
!WKBNE 1/95
                                 ENDDO
                              CASE (5)
!
!     DO ARITHMETIC IN DOUBLE PRECISION
!
                                 DO k = 1 , nbrrow
                                    j = imatn + z(irowp+k-1)
                                    is(1) = z(j)
                                    is(2) = z(j+1)
!WKBI 1/95
                                    xdd = xd(1)
                                    xd(1) = xd(1) + zd(k)
!WKBNB 1/95 FOLLOWING CODE WILL CAUSE A TRUE ZERO WHEN SUBTRACTING SAME NO.
!  WITHOUT THIS CODE, A SYMMETRIC MATRIX MIGHT HAVE UNSYMMETRIC TERMS ON
!  THE HP AND ULTRIX (SEE DEMO D01011A, MATRIX KAA, COLUMN 84 ROW 70)
                                    IF ( zd(k)/=0.0D0 ) THEN
                                       xdd = xdd/zd(k)
                                       IF ( xdd<=-.999999999998 .AND. xdd>=-1.000000000001 ) xd(1) = 0.0D0
                                    ENDIF
!WKBNE 1/95
                                    z(j) = is(1)
                                    z(j+1) = is(2)
!
                                 ENDDO
                                 EXIT SPAG_DispatchLoop_3
                              END SELECT
                           ENDDO SPAG_DispatchLoop_3
!
!     END OF DO LOOPS
!
                        ENDDO
                        jj = z(jj+2)
                        IF ( jj<ilist ) EXIT SPAG_Loop_2_2
                     ENDDO SPAG_Loop_2_2
                  ENDIF
               ENDIF
            ENDDO
!
!     ALL COLUMNS OF XGG IN CORE ARE NOW COMPLETE - SEND THEM
!     OUT TO THE XGG DATA BLOCK VIA THE BLDPK ROUTINE.
!
            CALL close(xblock,clsrew)
         ENDIF
         CALL gopen(xgg,z(buf1),openw)
         ipvt = igpx
         DO
!
!     PREPARE TO PACK ALL COLUMNS FOR CURRENT PIVOT
!
            col1 = z(ipvt)
            coln = col1 + z(ipvt+1) - 1
            icol = z(ipvt+2)
            irow = z(ipvt+3)
            imat = z(ipvt+4)
            ncol = irow - 1
            nrow = imat - 1
            jnext = icol
            nxtcol = z(jnext)
            ii = imat
            DO col = col1 , coln
!
!     INITIATE PACKING BY CALLING BLDPK. TEST FOR NULL COL.
!
               CALL bldpk(prec,prec,xgg,0,0)
               IF ( icol/=0 ) THEN
                  IF ( col>=nxtcol ) THEN
                     jnext = jnext + 1
                     nxtcol = z(jnext)
                     IF ( jnext>ncol ) nxtcol = coln + 1
                     IF ( prec==2 ) THEN
!
!     DOUBLE PRECISION
!
                        DO k = irow , nrow
                           iq = z(k)
                           q(1) = z(ii)
                           q(2) = z(ii+1)
                           CALL zblpki
                           ii = ii + 2
                        ENDDO
                     ELSE
!
!     NON-NULL COLUMN - SEND THE TERMS OUT VIA ZBLPKI
!
!     SINGLE PRECISION
!
                        DO k = irow , nrow
                           iq = z(k)
                           q(1) = z(ii)
                           CALL zblpki
                           ii = ii + 1
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
!
!     TERMINATE COLUMN BY CALLING BLDPKN
!
               CALL bldpkn(xgg,0,mcb)
            ENDDO
!
!     LOGIC TEST TO MAKE SURE POINTERS ENDED CORRECTLY
!
            nbrwds = 5
            IF ( icol/=0 ) THEN
               nbrwds = (imat-irow)*(irow-icol)*prec
               IF ( ii-imat/=nbrwds .AND. z(ipvt+1)/=1 ) THEN
                  msg(1) = 1432
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     TEST FOR LAST PIVOT
!
            IF ( ipvt>=npvt ) THEN
!
!     CLOSE XGG
!
               CALL close(xgg,op)
!
!     TEST FOR LAST PASS
!
               IF ( last ) THEN
!
!     XGG NOW COMPLETE - WRITE ITS TRAILER.
!
                  mcb(3) = mcb(2)
                  IF ( mcb(2)/=z(npvt)+z(npvt+1)-1 ) THEN
                     msg(1) = 1490
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     CALL wrttrl(mcb)
                     RETURN
                  ENDIF
               ELSE
                  first = .FALSE.
                  openr = rd
                  openw = wrt
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               ipvt = imat + nbrwds
            ENDIF
         ENDDO
         spag_nextblock_1 = 14
      CASE (14)
         msg(1) = 1344
         spag_nextblock_1 = 19
      CASE (15)
         msg(1) = 1346
         spag_nextblock_1 = 19
      CASE (16)
         msg(1) = 1352
         spag_nextblock_1 = 19
      CASE (17)
         msg(1) = 1362
         spag_nextblock_1 = 19
      CASE (18)
         msg(1) = 1332
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 140     msg(1) = 1345
         spag_nextblock_1 = 19
      CASE (19)
         WRITE (output,99001) sfm , msg(1)
99001    FORMAT (A25,' 3102, LOGIC ERROR EMA - ',I4)
         spag_nextblock_1 = 20
      CASE (20)
         WRITE (output,99002)
99002    FORMAT (/,' *** CONTENTS OF /MA1XX/')
         WRITE (output,99007) ihq
         WRITE (output,99003)
99003    FORMAT (/,' FIRST 250 WORDS OF OPEN CORE')
         j = 250
         WRITE (output,99007) (z(i),i=1,j)
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 21
      CASE (21)
         CALL fname(msg(3),msg(1))
         WRITE (output,99004) sfm , (msg(i),i=1,4)
99004    FORMAT (A25,' 3001, ATTEMPT TO OPEN DATA SET ',2A4,', FILE (',I4,') IN SUBROUTINE EMA (',I4,                               &
                &') WHICH WAS NOT DEFINED IN FIST.')
         spag_nextblock_1 = 20
      CASE (22)
         CALL fname(msg(3),msg(1))
         WRITE (output,99005) sfm , (msg(i),i=1,4)
99005    FORMAT (A25,' 3002, EOF ENCOUNTERED WHILE READING DATA SET ',2A4,', (FILE',I5,') IN SUBROUTINE EMA (',I4,1H))
         spag_nextblock_1 = 20
      CASE (23)
         CALL fname(msg(3),msg(1))
         WRITE (output,99006) sfm , (msg(i),i=1,4)
99006    FORMAT (A25,' 3003, ATTEMPT TO READ PAST END OF LOGICAL RECORD IN',' DATA SET ',2A4,' (FILE',I5,') IN SUBROUTINE EMA (',I4,&
                &1H))
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 160     msg(3) = scrout
         msg(4) = 1005
         spag_nextblock_1 = 21
         CYCLE SPAG_DispatchLoop_1
 180     msg(3) = xemd
         msg(4) = 1014
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 200     msg(3) = xemd
         msg(4) = 1017
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 220     msg(3) = scrin
         msg(4) = 1032
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 240     msg(3) = scrin
         msg(4) = 1035
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 260     msg(3) = scrin
         msg(4) = 1036
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 280     msg(3) = scrin
         msg(4) = 1036
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 300     msg(3) = scrin
         msg(4) = 1040
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 320     msg(3) = scrin
         msg(4) = 1040
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 340     msg(3) = gpectx
         msg(4) = 1221
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 360     msg(3) = xblock
         msg(4) = 1360
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 380     msg(3) = xblock
         msg(4) = 1360
         spag_nextblock_1 = 23
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (5X,10I10)
END SUBROUTINE ema
