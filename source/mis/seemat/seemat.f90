!*==seemat.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE seemat
   USE c_blank
   USE c_pltdat
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_xxparm
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: a , b , bcor , c , gobac , i , i1 , i100 , i2 , ib , iblcu1 , iblcu2 , icol1 , iii , ij , ija , ijb , ijm , ijmax ,   &
            & ip , ip1 , ip2 , ipak , ipij1 , ipij2 , ityp , iw , ixx , j100 , jblcu1 , jblcu2 , jj , kcor , kerror , lblk , lcor , &
            & nam , nblcur , nblk , nblkfm , nblks , nblks1 , ncc1 , ncc5 , ncol , ncol1 , ncols , nlnxx , nrow , nrows , nrows1
   INTEGER , SAVE :: blank , ncc , plus , xdddd , xdolr , xstar
   REAL :: bllx , blly , blrx , blry , bulx , buly , burx , bury , fij , fjj , fncc , fnccx , fnccy , xxxx , yyyy
   INTEGER , DIMENSION(10) :: iro
   INTEGER , DIMENSION(7) :: it
   INTEGER , DIMENSION(1) :: ix
   INTEGER , DIMENSION(2) , SAVE :: kpp , seemt
   INTEGER , DIMENSION(2) :: lbl , modid , symbl
   INTEGER , DIMENSION(25) :: lin
   INTEGER , DIMENSION(5) , SAVE :: name
   LOGICAL :: nobits , plotit , prntit , sq , table
   INTEGER , DIMENSION(9) , SAVE :: ttl1
   INTEGER , DIMENSION(4) , SAVE :: ttl2 , ttl3
   INTEGER , DIMENSION(3) , SAVE :: ttl4
   EXTERNAL andf , close , fname , fndplt , gopen , intpk , khrfn1 , korsz , line , map , mapset , mesage , orf , page1 , pltset ,  &
          & print , rdtrl , sopen , stplot , symbol , tapbit , tipe , typint , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     SUBROUTINE SEEMAT IS THE DMAP DRIVER FOR UTILITY MODULE SEEMAT
!     WHOSE DMAP CALL FOLLOWS
!
!     SEEMAT    A,B,C,D,E//C,N,PRINT(PLOT)/V,N,PFILE/C,N,FSIZE/
!                          C,N,MODIDA/C,N,MODELA/C,N,PAPERX/C,N,PAPERY
!
!     INPUT DATA BLOCKS  - A,B,C,D,E ARE MATRICES, ANY OF WHICH MAY BE
!                          PURGED.
!
!     OUTPUT DATA BLOCKS - NONE
!
!     PARAMETERS
!       1. BCD, -PRINT- MEANS USE SYSTEM PRINTER (DEFAULT).
!               -PLOT- MEANS USE SPECIFIED PLOTTER.
!       2. INTEGER, PLOT COUNTER (INPUT + OUTPUT).
!       3. INTEGER, FRAME SIZE = NUMBER OF CHARACTERS TO BE TYPED
!                   IN AN ASSUMED SQUARE FRAME (DEFAULT=100).
!       4. BCD, MODEL ID (DEFAULT=M).
!       5. INTEGER, MODEL NUMBER (DEFAULT=1).
!       6. REAL, X DIMENSION OF PLOT FRAME (DEFAULT=0.0).
!       7. REAL, Y DIMENSION OF PLOT FRAME (DEFAULT=0.0).
!      NOTE - PARAMETERS 2-7 ARE USED ONLY IF PARAMETER 1 = -PLOT-.
!
   !>>>>EQUIVALENCE (X(1),Ix(1)) , (iro(1),icol1) , (iro(2),iblcu1) , (iro(3),iblcu2) , (iro(4),jblcu1) , (iro(5),jblcu2) ,              &
!>>>>    & (iro(6),a,ipij1) , (iro(7),b,ipij2) , (iro(8),c) , (it(1),nam) , (it(2),ncols) , (it(3),nrows) , (it(5),ityp)
   DATA name , seemt/101 , 102 , 103 , 104 , 105 , 4HSEEM , 4HAT  /
   DATA blank , ncc , xstar , xdolr , xdddd/1H  , 100 , 1H* , 1H$ , 1HD/
   DATA kpp/4HPLOT , 4H    /
   DATA plus/4H+   /
   DATA ttl1/4HSEEM , 4HAT D , 4HISPL , 4HAY O , 4HF MA , 4HTRIX , 4H DAT , 4HA BL , 4HOCK /
   DATA ttl2/4HNO.  , 4HCOLU , 4HMNS  , 4H=   /
   DATA ttl3/4HNO.  , 4H  RO , 4HWS   , 4H=   /
   DATA ttl4/4H(TRA , 4HNSPO , 4HSED)/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ncc = 100
         plotit = .FALSE.
         prntit = .TRUE.
         nlnxx = nlines
         IF ( pp(1)==kpp(1) .AND. pp(2)==kpp(2) ) THEN
            plotit = .TRUE.
            prntit = .FALSE.
            table = .FALSE.
            ncc = fsize
            fncc = ncc
            nlnxx = ncc
         ENDIF
         lcor = korsz(x) - sysbuf
!
         ncc1 = ncc/4
         ncc5 = ncc - 5
         lblk = (ncc*nlnxx-1)/32 + 3
         IF ( prntit ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     INITIALIZE PLOTTER
!
         modid(1) = modida(1)
         modid(2) = modela
         CALL fndplt(pltter,model,modid)
         papsiz(1) = paperx
         papsiz(2) = papery
         kamran = 3
         nblkfm = 0
         CALL pltset
         lcor = lcor - pltbuf
         kcor = lcor + sysbuf + 1
         IF ( lcor<=0 ) CALL mesage(-8,sq,seemt)
         bcor = lcor - ncc1
         IF ( tapbit(ploter) ) THEN
            IF ( iabs(pltype)/=1 ) table = .TRUE.
            region(3) = amin1(axmax,aymax)
            region(4) = region(3)
            axmax = region(3)
            aymax = region(4)
            CALL mapset(0,0,1.01*fncc,1.01*fncc,0,0,axmax,aymax,2)
            CALL map(0.005*fncc,0.005*fncc,bllx,blly)
            CALL map(1.005*fncc,0.005*fncc,blrx,blry)
            CALL map(1.005*fncc,1.005*fncc,burx,bury)
            CALL map(0.005*fncc,1.005*fncc,bulx,buly)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            WRITE (nout,99001) uwm , ploter
99001       FORMAT (A25,' 1704, PLOT FILE -',A4,'- NOT SET UP')
            RETURN
         ENDIF
 20      CALL mesage(-1,ploter,seemt)
         spag_nextblock_1 = 2
      CASE (2)
!
         DO iii = 1 , 5
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
                  nam = name(iii)
                  CALL rdtrl(it)
                  IF ( nam<=0 ) CYCLE
                  CALL gopen(nam,x(lcor+1),0)
                  CALL fname(nam,lbl)
                  sq = .TRUE.
                  IF ( ncols/=nrows ) sq = .FALSE.
                  nblks = 0
                  ncol1 = 0
                  ijmax = max0(ncols,nrows)
                  nrows1 = nrows + 1
                  IF ( .NOT.(prntit) ) THEN
                     IF ( .NOT.(table) ) THEN
                        pfile = pfile + 1
                        CALL sopen(*20,ploter,x(kcor),pltbuf)
                        CALL stplot(pfile)
                        CALL map(0.23*fncc,0.50*fncc,xxxx,yyyy)
                        CALL print(xxxx,yyyy,1,ttl1,9,-1)
                        CALL print(xxxx,yyyy,1,ttl1,9,0)
                        CALL map(0.60*fncc,0.50*fncc,xxxx,yyyy)
                        CALL print(xxxx,yyyy,1,lbl,2,0)
                        CALL map(0.75*fncc,0.50*fncc,xxxx,yyyy)
                        CALL print(xxxx,yyyy,1,ttl4,3,0)
                        CALL map(0.40*fncc,0.40*fncc,xxxx,yyyy)
                        CALL print(xxxx,yyyy,1,ttl3,4,0)
                        CALL map(0.40*fncc,0.30*fncc,xxxx,yyyy)
                        CALL print(xxxx,yyyy,1,ttl2,4,0)
                        CALL map(0.55*fncc,0.40*fncc,xxxx,yyyy)
                        CALL typint(xxxx,yyyy,1,nrows,0,0)
                        CALL map(0.55*fncc,0.30*fncc,xxxx,yyyy)
                        CALL typint(xxxx,yyyy,1,ncols,0,0)
                        CALL line(bllx,blly,bulx,buly,1,-1)
                        CALL line(bllx,blly,bulx,buly,1,0)
                        CALL line(bulx,buly,burx,bury,1,0)
                        CALL line(burx,bury,blrx,blry,1,0)
                        CALL line(blrx,blry,bllx,blly,1,0)
                        CALL stplot(-1)
                     ENDIF
                     CALL page1
                     lnct = lnct + 5
                     WRITE (nout,99002) lbl(1) , lbl(2) , ncols , nrows
99002                FORMAT (//5X,'SEEMAT PLOT FOR TRANSPOSE OF',/22X,'MATRIX DATA ','BLOCK ',2A4,11X,'PLOT FILE ','    R','     C',&
                           & /10X,'SIZE =',I6,' ROWS BY',I6,' COLUMNS')
                     IF ( .NOT.(table) ) THEN
                        WRITE (nout,99003) pfile
99003                   FORMAT (1H0,62X,I5,2X,12HHEADER FRAME)
                     ENDIF
                  ENDIF
!
!
!     LOOP ON COLUMNS OF MATRIX
!
                  ncol = 1
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL intpk(*22,nam,0,ityp,0)
!
!     IF COLUMN IS NULL, RETURN FROM INTPK IS TO STATEMENT 2100
!     ITY IS TYPE OF ELEMENT STORED IN Z, NOT USED IN THIS PROGRAM
!     BLOCK IS DUMMY ENTRY NOT USED BY INTPK
!
!     LOOP ON ROWS OF MATRIX
!
                  nrow = 1
                  spag_nextblock_2 = 3
               CASE (3)
                  IF ( eol/=0 ) GOTO 22
!
!     READ ELEMENT OF MATRIX INTO /ZNTPKX/
!
                  CALL zntpki
!
!     COMPUTE BLOCK ID IN WHICH ELEMENT BELONGS
!
!     LOOK AT CURRENT BLOCK FIRST
!
                  IF ( nblks<=0 ) THEN
                     nblk = -1
                  ELSEIF ( ncol<=jblcu1 .OR. ncol>jblcu2 .OR. iz<=iblcu1 .OR. iz>iblcu2 ) THEN
!
!     SEARCH ALL BLOCKS TO FIND OLD ONE IN WHICH ELEMENT LIES
!
                     DO i2 = 1 , nblks
                        ip = lblk*(i2-1) + 1
                        ip1 = ip + 2
                        iblcu1 = ix(ip)
                        iblcu2 = iblcu1 + ncc
                        jblcu1 = ix(ip+1)
                        jblcu2 = jblcu1 + nlnxx
                        IF ( ncol>jblcu1 .AND. ncol<=jblcu2 .AND. iz>iblcu1 .AND. iz<=iblcu2 ) THEN
                           nblk = i2
                           spag_nextblock_2 = 4
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     nblk = -1
                  ELSE
                     nblk = nblcur
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (4)
                  IF ( nblk>0 ) THEN
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     SET UP NEW BLOCK IF THERE IS ROOM FOR IT IN CORE
!
                  nblks1 = nblks + 1
                  IF ( lblk*nblks1<=lcor ) THEN
!
!     SET BLOCK POINTERS AND BLANK OUT LINE IMAGE
!
                     ip = lblk*nblks + 1
                     ip1 = ip + 2
                     ip2 = ip + lblk - 1
                     DO i = ip1 , ip2
                        ix(i) = 0
                     ENDDO
                     DO ijm = 1 , ijmax
                        IF ( ijm*ncc>=iz ) THEN
                           ix(ip) = ncc*(ijm-1)
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     kerror = 1074
                     spag_nextblock_2 = 10
                  ELSE
                     WRITE (nout,99004) swm , nblks1
99004                FORMAT (A27,' 1701, AVAILABLE CORE EXCEEDED BY',I10,' LINE IMAGE',' BLOCKS.')
                     nblks = -1
                     spag_nextblock_2 = 11
                  ENDIF
               CASE (5)
                  DO ijm = 1 , ijmax
                     IF ( ijm*nlnxx>=ncol ) THEN
                        ix(ip+1) = nlnxx*(ijm-1)
                        spag_nextblock_2 = 6
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  kerror = 1079
                  spag_nextblock_2 = 10
               CASE (6)
                  iblcu1 = ix(ip)
                  iblcu2 = iblcu1 + ncc
                  jblcu1 = ix(ip+1)
                  jblcu2 = jblcu1 + nlnxx
                  nblks = nblks1
                  nblcur = nblks
                  IF ( nblks<=0 ) THEN
                     spag_nextblock_2 = 12
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 7
               CASE (7)
!
!     INSERT BIT INTO PACKED LINE IMAGE BLOCK
!
                  a = ncc*(ncol-ix(ip+1)-1) + (iz-ix(ip))
                  b = (a-1)/32
                  c = ip1 + b
                  b = a - 32*b
                  ix(c) = orf(ix(c),two(b))
!
!     END OF LOOP ON ROWS
!
                  nrow = nrow + 1
                  IF ( nrow<=nrows1 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  kerror = 2000
                  spag_nextblock_2 = 10
                  CYCLE SPAG_DispatchLoop_2
 22               IF ( ncol-ncol1<nlnxx ) THEN
                     spag_nextblock_2 = 8
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     OUTPUT GROUP OF LINE IMAGE BLOCKS
!
                  ASSIGN 24 TO gobac
                  spag_nextblock_2 = 9
                  CYCLE SPAG_DispatchLoop_2
 24               nblks = 0
                  ncol1 = ncol1 + nlnxx
                  spag_nextblock_2 = 8
               CASE (8)
!
!     END OF LOOP ON COLUMNS
!
                  ncol = ncol + 1
                  IF ( ncol<=ncols ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     OUTPUT RESIDUAL LINE IMAGE BLOCKS
!
                  ASSIGN 26 TO gobac
                  spag_nextblock_2 = 9
                  CYCLE SPAG_DispatchLoop_2
 26               nblks = 0
                  spag_nextblock_2 = 12
               CASE (9)
!
!     OUTPUT GROUP OF LINE IMAGE BLOCKS
!
                  IF ( nblks>0 ) THEN
                     DO i = 1 , nblks
                        ip = lblk*(i-1) + 1
                        IF ( prntit ) CALL page1
                        i1 = ix(ip)
                        j100 = i1 + ncc
                        DO ij = 1 , 10
                           iro(ij) = i1 + 10*ij
                        ENDDO
                        IF ( prntit ) WRITE (nout,99005) (iro(ij),ij=1,10)
99005                   FORMAT (13H0TRANSPOSE OF,9X,8HCOLUMN..,10I10)
                        IF ( prntit ) WRITE (nout,99006) lbl(1) , lbl(2)
99006                   FORMAT (8H MATRIX ,2A4,7X,3HROW,4X,10(9X,1H.),/23X,3H...,4X,100(1H.)/24X,1H.)
                        icol1 = ix(ip+1)
                        i100 = icol1 + nlnxx
                        ip1 = ip - ncc1 + 1
                        IF ( .NOT.(prntit) ) THEN
                           pfile = pfile + 1
                           CALL sopen(*20,ploter,x(kcor),pltbuf)
                           CALL stplot(pfile)
                           CALL tipe(xxxx,yyyy,1,plus,1,-1)
                           ipak = (ncc+99)/100
                           ija = 5*ipak
                           ijb = ncc - ija
                           fnccy = 1.005*fncc
                           DO ij = ija , ijb , ija
                              fij = float(ij)
                              CALL map(fij,fnccy,xxxx,yyyy)
                              CALL tipe(xxxx,yyyy,1,plus,1,0)
                           ENDDO
                           fnccx = 1.005*fncc
                           DO ij = ija , ijb , ija
                              fij = fncc - float(ij)
                              CALL map(fnccx,fij,xxxx,yyyy)
                              CALL tipe(xxxx,yyyy,1,plus,1,0)
                           ENDDO
                           fnccy = 0.005*fncc
                           DO ij = ija , ijb , ija
                              fij = fncc - float(ij)
                              CALL map(fij,fnccy,xxxx,yyyy)
                              CALL tipe(xxxx,yyyy,1,plus,1,0)
                           ENDDO
                           fnccx = 0.005*fncc
                           DO ij = ija , ijb , ija
                              fij = float(ij)
                              CALL map(fnccx,fij,xxxx,yyyy)
                              CALL tipe(xxxx,yyyy,1,plus,1,0)
                           ENDDO
                        ENDIF
                        DO ij = 1 , nlnxx
                           ip1 = ip1 + ncc1
                           ipij1 = ip1 + 1
                           ipij2 = ip1 + ncc1
                           ib = ncc*(ij-1)
                           iw = ib/32
                           ib = ib - 32*iw
                           iw = iw + ip + 2
                           nobits = .TRUE.
                           IF ( plotit ) THEN
                              fij = 101.0 - float(ij)
                              DO jj = 1 , ncc
                                 ib = ib + 1
                                 IF ( ib>32 ) THEN
                                    ib = 1
                                    iw = iw + 1
                                 ENDIF
                                 IF ( andf(ix(iw),two(ib))/=0 ) THEN
                                    nobits = .FALSE.
                                    fjj = float(jj)
                                    CALL map(fjj,fij,xxxx,yyyy)
                                    IF ( sq .AND. ix(ip+1)+ij==ix(ip)+jj ) THEN
                                       CALL tipe(xxxx,yyyy,1,xdddd,1,0)
                                    ELSEIF ( ix(ip+1)+ij==ncols .OR. ix(ip)+jj==nrows ) THEN
                                       CALL tipe(xxxx,yyyy,1,xdolr,1,0)
                                    ELSE
                                       CALL tipe(xxxx,yyyy,1,xstar,1,0)
                                    ENDIF
                                 ENDIF
                              ENDDO
                           ELSE
                              DO jj = 1 , ncc1
                                 lin(jj) = blank
                              ENDDO
                              DO jj = 1 , ncc
                                 ib = ib + 1
                                 IF ( ib>32 ) THEN
                                    ib = 1
                                    iw = iw + 1
                                 ENDIF
                                 IF ( andf(ix(iw),two(ib))/=0 ) THEN
                                    nobits = .FALSE.
                                    b = (jj-1)/4 + 1
                                    c = jj - 4*(b-1)
                                    ixx = xstar
                                    IF ( ix(ip+1)+ij==ncols .OR. ix(ip)+jj==nrows ) ixx = xdolr
                                    IF ( sq .AND. ix(ip+1)+ij==ix(ip)+jj ) ixx = xdddd
                                    lin(b) = khrfn1(lin(b),c,ixx,1)
                                 ENDIF
                              ENDDO
                              IF ( nobits ) THEN
                                 IF ( mod(ij,5)==0 ) THEN
                                    icol1 = icol1 + 5
                                    WRITE (nout,99012) icol1
                                 ELSE
                                    WRITE (nout,99013)
                                 ENDIF
                              ELSEIF ( mod(ij,5)==0 ) THEN
                                 icol1 = icol1 + 5
                                 WRITE (nout,99012) icol1 , (lin(jj),jj=1,ncc1)
                              ELSE
                                 WRITE (nout,99013) (lin(jj),jj=1,ncc1)
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( prntit ) WRITE (nout,99007)
99007                   FORMAT (1H0,29X,100(1H.)/30X,10(9X,1H.))
                        IF ( .NOT.(prntit) ) THEN
                           CALL stplot(-1)
                           lnct = lnct + 1
                           IF ( lnct>nlines ) CALL page1
                           WRITE (nout,99008) pfile , i100 , j100
99008                      FORMAT (1H ,62X,I5,2I6)
                        ENDIF
                     ENDDO
                  ENDIF
!
                  GOTO gobac
               CASE (10)
!
                  WRITE (nout,99009) swm , kerror
99009             FORMAT (A27,' 1705, LOGIC ERROR AT STATEMENT',I5,' IN SUBROUTINE SEEMAT.')
                  spag_nextblock_2 = 11
               CASE (11)
                  WRITE (nout,99010) sim , lbl
99010             FORMAT (A31,' 1702, UTILITY MODULE SEEMAT WILL ABANDON ','PROCESSING DATA BLOCK ',2A4)
                  spag_nextblock_2 = 12
               CASE (12)
                  CALL close(nam,1)
                  IF ( .NOT.(prntit) ) THEN
                     IF ( .NOT.(table) ) THEN
                        pfile = pfile + 1
                        CALL sopen(*20,ploter,x(kcor),pltbuf)
                        CALL stplot(pfile)
                        CALL line(bllx,blly,burx,bury,1,-1)
                        CALL line(bllx,blly,burx,bury,1,0)
                        CALL line(bulx,buly,burx,bury,1,0)
                        CALL line(bulx,buly,blrx,blry,1,0)
                        CALL line(blrx,blry,bllx,blly,1,0)
                        CALL line(bllx,blly,bulx,buly,1,0)
                        CALL line(burx,bury,blrx,blry,1,0)
                        symbl(1) = 3
                        symbl(2) = 6
                        CALL map(0.505*fncc,0.505*fncc,xxxx,yyyy)
                        CALL symbol(xxxx,yyyy,symbl,-1)
                        CALL symbol(xxxx,yyyy,symbl,0)
                        CALL stplot(-1)
                        lnct = lnct + 1
                        WRITE (nout,99011) pfile
99011                   FORMAT (63X,I5,2X,13HTRAILER FRAME)
                     ENDIF
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99012 FORMAT (16X,I10,4H .. ,25A4)
99013 FORMAT (28X,2H. ,25A4)
!
END SUBROUTINE seemat
