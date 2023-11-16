
SUBROUTINE seemat
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Axmax , Aymax , Edge(12) , Paperx , Papery , Papsiz(2) , Region(4) , Skpa(9) , Skparm(4) , X(1) , Z(4)
   INTEGER Eol , Eor , Fsize , Ix(1) , Iz , Jazz1(6) , Jazz2(2) , Jazz3(26) , Kamran , Lnct , Model , Modela , Modida(2) , Nblfm ,  &
         & Nbpc , Nbpw , Ncpw , Nlines , Nout , Pfile , Ploter , Pltbuf , Pltter , Pltype , Pp(2) , Sysbuf , Two(32)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*31 Sim
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Pp , Pfile , Fsize , Modida , Modela , Paperx , Papery
   COMMON /pltdat/ Model , Pltter , Region , Axmax , Aymax , Edge , Skpa , Pltype , Ploter
   COMMON /system/ Sysbuf , Nout , Jazz1 , Nlines , Jazz2 , Lnct , Jazz3 , Nbpc , Nbpw , Ncpw
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm , Sim
   COMMON /xxparm/ Pltbuf , Kamran , Nblfm , Skparm , Papsiz
   COMMON /zntpkx/ Z , Iz , Eol , Eor
   COMMON /zzzzzz/ X
!
! Local variable declarations
!
   INTEGER a , b , bcor , blank , c , gobac , i , i1 , i100 , i2 , ib , iblcu1 , iblcu2 , icol1 , iii , ij , ija , ijb , ijm ,      &
         & ijmax , ip , ip1 , ip2 , ipak , ipij1 , ipij2 , iro(10) , it(7) , ityp , iw , ixx , j100 , jblcu1 , jblcu2 , jj , kcor , &
         & kerror , kpp(2) , lbl(2) , lblk , lcor , lin(25) , modid(2) , nam , name(5) , nblcur , nblk , nblkfm , nblks , nblks1 ,  &
         & ncc , ncc1 , ncc5 , ncol , ncol1 , ncols , nlnxx , nrow , nrows , nrows1 , plus , seemt(2) , symbl(2) , ttl1(9) , ttl2(4)&
         & , ttl3(4) , ttl4(3) , xdddd , xdolr , xstar
   INTEGER andf , khrfn1 , korsz , orf
   REAL bllx , blly , blrx , blry , bulx , buly , burx , bury , fij , fjj , fncc , fnccx , fnccy , xxxx , yyyy
   LOGICAL nobits , plotit , prntit , sq , table
   LOGICAL tapbit
   EXTERNAL andf , orf
!
! End of declarations
!
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
   EQUIVALENCE (X(1),Ix(1)) , (iro(1),icol1) , (iro(2),iblcu1) , (iro(3),iblcu2) , (iro(4),jblcu1) , (iro(5),jblcu2) ,              &
    & (iro(6),a,ipij1) , (iro(7),b,ipij2) , (iro(8),c) , (it(1),nam) , (it(2),ncols) , (it(3),nrows) , (it(5),ityp)
   DATA name , seemt/101 , 102 , 103 , 104 , 105 , 4HSEEM , 4HAT  /
   DATA blank , ncc , xstar , xdolr , xdddd/1H  , 100 , 1H* , 1H$ , 1HD/
   DATA kpp/4HPLOT , 4H    /
   DATA plus/4H+   /
   DATA ttl1/4HSEEM , 4HAT D , 4HISPL , 4HAY O , 4HF MA , 4HTRIX , 4H DAT , 4HA BL , 4HOCK /
   DATA ttl2/4HNO.  , 4HCOLU , 4HMNS  , 4H=   /
   DATA ttl3/4HNO.  , 4H  RO , 4HWS   , 4H=   /
   DATA ttl4/4H(TRA , 4HNSPO , 4HSED)/
!
   ncc = 100
   plotit = .FALSE.
   prntit = .TRUE.
   nlnxx = Nlines
   IF ( Pp(1)==kpp(1) .AND. Pp(2)==kpp(2) ) THEN
      plotit = .TRUE.
      prntit = .FALSE.
      table = .FALSE.
      ncc = Fsize
      fncc = ncc
      nlnxx = ncc
   ENDIF
   lcor = korsz(X) - Sysbuf
!
   ncc1 = ncc/4
   ncc5 = ncc - 5
   lblk = (ncc*nlnxx-1)/32 + 3
   IF ( prntit ) GOTO 200
!
!     INITIALIZE PLOTTER
!
   modid(1) = Modida(1)
   modid(2) = Modela
   CALL fndplt(Pltter,Model,modid)
   Papsiz(1) = Paperx
   Papsiz(2) = Papery
   Kamran = 3
   nblkfm = 0
   CALL pltset
   lcor = lcor - Pltbuf
   kcor = lcor + Sysbuf + 1
   IF ( lcor<=0 ) CALL mesage(-8,sq,seemt)
   bcor = lcor - ncc1
   IF ( tapbit(Ploter) ) THEN
      IF ( iabs(Pltype)/=1 ) table = .TRUE.
      Region(3) = amin1(Axmax,Aymax)
      Region(4) = Region(3)
      Axmax = Region(3)
      Aymax = Region(4)
      CALL mapset(0,0,1.01*fncc,1.01*fncc,0,0,Axmax,Aymax,2)
      CALL map(0.005*fncc,0.005*fncc,bllx,blly)
      CALL map(1.005*fncc,0.005*fncc,blrx,blry)
      CALL map(1.005*fncc,1.005*fncc,burx,bury)
      CALL map(0.005*fncc,1.005*fncc,bulx,buly)
      GOTO 200
   ELSE
      WRITE (Nout,99001) Uwm , Ploter
99001 FORMAT (A25,' 1704, PLOT FILE -',A4,'- NOT SET UP')
      GOTO 99999
   ENDIF
 100  CALL mesage(-1,Ploter,seemt)
!
 200  DO iii = 1 , 5
!
      nam = name(iii)
      CALL rdtrl(it)
      IF ( nam<=0 ) CYCLE
      CALL gopen(nam,X(lcor+1),0)
      CALL fname(nam,lbl)
      sq = .TRUE.
      IF ( ncols/=nrows ) sq = .FALSE.
      nblks = 0
      ncol1 = 0
      ijmax = max0(ncols,nrows)
      nrows1 = nrows + 1
      IF ( .NOT.(prntit) ) THEN
         IF ( .NOT.(table) ) THEN
            Pfile = Pfile + 1
            CALL sopen(*100,Ploter,X(kcor),Pltbuf)
            CALL stplot(Pfile)
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
         Lnct = Lnct + 5
         WRITE (Nout,99002) lbl(1) , lbl(2) , ncols , nrows
99002    FORMAT (//5X,'SEEMAT PLOT FOR TRANSPOSE OF',/22X,'MATRIX DATA ','BLOCK ',2A4,11X,'PLOT FILE ','    R','     C',/10X,       &
                &'SIZE =',I6,' ROWS BY',I6,' COLUMNS')
         IF ( .NOT.(table) ) THEN
            WRITE (Nout,99003) Pfile
99003       FORMAT (1H0,62X,I5,2X,12HHEADER FRAME)
         ENDIF
      ENDIF
!
!
!     LOOP ON COLUMNS OF MATRIX
!
      ncol = 1
 250  CALL intpk(*550,nam,0,ityp,0)
!
!     IF COLUMN IS NULL, RETURN FROM INTPK IS TO STATEMENT 2100
!     ITY IS TYPE OF ELEMENT STORED IN Z, NOT USED IN THIS PROGRAM
!     BLOCK IS DUMMY ENTRY NOT USED BY INTPK
!
!     LOOP ON ROWS OF MATRIX
!
      nrow = 1
 300  IF ( Eol/=0 ) GOTO 550
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
      ELSEIF ( ncol<=jblcu1 .OR. ncol>jblcu2 .OR. Iz<=iblcu1 .OR. Iz>iblcu2 ) THEN
!
!     SEARCH ALL BLOCKS TO FIND OLD ONE IN WHICH ELEMENT LIES
!
         DO i2 = 1 , nblks
            ip = lblk*(i2-1) + 1
            ip1 = ip + 2
            iblcu1 = Ix(ip)
            iblcu2 = iblcu1 + ncc
            jblcu1 = Ix(ip+1)
            jblcu2 = jblcu1 + nlnxx
            IF ( ncol>jblcu1 .AND. ncol<=jblcu2 .AND. Iz>iblcu1 .AND. Iz<=iblcu2 ) THEN
               nblk = i2
               GOTO 350
            ENDIF
         ENDDO
         nblk = -1
      ELSE
         nblk = nblcur
      ENDIF
 350  IF ( nblk>0 ) GOTO 500
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
            Ix(i) = 0
         ENDDO
         DO ijm = 1 , ijmax
            IF ( ijm*ncc>=Iz ) THEN
               Ix(ip) = ncc*(ijm-1)
               GOTO 400
            ENDIF
         ENDDO
         kerror = 1074
         GOTO 800
      ELSE
         WRITE (Nout,99004) Swm , nblks1
99004    FORMAT (A27,' 1701, AVAILABLE CORE EXCEEDED BY',I10,' LINE IMAGE',' BLOCKS.')
         nblks = -1
         GOTO 850
      ENDIF
 400  DO ijm = 1 , ijmax
         IF ( ijm*nlnxx>=ncol ) THEN
            Ix(ip+1) = nlnxx*(ijm-1)
            GOTO 450
         ENDIF
      ENDDO
      kerror = 1079
      GOTO 800
 450  iblcu1 = Ix(ip)
      iblcu2 = iblcu1 + ncc
      jblcu1 = Ix(ip+1)
      jblcu2 = jblcu1 + nlnxx
      nblks = nblks1
      nblcur = nblks
      IF ( nblks<=0 ) GOTO 900
!
!     INSERT BIT INTO PACKED LINE IMAGE BLOCK
!
 500  a = ncc*(ncol-Ix(ip+1)-1) + (Iz-Ix(ip))
      b = (a-1)/32
      c = ip1 + b
      b = a - 32*b
      Ix(c) = orf(Ix(c),Two(b))
!
!     END OF LOOP ON ROWS
!
      nrow = nrow + 1
      IF ( nrow<=nrows1 ) GOTO 300
      kerror = 2000
      GOTO 800
 550  IF ( ncol-ncol1<nlnxx ) GOTO 650
!
!     OUTPUT GROUP OF LINE IMAGE BLOCKS
!
      ASSIGN 600 TO gobac
      GOTO 750
 600  nblks = 0
      ncol1 = ncol1 + nlnxx
!
!     END OF LOOP ON COLUMNS
!
 650  ncol = ncol + 1
      IF ( ncol<=ncols ) GOTO 250
!
!     OUTPUT RESIDUAL LINE IMAGE BLOCKS
!
      ASSIGN 700 TO gobac
      GOTO 750
 700  nblks = 0
      GOTO 900
!
!     OUTPUT GROUP OF LINE IMAGE BLOCKS
!
 750  IF ( nblks>0 ) THEN
         DO i = 1 , nblks
            ip = lblk*(i-1) + 1
            IF ( prntit ) CALL page1
            i1 = Ix(ip)
            j100 = i1 + ncc
            DO ij = 1 , 10
               iro(ij) = i1 + 10*ij
            ENDDO
            IF ( prntit ) WRITE (Nout,99005) (iro(ij),ij=1,10)
99005       FORMAT (13H0TRANSPOSE OF,9X,8HCOLUMN..,10I10)
            IF ( prntit ) WRITE (Nout,99006) lbl(1) , lbl(2)
99006       FORMAT (8H MATRIX ,2A4,7X,3HROW,4X,10(9X,1H.),/23X,3H...,4X,100(1H.)/24X,1H.)
            icol1 = Ix(ip+1)
            i100 = icol1 + nlnxx
            ip1 = ip - ncc1 + 1
            IF ( .NOT.(prntit) ) THEN
               Pfile = Pfile + 1
               CALL sopen(*100,Ploter,X(kcor),Pltbuf)
               CALL stplot(Pfile)
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
                     IF ( andf(Ix(iw),Two(ib))/=0 ) THEN
                        nobits = .FALSE.
                        fjj = float(jj)
                        CALL map(fjj,fij,xxxx,yyyy)
                        IF ( sq .AND. Ix(ip+1)+ij==Ix(ip)+jj ) THEN
                           CALL tipe(xxxx,yyyy,1,xdddd,1,0)
                        ELSEIF ( Ix(ip+1)+ij==ncols .OR. Ix(ip)+jj==nrows ) THEN
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
                     IF ( andf(Ix(iw),Two(ib))/=0 ) THEN
                        nobits = .FALSE.
                        b = (jj-1)/4 + 1
                        c = jj - 4*(b-1)
                        ixx = xstar
                        IF ( Ix(ip+1)+ij==ncols .OR. Ix(ip)+jj==nrows ) ixx = xdolr
                        IF ( sq .AND. Ix(ip+1)+ij==Ix(ip)+jj ) ixx = xdddd
                        lin(b) = khrfn1(lin(b),c,ixx,1)
                     ENDIF
                  ENDDO
                  IF ( nobits ) THEN
                     IF ( mod(ij,5)==0 ) THEN
                        icol1 = icol1 + 5
                        WRITE (Nout,99012) icol1
                     ELSE
                        WRITE (Nout,99013)
                     ENDIF
                  ELSEIF ( mod(ij,5)==0 ) THEN
                     icol1 = icol1 + 5
                     WRITE (Nout,99012) icol1 , (lin(jj),jj=1,ncc1)
                  ELSE
                     WRITE (Nout,99013) (lin(jj),jj=1,ncc1)
                  ENDIF
               ENDIF
            ENDDO
            IF ( prntit ) WRITE (Nout,99007)
99007       FORMAT (1H0,29X,100(1H.)/30X,10(9X,1H.))
            IF ( .NOT.(prntit) ) THEN
               CALL stplot(-1)
               Lnct = Lnct + 1
               IF ( Lnct>Nlines ) CALL page1
               WRITE (Nout,99008) Pfile , i100 , j100
99008          FORMAT (1H ,62X,I5,2I6)
            ENDIF
         ENDDO
      ENDIF
!
      GOTO gobac
!
 800  WRITE (Nout,99009) Swm , kerror
99009 FORMAT (A27,' 1705, LOGIC ERROR AT STATEMENT',I5,' IN SUBROUTINE SEEMAT.')
 850  WRITE (Nout,99010) Sim , lbl
99010 FORMAT (A31,' 1702, UTILITY MODULE SEEMAT WILL ABANDON ','PROCESSING DATA BLOCK ',2A4)
 900  CALL close(nam,1)
      IF ( .NOT.(prntit) ) THEN
         IF ( .NOT.(table) ) THEN
            Pfile = Pfile + 1
            CALL sopen(*100,Ploter,X(kcor),Pltbuf)
            CALL stplot(Pfile)
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
            Lnct = Lnct + 1
            WRITE (Nout,99011) Pfile
99011       FORMAT (63X,I5,2X,13HTRAILER FRAME)
         ENDIF
      ENDIF
   ENDDO
99012 FORMAT (16X,I10,4H .. ,25A4)
99013 FORMAT (28X,2H. ,25A4)
!
99999 RETURN
END SUBROUTINE seemat
