
SUBROUTINE mtrxin
!
!     TWO CAPABILITIES -
!
!     (1) TO PROVIDE DIRECT INPUT MATRICES CAPABILITY, IN DYNAMIC RIGID
!         FORMATS, AND
!     (2) TO CONVERT DMIG TYPE MATRICES TO NASTRAN MATRIX FORMAT.
!
!     REVISED  1/90 BY G.CHAN/UNISYS
!     NO INTEGER ROW AND COLUMN PACKING FOR 32-BIT WORD MACHINE, AND
!     REAL AND COMPLEX DMIG MATRIX GENERATION FROM DMIG INPUT CARDS
!     WITH DOUBLE PRECISION DATA
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alpha(4) , Beta(4) , X(4)
   INTEGER Clsrew , Filea(7) , Fileb(7) , Filec(7) , Ifile(6) , Ihalf , Jhalf , Loadnn , Luset , Mach , Mcbs(67) , Nomat(3) ,       &
         & Nomat1 , Nomat2 , Nomat3 , Nomats , Nout , Nwds(4) , Nz , Prc(2) , Rd , Rdrew , Row , Sysbuf , Typalp , Typbet , Wrt ,   &
         & Wrtrew , Xx(5) , Z(1)
   CHARACTER*23 Ufm
   DOUBLE PRECISION Xd(2)
   COMMON /blank / Luset , Nomat
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /saddx / Nomats , Nz , Mcbs
   COMMON /setup / Ifile
   COMMON /system/ Sysbuf , Nout , Xx , Loadnn
   COMMON /type  / Prc , Nwds
   COMMON /xmssg / Ufm
   COMMON /zblpkx/ X , Row
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER andf , korsz , lshift , orf , rshift
   INTEGER block(81) , buf(20) , buf1 , buf2 , buf3 , buf4 , buf5 , bufi(3) , bufx , casecc , col , db(13) , dmig(2) , eqex , file ,&
         & file1 , file2 , file3 , filei(3) , flag , i , i10 , i11 , i12 , i45 , i7 , icol , imat1 , imat2 , imat3 , imtrx , iprc , &
         & iptr , iqq , isw , itf , j , j1 , jn , jsw , k , khi , klo , kn , mask16 , mcb(50) , mpool , n , nam(2) , ncol , neqex , &
         & nfiles , nmtrx , nn , nodmig , nogo , nompoo , nwd , nwd1 , prec , ret , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 ,&
         & tfpool , tfset
   DOUBLE PRECISION bufd(2)
   REAL bufr(13)
   LOGICAL pack
   EXTERNAL andf , lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (buf(1),bufr(1)) , (X(1),Xd(1)) , (filei(1),file1) , (filei(2),file2) , (bufi(1),buf3) , (bufi(2),buf4) ,            &
    & (Nomat(1),Nomat1) , (Nomat(2),Nomat2) , (filei(3),file3) , (bufi(3),buf5) , (Nomat(3),Nomat3) , (bufd(1),db(13)) ,            &
    & (buf(1),db(2))
   EQUIVALENCE (Mcbs(1),Filea(1)) , (Mcbs(8),Typalp) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Fileb(1)) , (Mcbs(20),Typbet) ,               &
    & (Mcbs(21),Beta(1)) , (Mcbs(61),Filec(1))
   DATA mcb/201 , 9*0 , 202 , 9*0 , 203 , 29*0/ , casecc , mpool , eqex , tfpool/101 , 102 , 103 , 105/ , scr1 , scr2 , scr3 ,      &
      & scr4 , scr5 , scr6 , scr7/301 , 302 , 303 , 304 , 305 , 306 , 307/ , block/81*0/ , nam/4HMTRX , 4HIN  / , dmig/114 , 1/ ,   &
      & nfiles/21/ , imat1 , imat2 , imat3 , itf/139 , 141 , 143 , 15/
!
!     PERFORM GENERAL INITIALIZATION
!
   buf1 = korsz(Z) - Sysbuf - 2
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   Nomat1 = -1
   Nomat2 = -1
   Nomat3 = -1
   mask16 = Jhalf
!
!     IF MACHINE IS MORE THEN 32 BITS PER WORD, WE USE PACKING LOGIC
!     OTHERWISE, WE DO NOT PACK ROW AND COLUMN INDICES INTO ONE WORD
!
   pack = .FALSE.
   IF ( Ihalf>16 ) pack = .TRUE.
   DO i = 1 , nfiles , 10
      j1 = i + 1
      jn = i + 9
      DO j = j1 , jn
         mcb(j) = 0
      ENDDO
   ENDDO
   tfset = 0
   Typalp = 1
   Alpha(1) = 1.0
   Typbet = 1
   Beta(1) = 1.0
   nogo = 0
!
!     OPEN MPOOL. IF PURGED, SET FLAG.
!
   file = mpool
   nompoo = 0
   nodmig = 0
   CALL preloc(*100,Z(buf1),mpool)
   nompoo = 1
   CALL locate(*100,Z(buf1),dmig,flag)
   nodmig = 1
!
!     READ CASE CONTROL RECORD.
!     SET NAMES OF REQUESTED MATRICES.
!     IF CASE CONTROL IS PURGED, SET NAMES OF REQUESTED MATRICES EQUAL
!     NAMES OF OUTPUT DATA BLOCKS.
!
 100  file = casecc
   CALL open(*300,casecc,Z(buf2),Rdrew)
   CALL fwdrec(*2800,casecc)
   CALL read(*2600,*200,casecc,Z,buf2,1,flag)
   CALL mesage(-8,0,nam)
 200  CALL close(casecc,Clsrew)
   tfset = Z(itf)
   IF ( Z(imat1)/=0 ) THEN
      Nomat1 = 1
      mcb(8) = Z(imat1)
      mcb(9) = Z(imat1+1)
   ENDIF
   IF ( Z(imat2)/=0 ) THEN
      Nomat2 = 1
      mcb(18) = Z(imat2)
      mcb(19) = Z(imat2+1)
   ENDIF
   IF ( Z(imat3)/=0 ) THEN
      Nomat3 = 1
      mcb(28) = Z(imat3)
      mcb(29) = Z(imat3+1)
   ENDIF
!
!     IF TRANSFER FUNCTION MATRICES EXIST, BUILD THEM IN MATRIX FORMAT.
!     WRITE THEM ON 201,202,203 IF NO DMIG MATRICES TO ADD, OTHERWISE,
!     WRITE THEM ON SCR5,SCR6,SCR7.
!     IF NO TRANSFER FUNCTION MATRICES AND NO DMIG MATRICES, EXIT.
!
   IF ( Nomat1+Nomat2+Nomat3==-3 ) THEN
      nodmig = 0
      GOTO 400
   ELSE
      IF ( nodmig==0 ) GOTO 2500
      GOTO 400
   ENDIF
 300  DO i = 1 , 21 , 10
      mcb(31) = mcb(i)
      CALL rdtrl(mcb(31))
      IF ( mcb(31)==mcb(i) ) CALL fname(mcb(31),mcb(i+7))
   ENDDO
   GOTO 1200
 400  IF ( nodmig==0 .AND. tfset==0 ) GOTO 2600
   IF ( tfset==0 ) GOTO 1200
   file1 = scr5
!
!     TEST FOR PURGED OUTPUT DATA BLOCKS.
!
   DO i = 1 , 21 , 10
      CALL rdtrl(mcb(i))
   ENDDO
   IF ( Nomat1==-1 ) file1 = mcb(1)
   file2 = scr7
   IF ( Nomat3==-1 ) file2 = mcb(21)
   file3 = scr6
   IF ( Nomat2==-1 ) file3 = mcb(11)
   Nomat1 = 1
   Nomat2 = 1
   Nomat3 = 1
!
!     OPEN TFPOOL AND POSITION TO REQUESTED SET.
!     IF SET NOT IN TFPOOL, QUEUE MESSAGE AND TURN ON NOGO FLAG.
!
   file = tfpool
   CALL open(*500,tfpool,Z(buf2),Rdrew)
   DO
      CALL fwdrec(*500,tfpool)
      CALL read(*500,*500,tfpool,buf,1,0,flag)
      IF ( buf(1)==tfset ) GOTO 600
   ENDDO
 500  nogo = 1
   buf(1) = tfset
   buf(2) = 0
   CALL mesage(30,74,buf)
   IF ( dmig(1)/=tfset ) THEN
      CALL close(tfpool,Clsrew)
      GOTO 1200
   ENDIF
!
!     OPEN OUTPUT FILES. WRITE HEADER RECORDS.
!
 600  DO i = 1 , 3
!
!     CHECK FOR PURGED OUTPUT DATA BLOCKS.
!
      IF ( filei(i)>0 ) THEN
         file = filei(i)
         bufx = bufi(i)
         CALL gopen(file,Z(bufx),Wrtrew)
      ELSE
         Nomat(i) = -1
      ENDIF
   ENDDO
!
!     PACK MATRICES ONTO OUTPUT FILES.
!
   ncol = Luset
   icol = 1
   jsw = 0
   isw = 0
   i45 = 5
   IF ( pack ) i45 = 4
   i12 = i45 - 3
 700  DO i = 1 , 3
      IF ( filei(i)>0 ) CALL bldpk(1,1,filei(i),block(20*i-19),1)
   ENDDO
   IF ( isw/=0 ) GOTO 900
 800  IF ( jsw/=0 ) GOTO 1000
   CALL read(*2800,*1100,tfpool,buf,i45,0,flag)
   isw = 1
   col = buf(1)
   Row = buf(2)
   IF ( pack ) THEN
      col = rshift(buf(1),Ihalf)
      Row = andf(buf(1),mask16)
   ENDIF
 900  IF ( col<=icol ) THEN
      DO i = 1 , 3
         IF ( filei(i)>0 ) CALL bldpki(buf(i+i12),Row,filei(i),block(20*i-19))
      ENDDO
      isw = 0
      GOTO 800
   ENDIF
 1000 DO i = 1 , 3
      IF ( filei(i)>0 ) CALL bldpkn(filei(i),block(20*i-19),block(7*i+54))
   ENDDO
   icol = icol + 1
   IF ( icol<=ncol ) GOTO 700
!
!     CLOSE FILES AND WRITE TRAILERS. IF NO DMIG MATRICES, RETURN
!
   CALL close(tfpool,Clsrew)
   DO i = 1 , 3
      IF ( filei(i)>0 ) THEN
         i7 = 7*i
         block(i7+54) = filei(i)
         block(i7+56) = ncol
         block(i7+57) = 1
         block(i7+58) = 1
         CALL close(filei(i),1)
         CALL wrttrl(block(i7+54))
      ENDIF
   ENDDO
   IF ( nodmig/=0 ) GOTO 1200
   GOTO 2600
 1100 jsw = 1
   GOTO 1000
!
!     READ EQUIVALENCE TABLE INTO CORE
!
 1200 file = eqex
   CALL gopen(eqex,Z(buf2),0)
   CALL skprec(eqex,1)
   CALL read(*2800,*1300,eqex,Z,buf2,1,neqex)
   CALL mesage(-8,0,nam)
 1300 CALL close(eqex,Clsrew)
   kn = neqex/2
   nn = neqex - 1
   DO i = 1 , nn , 2
      Z(i+1) = Z(i+1)/10
   ENDDO
!
!     READ MATRIX HEADER INFORMATION.
!     LOOK UP MATRIX NAME IN NAME LIST. IF ABSENT, SKIP MATRIX.
!
 1400 CALL read(*2800,*2500,mpool,buf,9,0,flag)
!
!     BUF(5)= INPUT MATRIX TYPE, BUF(6)= OUTOUT MATRIX TYPE
!     BUF(1) AND BUF(2) ARE MATRIX NAME FROM DMIG CARDS.
!
   k = buf(6)
   prec = Prc(k)
   k = buf(5)
   iprc = mod(k,2)
   nwd = Nwds(k)
   nwd1 = nwd + 1
   i11 = 11
   IF ( .NOT.(pack) ) THEN
      i11 = 10
      nwd1 = nwd + 2
   ENDIF
   i10 = i11 - 1
   DO i = 1 , nfiles , 10
      IF ( mcb(i+7)==buf(1) .AND. mcb(i+8)==buf(2) ) GOTO 1500
   ENDDO
   DO
      CALL fread(mpool,buf,2,0)
      IF ( buf(1)==-1 ) GOTO 1400
      DO
         CALL fread(mpool,buf,2,0)
         IF ( buf(1)==-1 ) EXIT
         CALL fread(mpool,buf,-nwd,0)
      ENDDO
   ENDDO
!
!     OPEN SCRATCH FILE. SET POINTERS.
!
 1500 iptr = i
   file = mcb(iptr)
   mcb(iptr+2) = Luset
   mcb(iptr+3) = buf(4)
   mcb(iptr+4) = buf(6)
   mcb(iptr+9) = 1
   iqq = (iptr-1)/10
   Nomat(iqq+1) = +1
   isw = 0
   imtrx = neqex + 1
   i = imtrx
!
!
   CALL open(*2700,scr1,Z(buf2),Wrtrew)
!
!     READ COLUMN GRID AND COMPONENT, AND CHECK DUPLICATE.
!     CONVERT GRID AND COMPONENT TO SIL NO.
!
!     REMOVE DUPLICATE CHECK ADDED HERE IN 91 VERSION. CHECKING SHOULD
!     BE DONE EARLY IN IFP MODULE, AND NOT HERE. REMOVED ALSO NOGOX AND
!     ITS ASSOCIATED LINES.   3/93
!
 1600 CALL fread(mpool,buf(10),2,0)
   IF ( buf(10)==-1 ) THEN
!
!     SORT MATRIX.
!
      IF ( isw==0 ) THEN
         n = i - imtrx
         nmtrx = i - nwd1
         CALL close(scr1,Clsrew)
         IF ( pack ) CALL sorti(0,0,nwd1,1,Z(imtrx),n)
         IF ( .NOT.pack ) CALL sorti2(0,0,nwd1,1,Z(imtrx),n)
      ELSE
         CALL write(scr1,0,0,1)
         CALL close(scr1,Clsrew)
         CALL open(*2700,scr1,Z(buf2),Rdrew)
         Ifile(1) = scr2
         Ifile(2) = scr3
         Ifile(3) = scr4
         IF ( pack ) CALL sorti(scr1,0,nwd1,1,Z(imtrx),buf2-imtrx)
         IF ( .NOT.pack ) CALL sorti2(scr1,0,nwd1,1,Z(imtrx),buf2-imtrx)
         CALL close(scr1,Clsrew)
      ENDIF
!
!     OPEN OUTPUT FILE. WRITE HEADER RECORD
!     IF SORTED MATRIX NOT IN CORE, OPEN FILE WITH MATRIX.
!
      IF ( tfset/=0 ) file = scr1
      CALL open(*2700,file,Z(buf2),Wrtrew)
      CALL fname(file,buf(19))
      CALL write(file,buf(19),2,1)
      IF ( isw/=0 ) CALL open(*2700,Ifile(6),Z(buf3),Rdrew)
!
!     PACK MATRIX ONTO OUTPUT FILE.
!
      ncol = Luset
      j = imtrx
      icol = 1
      jsw = 0
      GOTO 2000
   ELSE
!
!
      ASSIGN 1700 TO ret
      GOTO 3000
   ENDIF
 1700 col = Z(2*k)
   IF ( buf(11)/=0 ) col = col + buf(11) - 1
   IF ( pack ) col = lshift(col,Ihalf)
!
!     READ A COLUMN OF THE MATRIX.
!     STORE IN CORE OR ON SCRATCH FILE IF TOO BIG FOR CORE.
!
 1800 CALL fread(mpool,buf(10),2,0)
   IF ( buf(10)==-1 ) GOTO 1600
   ASSIGN 1900 TO ret
   GOTO 3000
 1900 Row = Z(2*k)
   IF ( buf(11)/=0 ) Row = Row + buf(11) - 1
   buf(11) = Row
   buf(10) = col
   IF ( pack ) buf(11) = Row + col
   CALL fread(mpool,buf(12),nwd,0)
   IF ( isw/=0 ) THEN
      CALL write(scr1,buf(i11),nwd1,0)
   ELSEIF ( i+nwd1<buf2 ) THEN
      DO j = 1 , nwd1
         Z(i) = buf(j+i10)
         i = i + 1
      ENDDO
   ELSE
      isw = 1
      CALL write(scr1,Z(imtrx),i-imtrx,0)
      CALL write(scr1,buf(i11),nwd1,0)
   ENDIF
   GOTO 1800
 2000 CALL bldpk(buf(6),buf(6),file,0,0)
   IF ( jsw/=0 ) GOTO 2200
 2100 IF ( j>nmtrx ) GOTO 2300
   IF ( isw==0 ) THEN
      DO k = 1 , nwd1
         buf(k+i10) = Z(j)
         j = j + 1
      ENDDO
   ELSE
      CALL read(*2800,*2400,Ifile(6),buf(i11),nwd1,0,flag)
   ENDIF
   col = buf(10)
   Row = buf(11)
   IF ( pack ) THEN
      col = rshift(buf(11),Ihalf)
      Row = andf(buf(11),mask16)
   ENDIF
 2200 IF ( col>icol ) THEN
      jsw = 1
   ELSE
      jsw = 0
      IF ( prec==2 ) THEN
         IF ( iprc==0 ) THEN
            Xd(1) = bufd(1)
            Xd(2) = bufd(2)
         ELSE
            Xd(1) = bufr(12)
            Xd(2) = bufr(13)
         ENDIF
      ELSEIF ( iprc==0 ) THEN
         X(1) = bufd(1)
         X(2) = bufd(2)
      ELSE
         X(1) = bufr(12)
         X(2) = bufr(13)
      ENDIF
      CALL zblpki
      GOTO 2100
   ENDIF
 2300 CALL bldpkn(file,0,mcb(iptr))
   icol = icol + 1
   IF ( icol<=ncol ) GOTO 2000
   CALL close(file,Clsrew)
   IF ( isw/=0 ) CALL close(Ifile(6),Clsrew)
   CALL wrttrl(mcb(iptr))
!
!     IF TRANSFER FUNCTION MATRICES ARE TO BE ADDED, CALL MATRIX ADD
!     ROUTINE THEN RETURN TO READ NEXT MATRIX IN THE MATRIX POOL.
!
   IF ( tfset/=0 ) THEN
      j = 2
      IF ( iptr==1 ) j = 1
      IF ( iptr==11 ) j = 3
      DO i = 1 , 7
         k = iptr + i - 1
         Filea(i) = mcb(k)
         k = 7*j + i
         Fileb(i) = block(k+53)
         Filec(i) = 0
      ENDDO
      Filea(1) = scr1
      Filec(1) = mcb(iptr)
      Filec(2) = ncol
      Filec(3) = ncol
      Filec(4) = Filea(4)
      Filec(5) = Filea(5)
      Nz = buf1 - imtrx
      Nomats = 2
      k = orf(imtrx,1)
      CALL sadd(Z(k),Z(k))
      CALL wrttrl(Filec)
   ENDIF
   GOTO 1400
 2400 j = nmtrx + 1
   GOTO 2300
!
!     TEST FOR ALL REQUESTED MATRICES FOUND.
!
 2500 DO i = 1 , nfiles , 10
      IF ( mcb(i+7)/=0 .AND. mcb(i+9)==0 ) THEN
         CALL mesage(30,70,mcb(i+7))
         nogo = 1
      ENDIF
   ENDDO
 2600 IF ( nompoo/=0 ) CALL close(mpool,Clsrew)
   IF ( nogo/=0 ) CALL mesage(-61,0,nam)
   RETURN
!
!     FATAL ERRORS
!
 2700 n = -1
   GOTO 2900
 2800 n = -2
 2900 CALL mesage(n,file,nam)
!
!     BINARY SEARCH ROUTINE
!
 3000 klo = 1
   khi = kn
 3100 k = (klo+khi+1)/2
   DO
      IF ( buf(10)<Z(2*k-1) ) THEN
         khi = k
      ELSEIF ( buf(10)==Z(2*k-1) ) THEN
         EXIT
      ELSE
         klo = k
      ENDIF
      IF ( khi-klo<1 ) THEN
         nogo = 1
         buf(11) = 0
         EXIT
      ELSEIF ( khi-klo==1 ) THEN
         IF ( k==klo ) THEN
            k = khi
         ELSE
            k = klo
         ENDIF
         klo = khi
      ELSE
         GOTO 3100
      ENDIF
   ENDDO
   GOTO ret
END SUBROUTINE mtrxin
