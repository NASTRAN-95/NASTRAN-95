
SUBROUTINE algpr(Ierr)
   IMPLICIT NONE
   INTEGER Apress , Atemp , Clsrew , Ifail , Incr , Iprtk , Ir1 , Ir2 , Iz(1) , Norew , Nout , Pgeom , Rd , Rdrew , Strml , Sysbuf ,&
         & Typout , Wrt , Wrtrew
   REAL Fxcoor , Fycoor , Fzcoor , Pi , Radeg , Sign , Twopi , Z(1) , Zorign
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Apress , Atemp , Strml , Pgeom , Iprtk , Ifail , Sign , Zorign , Fxcoor , Fycoor , Fzcoor
   COMMON /condas/ Pi , Twopi , Radeg
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Norew
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ Typout , Ir1 , Ir2 , Incr
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Iz
   INTEGER Ierr
   INTEGER algdb , algdd , bgpdt , buf1 , buf2 , cstm , edt , eqexin , file , i , i1 , i2 , ibgpdt , iblk , ic , icc , ichord ,     &
         & icid , icopy , icstm , id , idata(24) , idx , ifangs(10) , ifcord , ifill(3) , ikp , inl , intn , ipunch , irle , irte , &
         & irte1 , irte2 , isecn , isil , isk , isplit , istatn , itrl(7) , ivec , ivecn , izero , j , jchord , jsta , jvec , k ,   &
         & kform , khi , klo , kn , kode , kpt , kptsa(10) , ksta , ktype , lchord , left , len(3) , length , m8 , naero , name(2) ,&
         & nb , nbgpdt , nblstn , ncase , ncdsx , nchord , ncstm , ndatr , ndpts , neqex , nl , nle , nle1 , nles , nlines , nlins
   REAL b1(21) , b2(21) , blafor(21,10) , cord(21) , cord2(21) , delx(21) , dely(21) , dispr(3) , dispr1(21,10) , dispr2(21,10) ,   &
      & dispr3(21,10) , dispt(3) , dispt1(21,10) , dispt2(21,10) , dispt3(21,10) , fchord(21) , jz(21) , phi(2,21) , pp(21) , qq(21)&
      & , r(21,10) , rdata(6) , rfill(3) , rle(21) , rsta(21,10) , rzero , ta(9) , tc(21) , te(21) , temp , xb(21,10) , xsta(21,10) &
      & , yb(21,10) , zb(21,10) , zed(21) , zr(21)
   INTEGER corwds , korsz
   LOGICAL debug
   INTEGER nmany , nnodes , nrad , nrec , nsign , nspec , nstns , nte , ntype , nvects , nwar , nwar1 , ret2 , scr1 , scr2 ,        &
         & stream(3) , ugv
!
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (idata(1),rdata(1)) , (ifill(1),rfill(1))
   DATA name/4HALGP , 4HR   /
   DATA stream/3292 , 92 , 292/
   DATA len/18 , 24 , 6/
   DATA iblk , izero , rzero/4H     , 0 , 0.0/
   DATA edt , eqexin , ugv , algdd , cstm , bgpdt , scr1 , scr2/102 , 103 , 104 , 105 , 106 , 107 , 301 , 302/
!
!
!     PERFORM GENERAL INITIALIZATION
!
   debug = .FALSE.
   CALL sswtch(20,j)
   IF ( j==1 ) debug = .TRUE.
   buf1 = korsz(Iz) - Sysbuf
   buf2 = buf1 - Sysbuf
   left = corwds(Iz(1),Iz(buf2-1))
   m8 = -8
   IF ( left<=0 ) CALL mesage(m8,0,name)
   Ir1 = 1
   Incr = 1
   Typout = 1
   Ierr = 0
!
   ifill(1) = iblk
   ifill(2) = izero
   rfill(3) = rzero
!
!     CREATE ALGDB WITH CORRECT LENGTH RECORDS -
!     BCD(18 WORDS), INTEGER(24 WORDS), REAL(6 WORDS)
!
   CALL gopen(algdd,Iz(buf1),Rdrew)
   CALL gopen(scr2,Iz(buf2),Wrtrew)
   itrl(1) = algdd
   CALL rdtrl(itrl)
   itrl(1) = scr2
   CALL wrttrl(itrl)
 100  CALL read(*300,*200,algdd,idata,99,1,nwar)
 200  CALL algpb(idata(1),ntype)
   length = len(ntype)
!
!     REMOVE NUMERIC ZEROS FROM BCD STRING
!
   IF ( ntype==1 ) THEN
      DO WHILE ( idata(nwar)==0 )
         nwar = nwar - 1
         IF ( nwar<=0 ) EXIT
      ENDDO
   ENDIF
   IF ( nwar<length ) THEN
      nwar1 = nwar + 1
      DO i = nwar1 , length
         idata(i) = ifill(ntype)
      ENDDO
   ENDIF
   CALL write(scr2,idata,length,1)
   GOTO 100
 300  CALL close(algdd,Clsrew)
   CALL close(scr2,Clsrew)
   algdb = scr2
!
!     IF UGV IS NOT IN FIST (PURGED) THEN THERE WILL BE NO DATA
!     MODIFICATION
!
   itrl(1) = ugv
   CALL rdtrl(itrl)
   IF ( itrl(1)<0 ) THEN
      Ierr = 1
      GOTO 3700
   ELSE
!
!     READ EQEXIN INTO CORE
!
      file = eqexin
      CALL gopen(eqexin,Iz(buf1),Rdrew)
      CALL read(*3200,*400,eqexin,Iz(1),left,1,neqex)
      CALL mesage(m8,0,name)
   ENDIF
 400  CALL fread(eqexin,Iz(neqex+1),neqex,1)
   CALL close(eqexin,Clsrew)
   kn = neqex/2
   IF ( debug ) CALL bug1('EQEX    ',10,Iz(1),neqex)
   IF ( debug ) CALL bug1('EQEX    ',10,Iz(neqex+1),neqex)
!
!     READ CSTM INTO CORE (CSTM MAY BE PURGED)
!
   file = cstm
   icstm = 2*neqex + 1
   ncstm = 0
   CALL open(*600,cstm,Z(buf1),Rdrew)
   CALL fwdrec(*3200,cstm)
   CALL read(*3200,*500,cstm,Iz(icstm),buf1-icstm,1,ncstm)
   CALL mesage(m8,0,name)
 500  CALL close(cstm,Clsrew)
   IF ( debug ) CALL bug1('CSTM    ',20,Iz(icstm),ncstm)
!
!     SET-UP FOR CALLS TO TRANSS
!
   CALL pretrs(Iz(icstm),ncstm)
!
!     UNPACK UGV DISPLACEMENT VECTOR (SUBCASE 2) INTO CORE
!
 600  ivec = icstm + ncstm
   file = ugv
   itrl(1) = file
   CALL rdtrl(itrl)
!
!     CHECK FOR VALID UGV VECTOR
!     THIS ROUTINE WILL ONLY PROCESS A REAL S.P. RECT. VECTOR
!     OF SIZE G X 2
!     (EXPANDED TO INCLUDE REAL D.P. RECT. VECTOR, G X 2,
!     BY G.CHAN/UNISYS)
!
   nvects = itrl(2)
   kform = itrl(4)
   ktype = itrl(5)
   IF ( nvects/=2 .OR. kform/=2 ) THEN
      WRITE (Nout,99001) Ufm
!
99001 FORMAT (A23,' - ALG MODULE - UGV DATA BLOCK IS NOT A REAL S.P. ','RECTANGULAR MATRIX OF ORDER G BY 2.')
      Ierr = -1
      GOTO 3700
   ELSE
      ivecn = ivec + ktype*itrl(3) - 1
      IF ( ivecn>=buf1 ) CALL mesage(m8,0,name)
!
!     OPEN UGV AND SKIP FIRST COLUMN (SUBCASE 1)
!
      CALL gopen(ugv,Iz(buf1),Rdrew)
      CALL fwdrec(*3200,ugv)
      Ir2 = itrl(3)
      CALL unpack(*700,ugv,Iz(ivec))
      GOTO 800
   ENDIF
!
!     NULL COLUMN
!
 700  DO i = ivec , ivecn
      Z(i) = 0.0
   ENDDO
 800  CALL close(ugv,Clsrew)
   IF ( debug ) CALL bug1('UGV     ',60,Iz(ivec),Ir2)
!
!     LOCATE STREAML1 CARDS ON EDT AND STORE IN CORE
!
   file = edt
   ichord = ivecn + 1
   CALL preloc(*3300,Iz(buf1),edt)
   CALL locate(*3400,Iz(buf1),stream,idx)
   CALL read(*3200,*900,edt,Iz(ichord),buf1-ichord,1,nchord)
   CALL mesage(m8,0,name)
 900  CALL close(edt,Clsrew)
   IF ( debug ) CALL bug1('CHOR    ',70,Iz(ichord),nchord)
   lchord = ichord + nchord - 1
!
!     READ THE BGPDT INTO CORE
!
   ibgpdt = lchord + 1
   file = bgpdt
   CALL gopen(bgpdt,Iz(buf1),Rdrew)
   CALL read(*3200,*1000,bgpdt,Iz(ibgpdt),buf1-ibgpdt,1,nbgpdt)
   CALL mesage(m8,0,name)
 1000 CALL close(bgpdt,Clsrew)
   IF ( debug ) CALL bug1('BGPD    ',80,Iz(ibgpdt),nbgpdt)
!
!     FOR EACH STREAML1 CARD -
!     (1) FIND BLADE NODES
!     (2) FIND EQUIVALENT INTERNAL NUMBERS OF THESE NODES
!     (3) LOCATE CORRESPONDING COMPONENTS OF DISPLACEMENT AND
!         CONVERT THEN TO BASIC VIA CSTM
!     (4) LOCATE BASIC GRID POINT DATA FOR BLADE NODES
!
   ic = ichord + 1
   icc = ichord
   jchord = 1
   nnodes = 0
 1100 istatn = 0
   DO
      id = Iz(ic)
      IF ( id/=-1 ) THEN
         istatn = istatn + 1
!
!
!     INTERNAL BINARY SEARCH ROUTINE
!
!     SEARCH EQEXIN FOR INTERNAL NUMBER AND SIL NUMBER OF EXTERNAL NODE
!
         klo = 1
         khi = kn
         k = (klo+khi+1)/2
         DO
            IF ( id<Iz(2*k-1) ) THEN
               khi = k
            ELSEIF ( id==Iz(2*k-1) ) THEN
               intn = Iz(2*k)
               isil = Iz(2*k+neqex)/10
               kode = Iz(2*k+neqex) - 10*isil
               IF ( debug ) CALL bug1('ISTL    ',1090,isil,1)
               IF ( debug ) CALL bug1('KODE    ',1090,kode,1)
!
!     LOCATE COORDINATE SYSTEM ID FOR THIS NODE IN THE BGPDT
!
               icid = 4*(intn-1) + ibgpdt
!
!     SET-UP COORDINATE SYSTEM TRANSFORMATION FOR DISPLACEMENTS.
!
               IF ( Iz(icid)>0 ) CALL transs(Iz(icid),ta)
!
!     COMPUTE POINTER INTO UGV
!     JVEC = IVEC + KTYPE *(ISIL-1)
!
               jvec = ivec + Typout*(isil-1)
!
!     PICK-UP DISPLACEMENTS
!
               IF ( kode/=1 ) THEN
!
!     SCALAR POINT
!
                  dispt(1) = Z(jvec)
                  dispt(2) = 0.0
                  dispt(3) = 0.0
                  dispr(1) = 0.0
                  dispr(2) = 0.0
                  dispr(3) = 0.0
!
!     GRID POINT
!
               ELSEIF ( Iz(icid)>0 ) THEN
!
!     DISPLACEMENTS MUST BE TRANSFORMED TO BASIC
!
                  CALL gmmats(ta,3,3,0,Z(jvec),3,1,0,dispt)
                  CALL gmmats(ta,3,3,0,Z(jvec+3),3,1,0,dispr)
               ELSE
!
!     DISPLACEMENTS ALREADY IN BASIC SYSTEM
!
                  dispt(1) = Z(jvec)
                  dispt(2) = Z(jvec+1)
                  dispt(3) = Z(jvec+2)
                  dispr(1) = Z(jvec+3)
                  dispr(2) = Z(jvec+4)
                  dispr(3) = Z(jvec+5)
               ENDIF
!
!     STORE BASIC GRID POINT COORDINATES FROM BGPDT
!
               xb(jchord,istatn) = Z(icid+1)
               yb(jchord,istatn) = Z(icid+2)
               zb(jchord,istatn) = Z(icid+3)
               dispt1(jchord,istatn) = dispt(1)
               dispt2(jchord,istatn) = dispt(2)
               dispt3(jchord,istatn) = dispt(3)
               dispr1(jchord,istatn) = dispr(1)
               dispr2(jchord,istatn) = dispr(2)
               dispr3(jchord,istatn) = dispr(3)
               IF ( debug ) CALL bug1('NODE    ',id,Z(icid+1),3)
               IF ( debug ) CALL bug1('NODE    ',id,dispt,3)
               IF ( debug ) CALL bug1('NODE    ',id,dispr,3)
               ic = ic + 1
               EXIT
            ELSE
               klo = k
            ENDIF
            IF ( khi-klo<1 ) THEN
               WRITE (Nout,99002) Ufm , Iz(icc) , id
99002          FORMAT (A23,' - ALG MODULE - STREAML1 BULK DATA CARD (SLN NO. =',I3,') REFERENCES UNDEFINED NODE NO.',I8)
               Ierr = -1
               GOTO 3700
            ELSEIF ( khi-klo==1 ) THEN
               IF ( k==klo ) THEN
                  k = khi
               ELSE
                  k = klo
               ENDIF
               klo = khi
            ELSE
               k = (klo+khi+1)/2
            ENDIF
         ENDDO
      ELSE
         icc = ic + 1
         ic = ic + 2
         nnodes = nnodes + istatn
         jchord = jchord + 1
         IF ( ic<lchord ) GOTO 1100
         jchord = jchord - 1
         IF ( jchord>21 ) THEN
            WRITE (Nout,99003) Uwm
99003       FORMAT (A25,' - ALG MODULE - MORE THAN 21 STREAML1 CARDS READ. ','FIRST 21 WILL BE USED.')
            GOTO 3700
         ELSE
!
!     MODIFY AERODYNAMIC INPUT  (OPEN ALGDB DATA BLOCK)
!
            file = algdb
            CALL gopen(algdb,Iz(buf1),Rdrew)
            CALL fwdrec(*3500,algdb)
            CALL read(*3200,*3600,algdb,idata,2,1,nwar)
            naero = idata(2)
            CALL skprec(algdb,1)
            CALL fread(algdb,idata,17,1)
            nlines = idata(1)
            nstns = idata(2)
            nspec = idata(4)
            ipunch = idata(8)
            isecn = idata(9)
            ifcord = idata(10)
            isplit = idata(13)
            irle = idata(15)
            irte = idata(16)
            nsign = idata(17)
            CALL skprec(algdb,1)
            DO isk = 1 , nstns
               CALL fread(algdb,idata,2,1)
               kptsa(isk) = idata(1)
               ifangs(isk) = idata(2)
               CALL skprec(algdb,idata(1))
               DO inl = 1 , nlines
                  CALL fread(algdb,rdata,2,1)
                  blafor(inl,isk) = rdata(2)
               ENDDO
            ENDDO
            DO isk = 1 , nspec
               CALL fread(algdb,rdata,6,1)
               zr(isk) = rdata(1)
               jz(isk) = rdata(1) + 0.4
               b1(isk) = rdata(2)
               b2(isk) = rdata(3)
               pp(isk) = rdata(4)
               qq(isk) = rdata(5)
               rle(isk) = rdata(6)
               CALL fread(algdb,rdata,6,1)
               tc(isk) = rdata(1)
               te(isk) = rdata(2)
               zed(isk) = rdata(3)
               cord(isk) = rdata(4)
               delx(isk) = rdata(5)
               dely(isk) = rdata(6)
               IF ( isecn==1 .OR. isecn==3 ) CALL skprec(algdb,1)
            ENDDO
            CALL close(algdb,Clsrew)
!
!     NUMBER OF BLADE STATIONS
!
            nblstn = irte - irle + 1
            IF ( nlines/=jchord ) THEN
               WRITE (Nout,99004) Ufm
99004          FORMAT (A23,' - ALG MODULE - INPUT IN ALGDB DATA BLOCK (FILE 105',                                                   &
                      &') INCONSISTENT WITH DATA ON STREAML1 BULK DATA CARDS.',/39X,                                                &
                      &'CHECK THE NUMBER OF COMPUTING STATIONS AND THE ','NUMBER OF STREAMSURFACES ON THE BLADE.')
               Ierr = -1
               GOTO 3700
            ELSEIF ( nnodes/=nlines*nblstn ) THEN
               WRITE (Nout,99004) Ufm
               Ierr = -1
               GOTO 3700
            ELSE
!
!     COMPUTE FCORD AND PHI
!
               DO k = 1 , nspec
                  j = jz(k)
                  temp = (xb(j,nblstn)-xb(j,1))**2 + (zb(j,nblstn)-zb(j,1))**2
                  IF ( ifcord==1 ) temp = temp + (yb(j,nblstn)-yb(j,1))**2
                  fchord(k) = cord(k)/sqrt(temp)
                  phi(1,k) = atan((zb(j,2)-zb(j,1))/(xb(j,2)-xb(j,1)))
                  phi(2,k) = atan((zb(j,nblstn)-zb(j,nblstn-1))/(xb(j,nblstn)-xb(j,nblstn-1)))
               ENDDO
!     COMPUTE NEW COORDINATES
!     GENERATE XSTA, RSTA AND R , SET KPTS = NLINES
               DO i = 1 , nlines
                  DO j = 1 , nblstn
                     xb(i,j) = xb(i,j) + Sign*dispt1(i,j)*Fxcoor
                     yb(i,j) = yb(i,j) + Sign*dispt2(i,j)*Fycoor
                     zb(i,j) = zb(i,j) + Sign*dispt3(i,j)*Fzcoor
                     xsta(i,j) = xb(i,j)
                     rsta(i,j) = zb(i,j) + Zorign
                     r(i,j) = rsta(i,j)
                  ENDDO
               ENDDO
!
!     COMPUTE CORD2
!
               DO k = 1 , nspec
                  j = jz(k)
                  temp = (xb(j,nblstn)-xb(j,1))**2 + (zb(j,nblstn)-zb(j,1))**2
                  IF ( ifcord==1 ) temp = temp + (yb(j,nblstn)-yb(j,1))**2
                  cord2(k) = fchord(k)*sqrt(temp)
               ENDDO
!
!     MODIFY B1, B2, RLE, TC, TE, CORD, DELX AND DELY
!
               i1 = (nblstn+1)/2
               i2 = i1
               IF ( i1*2/=nblstn+1 ) i2 = i2 + 1
               DO k = 1 , nspec
                  j = jz(k)
                  b1(k) = b1(k) - nsign*Sign*Radeg*(dispr3(j,1)*cos(phi(1,k))-dispr1(j,1)*sin(phi(1,k)))
                  b2(k) = b2(k) - nsign*Sign*Radeg*(dispr3(j,nblstn)*cos(phi(2,k))-dispr1(j,nblstn)*sin(phi(2,k)))
                  temp = cord(k)/cord2(k)
                  rle(k) = rle(k)*temp
                  tc(k) = tc(k)*temp
                  te(k) = te(k)*temp
                  cord(k) = cord2(k)
                  delx(k) = delx(k) + 0.5*Sign*Fxcoor*(dispt1(j,i1)+dispt1(j,i2))
                  dely(k) = dely(k) + 0.5*Sign*Fycoor*(dispt2(j,i1)+dispt2(j,i2))
               ENDDO
!
!     GENERATE NEW ALGDB DATA BLOCK
!
               CALL gopen(algdb,Iz(buf1),Rdrew)
               CALL gopen(scr1,Iz(buf2),Wrtrew)
               itrl(1) = algdb
               CALL rdtrl(itrl)
!
!     MODIFY THE NUMBER OF CARDS IN ALGDB
!
               ncdsx = 0
               DO kpt = irle , irte
                  ncdsx = ncdsx + nlines - kptsa(kpt)
               ENDDO
               itrl(2) = itrl(2) + ncdsx
               itrl(1) = scr1
               CALL wrttrl(itrl)
               ASSIGN 1200 TO ret2
               nrec = 5
               GOTO 3000
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!
!     COPY DATA FOR STATIONS 1 THRU (IRLE-1)
!
 1200 IF ( irle==1 ) GOTO 1400
   nles = irle - 1
   nrec = nles + nles*nlines
   DO ikp = 1 , nles
      nrec = nrec + kptsa(ikp)
   ENDDO
   ASSIGN 1300 TO ret2
   GOTO 3000
!
!     SKIP OVER EXISTING RECORDS FOR STATIONS IRLE THRU IRTE
!
 1300 nrec = nblstn + nblstn*nlines
   DO ikp = irle , irte
      nrec = nrec + kptsa(ikp)
   ENDDO
   CALL skprec(algdb,nrec)
!
!     CREATE NEW DATA RECORDS FOR STATIONS IRLE THRU IRTE
!
   ksta = 0
   DO jsta = irle , irte
      ksta = ksta + 1
      idata(1) = nlines
      idata(2) = ifangs(jsta)
      CALL write(scr1,idata,2,1)
      IF ( debug ) CALL bug1('ALGPR   ',329,idata,2)
      DO i = 1 , nlines
         rdata(1) = xsta(i,ksta)
         rdata(2) = rsta(i,ksta)
         IF ( debug ) CALL bug1('ALGPR   ',330,rdata,2)
         CALL write(scr1,rdata,2,1)
      ENDDO
      DO i = 1 , nlines
         rdata(1) = r(i,ksta)
         rdata(2) = blafor(i,ksta)
         IF ( debug ) CALL bug1('ALGPR   ',332,rdata,2)
         CALL write(scr1,rdata,2,1)
      ENDDO
   ENDDO
!
!     COPY DATA FOR STATIONS (IRTE+1) THRU NSTNS
!
 1400 IF ( irte/=nstns ) THEN
      irte1 = irte + 1
      irte2 = nstns - irte
      nrec = irte2 + irte2*nlines
      DO ikp = irte1 , nstns
         nrec = nrec + kptsa(ikp)
      ENDDO
      ASSIGN 1500 TO ret2
      GOTO 3000
   ENDIF
!
!     MODIFY THE NEXT NSPEC RECORDS
!
 1500 DO i = 1 , nspec
      CALL skprec(algdb,2)
      rdata(1) = zr(i)
      rdata(2) = b1(i)
      rdata(3) = b2(i)
      rdata(4) = pp(i)
      rdata(5) = qq(i)
      rdata(6) = rle(i)
      CALL write(scr1,rdata,6,1)
      IF ( debug ) CALL bug1('ALGPR   ',338,rdata,6)
      rdata(1) = tc(i)
      rdata(2) = te(i)
      rdata(3) = zed(i)
      rdata(4) = cord(i)
      rdata(5) = delx(i)
      rdata(6) = dely(i)
      CALL write(scr1,rdata,6,1)
      IF ( debug ) CALL bug1('ALGPR   ',339,rdata,6)
      IF ( isecn==1 .OR. isecn==3 ) THEN
         CALL fread(algdb,rdata,2,1)
         CALL write(scr1,rdata,2,1)
         IF ( debug ) CALL bug1('ALGPR   ',340,rdata,2)
      ENDIF
   ENDDO
!
!     COPY REST OF ANALYTIC DATA
!
   IF ( isplit>=1 ) THEN
      nrec = nspec
      DO i = 1 , nstns
         IF ( ifangs(i)==2 ) nrec = nrec + nlines
      ENDDO
      ASSIGN 1600 TO ret2
      GOTO 3000
   ENDIF
 1600 IF ( naero/=1 .AND. ipunch/=1 ) GOTO 2200
   nrec = 1
   ASSIGN 1700 TO ret2
   GOTO 3000
 1700 nrad = idata(1)
   ndpts = idata(2)
   ndatr = idata(3)
   ASSIGN 1800 TO ret2
   nrec = 2
   GOTO 3000
 1800 nb = nblstn - 1
   i = 1
 1900 nrec = 1
   ASSIGN 2000 TO ret2
   GOTO 3000
 2000 nrec = idata(1)
   ASSIGN 2100 TO ret2
   GOTO 3000
 2100 i = i + 1
   IF ( i<=nb ) GOTO 1900
   nrec = nrad*(ndpts+1) + ndatr
   ASSIGN 2200 TO ret2
   GOTO 3000
!
!     PROCESS AERODYNAMIC INPUT
!
 2200 IF ( naero==0 ) GOTO 2900
   ASSIGN 2300 TO ret2
   nrec = 3
   GOTO 3000
 2300 nstns = idata(1)
   ncase = idata(6)
   nmany = idata(16)
   nle = idata(19)
   nte = idata(20)
   nsign = idata(21)
   IF ( nstns==0 ) nstns = 11
   IF ( ncase==0 ) ncase = 1
   nrec = ncase + 3
   IF ( nmany>0 ) nrec = ncase + 4
   ASSIGN 2400 TO ret2
   GOTO 3000
!
!     COPY DATA FOR STATIONS 1 THRU (NLE-1)
!
 2400 IF ( nle==1 ) GOTO 2800
   nle1 = nle - 1
   i = 1
 2500 nrec = 1
   ASSIGN 2600 TO ret2
   GOTO 3000
 2600 nrec = idata(1)
   ASSIGN 2700 TO ret2
   GOTO 3000
 2700 i = i + 1
   IF ( i<=nle1 ) GOTO 2500
 2800 jsta = 0
!
!     MODIFY DATA FOR STATIONS NLE THRU NTE
!
   DO i = nle , nte
      jsta = jsta + 1
      CALL fread(algdb,nspec,1,1)
      CALL skprec(algdb,nspec)
      CALL write(scr1,nlines,1,1)
      IF ( debug ) CALL bug1('ALGPR   ',361,nlins,1)
      DO nl = 1 , nlines
         rdata(1) = xsta(nl,jsta)
         rdata(2) = rsta(nl,jsta)
         IF ( debug ) CALL bug1('ALGPR   ',362,rdata,2)
         CALL write(scr1,rdata,2,1)
      ENDDO
   ENDDO
!
!     COPY REST OF DATA
!
   ASSIGN 2900 TO ret2
   nrec = 65000
   GOTO 3000
!
!     CLOSE ALGDB AND SCR1
!
 2900 CALL close(algdb,Clsrew)
   CALL close(scr1,Clsrew)
!
!     PUNCH NEW ALGDB TABLE INTO DTI CARDS IF PGEOM=3.
!
   IF ( Pgeom==3 ) CALL algap(algdd,scr1)
   GOTO 3700
 3000 DO icopy = 1 , nrec
      CALL read(*3100,*3050,algdb,idata,99,1,nwar)
 3050 CALL write(scr1,idata,nwar,1)
      IF ( debug ) CALL bug1('ALGPR   ',1302,idata,nwar)
   ENDDO
   IF ( nrec>=65000 ) THEN
      WRITE (Nout,99005)
99005 FORMAT (/,' *** NO. OF RECORDS EXCEEDS HARDWARE LIMIT/ALGPR')
      CALL mesage(-37,0,0)
   ENDIF
 3100 GOTO ret2
!
 3200 CALL mesage(-2,file,name)
   Ierr = -1
   GOTO 3700
 3300 WRITE (Nout,99006) Ufm
99006 FORMAT (A23,' - ALG MODULE - EDT DATA BLOCK MAY NOT BE PURGED.')
   Ierr = -1
   GOTO 3700
 3400 WRITE (Nout,99007) Ufm
99007 FORMAT (A23,' - ALG MODULE - STREAML1 BULK DATA CARD MISSING ','FROM BULK DATA DECK.')
   Ierr = -1
   GOTO 3700
 3500 WRITE (Nout,99008) Ufm
99008 FORMAT (A23,' - ALG MODULE - ALGDB DATA BLOCK (FILE 105) DOES ','NOT HAVE ENOUGH RECORDS.')
   Ierr = -1
   GOTO 3700
 3600 CALL mesage(-3,file,name)
   Ierr = -1
 3700 RETURN
END SUBROUTINE algpr