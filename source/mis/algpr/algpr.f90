!*==algpr.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algpr(Ierr)
   USE c_blank
   USE c_condas
   USE c_names
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ierr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: algdb , buf1 , buf2 , file , i , i1 , i2 , ibgpdt , ic , icc , ichord , icid , icopy , icstm , id , idx , ifcord ,    &
            & ikp , inl , intn , ipunch , irle , irte , irte1 , irte2 , isecn , isil , isk , isplit , istatn , ivec , ivecn , j ,   &
            & jchord , jsta , jvec , k , kform , khi , klo , kn , kode , kpt , ksta , ktype , lchord , left , length , m8 , naero , &
            & nb , nbgpdt , nblstn , ncase , ncdsx , nchord , ncstm , ndatr , ndpts , neqex , nl , nle , nle1 , nles , nlines ,     &
            & nlins , nmany , nnodes , nrad , nrec , nsign , nspec , nstns , nte , ntype , nvects , nwar , nwar1 , ret2
   INTEGER , SAVE :: algdd , bgpdt , cstm , edt , eqexin , iblk , izero , scr1 , scr2 , ugv
   REAL , DIMENSION(21) :: b1 , b2 , cord , cord2 , delx , dely , fchord , jz , pp , qq , rle , tc , te , zed , zr
   REAL , DIMENSION(21,10) :: blafor , dispr1 , dispr2 , dispr3 , dispt1 , dispt2 , dispt3 , r , rsta , xb , xsta , yb , zb
   LOGICAL :: debug
   REAL , DIMENSION(3) :: dispr , dispt , rfill
   INTEGER , DIMENSION(24) :: idata
   INTEGER , DIMENSION(10) :: ifangs , kptsa
   INTEGER , DIMENSION(3) :: ifill
   INTEGER , DIMENSION(7) :: itrl
   INTEGER , DIMENSION(3) , SAVE :: len , stream
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(2,21) :: phi
   REAL , DIMENSION(6) :: rdata
   REAL , SAVE :: rzero
   REAL , DIMENSION(9) :: ta
   REAL :: temp
   REAL , DIMENSION(1) :: z
   EXTERNAL algap , algpb , bug1 , close , corwds , fread , fwdrec , gmmats , gopen , korsz , locate , mesage , open , preloc ,     &
          & pretrs , rdtrl , read , skprec , sswtch , transs , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (idata(1),rdata(1)) , (ifill(1),rfill(1))
   DATA name/4HALGP , 4HR   /
   DATA stream/3292 , 92 , 292/
   DATA len/18 , 24 , 6/
   DATA iblk , izero , rzero/4H     , 0 , 0.0/
   DATA edt , eqexin , ugv , algdd , cstm , bgpdt , scr1 , scr2/102 , 103 , 104 , 105 , 106 , 107 , 301 , 302/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     PERFORM GENERAL INITIALIZATION
!
         debug = .FALSE.
         CALL sswtch(20,j)
         IF ( j==1 ) debug = .TRUE.
         buf1 = korsz(iz) - sysbuf
         buf2 = buf1 - sysbuf
         left = corwds(iz(1),iz(buf2-1))
         m8 = -8
         IF ( left<=0 ) CALL mesage(m8,0,name)
         ir1 = 1
         incr = 1
         typout = 1
         Ierr = 0
!
         ifill(1) = iblk
         ifill(2) = izero
         rfill(3) = rzero
!
!     CREATE ALGDB WITH CORRECT LENGTH RECORDS -
!     BCD(18 WORDS), INTEGER(24 WORDS), REAL(6 WORDS)
!
         CALL gopen(algdd,iz(buf1),rdrew)
         CALL gopen(scr2,iz(buf2),wrtrew)
         itrl(1) = algdd
         CALL rdtrl(itrl)
         itrl(1) = scr2
         CALL wrttrl(itrl)
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*20,algdd,idata,99,1,nwar)
 20      CALL algpb(idata(1),ntype)
         length = len(ntype)
!
!     REMOVE NUMERIC ZEROS FROM BCD STRING
!
         IF ( ntype==1 ) THEN
            SPAG_Loop_1_1: DO WHILE ( idata(nwar)==0 )
               nwar = nwar - 1
               IF ( nwar<=0 ) EXIT SPAG_Loop_1_1
            ENDDO SPAG_Loop_1_1
         ENDIF
         IF ( nwar<length ) THEN
            nwar1 = nwar + 1
            DO i = nwar1 , length
               idata(i) = ifill(ntype)
            ENDDO
         ENDIF
         CALL write(scr2,idata,length,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(algdd,clsrew)
         CALL close(scr2,clsrew)
         algdb = scr2
!
!     IF UGV IS NOT IN FIST (PURGED) THEN THERE WILL BE NO DATA
!     MODIFICATION
!
         itrl(1) = ugv
         CALL rdtrl(itrl)
         IF ( itrl(1)<0 ) THEN
            Ierr = 1
            RETURN
         ELSE
!
!     READ EQEXIN INTO CORE
!
            file = eqexin
            CALL gopen(eqexin,iz(buf1),rdrew)
            CALL read(*480,*60,eqexin,iz(1),left,1,neqex)
            CALL mesage(m8,0,name)
         ENDIF
 60      CALL fread(eqexin,iz(neqex+1),neqex,1)
         CALL close(eqexin,clsrew)
         kn = neqex/2
         IF ( debug ) CALL bug1('EQEX    ',10,iz(1),neqex)
         IF ( debug ) CALL bug1('EQEX    ',10,iz(neqex+1),neqex)
!
!     READ CSTM INTO CORE (CSTM MAY BE PURGED)
!
         file = cstm
         icstm = 2*neqex + 1
         ncstm = 0
         CALL open(*100,cstm,z(buf1),rdrew)
         CALL fwdrec(*480,cstm)
         CALL read(*480,*80,cstm,iz(icstm),buf1-icstm,1,ncstm)
         CALL mesage(m8,0,name)
 80      CALL close(cstm,clsrew)
         IF ( debug ) CALL bug1('CSTM    ',20,iz(icstm),ncstm)
!
!     SET-UP FOR CALLS TO TRANSS
!
         CALL pretrs(iz(icstm),ncstm)
!
!     UNPACK UGV DISPLACEMENT VECTOR (SUBCASE 2) INTO CORE
!
 100     ivec = icstm + ncstm
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
            WRITE (nout,99001) ufm
!
99001       FORMAT (A23,' - ALG MODULE - UGV DATA BLOCK IS NOT A REAL S.P. ','RECTANGULAR MATRIX OF ORDER G BY 2.')
            Ierr = -1
            RETURN
         ELSE
            ivecn = ivec + ktype*itrl(3) - 1
            IF ( ivecn>=buf1 ) CALL mesage(m8,0,name)
!
!     OPEN UGV AND SKIP FIRST COLUMN (SUBCASE 1)
!
            CALL gopen(ugv,iz(buf1),rdrew)
            CALL fwdrec(*480,ugv)
            ir2 = itrl(3)
            CALL unpack(*120,ugv,iz(ivec))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NULL COLUMN
!
 120     DO i = ivec , ivecn
            z(i) = 0.0
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(ugv,clsrew)
         IF ( debug ) CALL bug1('UGV     ',60,iz(ivec),ir2)
!
!     LOCATE STREAML1 CARDS ON EDT AND STORE IN CORE
!
         file = edt
         ichord = ivecn + 1
         CALL preloc(*500,iz(buf1),edt)
         CALL locate(*520,iz(buf1),stream,idx)
         CALL read(*480,*140,edt,iz(ichord),buf1-ichord,1,nchord)
         CALL mesage(m8,0,name)
 140     CALL close(edt,clsrew)
         IF ( debug ) CALL bug1('CHOR    ',70,iz(ichord),nchord)
         lchord = ichord + nchord - 1
!
!     READ THE BGPDT INTO CORE
!
         ibgpdt = lchord + 1
         file = bgpdt
         CALL gopen(bgpdt,iz(buf1),rdrew)
         CALL read(*480,*160,bgpdt,iz(ibgpdt),buf1-ibgpdt,1,nbgpdt)
         CALL mesage(m8,0,name)
 160     CALL close(bgpdt,clsrew)
         IF ( debug ) CALL bug1('BGPD    ',80,iz(ibgpdt),nbgpdt)
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
         SPAG_Loop_1_3: DO
            istatn = 0
            DO
               id = iz(ic)
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
                  SPAG_Loop_3_2: DO
                     IF ( id<iz(2*k-1) ) THEN
                        khi = k
                     ELSEIF ( id==iz(2*k-1) ) THEN
                        intn = iz(2*k)
                        isil = iz(2*k+neqex)/10
                        kode = iz(2*k+neqex) - 10*isil
                        IF ( debug ) CALL bug1('ISTL    ',1090,isil,1)
                        IF ( debug ) CALL bug1('KODE    ',1090,kode,1)
!
!     LOCATE COORDINATE SYSTEM ID FOR THIS NODE IN THE BGPDT
!
                        icid = 4*(intn-1) + ibgpdt
!
!     SET-UP COORDINATE SYSTEM TRANSFORMATION FOR DISPLACEMENTS.
!
                        IF ( iz(icid)>0 ) CALL transs(iz(icid),ta)
!
!     COMPUTE POINTER INTO UGV
!     JVEC = IVEC + KTYPE *(ISIL-1)
!
                        jvec = ivec + typout*(isil-1)
!
!     PICK-UP DISPLACEMENTS
!
                        IF ( kode/=1 ) THEN
!
!     SCALAR POINT
!
                           dispt(1) = z(jvec)
                           dispt(2) = 0.0
                           dispt(3) = 0.0
                           dispr(1) = 0.0
                           dispr(2) = 0.0
                           dispr(3) = 0.0
!
!     GRID POINT
!
                        ELSEIF ( iz(icid)>0 ) THEN
!
!     DISPLACEMENTS MUST BE TRANSFORMED TO BASIC
!
                           CALL gmmats(ta,3,3,0,z(jvec),3,1,0,dispt)
                           CALL gmmats(ta,3,3,0,z(jvec+3),3,1,0,dispr)
                        ELSE
!
!     DISPLACEMENTS ALREADY IN BASIC SYSTEM
!
                           dispt(1) = z(jvec)
                           dispt(2) = z(jvec+1)
                           dispt(3) = z(jvec+2)
                           dispr(1) = z(jvec+3)
                           dispr(2) = z(jvec+4)
                           dispr(3) = z(jvec+5)
                        ENDIF
!
!     STORE BASIC GRID POINT COORDINATES FROM BGPDT
!
                        xb(jchord,istatn) = z(icid+1)
                        yb(jchord,istatn) = z(icid+2)
                        zb(jchord,istatn) = z(icid+3)
                        dispt1(jchord,istatn) = dispt(1)
                        dispt2(jchord,istatn) = dispt(2)
                        dispt3(jchord,istatn) = dispt(3)
                        dispr1(jchord,istatn) = dispr(1)
                        dispr2(jchord,istatn) = dispr(2)
                        dispr3(jchord,istatn) = dispr(3)
                        IF ( debug ) CALL bug1('NODE    ',id,z(icid+1),3)
                        IF ( debug ) CALL bug1('NODE    ',id,dispt,3)
                        IF ( debug ) CALL bug1('NODE    ',id,dispr,3)
                        ic = ic + 1
                        EXIT SPAG_Loop_3_2
                     ELSE
                        klo = k
                     ENDIF
                     IF ( khi-klo<1 ) THEN
                        WRITE (nout,99002) ufm , iz(icc) , id
99002                   FORMAT (A23,' - ALG MODULE - STREAML1 BULK DATA CARD (SLN NO. =',I3,') REFERENCES UNDEFINED NODE NO.',I8)
                        Ierr = -1
                        RETURN
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
                  ENDDO SPAG_Loop_3_2
               ELSE
                  icc = ic + 1
                  ic = ic + 2
                  nnodes = nnodes + istatn
                  jchord = jchord + 1
                  IF ( ic<lchord ) CYCLE SPAG_Loop_1_3
                  jchord = jchord - 1
                  IF ( jchord>21 ) THEN
                     WRITE (nout,99003) uwm
99003                FORMAT (A25,' - ALG MODULE - MORE THAN 21 STREAML1 CARDS READ. ','FIRST 21 WILL BE USED.')
                  ELSE
!
!     MODIFY AERODYNAMIC INPUT  (OPEN ALGDB DATA BLOCK)
!
                     file = algdb
                     CALL gopen(algdb,iz(buf1),rdrew)
                     CALL fwdrec(*540,algdb)
                     CALL read(*480,*560,algdb,idata,2,1,nwar)
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
                     CALL close(algdb,clsrew)
!
!     NUMBER OF BLADE STATIONS
!
                     nblstn = irte - irle + 1
                     IF ( nlines/=jchord ) THEN
                        WRITE (nout,99008) ufm
                        Ierr = -1
                     ELSEIF ( nnodes/=nlines*nblstn ) THEN
                        WRITE (nout,99008) ufm
                        Ierr = -1
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
                              xb(i,j) = xb(i,j) + sign*dispt1(i,j)*fxcoor
                              yb(i,j) = yb(i,j) + sign*dispt2(i,j)*fycoor
                              zb(i,j) = zb(i,j) + sign*dispt3(i,j)*fzcoor
                              xsta(i,j) = xb(i,j)
                              rsta(i,j) = zb(i,j) + zorign
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
                           b1(k) = b1(k) - nsign*sign*radeg*(dispr3(j,1)*cos(phi(1,k))-dispr1(j,1)*sin(phi(1,k)))
                           b2(k) = b2(k) - nsign*sign*radeg*(dispr3(j,nblstn)*cos(phi(2,k))-dispr1(j,nblstn)*sin(phi(2,k)))
                           temp = cord(k)/cord2(k)
                           rle(k) = rle(k)*temp
                           tc(k) = tc(k)*temp
                           te(k) = te(k)*temp
                           cord(k) = cord2(k)
                           delx(k) = delx(k) + 0.5*sign*fxcoor*(dispt1(j,i1)+dispt1(j,i2))
                           dely(k) = dely(k) + 0.5*sign*fycoor*(dispt2(j,i1)+dispt2(j,i2))
                        ENDDO
!
!     GENERATE NEW ALGDB DATA BLOCK
!
                        CALL gopen(algdb,iz(buf1),rdrew)
                        CALL gopen(scr1,iz(buf2),wrtrew)
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
                        ASSIGN 180 TO ret2
                        nrec = 5
                        spag_nextblock_1 = 8
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  RETURN
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
!
!     COPY DATA FOR STATIONS 1 THRU (IRLE-1)
!
 180     IF ( irle==1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nles = irle - 1
         nrec = nles + nles*nlines
         DO ikp = 1 , nles
            nrec = nrec + kptsa(ikp)
         ENDDO
         ASSIGN 200 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     SKIP OVER EXISTING RECORDS FOR STATIONS IRLE THRU IRTE
!
 200     nrec = nblstn + nblstn*nlines
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
         spag_nextblock_1 = 4
      CASE (4)
!
!     COPY DATA FOR STATIONS (IRTE+1) THRU NSTNS
!
         IF ( irte/=nstns ) THEN
            irte1 = irte + 1
            irte2 = nstns - irte
            nrec = irte2 + irte2*nlines
            DO ikp = irte1 , nstns
               nrec = nrec + kptsa(ikp)
            ENDDO
            ASSIGN 220 TO ret2
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     MODIFY THE NEXT NSPEC RECORDS
!
 220     DO i = 1 , nspec
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
            ASSIGN 240 TO ret2
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 240     IF ( naero/=1 .AND. ipunch/=1 ) GOTO 340
         nrec = 1
         ASSIGN 260 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 260     nrad = idata(1)
         ndpts = idata(2)
         ndatr = idata(3)
         ASSIGN 280 TO ret2
         nrec = 2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 280     nb = nblstn - 1
         i = 1
         spag_nextblock_1 = 5
      CASE (5)
         nrec = 1
         ASSIGN 300 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 300     nrec = idata(1)
         ASSIGN 320 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 320     i = i + 1
         IF ( i<=nb ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nrec = nrad*(ndpts+1) + ndatr
         ASSIGN 340 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS AERODYNAMIC INPUT
!
 340     IF ( naero==0 ) GOTO 440
         ASSIGN 360 TO ret2
         nrec = 3
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 360     nstns = idata(1)
         ncase = idata(6)
         nmany = idata(16)
         nle = idata(19)
         nte = idata(20)
         nsign = idata(21)
         IF ( nstns==0 ) nstns = 11
         IF ( ncase==0 ) ncase = 1
         nrec = ncase + 3
         IF ( nmany>0 ) nrec = ncase + 4
         ASSIGN 380 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     COPY DATA FOR STATIONS 1 THRU (NLE-1)
!
 380     IF ( nle==1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nle1 = nle - 1
         i = 1
         spag_nextblock_1 = 6
      CASE (6)
         nrec = 1
         ASSIGN 400 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 400     nrec = idata(1)
         ASSIGN 420 TO ret2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 420     i = i + 1
         IF ( i<=nle1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         jsta = 0
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
         ASSIGN 440 TO ret2
         nrec = 65000
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE ALGDB AND SCR1
!
 440     CALL close(algdb,clsrew)
         CALL close(scr1,clsrew)
!
!     PUNCH NEW ALGDB TABLE INTO DTI CARDS IF PGEOM=3.
!
         IF ( pgeom==3 ) CALL algap(algdd,scr1)
         RETURN
      CASE (8)
         DO icopy = 1 , nrec
            CALL read(*460,*450,algdb,idata,99,1,nwar)
 450        CALL write(scr1,idata,nwar,1)
            IF ( debug ) CALL bug1('ALGPR   ',1302,idata,nwar)
         ENDDO
         IF ( nrec>=65000 ) THEN
            WRITE (nout,99004)
99004       FORMAT (/,' *** NO. OF RECORDS EXCEEDS HARDWARE LIMIT/ALGPR')
            CALL mesage(-37,0,0)
         ENDIF
 460     GOTO ret2
!
 480     CALL mesage(-2,file,name)
         Ierr = -1
         RETURN
 500     WRITE (nout,99005) ufm
99005    FORMAT (A23,' - ALG MODULE - EDT DATA BLOCK MAY NOT BE PURGED.')
         Ierr = -1
         RETURN
 520     WRITE (nout,99006) ufm
99006    FORMAT (A23,' - ALG MODULE - STREAML1 BULK DATA CARD MISSING ','FROM BULK DATA DECK.')
         Ierr = -1
         RETURN
 540     WRITE (nout,99007) ufm
99007    FORMAT (A23,' - ALG MODULE - ALGDB DATA BLOCK (FILE 105) DOES ','NOT HAVE ENOUGH RECORDS.')
         Ierr = -1
         RETURN
 560     CALL mesage(-3,file,name)
         Ierr = -1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99008 FORMAT (A23,' - ALG MODULE - INPUT IN ALGDB DATA BLOCK (FILE 105',') INCONSISTENT WITH DATA ON STREAML1 BULK DATA CARDS.',    &
            & /39X,'CHECK THE NUMBER OF COMPUTING STATIONS AND THE ','NUMBER OF STREAMSURFACES ON THE BLADE.')
END SUBROUTINE algpr
