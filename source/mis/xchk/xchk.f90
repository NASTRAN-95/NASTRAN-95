!*==xchk.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xchk
!
!     THE PURPOSE OF THIS ROUTINE IS TO SAVE ON THE NEW PROBLEM NPTP
!     TAPE ALL FILES REQUESTED BY XCHK OSCAR ENTRY TOGETHER WITH ANY
!     OTHER DATA NECESSARY FOR RESTART.
!
!          ... DEFINITION OF PROGRAM VARIABLES ...
!     NPTPNT = POINTER TO GINO BUFFER FOR NEW PROBLEM NPTP TAPE
!     DPPNT  = POINTER TO GINO BUFFER FOR DATA POOL TAPE
!     FPNT   = POINTER TO GINO BUFFER FOR FILES LISTED IN FIAT TABLE
!     IOBUF  = INPUT/OUTPUT BUFFER AREA
!     IOPNT  = POINTER TO IOBUF
!     LIOBUF = LENGTH OF IOBUF
!     DICT   = PRELIMINARY FILE DICTIONARY
!     FDICT  = FINAL FILE DICTIONARY TO BE WRITTEN ON NEW PROBLEM TAPE
!     LDC    = POINTER TO LAST DICT ENTRY MADE.
!     DCPNT  = POINTER TO DICT ENTRY BEING SCANNED.
!     NPTFN  = NEW PROBLEM TAPE (NPTP) FILE NUMBER TO BE ASSIGNED
!     UCBPNT = UCB POINTER FOUND IN FIAT ENTRIES
!     MINFN  = SMALLEST DATA POOL FILE NUMBER
!     DPFCT  = DATA POOL FILE POSITION
!     OSCFN  = DATA POOL FILE NUMBER OF OSCAR FILE
!     EORFLG = END OF RECORD FLAG
!     PURGE  = TABLE OF PURGED CHECKPOINT FILES
!     LPURGE = LENGTH OF PURGE TABLE
!     PRGPNT = POINTER TO LAST PURGE ENTRY
!     REELCT = KEEPS TRACK OF HOW MANY PROBLEM TAPE REELS A FILE IS
!              USING
!     EQFLG  = EQUIVALENCE FLAG
!     DPLFLG = DATA POOL FLAG
!     EOTFLG = END OF TAPE FLAG
!     SETEOR = END OF RECORD FLAG SET
!     FNASS  = NPTP FILE NUMBER ASSIGNED FLAG
!     MASKHI = MASK FOR ALL BITS EXCEPT LOWEST ORDER 16 BITS OF A WORD.
!     NOFLGS = MASK FOR ALL FLAG BITS
!     ALLON  = ALL BITS ON
!     PTDIC  = ARRAY CONTAINING CHECKPOINT DICTIONARY
!     SEQNO  = SEQUENCE NO. OF LAST PTDIC ENTRY THAT WAS PUNCHED OUT.
!     NRLFL  = NEXT REEL/FILE NO. TO BE USED IN PTDIC
!     PTDTOP = POINTER TO FIRST WORD OF FIRST ENTRY IN PTDIC
!     PTDBOT = POINTER TO FIRST WORD OF LAST  ENTRY IN PTDIC
!     LCPTP  = POINTER TO FIRST WORD OF FIRST ENTRY OF NEW GROUP OF
!              ENTRIES TO BE PUT IN PTDIC.
!     LPTDIC = LENGTH (IN WORDS) OF PTDIC
!
   IMPLICIT NONE
   USE c_machin
   USE c_oscent
   USE c_output
   USE c_resdic
   USE c_stapid
   USE c_system
   USE c_xceitb
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xpfist
   USE c_xvps
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: allon , blksiz , bufsz , cppgct , diag09 , dpfct , dplflg , dppnt , eorflg , eotflg , eqflg , filcnt , fnass , fpnt , &
            & i , i1 , i2 , icfiat , ifstmp , ii , iopnt , j , j1 , j2 , k , lcptp , ldc , liobuf , lptdic , maskhi , minfn , n ,   &
            & n1 , name , nbpw , nfile , nflags , ngino , nlines , nlpp , noflgs , nosgn , npages , nptfn , nptpnt , nreel , nrlfl ,&
            & oscfn , otpe , prgpnt , ptdbot , ptdtop , recsz , reelct , return , seqno , seteor , ucbpnt
   INTEGER , DIMENSION(90) , SAVE :: blkcnt
   INTEGER , DIMENSION(2) , SAVE :: dcparm , nvps , nxchk , nxptdc
   INTEGER , DIMENSION(400) :: dict , fdict
   INTEGER , SAVE :: dpt , limit , lpurge , nblank , noscar , nptp
   INTEGER , DIMENSION(32) , SAVE :: hdg
   INTEGER , DIMENSION(2) :: head , svfst
   INTEGER , DIMENSION(1) :: iobuf , ptdic
   INTEGER , DIMENSION(100) :: purge
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!WKBR COMMON /XCEITB/ CEITBL(2)
!WKBI
!      INCLUDE 'NASNAMES.COM'
   !>>>>EQUIVALENCE (Zsys(1),Bufsz) , (Zsys(2),Otpe) , (Zsys(9),Nlpp) , (Zsys(11),Npages) , (Zsys(12),Nlines) , (Zsys(24),Icfiat) ,      &
!>>>>    & (Zsys(26),Cppgct) , (Zsys(40),Nbpw)
   !>>>>EQUIVALENCE (dcparm(1),nrlfl) , (dcparm(2),seqno) , (Gbuf(1),Iobuf(1),Ptdic(1))
   DATA nptp/4HNPTP/
   DATA dpt/4HPOOL/
   DATA nblank/4H    /
   DATA noscar/4HXOSC/
   DATA nxchk/4HXCHK , 4H    /
   DATA nvps/4HXVPS , 4H    /
   DATA nxptdc/4HXPTD , 4HIC  / , dcparm/4H(NON , 4HE)  /
   DATA hdg/4H     , 4HADDI , 4HTION , 4HS TO , 4H CHE , 4HCKPO , 4HINT  , 4HDICT , 4HIONA , 4HRY   , 22*4H    /
   DATA blkcnt/90*0/ , limit/90/
   DATA lpurge/100/
!
!     INITIALIZE
!
   filcnt = 0
   reelct = 0
   ldc = -2
   recsz = 0
   prgpnt = -1
   CALL sswtch(9,diag09)
   IF ( mach<5 ) CALL xflszd(0,blksiz,0)
!
!     MASKHI - O000000077777
   maskhi = 32767
!
!     DPLFLG - O004000000000
   dplflg = lshift(1,29)
!
!     SETEOR - O004000000000
   seteor = dplflg
!
!     FNASS  - O010000000000
   fnass = lshift(1,30)
!
!     EOTFLG - O010000000000
   eotflg = fnass
!
!     ALLON  - O777777777777
   allon = complf(0)
!
!     NOSGN  - O377777777777
   nosgn = rshift(allon,1)
!
!     EQFLG  - O400000000000
   eqflg = complf(nosgn)
!
!     NOFLGS - O003777777777
   noflgs = rshift(allon,nbpw-29)
!
!
!     FIND OSCAR FILE NUMBER IN DPL
!
   j1 = dpl(3)*3 + 1
   DO j = 4 , j1 , 3
      IF ( dpl(j)==noscar ) EXIT
   ENDDO
   oscfn = andf(dpl(j+2),maskhi)
   dpfct = oscfn
!
!     ALLOCATE CORE FOR GINO BUFFERS
!
   nptpnt = korsz(gbuf) - bufsz - 1
   dppnt = nptpnt - bufsz
   fpnt = dppnt - bufsz
   IF ( fpnt<1 ) CALL mesage(-8,0,nxchk)
!
!     INITIALIZE PTDIC PARAMETERS AND LOAD CHECKPOINT DICTIONARY
!
   ngino = nxptdc(1)
   CALL open(*1200,nxptdc,gbuf(nptpnt),0)
   CALL read(*2400,*100,nxptdc,dcparm,2,1,recsz)
 100  IF ( dcparm(1)/=nxptdc(1) ) GOTO 2400
   CALL read(*2400,*200,nxptdc,dcparm,2,1,recsz)
 200  ptdtop = 1
   lptdic = nptpnt - ptdtop
   CALL read(*2400,*300,nxptdc,ptdic(ptdtop),lptdic,1,recsz)
   GOTO 2000
 300  ptdbot = recsz + ptdtop - 3
   iopnt = ptdbot + 6
   liobuf = fpnt - iopnt
   IF ( liobuf<1 ) CALL mesage(-8,0,nxchk)
   CALL close(nxptdc,1)
   lcptp = ptdbot + 3
!
!     SAVE CHECKPOINT DMAP SEQ. NO. AND RECORD NO.
!
   ptdic(lcptp) = nblank
   ptdic(lcptp+1) = nblank
   ptdic(lcptp+2) = orf(oscar(2),lshift(andf(maskhi,oscar(6))+1,16))
   nptfn = nrlfl
!
!     GET FIRST/NEXT FILE NAME FROM OSCAR ENTRY
!
   i1 = oscar(7)*2 + 6
   DO i = 8 , i1 , 2
!
!     SEE IF FILE IS ALREADY IN DICT
!
      IF ( oscar(i)==nvps(1) .AND. oscar(i+1)==nvps(2) ) CYCLE
      IF ( ldc>=0 ) THEN
         DO j = 1 , ldc , 3
            IF ( dict(j)==oscar(i) .AND. dict(j+1)==oscar(i+1) ) GOTO 500
         ENDDO
      ENDIF
!
!     CHECK FIAT TABLE FOR FILE NAME
!
      j1 = fiat(3)*icfiat - 2
      DO j = 4 , j1 , icfiat
         IF ( oscar(i)==fiat(j+1) .AND. oscar(i+1)==fiat(j+2) ) GOTO 350
      ENDDO
!
!     SEE IF FILE IS IN DPL
!
      j1 = dpl(3)*3 + 1
      DO j = 4 , j1 , 3
         IF ( oscar(i)==dpl(j) .AND. oscar(i+1)==dpl(j+1) ) GOTO 450
      ENDDO
      GOTO 400
!
!     FILE IS IN FIAT - ENTER FILE AND ALL EQUIVALENCED FILES IN DICT
!
 350  IF ( andf(fiat(j),maskhi)/=maskhi ) THEN
!
!     FILE NOT PURGED - CHECK FIAT TRAILER WORDS TO INSURE THAT FILE HAS
!     BEEN GENERATED
!
         IF ( fiat(j+3)==0 .AND. fiat(j+4)==0 .AND. fiat(j+5)==0 ) THEN
            IF ( .NOT.(icfiat==11 .AND. (fiat(j+8)/=0 .OR. fiat(j+9)/=0 .OR. fiat(j+10)/=0)) ) GOTO 400
         ENDIF
         IF ( fiat(j)<0 ) THEN
            k = andf(fiat(j),orf(maskhi,eqflg))
            DO j = 4 , j1 , icfiat
               IF ( andf(fiat(j),orf(maskhi,eqflg))==k ) THEN
                  ldc = ldc + 3
!
!     EQUIVALENCED FILE FOUND
!
                  dict(ldc) = fiat(j+1)
                  dict(ldc+1) = fiat(j+2)
!
!     ENTER EQUIVALENCE FLAG, FIAT POINTER AND UCB POINTER IN DICT
!
                  dict(ldc+2) = orf(lshift(j,16),k)
               ENDIF
            ENDDO
         ELSE
            ldc = ldc + 3
            dict(ldc) = fiat(j+1)
            dict(ldc+1) = fiat(j+2)
            dict(ldc+2) = orf(lshift(j,16),andf(fiat(j),maskhi))
!
!     DESTROY ANY EQUIVS TO THIS FILE
!
!     FIND LAST DICTIONARY REFERENCE TO THIS DATA BLOCK NAME
!
            DO j = ptdtop , ptdbot , 3
               k = ptdbot - (j-ptdtop)
               IF ( dict(ldc)==ptdic(k) .AND. ptdic(k+1)==dict(ldc+1) ) GOTO 360
            ENDDO
         ENDIF
         CYCLE
!
!     FILE EXISTS IN DICTIONARY SEE IF IT IS EQUIVED
!
 360     IF ( andf(ptdic(k+2),eqflg)/=0 ) THEN
!
!     FILE IS EQUIVED.  PURGE ALL SUBSEQUENT ENTRIES FOR THIS FILE
!
            IF ( k/=ptdbot ) THEN
               DO j = k , ptdbot , 3
                  IF ( ptdic(j+2)==ptdic(k+2) ) THEN
!
!     PURGE FILE
!
                     prgpnt = prgpnt + 2
                     IF ( lpurge<prgpnt+1 ) GOTO 2200
                     purge(prgpnt) = ptdic(j)
                     purge(prgpnt+1) = ptdic(j+1)
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         CYCLE
      ENDIF
!
!     ENTER PURGED FILE IN PURGE TABLE
!
 400  prgpnt = prgpnt + 2
      IF ( lpurge<prgpnt+1 ) GOTO 2200
      purge(prgpnt) = oscar(i)
      purge(prgpnt+1) = oscar(i+1)
      CYCLE
!
!     FILE IS IN DPL - ENTER FILE AND ALL EQUIVALENCED FILES IN DICT
!
 450  k = andf(dpl(j+2),maskhi)
      dpfct = min0(oscfn,k)
      DO j = 4 , j1 , 3
         IF ( andf(dpl(j+2),maskhi)==k ) THEN
            ldc = ldc + 3
!
!     EQUIVALENCED FILE FOUND
!
            dict(ldc) = dpl(j)
            dict(ldc+1) = dpl(j+1)
!
!     ENTER EQUIVALENCE FLAG, DPLFLG AND FILE NO. IN DICT
!
            dict(ldc+2) = orf(dplflg,andf(dpl(j+2),orf(maskhi,eqflg)))
         ENDIF
      ENDDO
 500  ENDDO
!
!     MOVE DICT ENTRIES TO FDICT TABLE
!     GET FIRST NEXT/ENTRY IN DICT
!
   IF ( ldc>=1 ) THEN
      DO i = 1 , ldc , 3
!
!     IF DICT ENTRY IS EQUIVALENCED - SEE IF IT IS IN PTDIC
!
         IF ( andf(dict(i+2),fnass)==fnass ) CYCLE
         IF ( dict(i+2)<=0 ) THEN
!
!     SEARCH BACKWARD FOR PREVIOUS ENTRY
!
            DO j = ptdtop , ptdbot , 3
               k = ptdbot - (j-ptdtop)
               IF ( ptdic(k)==dict(i) .AND. ptdic(k+1)==dict(i+1) .AND. ptdic(k+2)/=0 ) GOTO 520
            ENDDO
         ENDIF
         GOTO 540
!
!     DICT ENTRY IS IN PTDIC
!
 520     fdict(i) = dict(i)
         fdict(i+1) = dict(i+1)
         fdict(i+2) = orf(ptdic(k+2),eqflg)
         ucbpnt = dict(i+2)
         dict(i+2) = fnass
!
!     ENTER PTDIC FILE NUMBER IN FDICT ENTRIES THAT ARE EQUIVALENCED TO
!     PTDIC ENTRY
!
         ucbpnt = andf(ucbpnt,orf(maskhi,dplflg))
         DO j = 1 , ldc , 3
            IF ( andf(dict(j+2),orf(maskhi,dplflg))==ucbpnt ) THEN
               fdict(j) = dict(j)
               fdict(j+1) = dict(j+1)
               fdict(j+2) = orf(eqflg,ptdic(k+2))
               dict(j+2) = fnass
            ENDIF
         ENDDO
!
!     MOVE DICT ENTRY TO FDICT IF NOT ALREADY MOVED
!
 540     IF ( andf(dict(i+2),fnass)/=fnass ) THEN
            fdict(i) = dict(i)
            fdict(i+1) = dict(i+1)
            fdict(i+2) = dict(i+2)
            IF ( andf(dict(i+2),dplflg)/=dplflg ) THEN
!
!     DICT ENTRY IS FIAT FILE - ENTER NPTP FILE NO. IN FDICT
!
               fdict(i+2) = orf(andf(fdict(i+2),eqflg),nptfn)
               dict(i+2) = orf(dict(i+2),fnass)
               IF ( dict(i+2)<=0 ) THEN
!
!     FILE IS EQUIVALENCED - ENTER NPTP FILE NO. IN FDICT FOR FILES THAT
!     THIS ENTRY IS EQUIVALENCED TO.
!
                  ucbpnt = andf(dict(i+2),maskhi)
                  j1 = i + 3
                  IF ( j1<=ldc ) THEN
                     DO j = j1 , ldc , 3
                        IF ( andf(dict(j+2),maskhi)==ucbpnt ) THEN
                           fdict(j) = dict(j)
                           fdict(j+1) = dict(j+1)
                           fdict(j+2) = fdict(i+2)
                           dict(j+2) = orf(dict(j+2),fnass)
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
               nptfn = 1 + nptfn
            ENDIF
         ENDIF
      ENDDO
      DO
!
!     NOW ASSIGN NPTP FILE NUMBERS TO DATA POOL FILES IN SAME ORDER THAT
!     FILES APPEAR ON DATA POOL TAPE.
!
         minfn = rshift(allon,1)
!
!     GET FIRST/NEXT DICT ENTRY
!
         DO i = 1 , ldc , 3
            IF ( andf(dict(i+2),fnass)/=fnass ) minfn = min0(minfn,andf(dict(i+2),maskhi))
         ENDDO
         IF ( minfn==rshift(allon,1) ) EXIT
         DO i = 1 , ldc , 3
            IF ( andf(dict(i+2),fnass)/=fnass ) THEN
               IF ( andf(dict(i+2),maskhi)==minfn ) THEN
                  fdict(i+2) = orf(nptfn,andf(fdict(i+2),eqflg))
                  dict(i+2) = orf(dict(i+2),fnass)
               ENDIF
            ENDIF
         ENDDO
         nptfn = nptfn + 1
      ENDDO
   ENDIF
!
!     OPEN DATA POOL TAPE SO IT IS POSITIONED BEFORE FIRST FILE TO
!     CHECKPOINT.
!
   IF ( dpfct<oscfn ) THEN
      j = 0
      dpfct = 1
   ELSE
      j = 2
      dpfct = oscfn
   ENDIF
   name = dpt
   CALL open(*1200,dpt,gbuf(dppnt),j)
   name = nptp
!
!     OPEN NEW PROBELM NPTP TAPE FOR WRITE
!
   CALL open(*1200,nptp,gbuf(nptpnt),3)
!
!     MAKE TEMPORARY ENTRY IN FIST FOR FIAT FILES
!
   ifstmp = 2*ipfst + 3
   svfst(1) = fist(ifstmp)
   svfst(2) = fist(ifstmp+1)
   fist(2) = ipfst + 1
   fist(ifstmp) = 301
!
!     WRITE FILES ON NEW PROBLEM NPTP TAPE AS SPECIFIED IN FDICT.
!
   n1 = nptfn - 1
!
!     GET FIRST/NEXT FDICT ENTRY
!
   n = nrlfl
   IF ( ldc<1 .OR. n1<n ) GOTO 800
 600  DO i = 1 , ldc , 3
      IF ( andf(fdict(i+2),noflgs)==n ) GOTO 700
   ENDDO
!
!     FDICT ENTRIES SHOULD ALL BE COPIED - MAKE SURE ALL IS O.K.
!
   DO i = 1 , ldc , 3
      IF ( andf(fdict(i+2),noflgs)>n ) GOTO 1600
   ENDDO
   nptfn = n
   GOTO 800
!
!     THIS FDICT ENTRY IS NEXT TO GO ON NEW PROBLEM NPTP TAPE.
!
 700  IF ( andf(dict(i+2),dplflg)==dplflg ) THEN
!
!     FILE IS ON POOL -- POSITION POOL AND COPY FILE USING CPYFIL
!
      ngino = dpt
      k = andf(dict(i+2),maskhi)
      CALL skpfil(dpt,k-dpfct)
      dpfct = k + 1
      filcnt = filcnt + 1
      IF ( filcnt>limit ) GOTO 2900
      CALL xflszd(k,blkcnt(filcnt),0)
      CALL cpyfil(dpt,nptp,iobuf(iopnt),liobuf,recsz)
   ELSE
!
!     FILE IS IN FIAT TABLE
!
      k = rshift(andf(noflgs,dict(i+2)),16)
      IF ( dict(i+2)<=0 ) THEN
!
!     GET SMALLEST FIAT POINTER FOR EQUIVALENCED FIAT FILES
!
         DO ii = 1 , ldc , 3
            IF ( andf(dict(ii+2),dplflg)/=dplflg ) THEN
               IF ( andf(dict(i+2),maskhi)==andf(dict(ii+2),maskhi) ) k = min0(rshift(andf(noflgs,dict(ii+2)),16),k)
            ENDIF
         ENDDO
      ENDIF
!
!     INSERT FIAT POINTER IN TEMPORARY FIST ENTRY
!
      fist(ifstmp+1) = k - 1
!
!     READ FIRST 2 WORDS OF DATA BLOCK, CHECK NAME AND WRITE TO NEW
!     PROBLEM NPTP TAPE SPECIAL HEADER AND 3 OR 6 TRAILER WORDS
!     (TOTAL OF 5 OR 8 WORDS IN THIS NPTP RECORD)
!
      ngino = fist(ifstmp)
      CALL open(*1000,ngino,gbuf(fpnt),0)
      filcnt = filcnt + 1
      IF ( filcnt>limit ) GOTO 2900
      CALL xflszd(-1,blkcnt(filcnt),ngino)
      CALL read(*1800,*1800,ngino,head,2,0,recsz)
      DO j = i , ldc , 3
         IF ( head(1)==fdict(j) .AND. head(2)==fdict(j+1) .AND. fdict(j+2)==fdict(i+2) ) GOTO 750
      ENDDO
      GOTO 1800
 750  CALL write(nptp,head,2,0)
      IF ( icfiat==11 ) THEN
         CALL write(nptp,fiat(k+3),3,0)
         CALL write(nptp,fiat(k+8),3,1)
      ELSE
         CALL write(nptp,fiat(k+3),3,1)
      ENDIF
!
!     COPY ENTIRE FILE ONTO NEW PROBLEM NPTP TAPE USING CPYFIL
!
      CALL write(nptp,head,2,0)
      CALL cpyfil(ngino,nptp,iobuf(iopnt),liobuf,recsz)
      CALL close(ngino,1)
   ENDIF
!
!     GET NEXT FDICT ENTRY
!
   CALL eof(nptp)
   n = n + 1
   IF ( n<=n1 ) GOTO 600
!
!     RESTORE FIST ENTRY
!
   fist(ifstmp) = svfst(1)
   fist(ifstmp+1) = svfst(2)
!
!     WRITE VPS TABLE ONTO NEW PROBLEM NPTP TAPE
!     MAKE ENTRY IN FDICT FOR VPS TABLE
!
 800  ldc = ldc + 3
   fdict(ldc) = nvps(1)
   fdict(ldc+1) = nblank
   fdict(ldc+2) = nptfn
   eorflg = seteor
   i = ldc
   CALL write(nptp,nvps,5,1)
   CALL write(nptp,vps,vps(2),1)
!
!     WRITE CEITBL TABLE ONTO PROBLEM TAPE
!
   CALL write(nptp,ceitbl,ceitbl(2),1)
!
!     WRITE /SYSTEM/ ONTO PROBLEM TAPE
!
   CALL write(nptp,bufsz,20,1)
   CALL eof(nptp)
   CALL close(nptp,2)
!
!     POSITION DATA POOL TAPE AT CORRECT OSCAR ENTRY FOR RETURN TO XSEM
!
   IF ( dpfct/=oscfn ) THEN
      CALL rewind(dpt)
      IF ( oscfn>1 ) CALL skpfil(dpt,oscfn-1)
      j1 = oscar(2)
      DO j = 1 , j1
         CALL fwdrec(*1400,dpt)
      ENDDO
   ENDIF
   CALL close(dpt,2)
!
!     UPDATE PTDIC AND ASSOCIATED VARIABLES
!
   nrlfl = nptfn + 1
   ptdbot = lcptp
   DO i = 1 , ldc , 3
      DO j = ptdtop , ptdbot , 3
!
!     SCAN PTDIC TO SEE IF FILE IS ALREADY THERE
!
         IF ( fdict(i)==ptdic(j) .AND. fdict(i+1)==ptdic(j+1) .AND. fdict(i+2)==ptdic(j+2) ) GOTO 900
      ENDDO
!
!     ENTER FILE IN PTDIC
!
      ptdbot = ptdbot + 3
      ptdic(ptdbot) = fdict(i)
      ptdic(ptdbot+1) = fdict(i+1)
      ptdic(ptdbot+2) = fdict(i+2)
 900  ENDDO
!
!     PUT PURGED FILES IN PTDIC
!
   IF ( prgpnt>=1 ) THEN
      DO i = 1 , prgpnt , 2
         DO j = ptdtop , ptdbot , 3
            IF ( purge(i)==ptdic(j) .AND. purge(i+1)==ptdic(j+1) .AND. ptdic(j+2)==0 ) GOTO 950
         ENDDO
         ptdbot = ptdbot + 3
         ptdic(ptdbot) = purge(i)
         ptdic(ptdbot+1) = purge(i+1)
         ptdic(ptdbot+2) = 0
 950  ENDDO
   ENDIF
!
!     CHECK FOR PTDIC OVERFLOW
!
   IF ( ptdbot+3-ptdtop>lptdic ) GOTO 2000
!
!
!     PUNCH AND PRINT LATEST ENTRIES IN PTDIC
!     INITIALIZE PAGE HEADING AND CHECK PAGE COUNT
!
   IF ( diag09/=1 ) THEN
      DO i = 1 , 32
         pghdg(i+96) = hdg(i)
         pghdg(i+128) = nblank
         pghdg(i+160) = nblank
      ENDDO
      IF ( cppgct/=npages ) CALL page
   ENDIF
   i1 = ((lcptp-ptdtop)/3) + 1
   i2 = ((ptdbot-ptdtop)/3) + 1
   DO i = i1 , i2
      j1 = (i-1)*3 + ptdtop
      j2 = j1 + 2
!
!     SEPARATE FLAGS, REEL NO., FILE NO.
!
      nflags = 0
      IF ( ptdic(j2)<0 ) nflags = 4
      nflags = orf(nflags,rshift(andf(ptdic(j2),nosgn),29))
      nreel = rshift(andf(ptdic(j2),noflgs),16)
      nfile = andf(ptdic(j2),maskhi)
      seqno = 1 + seqno
      IF ( ptdic(j1)==nblank ) THEN
!      IF (IROPEN .EQ. 1) GO TO 8055
!      OPEN (UNIT=4, FILE=DIC, STATUS='UNKNOWN')
!      IROPEN = 1
!8055  CONTINUE
         WRITE (irdict,99001) seqno , nreel
99001    FORMAT (I10,36H,   REENTER AT DMAP SEQUENCE NUMBER ,I5)
         IF ( diag09/=1 ) THEN
            nlines = nlines + 2
            IF ( nlines>=nlpp ) CALL page
            WRITE (otpe,99002) seqno , nreel
99002       FORMAT (1H ,/1H ,I9,36H,   REENTER AT DMAP SEQUENCE NUMBER ,I5)
         ENDIF
      ELSE
!      IF (IROPEN .EQ. 1) GO TO 815
!      OPEN (UNIT=4, FILE=DIC, STATUS='UNKNOWN')
!      IROPEN = 1
         WRITE (irdict,99003) seqno , ptdic(j1) , ptdic(j1+1) , nflags , nreel , nfile
99003    FORMAT (I10,4H,   ,2A4,12H,   FLAGS = ,I1,11H,   REEL = ,I2,11H,   FILE = ,I6)
         IF ( diag09/=1 ) THEN
            nlines = nlines + 1
            IF ( mach<5 .AND. nfile/=0 .AND. ptdic(j1)/=nvps(1) ) nlines = nlines + 1
            IF ( nlines>=nlpp ) CALL page
            WRITE (otpe,99004) seqno , ptdic(j1) , ptdic(j1+1) , nflags , nreel , nfile
99004       FORMAT (1H ,I9,4H,   ,2A4,12H,   FLAGS = ,I1,11H,   REEL = ,I2,11H,   FILE = ,I6)
            IF ( mach<5 .AND. nfile/=0 .AND. ptdic(j1)/=nvps(1) ) WRITE (otpe,99005) ptdic(j1) , ptdic(j1+1) , blkcnt(i-i1) , blksiz
99005       FORMAT (13X,6H FILE ,2A4,9H CONTAINS,I10,28H BLOCKS, EACH BLOCK CONTAINS,I5,7H WORDS.)
         ENDIF
      ENDIF
   ENDDO
!
!     WRITE PTDIC ONTO XPTD
!
   ngino = nxptdc(1)
   CALL open(*1200,nxptdc,gbuf(nptpnt),1)
   CALL write(nxptdc,nxptdc,2,1)
   CALL write(nxptdc,dcparm,2,1)
   CALL write(nxptdc,ptdic(ptdtop),ptdbot+3-ptdtop,1)
   CALL close(nxptdc,1)
   cppgct = npages
!
   fist(2) = ipfst
   RETURN
!
!
!     ERRORS -
!
 1000 n = 1101
   ASSIGN 1100 TO return
   GOTO 2600
 1100 WRITE (otpe,99013) fdict(i) , fdict(i+1)
   GOTO 3000
!
 1200 n = 1102
   ASSIGN 1300 TO return
   GOTO 2700
 1300 WRITE (otpe,99013) ngino , nblank
   GOTO 3000
!
 1400 n = 1103
   ASSIGN 1500 TO return
   GOTO 2700
 1500 WRITE (otpe,99006)
99006 FORMAT (4X,43HUNABLE TO POSITION DATA POOL TAPE CORRECTLY)
   GOTO 3000
!
 1600 n = 1104
   ASSIGN 1700 TO return
   GOTO 2700
 1700 WRITE (otpe,99007)
99007 FORMAT (4X,24HFDICT TABLE IS INCORRECT)
   GOTO 3000
!
 1800 n = 1105
   ASSIGN 1900 TO return
   GOTO 2600
 1900 WRITE (otpe,99014) fdict(i) , fdict(i+1) , head(1) , head(2)
   GOTO 3000
!
 2000 n = 1106
   ASSIGN 2100 TO return
   GOTO 2700
 2100 WRITE (otpe,99008)
99008 FORMAT (4X,32HCHECKPOINT DICTIONARY OVERFLOWED)
   GOTO 3000
!
 2200 n = 1108
   ASSIGN 2300 TO return
   GOTO 2700
 2300 WRITE (otpe,99009)
99009 FORMAT (4X,22HPURGE TABLE OVERFLOWED)
   GOTO 3000
!
 2400 n = 1109
   ASSIGN 2500 TO return
   GOTO 2700
 2500 WRITE (otpe,99014) nxptdc , dcparm
   GOTO 3000
!
!     USER FATAL ERROR
!
 2600 WRITE (otpe,99010) ufm , n
99010 FORMAT (A23,I5)
   GOTO 2800
!
!     SYSTEM FATAL ERROR
!
 2700 CALL page2(3)
   WRITE (otpe,99011) sfm , n
99011 FORMAT (A25,I5)
 2800 GOTO return
!
 2900 WRITE (otpe,99012) sfm
99012 FORMAT (A25,', BLKCNT ARRAY EXCEEDED IN XCHK')
!
 3000 CALL mesage(-37,0,nxchk)
99013 FORMAT (4X,26HCOULD NOT OPEN FILE NAMED ,2A4)
99014 FORMAT (4X,29HCANNOT FIND DATA BLOCK NAMED ,2A4,17H HEADER RECORD = ,2A4)
END SUBROUTINE xchk
