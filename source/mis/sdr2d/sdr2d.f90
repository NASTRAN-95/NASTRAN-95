!*==sdr2d.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2d
   USE c_blank
   USE c_names
   USE c_sdr2c1
   USE c_sdr2de
   USE c_sdr2x1
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: axcosi , axsine
   INTEGER , DIMENSION(50) , SAVE :: buf
   INTEGER :: buf0 , bufm1 , idx , ieqex , iflag , ilayer , index , iskip , isvsrc , isvvcn , isvvec , ixstnf , ixstns , limit ,    &
            & ncc , neqex
   REAL , DIMENSION(2) :: bufr
   REAL :: coef1
   INTEGER , DIMENSION(7) :: itr
   INTEGER , DIMENSION(2) , SAVE :: kdefrm , nmef1l , nmes1l , pcomp , pcomp1 , pcomp2
   INTEGER , SAVE :: xset0
   REAL , DIMENSION(1) :: zz
   EXTERNAL bckrec , close , fname , fread , fwdrec , intpk , korsz , locate , mesage , open , preloc , rdtrl , read , rewind ,     &
          & sdr2e , skprec , write , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SDR2D PERFORMS THE FINAL STRESS AND FORCE RECOVERY COMPUTATIONS.
!     CASE CONTROL AND THE DISPLACEMENT VECTOR FILE ARE PROCESSED IN
!     PARALLEL.  THE ESTA IS PASSED ONCE FOR EACH VECTOR IN UGV FOR
!     WHICH A STRESS OR FORCE OUTPUT REQUEST EXISTS.  THE ESTA IS HELD
!     COMPLETELY IN CORE IF POSSIBLE.  STRESS OUTPUT IS WRITTEN ON OES1.
!     FORCE OUTPUT IS WRITTEN ON OEF1.
!
!    1,               IDSTRS   ,IDFORC   ,ILOGIC(2)
   !>>>>EQUIVALENCE (buf(1),bufr(1)) , (Z(1),Zz(1))
!    1,               (IDSTRS,ILOGIC(1)) ,(IDFORC,ILOGIC(2))
   DATA buf/50*0/ , kdefrm/104 , 1/ , xset0/100000000/
   DATA nmes1l/4HOES1 , 4HL   / , nmef1l/4HOEF1 , 4HL   /
   DATA pcomp/5502 , 55/ , pcomp1/5602 , 56/ , pcomp2/5702 , 57/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERAL INITIALIZATION
!
         bufm1 = korsz(z) - sysbuf + 1
         buf0 = bufm1 - sysbuf - 1
         buf1 = buf0 - sysbuf - 1
         IF ( comps/=-1 ) buf1 = bufm1
         buf2 = buf1 - sysbuf - 1
         i2 = 1
         incr2 = 1
         icc = 0
         ilist = 1
         nlist = 0
         jlist = 1
         kfrq = 0
         axsine = .FALSE.
         axcosi = .FALSE.
         sorc = 0
!
!     READ TRAILER ON INPUT FILE. SET PARAMETERS.
!
         icb(1) = ugv
         CALL rdtrl(icb)
         IF ( icb(1)/=ugv ) THEN
!
!     UGV FILE PURGED, CAN NOT PROCESS STRESSES OR FORCES
!
            CALL mesage(30,76,0)
            GOTO 380
         ELSE
            nvects = icb(2)
            IF ( icb(5)>2 ) THEN
!
!     COMPLEX VECTOR.
!
               ktype = 2
               qtype2 = 3
               ktype1 = 3
               nwds = 14
               ktypex = 1000
            ELSE
!
!     REAL VECTOR.
!
               ktype = 1
               qtype2 = 1
               ktype1 = 2
               nwds = 8
               ktypex = 0
            ENDIF
!
!     OPEN CASE CONTROL AND SKIP HEADER. THEN BRANCH ON APPROACH.
!
            file = casecc
            CALL open(*400,casecc,z(buf1),rdrew)
            CALL fwdrec(*420,casecc)
            eofcc = .FALSE.
!
            IF ( branch==1 .OR. branch==3 .OR. branch==7 .OR. branch==10 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( branch==4 .OR. branch==8 ) THEN
!
!     DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1 - SKIP 1ST DATA RECORD ON
!     CC.
!
               CALL fwdrec(*420,casecc)
               IF ( app(1)/=bk1(1) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( branch==5 .OR. branch==6 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
               file = pg
               CALL open(*400,file,z(buf2),rdrew)
               i = ilist
               m = 3
               ix = 1
               IF ( app(1)==frq(1) .OR. app(1)==trn(1) ) ix = 2
               DO
                  CALL read(*420,*60,file,buf(1),m,0,flag)
                  z(i) = buf(m)
                  z(i+1) = 0
                  i = i + ix
                  m = 1
               ENDDO
            ENDIF
!            STA,REI,DS0,DS1,FRQ,TRN,BK0,BK1,CEI,PLA
!
!     EIGENVALUES - READ LIST OF MODE NOS. AND EIGENVALUES INTO CORE.
!     BUCKLING POSSIBLE HERE TOO
!
            file = eigr
            CALL open(*400,eigr,z(buf2),rdrew)
            CALL fwdrec(*420,eigr)
            CALL fwdrec(*420,eigr)
            i = ilist
            m = 8 - ktype
            iskip = 0
            index = 2
            IF ( app(1)/=rei(1) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            SPAG_Loop_1_1: DO
!
!     CHECK TO SEE IF ALL GENERALIZED MASS VALUES ARE ZERO
!
               CALL read(*420,*20,eigr,buf,m,0,flag)
               IF ( buf(6)/=0.0 ) THEN
                  index = 0
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
 20      CALL skprec(eigr,-1)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_2: DO
            CALL read(*420,*40,eigr,buf(1),m,0,flag)
            IF ( app(1)/=rei(1) ) EXIT SPAG_Loop_1_2
            IF ( index==2 ) EXIT SPAG_Loop_1_2
!
!     MATCH CORRECT MODE NOS. AND EIGENVALUES WITH PROPER
!     FORCES AND STRESSES WHEN USING GIVENS METHOD WITH F1.GT.0.0
!
            IF ( index==1 ) EXIT SPAG_Loop_1_2
            IF ( buf(6)/=0.0 ) THEN
               index = 1
               EXIT SPAG_Loop_1_2
            ELSE
               iskip = iskip + 1
            ENDIF
         ENDDO SPAG_Loop_1_2
         z(i) = buf(1) - iskip
         z(i+1) = buf(3)
         z(i+2) = buf(4)
         i = i + ktype1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(eigr,clsrew)
         nlist = i - ktype1
         icc = i
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(file,clsrew)
         nlist = i - ix
         icc = i
         spag_nextblock_1 = 3
      CASE (3)
!
!     ALLOCATE CORE FOR CASE CONTROL, EDT, GPTT, ESTA, VECTOR
!     BALANCE OF REQUIRED BUFFERS
!       BUF1 = CASECC     BUF5 = GPTT
!       BUF2 = VECTOR     BUF6 = EDT
!       BUF3 = OES1       BUF7 = EQEXIN
!       BUF4 = OEF1       BUF8 = ESTA
!     SOME OF THE ABOVE MAY NOT BE REQUIRED AND THUS WILL NOT BE
!     ALLOCATED..
!
         buf3 = buf2 - sysbuf - 1
         IF ( stress==0 ) buf3 = buf2
         buf4 = buf3 - sysbuf - 1
         IF ( force==0 ) buf4 = buf3
         buf5 = buf4 - sysbuf - 1
         IF ( tloads==0 ) buf5 = buf4
         buf6 = buf5 - sysbuf - 3
         IF ( kwdedt==0 ) buf6 = buf5
         buf7 = buf6 - sysbuf - 1
         IF ( isopl==0 ) buf7 = buf6
         buf8 = buf7 - sysbuf - 1
!
!     IF COMPOSITE ELEMENTS ARE PRESENT, READ PCOMPS INTO CORE
!
         IF ( comps/=-1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = pcomps
         n = -1
         CALL preloc(*460,z(buf2),pcomps)
         ipcmp = icc + 1
         ipcmp1 = ipcmp
         ipcmp2 = ipcmp
         npcmp = 0
         npcmp1 = 0
         npcmp2 = 0
         n = -2
!
         CALL locate(*80,z(buf2),pcomp,idx)
         CALL read(*460,*80,pcomps,z(ipcmp),buf2-ipcmp,1,npcmp)
         CALL mesage(-8,0,nam)
 80      ipcmp1 = ipcmp1 + npcmp
         ipcmp2 = ipcmp1
!
         CALL locate(*100,z(buf2),pcomp1,idx)
         CALL read(*460,*100,pcomps,z(ipcmp1),buf2-ipcmp1,1,npcmp1)
         CALL mesage(-8,0,nam)
 100     ipcmp2 = ipcmp2 + npcmp1
!
         CALL locate(*120,z(buf2),pcomp2,idx)
         CALL read(*460,*120,pcomps,z(ipcmp2),buf2-ipcmp2,1,npcmp2)
         CALL mesage(-8,0,nam)
 120     icc = ipcmp2 + npcmp2 - 1
!
         CALL close(pcomps,clsrew)
         spag_nextblock_1 = 4
      CASE (4)
!
!     IF ESTA FITS IN CORE BUF8 MAY BE BUF7 SINCE IT WILL ONLY BE USED
!     TO READ ESTA IN ONCE..
!
         iedt = icc + kwdcc + 1
         igptta = iedt + kwdedt
         itr(1) = eqexin
         CALL rdtrl(itr)
         neqex = 2*itr(2)
         IF ( isopl8/=8 ) neqex = 0
         ieqex = igptta + kwdgpt
         ivec = ieqex + neqex
         ivecn = ivec + ktype*icb(3) - 1
!
!     IF CONICAL SHELL DOUBLE VECTOR SPACE
!
         IF ( axic .AND. ktype==1 ) ivecn = ivecn + icb(3)*ktype
         iesta = ivecn + 1
         midvec = (ivec+ivecn)/2 + 1
         IF ( axic .AND. ktype==1 ) midvec = 0
         IF ( axic .AND. ktype==1 ) ivecn = ivecn - icb(3)*ktype
         IF ( kwdest<=(buf7-iesta) ) buf8 = buf7
!
!     OPEN ESTA
!
         file = esta
         CALL open(*400,esta,z(buf8),rdrew)
!
!     REMAINING CORE
!
         icore = buf8 - iesta
         nesta = 0
!
!     WILL ESTA FIT IN CORE
!
         IF ( icore<=0 ) CALL mesage(-8,0,nam)
         IF ( kwdest>icore ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ESTA WILL FIT. READ IT IN PLACING A ZERO WORD AT END OF EACH
!     RECORD.
!
         i = iesta
         spag_nextblock_1 = 5
      CASE (5)
         CALL read(*160,*140,esta,z(i),icore,1,nwords)
         CALL rewind(esta)
         icore = buf8 - iesta
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 140     i = i + nwords + 1
         z(i-1) = 0
         icore = icore - nwords - 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     ALL ESTA NOW IN CORE
!
 160     nesta = i - 1
         CALL close(esta,clsrew)
         IF ( nesta<=iesta ) THEN
            WRITE (opte,99001) uwm
99001       FORMAT (A25,' 3303, STRESSES OR FORCES REQUESTED FOR SET(S) ','WHICH CONTAIN NO VALID ELEMENTS.')
            GOTO 380
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
         file = ugv
         CALL open(*400,ugv,z(buf2),rdrew)
         CALL fwdrec(*420,ugv)
!
!     IF ANY ISOPARAMETRIC ELEMENTS PRESENT, GET SECOND RECORD OF EQEXIN
!
         IF ( isopl/=0 ) THEN
            file = eqexin
            CALL open(*400,eqexin,z(buf7),rdrew)
            CALL fwdrec(*420,eqexin)
            CALL fwdrec(*420,eqexin)
            isopl = eqexin
            IF ( isopl8==8 ) THEN
               CALL fread(eqexin,z(ieqex),neqex,0)
               CALL bckrec(eqexin)
            ENDIF
         ENDIF
!
!     IF ANY STRESS OUTPUT IS REQUESTED,
!     OPEN OES1 AND WRITE HEADER RECORD
!
         IF ( stress/=0 ) THEN
            file = oes1
            CALL open(*180,oes1,z(buf3),wrtrew)
            CALL fname(oes1,ocb)
            DO i = 1 , 3
               ocb(i+2) = date(i)
            ENDDO
            ocb(6) = time
            ocb(7) = 1
            CALL write(oes1,ocb,7,1)
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 180     CALL mesage(1,oes1,nam)
         stress = 0
         spag_nextblock_1 = 7
      CASE (7)
!
!     IF ANY STRESS OR FORCE OUTPUT IS REQUESTED AND COMPOSITE ELEMENTS
!     ARE PRESENT, OPEN OES1L AND OEF1L AND WRITE HEADER RECORDS
!
         IF ( .NOT.(comps/=-1 .OR. (stress==0 .AND. force==0)) ) THEN
            ilayer = 0
            file = oes1l
            CALL open(*200,oes1l,z(bufm1),wrtrew)
            CALL write(oes1l,nmes1l,2,1)
            file = oef1l
            CALL open(*200,oef1l,z(buf0),wrtrew)
            CALL write(oef1l,nmef1l,2,1)
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 200     CALL mesage(1,file,nam)
         stress = 0
         force = 0
         spag_nextblock_1 = 8
      CASE (8)
!
!     IF ANY FORCE OUTPUT IS REQUESTED,
!     OPEN OEF1 AND WRITE HEADER RECORD
!
         IF ( force/=0 ) THEN
            file = oef1
            CALL open(*220,oef1,z(buf4),wrtrew)
            CALL fname(oef1,ocb)
            DO i = 1 , 3
               ocb(i+2) = date(i)
            ENDDO
            ocb(6) = time
            ocb(7) = 1
            CALL write(oef1,ocb,7,1)
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 220     CALL mesage(1,oef1,nam)
         force = 0
         spag_nextblock_1 = 9
      CASE (9)
         IF ( stress==0 .AND. force==0 ) GOTO 380
!
!     INITIALIZE UGV VEC, WHICH WILL BE THE NUMBER OF THE VECTOR WE
!     ARE NOW POSITIONED TO READ.
!
         ugvvec = 1
         isvvec = ivec
         isvvcn = ivecn
         iflag = 0
         spag_nextblock_1 = 10
      CASE (10)
!
!     READ A RECORD IN CASE CONTROL. SET SYMMETRY FLAG.
!
         CALL read(*360,*240,casecc,z(icc+1),kwdcc+1,1,flag)
         CALL mesage(8,0,nam)
         GOTO 380
 240     ix = icc + isymfl
         symflg = z(ix)
         ncc = icc + flag
!
!     FOR CONICAL SHELL SET SORC FLAG
!
         ix = icc + isorc
         IF ( iflag==1 ) sorc = isvsrc
         IF ( symflg==0 ) sorc = z(ix)
         IF ( sorc==1 ) axsine = .TRUE.
         IF ( sorc==2 ) axcosi = .TRUE.
         IF ( axic .AND. symflg==0 ) isvsrc = sorc
         ivec = isvvec
         ivecn = isvvcn
         iflag = 0
         IF ( axic .AND. axsine .AND. axcosi .AND. ugvvec==3 ) iflag = 1
         IF ( axic .AND. sorc==0 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT.
!     IF NOT, TEST FOR RECORD SKIP ON UGV THEN GO TO END OF THIS
!     REQUEST. IF SO, SET POINTERS TO SET DEFINING REQUEST.
!
         ix = icc + istr
         stresx = z(ix)
         sdest = z(ix+1)
         xsetns = -1
         ix = icc + ielf
         forcex = z(ix)
         fdest = z(ix+1)
         xsetnf = -1
         nstrop = z(icc+183)
!
!     DEBUG PRINTOUT
!
!
         IF ( comps==-1 .AND. nstrop>1 ) ilayer = ilayer + 1
         IF ( stresx>0 ) THEN
            ix = icc + ilsym
            isetno = ix + z(ix) + 1
            SPAG_Loop_1_3: DO
               isets = isetno + 2
               nsets = z(isetno+1) + isets - 1
               IF ( z(isetno)==stresx ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET FOR STRESSES
!
                  IF ( stresx<xset0 ) EXIT SPAG_Loop_1_3
                  xsetns = sdest/10
                  sdest = sdest - 10*xsetns
                  IF ( xsetns==0 ) EXIT SPAG_Loop_1_3
                  ixstns = ix + z(ix) + 1
                  DO
                     ixsets = ixstns + 2
                     nxsets = z(ixstns+1) + ixsets - 1
                     IF ( z(ixstns)==stresx ) EXIT SPAG_Loop_1_3
                     ixstns = nxsets + 1
                     IF ( ixstns>=ncc ) THEN
                        stresx = -1
                        EXIT SPAG_Loop_1_3
                     ENDIF
                  ENDDO
               ELSE
                  isetno = nsets + 1
                  IF ( isetno>ncc ) THEN
                     stresx = -1
                     EXIT SPAG_Loop_1_3
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
         IF ( forcex>0 ) THEN
            ix = icc + ilsym
            isetno = ix + z(ix) + 1
            SPAG_Loop_1_4: DO
               isetf = isetno + 2
               nsetf = z(isetno+1) + isetf - 1
               IF ( z(isetno)==forcex ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET FOR FORCES
!
                  IF ( forcex<xset0 ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  xsetnf = fdest/10
                  fdest = fdest - 10*xsetnf
                  IF ( xsetnf==0 ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ixstnf = ix + z(ix) + 1
                  DO
                     ixsetf = ixstnf + 2
                     nxsetf = z(ixstnf+1) + ixsetf - 1
                     IF ( z(ixstnf)==forcex ) THEN
                        spag_nextblock_1 = 12
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     ixstnf = nxsetf + 1
                     IF ( ixstnf>=ncc ) THEN
                        forcex = -1
                        EXIT SPAG_Loop_1_4
                     ENDIF
                  ENDDO
               ELSE
                  isetno = nsetf + 1
                  IF ( isetno>ncc ) THEN
                     forcex = -1
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_4
         ENDIF
         IF ( .NOT.(stresx/=0 .OR. forcex/=0 .OR. axic) ) THEN
!
!     NO REQUESTS THIS CC RECORD FOR STRESSES OR FORCES.
!     THUS SKIP CORRESPONDING UGV RECORD UNLESS SYMFLG IS ON, IN WHICH
!     CASE WE SKIP NO UGV RECORD SINCE THE SYMMETRY CASE HAS NO UGV
!     VECTOR, BUT IN FACT WOULD HAVE USED A SUMMATION OF THE IMMEDIATELY
!     PRECEEDING LSYM VECTORS.
!
!     IF END OF CC AND NO STRESS OR FORCE OUTPUT REQUEST WE ARE DONE
!
            IF ( eofcc ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( symflg/=0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fwdrec(*420,ugv)
            ugvvec = ugvvec + 1
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     THERE IS A REQUEST FOR STRESSES AND OR FORCES
!     FIRST DETERMINE APPROPRIATE GPTT AND EDT RECORDS IF REQUIRED
!
         ix = icc + itload
         tloads = z(ix)
         ngptt = 0
         IF ( tloads/=0 ) THEN
            file = gptt
            CALL close(gptt,clsrew)
            CALL open(*400,gptt,z(buf5),rdrew)
!
!     SKIP NAME
!
            CALL read(*420,*440,gptt,buf,2,0,n)
            SPAG_Loop_1_5: DO
!
!     PICK UP 3 WORDS OF SET INFORMATION
!
               CALL read(*420,*440,gptt,buf,3,0,n)
               IF ( buf(1)==tloads ) THEN
                  deftmp = bufr(2)
                  tmprec = buf(3)
                  EXIT SPAG_Loop_1_5
               ENDIF
            ENDDO SPAG_Loop_1_5
         ENDIF
!
         ix = icc + ieldef
         eldef = z(ix)
         IF ( eldef==0 .OR. kwdedt==0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = edt
         CALL preloc(*400,z(buf6),edt)
         CALL locate(*260,z(buf6),kdefrm,flag)
         idef = iedt
         i = idef
         DO
            CALL read(*420,*260,edt,buf(1),3,0,flag)
            IF ( buf(1)==eldef ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 260     buf(1) = eldef
         buf(2) = 0
         CALL mesage(-30,46,buf)
         spag_nextblock_1 = 13
      CASE (13)
         CALL read(*420,*280,edt,buf(1),3,0,flag)
         IF ( buf(1)/=eldef ) GOTO 280
         spag_nextblock_1 = 14
      CASE (14)
         z(i) = buf(2)
         z(i+1) = buf(3)
         i = i + 2
         IF ( i<igptta ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL mesage(-8,0,nam)
 280     ndef = i - 2
         CALL close(edt,clsrew)
         spag_nextblock_1 = 15
      CASE (15)
!
!     UNPACK VECTOR INTO CORE
!
         coef1 = 1.0
         IF ( symflg==0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SYMMETRY SEQUENCE-- BUILD VECTOR IN CORE.
!
         ix = icc + ilsym
         lsym = z(ix)
!
!     IF SYMFLG IS NEGATIVE, THIS IS A REPEAT SUBCASE.  USE PRESENT
!     VECTOR IN CORE.
!
         IF ( symflg<0 .AND. app(1)==sta(1) ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( symflg<0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = ivec , ivecn
            zz(i) = 0.0
         ENDDO
         IF ( lsym>ugvvec-1 ) THEN
            ocb(1) = lsym
            ocb(2) = ugvvec - 1
            CALL mesage(30,92,ocb(1))
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            limit = lsym
            IF ( iflag==1 ) limit = 1
            DO i = 1 , limit
               CALL bckrec(ugv)
            ENDDO
            isymn = ix + lsym
            i = ix + 1
            IF ( iflag==1 ) i = i + 1
            j2 = icb(3)
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
         coef = zz(i)
         CALL intpk(*300,ugv,0,qtype2,0)
         SPAG_Loop_1_6: DO
            CALL zntpki
            ix = ivec + ixx - 1
            IF ( ktype==1 ) THEN
               zz(ix) = zz(ix) + coef*xx(1)
            ELSE
               zz(ix+j2) = zz(ix+j2) + coef*xx(1)
               zz(ix) = zz(ix) + coef*xx(2)
            ENDIF
            IF ( eol/=0 ) EXIT SPAG_Loop_1_6
         ENDDO SPAG_Loop_1_6
 300     IF ( iflag==1 ) THEN
!
!     CONICAL SHELL BOTH CASE
!     2 VECTORS IN CORE -
!     2-ND VECTOR IS NOW IN CORE AT Z(IVEC) THRU Z(IVECN)...
!     GET 1-ST VECTOR AND PUT IT AT Z(IVECN+1) THRU Z(2*IVECN-MIDVEC+1)
!
!
            midvec = ivec
            ivec = ivecn + 1
            ivecn = ivecn + (ivecn-midvec+1)
            coef1 = zz(icc+ilsym+1)
!
!     IF FALL HERE AND SORC=1 THE VECTOR IN CORE IS THE SINE VECTOR AND
!     IF SORC=2 THE VECTOR IN CORE IS THE COSINE VECTOR.  THUS THE FIRST
!     VECTOR WAS THE OTHER VECTOR RESPECTIVELY
!     BY THE WAY THE VECTOR IN CORE IS THE SECOND VECTOR.
!
            CALL bckrec(ugv)
            CALL bckrec(ugv)
         ELSE
            i = i + 1
            IF ( i<=isymn ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
!
!     NOT SYMMETRY-- UNPACK VECTOR.
!
         j2 = icb(3)
         IF ( iflag/=1 ) THEN
            IF ( ugvvec>nvects ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         DO i = ivec , ivecn
            zz(i) = 0.0
         ENDDO
         CALL intpk(*320,ugv,0,qtype2,0)
         SPAG_Loop_1_7: DO
            CALL zntpki
            ix = ivec + ixx - 1
            IF ( ktype==1 ) THEN
               zz(ix) = coef1*xx(1)
            ELSE
               zz(ix) = coef1*xx(2)
               zz(ix+j2) = coef1*xx(1)
            ENDIF
            IF ( eol/=0 ) EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
 320     IF ( app(1)==trn(1) ) THEN
            CALL fwdrec(*340,ugv)
            ugvvec = ugvvec + 1
            CALL fwdrec(*340,ugv)
            ugvvec = ugvvec + 1
         ENDIF
 340     IF ( iflag/=1 ) ugvvec = ugvvec + 1
         IF ( iflag==1 ) CALL skprec(ugv,1)
         spag_nextblock_1 = 18
      CASE (18)
!
!     READY NOW TO SWEEP THROUGH THE ESTA ONCE.
!     SDR2E DOES ALL THE PROCESSING OF PHASE II ELEMENT COMPUTATIONS.
!     THE ESTA FILE, BE IT IN CORE OR NOT, IS SWEPT THRU ONCE FOR THE
!     FOLLOWING CALL.
!
         IF ( iflag==1 ) sorc = sorc + 1
         IF ( sorc==3 ) sorc = 1
         CALL sdr2e(*380,ieqex,neqex)
         spag_nextblock_1 = 19
      CASE (19)
!
!     CONCLUDE PROCESSING OF THIS VECTOR
!     INITIALIZE FOR NEXT VECTOR
!     CANCEL THIS INITIALIZATION IN SOME CASES IF A REPEAT CASE.
!
         IF ( branch==2 .OR. branch==4 .OR. branch==8 .OR. branch==9 ) THEN
            jlist = jlist + ktype1
            IF ( .NOT.eofcc ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( branch==3 .OR. branch==7 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( branch==5 ) THEN
!
!     FREQUENCY RESPONSE, PICK UP NEXT VECTOR UNLESS ALL FREQUENCIES
!     COMPLETED
!
            jlist = jlist + 2
            IF ( jlist<=nlist .AND. ugvvec<=nvects ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            kfrq = 0
            jlist = ilist
            DO i = ilist , nlist , 2
               z(i+1) = 0
            ENDDO
            IF ( ugvvec<=nvects ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( branch==6 ) THEN
!
!     TRANSIENT RESPONSE
!
            jlist = jlist + 2
            IF ( jlist<=nlist .AND. .NOT.eofcc ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jlist>nlist .OR. ugvvec>nvects ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
!
         ELSEIF ( .NOT.eofcc ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS ANY REMAINING VECTORS WITH LAST CC RECORD
!
         IF ( ugvvec<=nvects .AND. symflg==0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
!
!     EOF HIT ON CASECC FILE
!     PROCESS ANY MORE VECTORS USING LAST CASECC RECORD
!
 360     eofcc = .TRUE.
         IF ( nvects>=ugvvec ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
!
!     WRITE TRAILERS AND CLOSE ANY OPEN FILES
!
         ocb(2) = 63
         IF ( stress/=0 ) THEN
            ocb(1) = oes1
            CALL wrttrl(ocb(1))
            IF ( comps==-1 .AND. ilayer/=0 ) THEN
               ocb(1) = oes1l
               CALL wrttrl(ocb(1))
            ENDIF
         ENDIF
         IF ( force/=0 ) THEN
            ocb(1) = oef1
            CALL wrttrl(ocb(1))
            IF ( comps==-1 .AND. ilayer/=0 ) THEN
               ocb(1) = oef1l
               CALL wrttrl(ocb(1))
            ENDIF
         ENDIF
 380     DO i = 1 , 12
            IF ( i==2 ) THEN
               file = oef1
            ELSEIF ( i==3 ) THEN
               file = ugv
            ELSEIF ( i==4 ) THEN
               file = casecc
            ELSEIF ( i==5 ) THEN
               file = edt
            ELSEIF ( i==6 ) THEN
               file = gptt
            ELSEIF ( i==7 ) THEN
               file = pg
            ELSEIF ( i==8 ) THEN
               file = eigr
            ELSEIF ( i==9 ) THEN
               file = esta
            ELSEIF ( i==10 ) THEN
               file = eqexin
            ELSEIF ( i==11 ) THEN
               file = oes1l
            ELSEIF ( i==12 ) THEN
               file = oef1l
            ELSE
               file = oes1
            ENDIF
            CALL close(file,clsrew)
         ENDDO
         RETURN
!
 400     n = 1
         GOTO 460
 420     n = 2
         GOTO 460
 440     n = 3
 460     CALL mesage(n,file,nam)
         GOTO 380
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdr2d
