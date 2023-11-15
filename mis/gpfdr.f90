
SUBROUTINE gpfdr
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Rz(1)
   INTEGER App(2) , Cls , Clseof , Clsrew , Elem(1) , Ieol , Incr , Incrx , Irow , Irowx , Last , Nelems , Nrow , Outpt , Rd ,      &
         & Rdrew , Sysbuf , Typout , Wrt , Wrtrew , Z(1)
   DOUBLE PRECISION Dz(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / App
   COMMON /gpta1 / Nelems , Last , Incr , Elem
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls , Clseof
   COMMON /system/ Sysbuf , Outpt
   COMMON /unpakx/ Typout , Irow , Nrow , Incrx
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zntpkx/ A , Irowx , Ieol
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   LOGICAL any , anygp , axic , axif , diagm , dicout , double , enfile , enflag , engout , eorst4 , gpfile , silin
   INTEGER branch , buf(100) , buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , casecc , comp , comps(32) , core , dicloc , ect , ectwds , &
         & eldvis , elnset , eltype , entrys , eor , eqexin , estid , exelid , extgp , extid , file , gpdvis , gpect , gpset ,      &
         & gpsil , grdpts , grid1 , gridl , gsize , i , i1 , i2 , ibuf1 , icc , iccz , icode , icont , id , idext , idicts ,        &
         & idrec(10) , idtab , idx , iellst , ieln , igp , igplst , iii(2) , ikdic , ikmat , ilab , iloc1 , ilsym , ipdic , ipge ,  &
         & iptr1 , iptr2 , iretrn , iset , isil , isilex , isub , isubc , isum(10) , isym , itab1 , itab2 , itemp , itit , itype ,  &
         & iuge , ivec , ivecz , iwords , j
   REAL coef , enoeor , fvec(10) , rbuf(5) , ridrec(146) , rout(10) , rsum(10) , vec(6)
   DOUBLE PRECISION diii , elengy , toteng
   INTEGER jdicts , jk , jkmat , jpge , jsize , jtab1 , jtab2 , jtype , k , kdict , kkk , kmat , kt , ktype , kvec(10) , l , label ,&
         & lama , lastid , lbuf , ldict , ldicts , lellst , length , lfeq , lgplst , lines , loc , lpdic , lset , lsil , lsym ,     &
         & mcb(7) , method(20) , meths , mm , mode , movepq , n , name(2) , name1 , name2 , names(2) , ncc , ncomp2 , ncomps ,      &
         & ndicts , ndtab , nentry , nerror , next , nexten , nextgp , ngrids , nkdic , nkmat , noeor , npdic , npge , nsilex ,     &
         & nsize , ntab1 , nuge , nvec , ogpf1 , oldcod , oldid , oload(2) , onrgy1 , ospcf(2) , out(10) , pg , phead(3) , pivot ,  &
         & points , ptr , qg , recid(3) , recidx(3) , scale(2)
   INTEGER korsz
   INTEGER scrt1 , scrt2 , scrt3 , scrt4 , set , subcas , subr(2) , subtit , symflg , title , total , trl(7) , ug , ugpgqg
!
! End of declarations
!
!
!     GRID-POINT-FORCE-DATA-RECOVERY (MODULE)
!
!     THIS MODULE FORMULATES OFP TYPE OUTPUT DATA BLOCKS OF ELEMENT-
!     STRAIN ENERGYS AND GRID-POINT FORCE BALANCES.
!
!     DMAP CALLING SEQUENCES.
!
!     SOLUTION 1 -
!     GPFDR  CASECC,UGV,KMAT,KDICT,ECT,EQEXIN,GPECT,PG,QG/ONRGY1,OGPF1/
!            *STATICS* $
!     SOLUTION 3 -
!     GPFDR  CASECC,PHIG,KMAT,KDICT,ECT,EQEXIN,GPECT,LAMA,/ONRGY1,OGPF1/
!            *REIG* $
!
!     COMMENT FROM G.CHAN/UNISYS, 1/88 -
!     FOR MACHINES OF 32 OR 36 BIT WORDS, THE STRAIN ENERGY COMPUTATION
!     (OTHER COMPUTATIONS TOO) MUST BE DONE IN DOUBLE PRECISION. SINCE
!     THE K-MATRIX NORMALLY IN 10**7, AND THE DISPLACEMENT VECTOR IN
!     10**-2 OR 10**-3 RANGE, SINGLE PRECISION COMPUTATION GIVES BAD
!     RESULT.
!
   EQUIVALENCE (Z(1),Rz(1),Dz(1)) , (buf(1),rbuf(1)) , (out(1),rout(1)) , (name1,names(1)) , (name2,names(2)) , (idrec(1),ridrec(1))&
    & , (diii,iii(1)) , (isum(1),rsum(1)) , (kvec(1),fvec(1))
   DATA enoeor , eor/0 , 1/ , lbuf/100/ , subr/4HGPFD , 4HR   /
   DATA casecc , ug , kmat , kdict , ect , eqexin , gpect , pg , qg/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109/
   DATA onrgy1 , ogpf1 , scrt1 , scrt2 , scrt3 , scrt4 , lama/201 , 202 , 301 , 302 , 303 , 304 , 108/
   DATA meths/10/ , oload/4HAPP- , 4HLOAD/ , ospcf/4HF-OF , 4H-SPC/
   DATA scale/5 , 0/ , isum/0 , 0 , 4H*TOT , 4HALS* , 0 , 0 , 0 , 0 , 0 , 0/
   DATA method/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN , 4HSNT  , 4HBKL0 , &
       &4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H    /
!
!     CASE CONTROL POINTERS
!
   DATA title , subtit , label/39 , 71 , 103/
   DATA isym , igp , ieln , ilsym , isubc/16 , 167 , 170 , 200 , 1/
!
!     DETERMINE APPROACH
!
   n = 2*meths - 1
   DO i = 1 , n , 2
      IF ( App(1)==method(i) ) GOTO 100
   ENDDO
   WRITE (Outpt,99001) Uwm , App
99001 FORMAT (A25,' 2342, UNRECOGNIZED APPROACH PARAMETER ',2A4,' IN GPFDR INSTRUCTION.')
   i = 19
   nerror = 0
   GOTO 6200
!
 100  branch = (i+1)/2
!
!     INITIALIZATION AND BUFFER ALLOCATION.
!
   core = korsz(Z)
   buf1 = core - Sysbuf - 2
   buf2 = buf1 - Sysbuf - 2
   buf3 = buf2 - Sysbuf - 2
   buf4 = buf3 - Sysbuf - 2
   buf5 = buf4 - Sysbuf - 2
   buf6 = buf5 - Sysbuf - 2
   core = buf6 - 1
!
!     READ IN FREQUENCIES IF APPROACH IS REIGEN
!
   IF ( branch/=2 ) GOTO 300
   mode = 0
   CALL open(*300,lama,Z(buf1),Rdrew)
   CALL fwdrec(*200,lama)
   CALL fwdrec(*200,lama)
   lfeq = core
   DO
      CALL read(*200,*200,lama,buf,7,0,iwords)
      Rz(core) = rbuf(5)
      core = core - 1
   ENDDO
 200  CALL close(lama,Clsrew)
!
!     GPTA1 DUMMY ELEMENT SETUP CALL.
!
 300  CALL delset
   nerror = 1
   IF ( core<=0 ) THEN
      CALL mesage(8,0,subr)
      GOTO 6200
   ELSE
!
!     OPEN CASE CONTROL
!
      file = casecc
      nerror = 2
      CALL open(*5800,casecc,Z(buf1),Rdrew)
      CALL fwdrec(*5900,casecc)
!
!     OPEN VECTOR FILE.
!
      file = ug
      CALL open(*5800,ug,Z(buf2),Rdrew)
      CALL fwdrec(*5900,ug)
      trl(1) = ug
      CALL rdtrl(trl)
      gsize = trl(3)
!
!     PREPARE OUTPUT BLOCKS FOR ANY OUTPUTS POSSIBLE
!
      enfile = .FALSE.
      CALL open(*400,onrgy1,Z(buf3),Wrtrew)
      enfile = .TRUE.
      CALL fname(onrgy1,name)
      CALL write(onrgy1,name,2,eor)
      CALL close(onrgy1,Clseof)
      mcb(1) = onrgy1
      CALL rdtrl(mcb)
      mcb(2) = 0
      CALL wrttrl(mcb)
   ENDIF
!
 400  gpfile = .FALSE.
   nerror = 4
   CALL open(*500,ogpf1,Z(buf3),Wrtrew)
   gpfile = .TRUE.
   CALL fname(ogpf1,name)
   CALL write(ogpf1,name,2,eor)
   CALL close(ogpf1,Clseof)
!
 500  movepq = 1
   silin = .FALSE.
   trl(1) = eqexin
   CALL rdtrl(trl)
   points = trl(2)
   isilex = 1
   nsilex = 2*points
   nerror = 5
   IF ( nsilex>core ) THEN
      CALL mesage(8,0,subr)
      GOTO 6200
   ELSE
      iccz = nsilex
      icc = iccz + 1
      GOTO 700
   ENDIF
!
!     OPEN CASECC AND UGV WITH NO REWIND
!
 600  file = casecc
   nerror = 8
   CALL open(*5800,casecc,Z(buf1),Rd)
   file = ug
   CALL open(*5800,ug,Z(buf2),Rd)
!
!     READ NEXT CASE CONTROL RECORD.
!
 700  CALL read(*5700,*800,casecc,Z(iccz+1),core-iccz,eor,iwords)
   nerror = 7
   CALL mesage(8,0,subr)
   GOTO 6200
!
 800  ncc = iccz + iwords
   itemp = iccz + isubc
   subcas = Z(itemp)
!
!     SYMMETRY-REPCASE, GP-FORCE REQUEST, AND EL-ENERGY REQUEST CHECKS
!
   itemp = iccz + isym
   symflg = Z(itemp)
!
!     SET REQUEST PARAMETERS FOR GP-FORCE AND EL-ENERGY.
!
   itemp = iccz + igp
   gpset = Z(itemp)
   IF ( .NOT.gpfile ) gpset = 0
   gpdvis = Z(itemp+1)
   itemp = iccz + ieln
   elnset = Z(itemp)
   IF ( .NOT.enfile ) elnset = 0
   eldvis = Z(itemp+1)
   IF ( gpset<=0 .AND. elnset<=0 ) GOTO 900
!
!     POINTERS TO SET LIST DOMAINS
!
   itemp = iccz + ilsym
   lsym = Z(itemp)
   itemp = itemp + lsym + 1
   DO
      set = Z(itemp)
      iset = itemp + 2
      lset = Z(itemp+1)
!
!     CHECK IF THIS SET IS THE ONE FOR GP-FORCE
!
      IF ( set==gpset ) THEN
         igplst = iset
         lgplst = lset
      ENDIF
!
!     CHECK IF THIS SET IS THE ONE FOR EL-ENERGY
!
      IF ( set==elnset ) THEN
         iellst = iset
         lellst = lset
      ENDIF
!
      itemp = iset + lset
      IF ( itemp>=ncc ) EXIT
   ENDDO
!
!     IS THIS A REPCASE.  IF SO BACK-RECORD UG (REP-CASE OK ONLY FOR
!                                               STATICS)
!
 900  IF ( symflg<0 ) THEN
!
!     NEGATIVE SYMFLG IMPLIES A REP-CASE.
!
      IF ( App(1)/=method(1) ) GOTO 700
!
!     REP-CASE AND STATICS APPROACH THUS POSITION BACK ONE
!     VECTOR ON UG UNLESS THERE IS NO REQUEST FOR GP-FORCE OR
!     EL-ENERGY TO BEGIN WITH.
!
      IF ( gpset==0 .AND. elnset==0 ) GOTO 700
      CALL bckrec(ug)
      movepq = movepq - 1
!
!     NOT A REP-CASE BUT STILL IF THERE IS NO REQUEST FOR
!     GP-FORCE OR EL-ENERGY POSITION OVER VECTORS ASSOCIATED
!     WITH THIS CASE.
!
   ELSEIF ( gpset==0 .AND. elnset==0 ) THEN
      IF ( symflg==0 ) THEN
!
!     NOT A SYMMETRY CASE (WHICH WOULD USE VECTORS ALREADY READ, THUS
!     SKIP A VECTOR ASSOCIATED WITH THIS CASE.
!
         nerror = 8
!IBMD 6/93 CALL FWDREC (*1770,UG)
!IBMNB 6/93
! MAJOR LOOP OF MODULE TERMINATES WITH ENDING OF CASE CONTROL OR
! END OF EIGENVECTORS COMPUTED.  IF MODES CARD IS USED AND SPECIFIES
! MORE MODES THAN WERE COMPUTED, THEN THE FOLLOWING WILL TERMINATE
! THE LOOP.  (SEE DEMO T03011A WHICH COMPUTED 4 EIGENVALUES BUT HAD
! A MODES CARD SPECIFYING 5 MODES)
         CALL fwdrec(*5700,ug)
!IBMNE
         movepq = movepq + 1
      ENDIF
      GOTO 700
   ENDIF
!
!  BRING VECTOR INTO CORE, BRANCH IF SYMMETRY CASE.
!
   ivec = ncc + 1
   ivecz = ncc
   nvec = ivecz + gsize
   nerror = 9
   IF ( nvec>core ) THEN
      CALL mesage(8,0,subr)
      GOTO 6200
   ELSE
      ASSIGN 1300 TO iretrn
      ugpgqg = ug
   ENDIF
 1000 IF ( symflg<=0 ) THEN
!
      Irow = 1
      Nrow = gsize
      Incrx = 1
      Typout = 1
      CALL unpack(*1100,ugpgqg,Rz(ivec))
   ELSE
!
!     SYMMETRY SEQUENCE.  SUM VECTORS OF SEQUENCE APPLYING COEFFICIENTS.
!
      itemp = iccz + ilsym
      lsym = Z(itemp)
!
!     BACK UP OVER THE VECTORS OF THE SEQUENCE
!
      DO i = 1 , lsym
         CALL bckrec(ugpgqg)
      ENDDO
!
      DO i = ivec , nvec
         Rz(i) = 0.0
      ENDDO
!
      DO i = 1 , lsym
         itemp = itemp + 1
         coef = Rz(itemp)
!
!     SUM IN COEF*VECTOR(I)
!
         CALL intpk(*1050,ugpgqg,0,1,0)
         DO
            CALL zntpki
            j = ivecz + Irowx
            Rz(j) = Rz(j) + coef*A(1)
            IF ( Ieol/=0 ) EXIT
         ENDDO
 1050 ENDDO
   ENDIF
   GOTO 1200
!
!     NULL VECTOR (SET VECTOR SPACE TO ZERO)
!
 1100 DO i = ivec , nvec
      Rz(i) = 0.0
   ENDDO
 1200 GOTO iretrn
!
!     AT THIS POINT VECTOR IS IN CORE ALONG WITH THE CASE CONTROL RECORD
!
!     NOW START ECT PASS.  IN THIS PASS GP-FORCES REQUESTED WILL BE
!     WRITTEN TO PMAT (A SCRATCH SET ACTUALLY=SCRT1), AND BY THE GINO
!     DIRECT-ACCESS METHOD.  ALSO EL-ENERGY OUTPUTS WILL BE FORMED FOR
!     ANY REQUESTED ELEMENTS.
!
!     NOTE.  THE ASSEMBLY OF GP-FORCES FOR OUTPUT IS ACCOMPLISHED AFTER
!     ALL GP-FORCES REQUESTED HAVE BEEN WRITTEN TO PMAT.
!
 1300 CALL close(casecc,Cls)
   CALL close(ug,Cls)
   IF ( silin ) GOTO 1600
!
!     GET SECOND RECORD OF EQEXIN INTO CORE AND TRANSFER CODES FROM
!     SILS TO EXTERNALS AND THEN INSURE SORT ON SILS.
!
   nerror = 6
   file = eqexin
   CALL open(*5800,eqexin,Z(buf1),Rdrew)
   CALL fwdrec(*5900,eqexin)
   CALL fwdrec(*5900,eqexin)
   CALL read(*5900,*1500,eqexin,Z(isilex),core-isilex,noeor,iwords)
 1400 WRITE (Outpt,99002) Swm , eqexin
99002 FORMAT (A27,' 2343.  DATA BLOCK',I5,' IS EITHER NOT -EQEXIN- OR ','POSSIBLY INCORRECT.')
   GOTO 6200
!
 1500 IF ( iwords/=2*points ) GOTO 1400
   CALL close(eqexin,Clsrew)
   DO i = isilex , nsilex , 2
      Z(i) = 10*Z(i) + mod(Z(i+1),10)
      Z(i+1) = Z(i+1)/10
   ENDDO
   silin = .TRUE.
   CALL sort(0,0,2,2,Z(isilex),nsilex-isilex+1)
!
!     SET UP OFP ID RECORD WITH TITLE, SUBTITLE, AND LABEL.
!
 1600 itit = iccz + title
   isub = iccz + subtit
   ilab = iccz + label
   DO i = 1 , 32
      idrec(i+50) = Z(itit)
      idrec(i+82) = Z(isub)
      idrec(i+114) = Z(ilab)
      itit = itit + 1
      isub = isub + 1
      ilab = ilab + 1
   ENDDO
   DO i = 1 , 50
      idrec(i) = 0
   ENDDO
   file = ect
   nerror = 10
   CALL open(*5800,ect,Z(buf4),Rdrew)
   file = kmat
   CALL open(*5800,kmat,Z(buf5),Rdrew)
!
!     DETERMINE PRECISION OF KMAT DATA
!
   mcb(1) = kmat
   CALL rdtrl(mcb)
   double = .FALSE.
   IF ( mcb(2)==2 ) double = .TRUE.
   file = kdict
   CALL open(*5800,kdict,Z(buf6),Rdrew)
   CALL fwdrec(*5900,kdict)
!
!     PMAT WILL BE ON SCRATCH1
!     PDICT WILL BE ON SCRATCH2
!
   file = scrt1
   nerror = 11
   CALL open(*5800,scrt1,Z(buf1),Wrtrew)
   file = scrt2
   CALL open(*5800,scrt2,Z(buf2),Wrtrew)
!
!     REQUESTED OUTPUT ELEMENT ENERGIES WILL BE TEMPORARILY WRITTEN ON
!     SCRT3 WHILE THE TOTAL ENERGY IS SUMMED.
!
   file = scrt3
   IF ( elnset/=0 ) CALL open(*5800,scrt3,Z(buf3),Wrtrew)
   nextgp = 1
   lastid = 0
   oldcod = 0
   toteng = 0.0D0
   estid = 0
   axic = .FALSE.
   axif = .FALSE.
!
!     ECT PASS OF ALL ELEMENT TYPES PRESENT.
!
!     DETERMINE NEXT ELEMENT TYPE TO FIND ON ECT AND THEN FIND ITS
!     TYPE IN ECT.
!
 1700 file = kdict
   nerror = 12
   CALL read(*2500,*6000,kdict,recid,3,noeor,iwords)
   kt = recid(1)
!
!           CCONAX       CTRIAAX       CTRAPAX
   IF ( kt==35 .OR. kt==70 .OR. kt==71 ) axic = .TRUE.
!         CFLUID2/3/4  AND CFMASS
   IF ( kt>=43 .AND. kt<=46 ) axif = .TRUE.
!         CAXIF2/3/4 AND CSLOT3/4
   IF ( kt>=47 .AND. kt<=51 ) axif = .TRUE.
!
   file = ect
   CALL fwdrec(*5900,ect)
 1800 CALL read(*5900,*6000,ect,recidx,3,noeor,iwords)
!     2147483647 = 2**31-1
   IF ( recidx(1)==2147483647 ) GOTO 5900
   DO i = 1 , Last , Incr
      IF ( Elem(i+3)==recidx(1) ) THEN
         eltype = (i/Incr) + 1
         ectwds = Elem(i+5)
         IF ( ectwds<=lbuf ) THEN
!
            grdpts = Elem(i+9)
            grid1 = Elem(i+12)
            name1 = Elem(i)
            name2 = Elem(i+1)
            GOTO 2000
         ELSE
            WRITE (Outpt,99003) Swm , Elem(i) , Elem(i+1)
99003       FORMAT (A27,' 2344. GPFDR FINDS ELEMENT = ',2A4,' HAS AN ECT ','ENTRY LENGTH TOO LONG FOR A PROGRAM LOCAL ARRAY.')
            GOTO 6200
         ENDIF
      ENDIF
   ENDDO
!
!     UNRECOGNIZED ELEMENT DATA ON ECT.
!
   WRITE (Outpt,99004) Swm , recidx
99004 FORMAT (A27,' 2345.  GPFDR FINDS AND IS IGNORING UNDEFINED ECT ','DATA WITH LOCATE NUMBERS = ',3I8)
   file = ect
 1900 DO
!
!     PASS THIS ECT RECORD BUT KEEP ESTID COUNTER IN SYNC.
!
      CALL read(*5900,*1800,ect,buf,ectwds,noeor,iwords)
      estid = estid + 1
   ENDDO
!
 2000 IF ( eltype/=recid(1) ) GOTO 1900
   file = kdict
   ldict = recid(2)
   IF ( recid(3)==grdpts ) THEN
!
      ikdic = nvec + 1
      nkdic = nvec + ldict
      dicout = .FALSE.
      engout = .FALSE.
!
!     ALLOCATE A P-DICTIONARY FOR THE ELEMENTS GP-FORCE VECTOR
!     CONTRIBUTION.  CONTENTS = ESTID, EXT-EL.-ID, GINO-LOCS (GRDPTS)
!
      ipdic = nkdic + 1
      npdic = ipdic + grdpts + 1
      lpdic = grdpts + 2
      nerror = 13
      IF ( npdic>core ) THEN
         CALL mesage(8,0,subr)
         GOTO 6200
      ELSE
         iloc1 = nkdic - grdpts
         phead(1) = eltype
         phead(2) = lpdic
         phead(3) = grdpts
!
!     LOOP IS NOW MADE ON THE ELEMENT ENTRIES OF THIS ELEMENT TYPE.
!
         nexten = 1
         GOTO 2200
      ENDIF
   ENDIF
 2100 WRITE (Outpt,99005) Swm , eltype , kdict
99005 FORMAT (A27,' 2346.  GPFDR FINDS DATA FOR EL-TYPE =',I9,' IN DATA BLOCK',I9,/5X,                                              &
             &'NOT TO BE IN AGREEMENT WITH THAT WHICH IS EXPECTED.')
   GOTO 6200
!
!     READ NEXT ELEMENT DICTIONARY FROM KDICT OF CURRENT ELEMENT TYPE
!     AND FIND ECT ENTRY WITH SAME ESTID.
!
 2200 file = kdict
   CALL read(*5900,*2400,kdict,Z(ikdic),ldict,noeor,iwords)
   file = ect
   nerror = 14
   DO
      CALL read(*5900,*6000,ect,buf,ectwds,noeor,iwords)
      estid = estid + 1
      IF ( Z(ikdic)<estid ) GOTO 2100
      IF ( Z(ikdic)==estid ) THEN
!
!     DECODE THE CODE WORD INTO A LIST OF INTEGERS
!
         IF ( Z(ikdic+3)/=oldcod ) THEN
            oldcod = Z(ikdic+3)
            CALL decode(oldcod,comps,ncomps)
            ncomp2 = ncomps
            IF ( double ) ncomp2 = ncomps + ncomps
         ENDIF
!
!     DETERMINE ACTIVE CONNECTIONS
!
         nsize = Z(ikdic+2)
         ngrids = nsize/ncomp2
         IF ( ngrids<=grdpts ) THEN
!
!     ELEMENT ONLY DISPLACEMENT AND LOAD SPACE.
!
            iuge = npdic + 1
            IF ( double ) iuge = iuge/2 + 1
            nuge = iuge + nsize - 1
            ipge = nuge + 1
            npge = nuge + nsize
            IF ( npge>core ) THEN
               CALL mesage(8,0,subr)
               GOTO 6200
            ELSE
!
!     ECT ENTRY AND K-DICTIONARY ENTRY NOW AT HAND.
!
!     SET FLAG IF EL-ENERGY IS TO BE OUTPUT FOR THIS ELEMENT.
!
               exelid = buf(1)
               Z(ipdic) = estid
               Z(ipdic+1) = exelid
               enflag = .FALSE.
               IF ( axic ) exelid = mod(exelid,10000)
               IF ( axif ) exelid = mod(exelid,1000000)
               IF ( elnset<0 ) THEN
               ELSEIF ( elnset==0 ) THEN
                  EXIT
               ELSE
!
!     FIND THIS EXTERNAL ELEMENT ID IN THE REQUESTED SET LIST FOR
!     ELEMENT ENERGY OUTPUTS.
!
                  CALL setfnd(*2300,Z(iellst),lellst,exelid,nexten)
               ENDIF
               enflag = .TRUE.
               EXIT
            ENDIF
         ELSE
            WRITE (Outpt,99006) Uwm , buf(1)
99006       FORMAT (A25,' 2347.  GPFDR FINDS TOO MANY ACTIVE CONNECTING GRID',' POINTS FOR ELEMENT ID =',I9)
            GOTO 6200
         ENDIF
      ENDIF
   ENDDO
 2300 gridl = grid1 + grdpts - 1
!
!     REORDER ECT CONNECTION LIST ACCORDING TO SIL SEQUENCE.
!
   j = grid1 - 1
   DO
      j = j + 1
      IF ( j>=gridl ) THEN
!
!     NOW SET INTERNAL GRID POINT ID-S IN THE ECT ENTRY NEGATIVE IF THEY
!     ARE TO HAVE THEIR GP-FORCE BALANCE OUTPUT.
!
         anygp = .FALSE.
         IF ( gpset/=0 ) THEN
            DO i = grid1 , gridl
               IF ( buf(i)>0 ) THEN
                  IF ( gpset<0 ) THEN
                  ELSEIF ( gpset==0 ) THEN
                     CYCLE
                  ELSE
                     idx = isilex + 2*buf(i)
                     id = Z(idx-2)/10
                     IF ( axic ) id = mod(id,1000000)
                     IF ( axif ) id = mod(id,500000)
                     IF ( id<lastid ) nextgp = 1
                     lastid = id
                     CALL setfnd(*2310,Z(igplst),lgplst,id,nextgp)
                  ENDIF
                  buf(i) = -buf(i)
                  anygp = .TRUE.
               ENDIF
 2310       ENDDO
         ENDIF
!
!     IF NO GRID POINTS OF THIS ELEMENT WERE FLAGGED AND THERE IS
!     NO POTENTIAL OF ANY ELEMENT ENERGY OUTPUTS THEN SKIP THIS ELEMENT
!     AT THIS POINT.
!
         IF ( .NOT.anygp .AND. elnset==0 ) GOTO 2200
!
!     BUILD A NON-EXPANDED ELEMENT DISPLACEMENT VECTOR AT THIS TIME.
!
         j = iuge
         DO i = grid1 , gridl
            IF ( buf(i)<0 ) THEN
               gpsil = isilex - 2*buf(i) - 1
            ELSEIF ( buf(i)==0 ) THEN
               CYCLE
            ELSE
               gpsil = isilex + 2*buf(i) - 1
            ENDIF
            isil = Z(gpsil)
            DO k = 1 , ncomps
               lsil = isil + comps(k)
               Dz(j) = dble(Rz(ivecz+lsil))
               j = j + 1
            ENDDO
         ENDDO
!
         IF ( j-1==nuge ) THEN
!
!     TOTAL ELEMENT FORCE VECTOR IS NOW COMPUTED.
!
            DO i = ipge , npge
               Dz(i) = 0.0D0
            ENDDO
!
            jsize = nsize
            ikmat = npge + 1
            IF ( double ) THEN
               jsize = jsize + nsize
               ikmat = npge*2 + 1
            ENDIF
            nkmat = ikmat + jsize - 1
            IF ( nkmat>core ) THEN
               CALL mesage(8,0,subr)
               GOTO 6200
            ELSE
               diagm = .FALSE.
               IF ( Z(ikdic+1)==2 ) diagm = .TRUE.
!
!     LOOP THROUGH ALL PARTITIONS ON KMAT FOR THIS ELEMENT.
!
               jpge = ipge
               DO i = 1 , grdpts
                  itemp = iloc1 + i
                  IF ( Z(itemp)/=0 ) THEN
                     CALL filpos(kmat,Z(itemp))
                     IF ( diagm ) THEN
!
!     DIAGONAL MATRIX.  THUS ONLY DIAGONAL TERMS OF PARTITION CAN
!     BE READ.
!
                        nerror = 17
                        CALL read(*5900,*6000,kmat,Z(ikmat),ncomp2,noeor,iwords)
                        IF ( double ) THEN
!
                           jkmat = ikmat
                           DO j = 1 , ncomps
                              iii(1) = Z(jkmat)
                              iii(2) = Z(jkmat+1)
                              Dz(jpge) = Dz(iuge+j-1)*diii
                              jkmat = jkmat + 2
                              jpge = jpge + 1
                           ENDDO
                        ELSE
!
                           DO j = 1 , ncomps
                              Dz(jpge) = Dz(iuge+j-1)*dble(Rz(ikmat+j-1))
                              jpge = jpge + 1
                           ENDDO
                        ENDIF
                     ELSE
!
!     FULL MATRIX.  READ COLUMNS OF ROW-STORED VERETICAL PARTITION.
!
                        nerror = 16
                        DO k = 1 , ncomps
                           CALL read(*5900,*6000,kmat,Z(ikmat),jsize,noeor,iwords)
                           jkmat = ikmat
                           IF ( double ) THEN
!
                              DO j = iuge , nuge
                                 iii(1) = Z(jkmat)
                                 iii(2) = Z(jkmat+1)
                                 Dz(jpge) = Dz(jpge) + Dz(j)*diii
                                 jkmat = jkmat + 2
                              ENDDO
                           ELSE
                              DO j = iuge , nuge
                                 Dz(jpge) = Dz(jpge) + Dz(j)*dble(Rz(jkmat))
                                 jkmat = jkmat + 1
                              ENDDO
                           ENDIF
!
                           jpge = jpge + 1
                        ENDDO
                     ENDIF
                  ENDIF
!
               ENDDO
!
!     ENERGY COMPUTATION IS NOW MADE IF NECESSARY.
!
!       U   =  0.5(PG ) X (UG )
!        T           E         E
!
!
               IF ( elnset/=0 ) THEN
                  jpge = ipge
                  elengy = 0.0D0
                  DO i = iuge , nuge
                     elengy = elengy + Dz(i)*Dz(jpge)
                     jpge = jpge + 1
                  ENDDO
!
!     NOTE, TOTAL ENERGY WILL BE DIVIDED BY 2.0 LATER.
!
                  toteng = toteng + elengy
!
!     WRITE THIS ELEMENTS ENERGY ON SCRT3 FOR LATER OUTPUT IF REQUESTED.
!
                  IF ( enflag ) THEN
                     out(1) = buf(1)
                     rout(2) = sngl(elengy)*0.50
                     IF ( .NOT.engout ) CALL write(scrt3,names,2,noeor)
                     CALL write(scrt3,out,2,noeor)
                     engout = .TRUE.
                  ENDIF
               ENDIF
!
!     GRID POINT FORCE BALANCE OUTPUTS FOR REQUESTED GIRD POINTS.
!
               IF ( anygp ) THEN
!
!     EXPAND TO 6X1 FROM PGE EACH GRID POINT FORCE TO BE OUTPUT.
!
!     FORCES COMPUTED FOR COMPONENTS OTHER THAN 1 THRU 6 ARE NOT
!     NOW OUTPUT FROM MODULE GPFDR...  FUTURE ADDITIONAL CAPABLILITY.
!     OFP MODS NEEDED AT THAT TIME.
!
                  jpge = ipge
                  dicloc = ipdic + 2
                  DO i = dicloc , npdic
                     Z(i) = 0
                  ENDDO
                  DO i = grid1 , gridl
                     IF ( buf(i)<0 ) THEN
!
!     OK THIS GRID POINT GETS OUTPUT.
!
                        DO j = 1 , 6
                           vec(j) = 0.0
                        ENDDO
                        DO j = 1 , ncomps
                           comp = comps(j)
                           IF ( comp<=5 ) vec(comp+1) = -sngl(Dz(jpge))
                           jpge = jpge + 1
                        ENDDO
!
                        CALL write(scrt1,vec,6,eor)
                        CALL savpos(scrt1,Z(dicloc))
                        dicloc = dicloc + 1
                     ELSEIF ( buf(i)/=0 ) THEN
!
!     THIS GRID POINT NOT IN GP-FORCE BALANCE REQUEST LIST.
!
                        jpge = jpge + ncomps
                        dicloc = dicloc + 1
                     ENDIF
                  ENDDO
!
!     OUTPUT THE DICTIONARY
!
                  IF ( .NOT.dicout ) CALL write(scrt2,phead,3,noeor)
                  CALL write(scrt2,Z(ipdic),lpdic,noeor)
!
!     GO FOR NEXT ELEMENT OF CURRENT TYPE.
!
                  dicout = .TRUE.
               ENDIF
               GOTO 2200
            ENDIF
         ELSE
            WRITE (Outpt,99007) Swm , buf(1)
99007       FORMAT (A27,' 2348.  GPFDR DOES NOT UNDERSTAND THE MATRIX-','DICTIONARY ENTRY FOR ELEMENT ID =',I9)
            GOTO 6200
         ENDIF
      ELSE
         gpsil = isilex + 2*buf(j) - 1
         lsil = Z(gpsil)
         i = j
         DO
            i = i + 1
            IF ( i>gridl ) EXIT
            gpsil = isilex + 2*buf(i) - 1
            isil = Z(gpsil)
            IF ( isil<=lsil ) THEN
               lsil = buf(j)
               buf(j) = buf(i)
               buf(i) = lsil
               lsil = isil
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!     END OF ELEMENT ENTRIES OF CURRENT ELEMENT TYPE.
!     COMPLETE RECORDS IN PDIC, AND SCRT3=EL-ENERGY.
!
 2400 IF ( dicout ) CALL write(scrt2,0,0,eor)
   IF ( engout ) CALL write(scrt3,0,0,eor)
!
!     GO FOR NEXT ELEMENT TYPE
!
   GOTO 1700
!
!     END OF ALL ELEMENT DATA ON ECT (WRAP UP PHASE I OF GPFDR).
!
 2500 CALL close(kmat,Clsrew)
   CALL close(kdict,Clsrew)
   CALL close(ect,Clsrew)
   CALL close(scrt1,Clsrew)
   CALL close(scrt2,Clsrew)
   CALL close(scrt3,Clsrew)
!
!     PREPARE AND WRITE THE ELEMENT ENERGY OUTPUTS NOW RESIDENT ON SCRT3
!
   IF ( elnset==0 ) GOTO 2900
!
!     OFP ID RECORD DATA
!     DEVICE, OFP-TYPE, TOTAL ENERGY, SUBCASE, ELEMENT NAME, WORDS
!     PER ENTRY.
!
   idrec(1) = 10*branch + eldvis
   idrec(2) = 18
   ridrec(3) = sngl(toteng)*0.50
   idrec(4) = subcas
   idrec(10) = 3
!
!     IF APPROACH IS REIG, PUT MODE NO. AND FREQ. INTO IDREC, 8 AND 9
!     WORDS
!
   IF ( branch==2 ) THEN
      ridrec(9) = Rz(lfeq-mode)
      mode = mode + 1
      idrec(8) = mode
   ENDIF
!
   nerror = 22
   file = onrgy1
   CALL open(*5800,onrgy1,Z(buf2),Wrt)
   file = scrt3
   CALL open(*5800,scrt3,Z(buf3),Rdrew)
!
!     TOTENG FACTOR FOR MULTIPLICATION TO GET DECIMAL PERCENTAGE BELOW
!
   IF ( toteng/=0.0D0 ) toteng = 200.0D0/toteng
!
!     READ ELEMENT NAME INTO IDREC RECORD.
!
   jtype = 0
 2600 CALL read(*2800,*6000,scrt3,idrec(6),2,noeor,iwords)
   CALL write(onrgy1,idrec,146,eor)
   DO
      CALL read(*5900,*2700,scrt3,buf,2,noeor,iwords)
      jtype = jtype + 1
      buf(1) = 10*buf(1) + eldvis
      rbuf(3) = rbuf(2)*sngl(toteng)
      CALL write(onrgy1,buf,3,noeor)
   ENDDO
!
 2700 CALL write(onrgy1,0,0,eor)
   GOTO 2600
!
 2800 CALL close(onrgy1,Clseof)
   mcb(1) = onrgy1
   CALL rdtrl(mcb)
   mcb(2) = mcb(2) + jtype
   CALL wrttrl(mcb)
   CALL close(scrt3,Clsrew)
   idrec(3) = 0
   idrec(6) = 0
   idrec(7) = 0
!
!     A GRID-POINT-FORCE-BALANCE-OUTPUT-MAP IS NOW CONSTRUCTED. (GPFBOM)
!
!     CONTENTS...  1 LOGICAL RECORD FOR EACH GRID POINT TO BE OUTPUT
!     ===========
!
!     REPEATING 4       * EXTERNAL-ELEMENT-ID
!     WORD ENTRIES     *  ELEMENT NAME FIRST 4H
!     OF THE CON-      *  ELEMENT NAME LAST  4H
!     NECTED ELEMENTS   * GINO-LOC OF THE 6X1 FORCE VECTOR CONTRIBUTION
!
!     FOR EACH RECORD WRITTEN ABOVE, A 3-WORD ENTRY IS WRITTEN TO A
!     COMPANION DICTIONARY FILE GIVING,
!
!                      *  1-THE EXTERNAL GRID POINT ID
!     REPEATING ENTRY *   2-THE GINO-LOC TO THE ABOVE RECORD
!                      *  3-THE NUMBER OF ENTRIES IN THE RECORD
!
!
!     ALLOCATE A TABLE WITH AN ENTRY FOR EACH ELEMENT TYPE.
!     POSSIBLE IN IT.  EACH ENTRY TO HAVE 3 WORDS.
!
!     ENTRY I =      1= PTR TO DICTIONARY DATA FOR ELEMENT TYPE-I
!     *********      2= LENGTH OF DICTIONARY DATA
!                    3= NUMBER OF ENTRIES
!
 2900 IF ( gpset==0 ) GOTO 600
   idtab = ncc + 1
   ndtab = idtab + Nelems*3 - 1
   jdicts = ndtab + 1
   IF ( jdicts>core ) THEN
      CALL mesage(8,0,subr)
      GOTO 6200
   ELSE
      DO i = idtab , ndtab
         Z(i) = 0
      ENDDO
!
!     READ IN DICTIONARIES OF PMAT VECTORS.  (SCRT2)
!
      file = scrt2
      CALL open(*5800,scrt2,Z(buf2),Rdrew)
   ENDIF
!
!     READ AN ELEMENT TYPE HEADER (FIRST 3-WORDS OF EACH RECORD)
!
 3000 CALL read(*3200,*6000,scrt2,buf,3,noeor,iwords)
   itype = buf(1)
   ldict = buf(2)
   grdpts = buf(3)
   k = Incr*itype - Incr
   j = idtab + 3*itype - 3
   Z(j) = jdicts
!
!     BLAST READ IN THE DICTIONARIES OF THIS TYPE.
!
   CALL read(*5900,*3100,scrt2,Z(jdicts),core-jdicts,noeor,iwords)
   nerror = 18
   CALL mesage(8,0,subr)
   GOTO 6200
!
 3100 Z(j+1) = iwords
   Z(j+2) = iwords/ldict
   jdicts = jdicts + iwords
   nerror = 19
   IF ( core>jdicts ) GOTO 3000
   CALL mesage(8,0,subr)
   GOTO 6200
!
 3200 CALL close(scrt2,Clsrew)
!
!     DICTIONARIES ALL IN CORE.  SCRT2 IS AVAILABLE FOR USE AS THE
!     -GPFBOM-.
!
   ndicts = jdicts - 1
!
!     PASS THE -GPECT- AND BUILD THE -GPFBOM- (ON SCRT2) AND ITS
!     COMPANION DICTIONARY FILE (ON SCRT3).
!
   file = scrt2
   CALL open(*5800,scrt2,Z(buf2),Wrtrew)
   file = scrt3
   CALL open(*5800,scrt3,Z(buf3),Wrtrew)
!
   file = gpect
   CALL open(*5800,gpect,Z(buf4),Rdrew)
   CALL fwdrec(*5900,gpect)
   oldid = 0
   next = 1
!
!     READ PIVOT HEADER DATA FROM -GPECT- RECORD.
!
 3300 CALL read(*3900,*6000,gpect,buf,2,noeor,iwords)
   pivot = buf(1)
!
!     CONVERT SIL TO EX-ID
!
   CALL bisloc(*3600,pivot,Z(isilex+1),2,points,j)
   j = isilex + j - 1
   extid = Z(j)/10
   idext = extid
   IF ( axic ) idext = mod(extid,1000000)
   IF ( axif ) idext = mod(extid,500000)
   nentry = 0
!
!     CHECK FOR OUTPUT REQUEST THIS EX-ID
!
   IF ( gpset<0 ) THEN
   ELSEIF ( gpset==0 ) THEN
      GOTO 3700
   ELSE
      IF ( idext<oldid ) next = 1
      oldid = idext
      CALL setfnd(*3700,Z(igplst),lgplst,idext,next)
   ENDIF
 3400 DO
!
!     YES GP-FORCE BALANCE FOR PIVOT IS TO BE OUTPUT.
!
!     PROCESS ALL ELEMENTS CONNECTING THIS PIVOT.
!
      CALL read(*5900,*3800,gpect,length,1,noeor,iwords)
      length = iabs(length)
      IF ( length<=lbuf ) THEN
!
!     LOCATE ELEMENT FORCE DICTIONARY FOR THIS ELEMENT ENTRY.
!
         CALL read(*5900,*6000,gpect,buf,length,noeor,iwords)
         ktype = buf(2)*3 - 3 + idtab
         ptr = Z(ktype)
         ldicts = Z(ktype+2)
         IF ( ldicts==0 ) EXIT
         n = Z(ktype+1)
         CALL bisloc(*3500,buf(1),Z(ptr),n/ldicts,ldicts,j)
         j = ptr + j
         out(1) = Z(j)
!
!     FOUND DICTIONARY.  DETERMINE GINO-LOC TO USE.
!
         DO i = 3 , length
            j = j + 1
            IF ( buf(i)==pivot .AND. Z(j)>0 ) GOTO 3420
         ENDDO
         WRITE (Outpt,99008) Swm , pivot , out(1) , gpect
99008    FORMAT (A27,' 2350.  GPFDR CANNOT FIND PIVOT SIL =',I10,/5X,'AMONG THE SILS OF ELEMENT ID =',I9,' AS READ FROM DATA BLOCK',&
               & I5,',  ENTRY THUS IGNORED.')
         CYCLE
!
 3420    k = buf(2)*Incr - Incr
         out(2) = Elem(k+1)
         out(3) = Elem(k+2)
         out(4) = Z(j)
!
!     GINO-LOC IN P-DICTIONARY NO LONGER NEEDED, THUS SET IT NEGATIVE
!     TO AVOID RE-USE IN CASE WHERE AN ELEMENT CONNECTS SAME GRID MORE
!     THAN ONCE.
!
         Z(j) = -Z(j)
!
!     OUTPUT THE 4-WORD ENTRY TO -GPFBOM-
!
         CALL write(scrt2,out,4,noeor)
!
!     INCREMENT COUNTS
!
!
!     GET THE NEXT ELEMENT ENTRY.
!
         nentry = nentry + 1
      ELSE
         WRITE (Outpt,99009) Swm , pivot , gpect
99009    FORMAT (A27,' 2349.  GPFDR FINDS AN ELEMENT ENTRY CONNECTING ','PIVOT SIL =',I9,' ON DATA BLOCK',I5,/5X,                   &
                &'TOO LARGE FOR A LOCAL ARRAY. ENTRY IS BEING IGNORED.')
         CALL read(*5900,*6000,gpect,0,-length,noeor,iwords)
      ENDIF
   ENDDO
!
!     HERE WHEN PMAT DICTIONARY MISSING FOR AN ELEMENT
!     CONNECTED TO A GRID POINT TO HAVE GP-FORCE BALANCE OUTPUT.
!
 3500 kkk = buf(2)*Incr - Incr
   WRITE (Outpt,99010) Uim , Elem(kkk+1) , Elem(kkk+2) , extid
99010 FORMAT (A29,' 2351. A FORCE CONTRIBUTION  DUE TO ELEMENT TYPE = ',2A4,', ON POINT ID =',I10,/5X,                              &
             &'WILL NOT APPEAR IN THE GRID-POINT-FORCE-BALANCE SUMMARY.')
   GOTO 3400
!
!     SIL NOT FOUND IN LIST OF SILS, OR NOT REQUESTED.
!
 3600 WRITE (Outpt,99011) Swm , pivot , gpect
99011 FORMAT (A27,' 2352.  GPFDR IS NOT ABLE TO FIND PIVOT SIL =',I10,' AS READ FROM DATA BLOCK',I5,/5X,'IN TABLE OF SILS.')
!
 3700 CALL fwdrec(*5900,gpect)
   GOTO 3300
!
!     HERE WHEN END OF RECORD ON GPECT.
!     COMPLETE THE RECORD ON -GPFBOM- AND WRITE DICTIONARY ENTRY FOR THE
!     COMPLETED RECORD.
!
 3800 CALL write(scrt2,0,0,eor)
   buf(1) = extid
   buf(3) = nentry
   CALL savpos(scrt2,buf(2))
   CALL write(scrt3,buf,3,noeor)
!
!     GO FOR NEXT PIVOT SIL
!
   GOTO 3300
!
!     HERE WHEN END OF FILE ON -GPECT-.
!
 3900 CALL close(gpect,Clsrew)
   CALL close(scrt2,Clsrew)
   CALL close(scrt3,Clsrew)
!
!     SO AS TO OUTPUT THE FORCE BALANCES IN EXTERNAL GRID POINT ORDER
!     THE FOLLOWING STEPS ARE NOW PERFORMED ON THE DICTIONARY ENTRIES OF
!     THE -GPFBOM- COMPANION FILE (SCRT3).
!
!     1) ALL OF THE COMPANION FILE DICTIONARIES ARE READ INTO CORE.
!     2) THEY ARE SORTED ON THE EXTERNAL IDS.
!     3) THEY ARE PARTITIONED INTO GROUPS BASED ON A CONSIDERATION OF
!        THE NEED FOR 12 WORDS OF CORE FOR EACH ENTRY OF EACH -GPFBOM-
!        RECORD REPRESENTED BY THE GROUP IN THE FINAL OUTPUT PASS.
!     4) EACH ENTRYS 3-RD WORD (THE NUMBER OF ENTRIES IN THE RECORD) IS
!        REPLACED WITH THE INTEGER POSITION OF THE ENTRY IN THE GROUP.
!     5) EACH GROUP IS SORTED ON GINO-LOC AND WRITTEN BACK
!        TO THE COMPANION FILE AS A LOGICAL RECORD.  (THIS INSURES THAT
!        NO MORE THAN ONE PASS OF THE -GPFBOM- IS MADE PER GROUP WHEN
!        CONSTRUCTING TABLE-1 AND TABLE-2 IN THE FINAL OUTPUT PASS.)
!
   file = scrt3
   nerror = 20
   CALL open(*5800,scrt3,Z(buf3),Rdrew)
!
!     BLAST-READ 3-WORD -GPFBOM- DICTIONARY ENTRIES INTO CORE.
!
   idicts = ncc + 1
   CALL read(*5900,*4000,scrt3,Z(idicts),core-idicts,noeor,iwords)
   CALL mesage(8,0,subr)
   GOTO 6200
!
 4000 ndicts = idicts + iwords - 1
   CALL close(scrt3,Clsrew)
   nerror = 21
   CALL open(*5800,scrt3,Z(buf3),Wrtrew)
!
!     SORT ENTRIES ON EXTERNAL ID
!
   CALL sort(0,0,3,1,Z(idicts),iwords)
!
!     DETERMINE A -GPFBOM- GROUP OF RECORDS FOR OUTPUT.  EACH -GPFBOM-
!     RECORDS ENTRY WILL REQUIRE 12 WORDS OF CORE IN THE FINAL OUTPUT
!     PROCEEDURES.
!
   entrys = (core-ncc)/12
 4100 j = idicts
   total = 0
   DO WHILE ( total+Z(j+2)<=entrys )
      total = total + Z(j+2)
      j = j + 3
      IF ( j>=ndicts ) EXIT
   ENDDO
!
!     GROUP RANGE HAS BEEN FOUND.  REPLACE EACH ENTRYS -GPFBOM- ENTRY
!     COUNT WITH THE OUTPUT ORDER OF THE EXTERNAL ID ENTRY HERE.
!
   jdicts = j - 1
   k = 1
   DO i = idicts , jdicts , 3
      jk = Z(i+2)
      Z(i+2) = k
      k = k + jk
   ENDDO
!
!     SORT THIS GROUP OF 3-WORD ENTRIES ON THE GINO-LOCS.
!
   length = jdicts - idicts + 1
   CALL sort(0,0,3,2,Z(idicts),length)
!
!     OUTPUT AS A LOGICAL RECORD.
!
   CALL write(scrt3,Z(idicts),length,eor)
!
!     PROCESS NEXT GROUP IF THERE ARE MORE.
!
   idicts = jdicts + 1
   IF ( idicts<ndicts ) GOTO 4100
!
!     ALL GROUPS HAVE BEEN DETERMINED, SEQUENCED, SORTED ON GINO-LOCS,
!     AND OUTPUT.
!
   CALL close(scrt3,Clsrew)
!
!     PREPARE GRID-POINT-FORCE-BALANCE ENTRIES WITH RESPECT TO APPLIED-
!     LOAD AND SINGLE-POINT-CONSTRAINT FORCES.
!
!     LINE ENTRIES WILL BE WRITTEN TO SCRT4 FROM THE VECTOR IN CORE
!     FOR EACH OF PG AND QG CONTAINING,
!
!     EXTERNAL GP ID, 0, 4H----, 4H----, T1, T2, T3, R1, R2, R3,
!
!     ONLY FOR THOSE POINTS WHICH MAY BE OUTPUT IN THE GRID-POINT FORCE
!     BALANCE.
!
!     (NULL ENTRIES ARE NOT OUTPUT)
!
!     AFTER ALL ENTRIES FOR PG AND QG DESIRED HAVE BEEN WRITTEN TO
!     SCRT4 THEY ARE BROUGHT BACK INTO CORE, SORTED ON EXTERNAL GP ID
!     AND RE-OUTPUT TO SCRT4.
!
   file = scrt4
   CALL open(*5800,scrt4,Z(buf1),Wrtrew)
!
!     PROCESS PG.
!
   ugpgqg = pg
   buf(2) = 0
   buf(3) = oload(1)
   buf(4) = oload(2)
   lastid = 0
   nextgp = 1
   ASSIGN 4200 TO icont
   GOTO 4500
!
!     PROCESS QG
!
 4200 ugpgqg = qg
   buf(3) = ospcf(1)
   buf(4) = ospcf(2)
   lastid = 0
   nextgp = 1
   ASSIGN 4300 TO icont
   GOTO 4500
!
!     SORT SCRT4 ENTRIES ON EXTERNAL GP ID
!
 4300 CALL write(scrt4,0,0,eor)
   CALL close(scrt4,Clsrew)
   movepq = 0
   CALL open(*5800,scrt4,Z(buf1),Rdrew)
   CALL read(*5900,*4400,scrt4,Z(icc),buf1-icc,noeor,iwords)
   WRITE (Outpt,99012) Uwm , subcas
99012 FORMAT (A25,' 2353.  INSUFFICIENT CORE TO HOLD ALL NON-ZERO APP-','LOAD AND F-OF-SPC OUTPUT LINE ENTRIES OF',/5X,             &
             &'GRID-POINT-FORCE-BALANCE REQUESTS. SOME POINTS REQUESTED',' FOR OUTPUT WILL BE MISSING THEIR APP-LOAD OR F-OF-SPC',  &
            & /5X,'CONTRIBUTION IN THE PRINTED BALANCE.')
   iwords = buf1 - icc - mod(buf1-icc,10)
 4400 CALL sort(0,0,10,1,Z(icc),iwords)
   CALL close(scrt4,Clsrew)
   CALL open(*5800,scrt4,Z(buf1),Wrtrew)
   CALL write(scrt4,Z(icc),iwords,eor)
   CALL close(scrt4,Clsrew)
!
!     FINAL OUTPUT PHASE FOR CURRENT CASE CONTROL.
!
!     THE -GPFBOM- COMPANION FILE IS PROCESSED RECORD BY RECORD.
!
!     FOR EACH RECORD THEN,
!
!     1) A 3-WORD ENTRY IS READ GIVING 1) EXTERNAL GP-ID
!                                      2) GINO-LOC OF -GPFBOM- RECORD
!                                      3) OUTPUT ORDER WITHIN THE GROUP.
!
!     2) -GPFBOM- IS POSITIONED USING THE GINO-LOC.
!
!     3) A POINTER IS DETERMINED INTO TABLE-2 OF WHERE OUTPUTS BELONG
!        =10*ORDER - 10  (A ZERO POINTER) + TABLE BASE (A ZERO POINTER)
!
!     4) ENTRIES ARE READ FROM -GPFBOM- CONTAINING,
!
!                                      1) EXTERNAL ELEMENT ID
!                                      2) ELEMENT NAME FIRST 4H
!                                      3) ELEMENT NAME LAST  4H
!                                      4) GINO LOC TO 6X1 FORCE VECTOR
!
!        UNTIL AN EOR IS ENCOUNTERED.
!
!        FOR EACH ENTRY READ A 2-WORD ENTRY IS ADDED TO TABLE-1
!        CONSISTING OF                 1) GINO-LOC TO THE 6X1 VECTOR
!                                      2) PTR INTO TABLE-2
!
!        AND A 10-WORD ENTRY IS ADDED TO TABLE-2 AT Z(PTR)
!        CONSISTING OF                 1) EXTERNAL GP-ID
!                                      2) EXTERNAL ELEMENT-ID
!                                      3) NAME FIRST 4H
!                                      4) NAME LAST  4H
!                                      5 THRU 10)   NOT SET YET.
!
!     5) WHEN ALL ENTRIES OF THE -GPFBOM- RECORDS OF THE GROUP
!        (AS SPECIFIED BY ONE RECORD ON THE COMPANINON FILE) ARE IN CORE
!        TABLE-1 IS SORTED ON GINO LOCS.
!        THIS WILL PREVENT HAVING TO MAKE MORE THAN ONE PASS
!        OF THE PMAT DATA PER GROUP.
!
!     6) A SERIAL PASS OF TABLE-1 IS MADE AND EACH 6X1 VECTOR IS
!        READ DIRECTLY INTO Z(PTR+4) OF TABLE-2.
!
!     7) OUTPUT TO THE FORCE BALANCE DATA BLOCK IS MADE WITH THE
!        STANDARD OFP METHOD OF HEADER RECORD, AND REPEATING ENTRY DATA
!        RECORD.  A HEADER RECORD WILL BE OUTPUT EACH TIME THE GRID
!        POINT CHANGES.
!
!
!     ALLOCATE TABLE-1 AND TABLE-2
!
   itab1 = ncc + 1
   ntab1 = ncc + 2*entrys
   itab2 = ntab1 + 1
!
!     OPEN -GPFBOM- (SCRT2) AND ITS COMPANION DICTIONARY FILE (SCRT3).
!
   file = scrt2
   nerror = 23
   CALL open(*5800,scrt2,Z(buf2),Rdrew)
   file = scrt3
   CALL open(*5800,scrt3,Z(buf3),Rdrew)
!
!     OPEN THE OUTPUT FILE FOR GP-FORCES.
!
   file = ogpf1
   CALL open(*5800,ogpf1,Z(buf4),Wrt)
   lines = 0
   idrec(1) = 10*branch + gpdvis
   idrec(2) = 19
   idrec(4) = subcas
   idrec(10) = 10
!
!     OPEN THE PMAT 6X1 FORCE VECTORS FILE.
!
   file = scrt1
   CALL open(*5800,scrt1,Z(buf1),Rdrew)
!
!     INITIALIZE INPUT OF APP-LOAD AND F-OF-SPC LINE ENTRIES FROM SCRT4.
!
   file = scrt4
   CALL open(*5800,scrt4,Z(buf5),Rdrew)
   CALL read(*5900,*4900,scrt4,kvec,10,noeor,iwords)
   eorst4 = .FALSE.
   GOTO 5000
!
!     INTERNAL ROUTINE TO GET A VECTOR IN CORE (PG OR QG) AND WRITE
!     SELECTED NON-ZERO ENTRIES TO SCRT4 FOR INCLUSION LATER IN THE
!     GRID-POINT-FORCE-BALANCE.
!
 4500 CALL open(*4800,ugpgqg,Z(buf2),Rd)
   IF ( movepq<0 ) THEN
!
!     BACK POSITION DATA BLOCK
!
      j = iabs(movepq)
      DO i = 1 , j
         CALL bckrec(ugpgqg)
      ENDDO
   ELSEIF ( movepq/=0 ) THEN
!
!     FORWARD POSITION DATA BLOCK
!
      file = ugpgqg
      DO i = 1 , movepq
         CALL fwdrec(*5900,ugpgqg)
      ENDDO
   ENDIF
!
!     GET VECTOR INTO CORE.
!
   ASSIGN 4600 TO iretrn
   GOTO 1000
!
!     OUTPUT NON-ZERO ENTRIES REQUESTED
!
 4600 CALL close(ugpgqg,Cls)
   DO i = isilex , nsilex , 2
      icode = mod(Z(i),10)
      i1 = ivecz + Z(i+1)
      i2 = i1 + scale(icode)
      DO j = i1 , i2
         IF ( Rz(j)/=0 ) GOTO 4650
      ENDDO
      CYCLE
!
!     NON-ZERO ENTRY.  CHECK FOR OUTPUT.
!
 4650 buf(1) = Z(i)/10
      ibuf1 = buf(1)
      IF ( axic ) ibuf1 = mod(ibuf1,1000000)
      IF ( axif ) ibuf1 = mod(ibuf1,500000)
      IF ( ibuf1<lastid ) nextgp = 1
      lastid = ibuf1
      IF ( gpset<0 ) THEN
      ELSEIF ( gpset==0 ) THEN
         EXIT
      ELSE
         CALL setfnd(*4700,Z(igplst),lgplst,ibuf1,nextgp)
      ENDIF
      l = 5
      DO j = i1 , i2
         buf(l) = Z(j)
         l = l + 1
      ENDDO
      IF ( l<11 ) THEN
         DO j = l , 10
            rbuf(l) = 0.0
         ENDDO
      ENDIF
      buf(1) = buf(1)*10 + gpdvis
      CALL write(scrt4,buf,10,noeor)
 4700 ENDDO
 4800 GOTO icont
 4900 eorst4 = .TRUE.
!
!     PROCESS ONE GROUP OF -GPFBOM- RECORDS AS SPECIFIED BY THE 3-WORD
!     ENTRIES OF ONE RECORD ON SCRT3.
!
 5000 any = .FALSE.
   oldid = 0
   CALL write(ogpf1,idrec,146,eor)
 5100 iptr1 = itab1 - 1
   jtab1 = itab1 - 1
   jtab2 = itab2 - 1
   file = scrt2
 5200 CALL read(*5600,*5300,scrt3,buf,3,noeor,iwords)
   extgp = buf(1)
   loc = buf(2)
   iptr2 = itab2 + 10*buf(3) - 11
!
!     POSITION -GPFBOM- TO RECORD OF 4-WORD ENTRIES FOR THIS EXTERNAL GP
!
   CALL filpos(scrt2,loc)
   nerror = 24
   DO
!
!     READ AND DISTRIBUTE THE DATA OF THE 4-WORD ENTRIES.
!
      CALL read(*5900,*5200,scrt2,buf,4,noeor,iwords)
      Z(iptr1+1) = buf(4)
      Z(iptr1+2) = iptr2
      Z(iptr2+1) = extgp
      Z(iptr2+2) = buf(1)
      Z(iptr2+3) = buf(2)
      Z(iptr2+4) = buf(3)
      iptr1 = iptr1 + 2
      iptr2 = iptr2 + 10
      jtab1 = jtab1 + 2
      jtab2 = jtab2 + 10
   ENDDO
!
!     HERE ON END OF A GROUP.  SORT TABLE-1 ON GINO LOCS.
!     AND FILL TABLE-2 WITH 6X1 FORCE VECTORS.
!
 5300 CALL sort(0,0,2,1,Z(itab1),jtab1-itab1+1)
!
   nerror = 25
   file = scrt1
   DO i = itab1 , jtab1 , 2
      CALL filpos(scrt1,Z(i))
      ptr = Z(i+1)
      CALL read(*5900,*6000,scrt1,Z(ptr+5),6,noeor,iwords)
   ENDDO
!
!     OUTPUT DATA.  START NEW SUM WHEN ENCOUNTERING A NEW GP-ID.
!     APPLIED-LOADS AND FORCES-OF-SPC WILL INITIALIZE SUM, IF THEY EXIST
!     FOR GRID POINT IN QUESTION,  OHTERWISE SUM IS INITIALIZED TO ZERO.
!
   DO i = itab2 , jtab2 , 10
!
!     IS THIS SAME GRID POINT ID AS CURRENTLY BEING SUMMED.  IF SO,
!     CONTINUE OUTPUT OF LINE ENTRY AND SUM IN.  OTHERWISE OUTPUT
!     SUM LINE, AND NEW ID-S APPLIED-LOAD AND F-OF-SPC ENTRY.
!
 5350 DO WHILE ( Z(i)/=oldid )
!
!     CHANGE IN GRID POINT ID.
!
         isum(1) = oldid*10 + gpdvis
         IF ( any ) CALL write(ogpf1,isum,10,noeor)
         IF ( any ) lines = lines + 1
         any = .FALSE.
!
!     OUTPUT ALL LINE ENTRIES OF APP-LOADS AND F-OF-SPC UNTIL
!     MATCH ON NEW ID IS FOUND OR CURRENT FVEC IS NOT YET NEEDED.
!
         IF ( eorst4 ) GOTO 5450
         IF ( kvec(1)/10>Z(i) ) GOTO 5450
         DO j = 5 , 10
            rsum(j) = fvec(j)
         ENDDO
         oldid = kvec(1)/10
         CALL write(ogpf1,kvec,10,noeor)
         lines = lines + 1
         any = .TRUE.
         DO
!
!     SUM IN ANY MORE FROM SCRT4 OF CURRENT ID, OUTPUT LINE ENTRIES.
!
            CALL read(*5900,*5400,scrt4,kvec,10,noeor,iwords)
            IF ( kvec(1)/10/=oldid ) EXIT
            CALL write(ogpf1,kvec,10,noeor)
            lines = lines + 1
            DO j = 5 , 10
               rsum(j) = rsum(j) + fvec(j)
            ENDDO
         ENDDO
      ENDDO
      GOTO 5500
!
 5400 eorst4 = .TRUE.
      GOTO 5350
!
!     NO APP-LOAD OR F-OF-SPC ENTRIES LEFT OR CURRENT ONE NOT NEEDED YET
!
 5450 DO j = 5 , 10
         rsum(j) = 0.0
      ENDDO
      any = .TRUE.
      oldid = Z(i)
!
 5500 Z(i) = 10*Z(i) + gpdvis
      CALL write(ogpf1,Z(i),10,noeor)
      lines = lines + 1
      DO j = 5 , 10
         rsum(j) = rsum(j) + Rz(i+j-1)
      ENDDO
!
   ENDDO
!
   isum(1) = oldid*10 + gpdvis
   IF ( any ) CALL write(ogpf1,isum,10,noeor)
   IF ( any ) lines = lines + 1
   any = .FALSE.
!
!     GO FOR NEXT GROUP FROM THE -GPFBOM-.
!
   GOTO 5100
!
!     HERE ON EOF ON -GPFBOM- COMPANION FILE.  THUS AT CONCLUSION OF
!     OUTPUT PHASE FOR GP-FORCE BALANCE ONE SUBCASE, OR ONE TIME STEP OF
!     ONE SUBCASE.
!
 5600 CALL close(scrt1,Clsrew)
   CALL close(scrt2,Clsrew)
   CALL close(scrt3,Clsrew)
   CALL close(scrt4,Clsrew)
   mcb(1) = ogpf1
   CALL rdtrl(mcb)
   mcb(2) = mcb(2) + lines
   CALL wrttrl(mcb)
   CALL close(ogpf1,Clseof)
   GOTO 600
!
!     NORMAL COMPLETION.
!
 5700 CALL close(casecc,Clsrew)
   CALL close(ug,Clsrew)
   RETURN
!
!     HERE ON ERROR CONDITIONS.
!
 5800 mm = 1
   GOTO 6100
 5900 mm = 2
   GOTO 6100
 6000 mm = 3
 6100 CALL mesage(mm,file,subr)
 6200 WRITE (Outpt,99013) Swm , nerror
99013 FORMAT (A27,' 2354.',/5X,'GPFDR MODULE IS UNABLE TO CONTINUE ','AND HAS BEEN TERMINATED DUE TO ERROR MESSAGE PRINTED ',       &
             &'ABOVE OR BELOW THIS MESSAGE.',/5X,'THIS ERROR OCCURRED ','IN GPFDR CODE WHERE THE VARIABLE -NERROR- WAS SET =',I5)
   DO i = 100 , 300 , 100
      DO j = 1 , 9
         CALL close(i+j,Clsrew)
      ENDDO
   ENDDO
END SUBROUTINE gpfdr
