
SUBROUTINE sdr2c
   IMPLICIT NONE
   INTEGER Acc , App(2) , Axif , Bgpdt , Bk0(2) , Bk1(2) , Branch , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Casecc , Cei(2) , Cstm ,     &
         & Date(3) , Displ , Ds0(2) , Ds1(2) , Dtype(8) , Edt , Eigr , Eol , Eqexin , File , Force , Frq(2) , Harms , I2 , Iacc ,   &
         & Icb(7) , Icstm , Idispl , Idload , Ieigen , Ieldef , Ielf , Ifrout , Iheat , Iloads , Ilsym , Incr2 , Isload , Isopl ,   &
         & Isorc , Ispcf , Istr , Isymfl , Itload , Ittl , Ivec , Ivecn , Ivel , Ixx , Iy , J2 , Knset , Ksystm(65) , Ktype ,       &
         & Kwdcc , Kwdedt , Kwdest , Kwdgpt , Loads , Mcb(7) , Mpt , Mset , Nam(2) , Ncstm , Nharms , Nrigds , Nrings , Ocb(7) ,    &
         & Opg1 , Oqg1 , Ougv1 , Pg , Phig , Pla(22) , Plots
   REAL All , Any , Clsrew , Deform , Degra , Dit , Eldef , End , Eor , Est , Esta , Gptt , Gptta , Oef1 , Oeigr , Oes1 , Ophig ,   &
      & Pi , Pphig , Raddeg , Rd , Rdrew , S4pisq , Strspt , Temp , Tloads , Twopi , Wrt , Wrtrew , Xx(4) , Y(4) , Zz(1)
   LOGICAL Axic , Ddrmm
   INTEGER Pugv1 , Qg , Qtype2 , Rei(2) , Sil , Sort2 , Spcf , Sta(2) , Stress , Symflg , Sysbuf , Time , Trn(2) , Ugv , Vel , Z(1)
   COMMON /blank / App , Sort2
   COMMON /condas/ Pi , Twopi , Raddeg , Degra , S4pisq
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /sdr2x1/ Ieigen , Ieldef , Itload , Isymfl , Iloads , Idispl , Istr , Ielf , Iacc , Ivel , Ispcf , Ittl , Ilsym , Ifrout ,&
                 & Isload , Idload , Isorc
   COMMON /sdr2x2/ Casecc , Cstm , Mpt , Dit , Eqexin , Sil , Gptt , Edt , Bgpdt , Pg , Qg , Ugv , Est , Phig , Eigr , Opg1 , Oqg1 ,&
                 & Ougv1 , Oes1 , Oef1 , Pugv1 , Oeigr , Ophig , Pphig , Esta , Gptta , Harms
   COMMON /sdr2x4/ Nam , End , Mset , Icb , Ocb , Mcb , Dtype , Icstm , Ncstm , Ivec , Ivecn , Temp , Deform , File , Buf1 , Buf2 , &
                 & Buf3 , Buf4 , Buf5 , Any , All , Tloads , Eldef , Symflg , Branch , Ktype , Loads , Spcf , Displ , Vel , Acc ,   &
                 & Stress , Force , Kwdest , Kwdedt , Kwdgpt , Kwdcc , Nrigds , Sta , Rei , Ds0 , Ds1 , Frq , Trn , Bk0 , Bk1 ,     &
                 & Cei , Pla , Nrings , Nharms , Axic , Knset , Isopl , Strspt , Ddrmm
   COMMON /system/ Ksystm
   COMMON /unpakx/ Qtype2 , I2 , J2 , Incr2
   COMMON /zblpkx/ Y , Iy
   COMMON /zntpkx/ Xx , Ixx , Eol , Eor
   COMMON /zzzzzz/ Z
   LOGICAL anyout , axcosi , axsine
   INTEGER blanks , buf(50) , dest , eof , extra , flag , formt , fsetno , i , ibufsv , icc , id , iflag , ilist , incore , index , &
         & infil , ireq , ireqx , iseq , iset , isetf , isetnf , isetno , iskip , isymn , itabl , itemp , ix , ixset , ixsetn , j , &
         & jcount , jharm , jlist , jtj , k , kcount , kfrq , khi , klo , kn , kplot , ktype1 , ktypex , kwds , kx , l , ll , lsym ,&
         & m , m8 , mmreig , n , ncc , neqex , nlist , nset , nsetf , nvects , nwds , nxset , oharms , outfl , pbuff(4) , platit(12)&
         & , retx , setno , sorc , xset0 , xsetno
   REAL bufr(11) , coef , diff , diff1 , omega , pbufr(4) , redner
   INTEGER korsz
!
!     SDR2C PROCESSES OUTPUT REQUESTS FOR SINGLE-POINT FORCES OF
!     CONSTRAINT, LOADS, DISPLACEMENTS, VELOCITIES, ACCELERATIONS AND
!     EIGENVECTORS.
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(15),Date(1)) , (Ksystm(18),Time) , (Ksystm(20),Plots) , (Ksystm(38),Axif) ,             &
    & (Ksystm(56),Iheat) , (buf(1),bufr(1)) , (Z(1),Zz(1)) , (pbuff(1),pbufr(1))
   DATA buf/50*0/
   DATA blanks/4H    /
   DATA xset0/100000000/
   DATA platit/4HLOAD , 4H FAC , 4HTOR  , 9*0/
   DATA mmreig/4HMMRE/
!
!     IF THIS IS A DYNAMIC-DATA-RECOVERY-MATRIX-METHOD REIG PROBLEM
!     THEN ALL EIGENVECTORS ARE TO BE OUTPUT FOR THE DDRMM MODULE.
!
   setno = 0
   IF ( Ddrmm .AND. ireq==Idispl ) setno = -1
!
!     PERFORM GENERAL INITIALIZATION
!
   Buf1 = korsz(Z) - Sysbuf + 1
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Buf4 = Buf3 - Sysbuf
   Buf5 = Buf4 - Sysbuf
   iseq = 1
   m8 = -8
   I2 = 1
   Incr2 = 1
   kplot = 0
   extra = 0
   axsine = .FALSE.
   axcosi = .FALSE.
!
!     READ SECOND RECORD OF EQEXIN OR EQDYN INTO CORE.
!
   File = Eqexin
   CALL gopen(Eqexin,Z(Buf1),0)
   CALL skprec(Eqexin,1)
   CALL read(*5200,*100,Eqexin,Z,Buf5,1,neqex)
   CALL mesage(m8,0,Nam)
 100  CALL close(Eqexin,Clsrew)
   itabl = 1
   kn = neqex/2
   icc = neqex
   ilist = neqex + 1
!
!     INITIALIZE FOR PROCESSING SPECIFIC REQUEST.
!
 200  IF ( iseq<2 ) THEN
!
!     LOAD VECTOR.
!
      IF ( Loads==0 .OR. App(1)==Rei(1) .OR. App(1)==Cei(1) .OR. App(1)==Bk1(1) ) GOTO 4700
      infil = 115
      outfl = Opg1
      ireq = Iloads
   ELSEIF ( iseq==2 ) THEN
!
!     SINGLE-POINT FORCES OF CONSTRAINT.
!
      IF ( Spcf==0 ) GOTO 4700
      infil = Qg
      outfl = Oqg1
      ireq = Ispcf
   ELSE
!
!     DISPLACEMENT VECTOR OR EIGENVECTOR
!
      IF ( Displ==0 .AND. Vel==0 .AND. Acc==0 .AND. Plots==0 ) GOTO 4700
      infil = Ugv
      outfl = Ougv1
      jtj = Vel + Acc
      IF ( App(1)/=mmreig .OR. Displ/=0 .OR. jtj==0 ) THEN
         ireq = Idispl
      ELSEIF ( Vel==0 ) THEN
         ireq = Iacc
      ELSE
         ireq = Ivel
      ENDIF
   ENDIF
!
!     READ TRAILER ON INPUT FILE. SET PARAMETERS.
!
   Icb(1) = infil
   CALL rdtrl(Icb)
   IF ( Icb(1)/=infil ) GOTO 4900
   nvects = Icb(2)
   IF ( Icb(5)>2 ) THEN
!
!     COMPLEX VECTOR.
!
      Ktype = 2
      Qtype2 = 3
      ktype1 = 3
      nwds = 14
      ktypex = 1000
   ELSE
!
!     REAL VECTOR.
!
      Ktype = 1
      Qtype2 = 1
      ktype1 = 2
      nwds = 8
      ktypex = 0
   ENDIF
!
!     OPEN CASE CONTROL AND SKIP HEADER. THEN BRANCH ON APPROACH.
!
   CALL gopen(Casecc,Z(Buf1),0)
   pbuff(2) = 1
   IF ( Branch==1 .OR. Branch==3 .OR. Branch==7 .OR. Branch==10 ) GOTO 700
   IF ( Branch==4 .OR. Branch==8 ) THEN
!
!     DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1 - SKIP 1ST DATA RECORD ON
!     CC.
!
      CALL skprec(Casecc,1)
      pbuff(2) = 4
      IF ( App(1)/=Bk1(1) ) THEN
         pbuff(2) = 1
         GOTO 700
      ENDIF
   ELSEIF ( Branch==5 .OR. Branch==6 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
      File = Pg
      CALL open(*5100,File,Z(Buf2),Rdrew)
      i = ilist
      m = 3
      ix = 1
      pbuff(2) = 3
      IF ( App(1)==Frq(1) ) pbuff(2) = 2
      IF ( App(1)==Frq(1) .OR. App(1)==Trn(1) ) ix = 2
      DO
         CALL read(*5200,*600,File,buf,m,0,flag)
         Z(i) = buf(m)
         Z(i+1) = 0
         i = i + ix
         m = 1
      ENDDO
   ENDIF
!
!     EIGENVALUES - READ LIST OF MODE NOS. AND EIGENVALUES INTO CORE.
!
   File = Eigr
   CALL gopen(Eigr,Z(Buf2),0)
   CALL skprec(Eigr,1)
   IF ( App(1)==Cei(1) ) pbuff(2) = 5
   IF ( App(1)==Rei(1) ) pbuff(2) = 4
   i = ilist
   m = 8 - Ktype
   iskip = 0
   index = 2
   IF ( App(1)/=Rei(1) ) GOTO 400
   DO
!
!     CHECK TO SEE IF ALL GENERALIZED MASS VALUES ARE ZERO
!
      CALL read(*5200,*300,Eigr,buf,m,0,flag)
      IF ( buf(6)/=0.0 ) THEN
         index = 0
         EXIT
      ENDIF
   ENDDO
 300  CALL skprec(Eigr,-1)
 400  DO
      CALL read(*5200,*500,Eigr,buf,m,0,flag)
      IF ( App(1)/=Rei(1) ) EXIT
      IF ( index==2 ) EXIT
!
!     MATCH CORRECT MODE NOS. AND EIGENVALUES WITH PROPER
!     EIGENVECTORS WHEN USING GIVENS METHOD WITH F1.GT.0.0
!
      IF ( index==1 ) EXIT
      IF ( buf(6)/=0. ) THEN
         index = 1
         EXIT
      ELSE
         iskip = iskip + 1
      ENDIF
   ENDDO
   Z(i) = buf(1) - iskip
   Z(i+1) = buf(3)
   Z(i+2) = buf(4)
   i = i + ktype1
   GOTO 400
 500  CALL close(Eigr,Clsrew)
   nlist = i - ktype1
   icc = i
   GOTO 700
 600  CALL close(File,Clsrew)
   nlist = i - ix
   icc = i
!
!     OPEN OUTPUT FILE. WRITE HEADER RECORD.
!
 700  File = outfl
   anyout = .FALSE.
   CALL open(*4900,outfl,Z(Buf2),Wrtrew)
   Ocb(1) = outfl
   CALL fname(outfl,buf)
   DO i = 1 , 3
      buf(i+2) = Date(i)
   ENDDO
   buf(6) = Time
   buf(7) = 1
   CALL write(outfl,buf,7,1)
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
   File = infil
   CALL open(*4800,infil,Z(Buf3),Rdrew)
   CALL fwdrec(*5200,infil)
!
!     SET PARAMETERS TO KEEP CASE CONTROL AND VECTORS IN SYNCH.
!
   eof = 0
   jcount = 0
   kcount = 1
   jlist = ilist
   kfrq = 0
   incore = 0
   kwds = 0
!
!     READ A RECORD IN CASE CONTROL. SET SYMMETRY FLAG.
!
 800  CALL read(*4500,*900,Casecc,Z(icc+1),Buf5-icc,1,ncc)
   CALL mesage(m8,0,Nam)
 900  ix = icc + Isymfl
   itemp = icc + Harms
!
!     OHARMS WILL BE 1 GREATER THAN THE MAXIMUM OUTPUT HARMONIC
!
   oharms = Z(itemp)
   IF ( oharms<0 .AND. Axif/=0 ) oharms = Axif
   IF ( oharms<0 ) oharms = Nharms
!
!     IF A FLUID PROBLEM CONVERT USER HARMONIC TO INTERNAL HARMONIC MAX.
!
   IF ( oharms/=0 ) THEN
      IF ( Axif/=0 ) THEN
         oharms = oharms - 1
         oharms = 2*oharms + 3
      ENDIF
   ENDIF
   Symflg = Z(ix)
   IF ( Symflg==0 ) sorc = Z(icc+Isorc)
   IF ( sorc==1 ) axsine = .TRUE.
   IF ( sorc==2 ) axcosi = .TRUE.
   iflag = 0
   IF ( Axic .AND. axsine .AND. axcosi .AND. jcount==2 ) iflag = 1
   Ivec = icc + ncc + 1
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT.
!     IF NOT, TEST FOR RECORD SKIP ON INFIL  THEN GO TO END OF THIS
!     REQUEST.
!     IF SO, SET POINTERS TO SET DEFINING REQUEST.
!
 1000 ireqx = icc + ireq
   setno = Z(ireqx)
   dest = Z(ireqx+1)
   formt = iabs(Z(ireqx+2))
   xsetno = -1
   IF ( setno<0 ) THEN
   ELSEIF ( setno==0 ) THEN
      IF ( Symflg/=0 ) GOTO 3800
      IF ( App(1)==Frq(1) ) THEN
         IF ( iseq==3 ) GOTO 1100
      ENDIF
      IF ( Plots==0 ) THEN
         CALL fwdrec(*5200,infil)
         jcount = jcount + 1
         GOTO 3800
      ENDIF
   ELSE
      ix = icc + Ilsym
      isetno = ix + Z(ix) + 1
      DO
         iset = isetno + 2
         nset = Z(isetno+1) + iset - 1
         IF ( Z(isetno)==setno ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET.
!
            IF ( setno<xset0 ) EXIT
            xsetno = dest/10
            dest = dest - 10*xsetno
            IF ( xsetno==0 ) EXIT
            ixsetn = ix + Z(ix) + 1
            DO
               ixset = ixsetn + 2
               nxset = Z(ixsetn+1) + ixset - 1
               IF ( Z(ixsetn)==xsetno ) GOTO 1100
               ixsetn = nxset + 1
               IF ( ixsetn>=Ivec ) THEN
                  xsetno = -1
                  GOTO 1100
               ENDIF
            ENDDO
         ELSE
            isetno = nset + 1
            IF ( isetno>=Ivec ) THEN
               setno = -1
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF
!
!     UNPACK VECTOR INTO CORE (UNLESS VECTOR IS ALREADY IN CORE).
!
 1100 IF ( incore/=0 ) GOTO 1700
   Ivecn = Ivec + Ktype*Icb(3) - 1
   IF ( Ivecn>=Buf5 ) CALL mesage(m8,0,Nam)
   IF ( Symflg==0 ) GOTO 1400
!
!     SYMMETRY SEQUENCE - BUILD VECTOR IN CORE.
!
   ix = icc + Ilsym
   lsym = Z(ix)
!
!     IF SYMFLG IS NEGATIVE THIS IS A REPEAT SUBCASE. BCKREC VECTOR
!     AND READ IT INTO CORE.
!
   IF ( Symflg<0 .AND. App(1)==Sta(1) ) THEN
!
!     REPEAT SUBCASE
!
      jcount = jcount - 1
      CALL bckrec(infil)
      GOTO 1400
   ELSE
      IF ( Symflg<0 ) GOTO 800
      DO i = Ivec , Ivecn
         Zz(i) = 0.
      ENDDO
      DO i = 1 , lsym
         CALL bckrec(infil)
      ENDDO
      isymn = ix + lsym
      i = ix + 1
   ENDIF
 1200 coef = Zz(i)
   CALL intpk(*1300,infil,0,Qtype2,0)
   DO
      CALL zntpki
      ix = Ivec + Ixx - 1
      Zz(ix) = Zz(ix) + coef*Xx(1)
      IF ( Ktype==2 ) Zz(ix+1) = Zz(ix+1) + coef*Xx(2)
      IF ( Eol/=0 ) EXIT
   ENDDO
 1300 i = i + 1
   IF ( i>isymn ) GOTO 1700
   GOTO 1200
!
!     NOT SYMMETRY - UNPACK VECTOR.
!
 1400 J2 = Icb(3)
   IF ( jcount>=nvects ) GOTO 4600
   CALL unpack(*1500,infil,Z(Ivec))
   GOTO 1600
 1500 DO i = Ivec , Ivecn
      Zz(i) = 0.
   ENDDO
 1600 jcount = jcount + 1
!
!     TEST FOR CONTINUATION FROM HERE.
!
 1700 IF ( setno==0 ) THEN
      IF ( App(1)==Frq(1) ) GOTO 4000
   ENDIF
!
!     PREPARE TO WRITE ID RECORD ON OUTPUT FILE.
!
   IF ( Branch==2 .OR. Branch==8 .OR. Branch==9 ) THEN
!
!     EIGENVALUES OR BUCKLING PHASE 1.
!
      IF ( iseq==2 ) buf(2) = ktypex + 3
      IF ( iseq==3 ) buf(2) = ktypex + 7
      buf(5) = Z(jlist)
      buf(6) = Z(jlist+1)
      buf(7) = Z(jlist+2)
      buf(8) = 0
!     PBUFF(2) = 2  THIS CARD WAS REMOVED SINCE LEVEL 16. NO LONGER NEED
      pbuff(3) = buf(5)
      IF ( App(1)==Bk1(1) ) pbuff(3) = -buf(5)
      pbuff(4) = buf(6)
      IF ( App(1)/=Bk1(1) .AND. App(1)/=Cei(1) ) pbufr(4) = sqrt(abs(bufr(6)))/Twopi
      IF ( App(1)==Cei(1) ) pbufr(4) = abs(bufr(7))/Twopi
   ELSEIF ( Branch==5 ) THEN
!
!     FREQUENCY RESPONSE.
!
      ix = icc + Idload
      buf(8) = Z(ix)
      buf(6) = 0
      buf(7) = 0
      pbuff(2) = 2
      pbuff(3) = buf(8)
      IF ( iseq/=3 ) THEN
         buf(2) = Dtype(iseq) + ktypex
      ELSEIF ( kcount<2 ) THEN
         buf(2) = 1001
      ELSEIF ( kcount==2 ) THEN
         buf(2) = 1010
      ELSE
         buf(2) = 1011
      ENDIF
      GOTO 1800
   ELSEIF ( Branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
      buf(5) = Z(jlist)
      IF ( kcount<2 ) THEN
         buf(2) = 1
      ELSEIF ( kcount==2 ) THEN
         buf(2) = 10
      ELSE
         buf(2) = 11
      ENDIF
      IF ( ireq==Iloads ) buf(2) = 2
      IF ( ireq==Ispcf ) buf(2) = 3
      ix = icc + Idload
      buf(8) = Z(ix)
      buf(6) = 0
      buf(7) = 0
      pbuff(2) = 3 + 10*(kcount-1)
      pbuff(3) = buf(8)
      pbuff(4) = buf(5)
      GOTO 1800
   ELSE
!
!     NORMAL STATICS OR DIFF.STIFF. PHASE O OR 1 OR BUCKLING PHASE 0.
!
      buf(2) = Dtype(iseq)
      ix = icc + Isload
      buf(5) = Z(icc+1)
      buf(6) = 0
      buf(7) = 0
      buf(8) = Z(ix)
      pbuff(2) = 1
      pbuff(3) = Z(ix)
      pbuff(4) = 0
      IF ( Branch==10 ) THEN
         ix = icc + Ittl + 84
         Z(ix) = platit(1)
         Z(ix+1) = platit(2)
         Z(ix+2) = platit(3)
         CALL int2al(jcount,Z(ix+3),platit(4))
      ENDIF
   ENDIF
   GOTO 2000
 1800 IF ( kfrq==0 ) THEN
!
!     FIRST TIME FOR THIS LOAD VECTOR ONLY - MATCH LIST OF USER
!     REQUESTED FREQS WITH ACTUAL FREQS. MARK FOR OUTPUT EACH ACTUAL
!     FREQ WHICH IS CLOSEST TO USER REQUEST.
!
      kfrq = 1
      ix = icc + Ifrout
      fsetno = Z(ix)
      IF ( fsetno>0 ) THEN
         ix = icc + Ilsym
         isetnf = ix + Z(ix) + 1
         DO
            isetf = isetnf + 2
            nsetf = Z(isetnf+1) + isetf - 1
            IF ( Z(isetnf)==fsetno ) THEN
               DO i = isetf , nsetf
                  k = 0
                  diff = 1.E25
                  bufr(1) = Zz(i)
                  DO j = ilist , nlist , 2
                     IF ( Z(j+1)==0 ) THEN
                        diff1 = abs(Zz(j)-bufr(1))
                        IF ( diff1<diff ) THEN
                           diff = diff1
                           k = j
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( k/=0 ) Z(k+1) = 1
               ENDDO
               GOTO 1900
            ELSE
               isetnf = nsetf + 1
               IF ( isetnf>=Ivec ) THEN
                  fsetno = -1
                  EXIT
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      DO j = ilist , nlist , 2
         Z(j+1) = 1
      ENDDO
   ENDIF
!
!     DETERMINE IF CURRENT FREQ IS MARKED FOR OUTPUT.
!
 1900 IF ( Z(jlist+1)==0 ) GOTO 3800
   buf(5) = Z(jlist)
   pbuff(4) = buf(5)
!
!     WRITE ID RECORD ON OUTPUT FILE.
!
 2000 IF ( setno==0 .AND. Plots/=0 ) GOTO 3100
   buf(1) = dest + 10*Branch
   buf(3) = 0
!
!     IF CONICAL SHELL PROBLEM, SET MINOR ID = 1000 FOR USE BY OFP
!
   IF ( Axic ) buf(3) = 1000
   buf(4) = Z(icc+1)
   IF ( Ddrmm ) buf(4) = 9999
   buf(9) = iabs(Z(ireqx+2))
   IF ( buf(9)==1 .AND. Ktype==2 ) buf(9) = 2
   formt = buf(9)
   buf(10) = nwds
   CALL write(outfl,buf,50,0)
   ix = icc + Ittl
   CALL write(outfl,Z(ix),96,1)
!
!     BUILD DATA RECORD ON OUTPUT FILE.
!
   IF ( setno/=-1 ) THEN
!
!     SET .NE. ALL  -  OUTPUT ONLY POINTS DEFINED IN SET.
!
      jharm = 0
      i = iset
      GOTO 2200
   ELSE
!
!     SET .EQ. ALL  -  OUTPUT ALL POINTS DEFINED IN EQEXIN.
!
      kx = 1
      n = neqex - 1
      ASSIGN 2100 TO retx
      GOTO 2600
   ENDIF
 2100 kx = kx + 2
   IF ( kx>n ) GOTO 3100
   GOTO 2600
 2200 IF ( i==nset ) GOTO 2400
   IF ( Z(i+1)>0 ) GOTO 2400
   n = -Z(i+1)
   buf(1) = Z(i)
   ibufsv = buf(1)
   i = i + 1
   ASSIGN 2300 TO retx
   GOTO 5000
 2300 buf(1) = ibufsv + 1
   ibufsv = buf(1)
   IF ( buf(1)>n ) GOTO 2500
   GOTO 5000
 2400 buf(1) = Z(i)
   ASSIGN 2500 TO retx
   GOTO 5000
 2500 i = i + 1
   IF ( i<=nset ) GOTO 2200
   jharm = jharm + 1
   IF ( .NOT.Axic .AND. Axif==0 ) GOTO 3100
   IF ( jharm>oharms ) GOTO 3100
   i = iset
   GOTO 2200
!
!     PICK UP POINTER TO GRID POINT DATA AND GRID POINT TYPE.
!
 2600 buf(1) = Z(kx)
   IF ( iflag==1 .AND. buf(1)>=1000000 ) GOTO retx
   j = Z(kx+1)/10
   buf(2) = Z(kx+1) - 10*j
   j = Ivec + Ktype*(j-1)
   IF ( buf(2)==1 ) THEN
!
!     GRID POINT.
!
      flag = 0
      IF ( Ktype==2 ) THEN
!
!     COMPLEX GRID POINT.
!
         DO k = 1 , 11 , 2
            bufr(k+2) = Zz(j)
            bufr(k+3) = Zz(j+1)
            IF ( bufr(k+2)/=0. .OR. bufr(k+3)/=0. .OR. Sort2>=0 ) flag = 1
            IF ( formt==3 ) THEN
               redner = sqrt(bufr(k+2)**2+bufr(k+3)**2)
               IF ( redner/=0 ) THEN
                  bufr(k+3) = atan2(bufr(k+3),bufr(k+2))*Raddeg
                  IF ( bufr(k+3)<-0.00005 ) bufr(k+3) = bufr(k+3) + 360.0
               ELSE
                  bufr(k+3) = 0.0
               ENDIF
               bufr(k+2) = redner
            ENDIF
            j = j + 2
         ENDDO
         IF ( iseq<=2 .AND. flag==0 ) GOTO retx
      ELSE
         DO k = 1 , 6
            bufr(k+2) = Zz(j)
            IF ( bufr(k+2)/=0.0 .OR. Sort2>=0 ) flag = 1
            j = j + 1
         ENDDO
         IF ( iseq<=2 .AND. flag==0 ) GOTO retx
      ENDIF
   ELSE
!
!     SCALAR OR EXTRA POINT.
!
      buf(3) = Z(j)
      IF ( Ktype==2 ) THEN
!
!     COMPLEX SCALAR OR EXTRA POINT.
!
         buf(4) = Z(j+1)
         IF ( iseq<=2 .AND. bufr(3)==0.0 .AND. bufr(4)==0.0 .AND. Sort2<0 ) GOTO retx
         DO k = 5 , 14
            buf(k) = 0
         ENDDO
         IF ( formt==3 ) THEN
            redner = sqrt(bufr(3)**2+bufr(4)**2)
            IF ( redner/=0 ) THEN
               bufr(4) = atan2(bufr(4),bufr(3))*Raddeg
               IF ( bufr(4)<-0.00005 ) bufr(4) = bufr(4) + 360.0
            ELSE
               bufr(4) = 0.0
            ENDIF
            bufr(3) = redner
         ENDIF
      ELSE
         IF ( iseq<=2 .AND. bufr(3)==0.0 .AND. Sort2<0 ) GOTO retx
         DO k = 4 , 8
            buf(k) = 0
         ENDDO
      ENDIF
   ENDIF
!
!     WRITE ENTRY ON OUTPUT FILE.
!
!     IF COMPLEX  TRANSPOSE DATA FOR OFP (REAL TOP, IMAG BOTTOM)
!
   IF ( nwds==14 ) THEN
      itemp = buf(4)
      buf(4) = buf(5)
      buf(5) = buf(7)
      buf(7) = buf(11)
      buf(11) = buf(8)
      buf(8) = buf(13)
      buf(13) = buf(12)
      buf(12) = buf(10)
      buf(10) = buf(6)
      buf(6) = buf(9)
      buf(9) = itemp
   ENDIF
!
   anyout = .TRUE.
!
!     IF CONICAL SHELL DECODE GRID POINT NUMBER IF GREATER THAN 1000000.
!
   IF ( Axic ) THEN
      IF ( buf(1)>=1000000 ) THEN
         itemp = buf(1)/1000000
!
!     STOP OUTPUT WHEN PRESENT HARMONIC EXCEEDS OUTPUT HARMONIC SIZE REQ
!
         IF ( itemp>oharms ) GOTO 3100
         buf(1) = buf(1) - itemp*1000000
         buf(2) = itemp - 1
         GOTO 2700
      ELSE
         buf(2) = blanks
      ENDIF
   ENDIF
!
!     IF A FLUID PROBLEM THEN A CHECK IS MADE ON THE HARMONIC ID
!
   IF ( Axif>0 ) THEN
      IF ( buf(1)>=500000 ) THEN
         itemp = buf(1) - mod(buf(1),500000)
         itemp = itemp/500000
!
!     STOP THE OUTPUT IF THE HARMONIC IS GREATER THAN THE OUTPUT
!     REQUEST FOR HARMONICS
!
         IF ( itemp>=oharms ) GOTO 3100
      ENDIF
   ENDIF
!
!     DETERMINE DESTINATION FOR ENTRY.
!
 2700 id = buf(1)
   buf(1) = 10*id + dest
   IF ( xsetno<0 ) THEN
   ELSEIF ( xsetno==0 ) THEN
      buf(1) = 10*id
   ELSE
      ix = ixset
      GOTO 2800
   ENDIF
   GOTO 3000
 2800 IF ( ix/=nxset ) THEN
      IF ( Z(ix+1)<=0 ) THEN
         IF ( id>=Z(ix) .AND. id<=-Z(ix+1) ) GOTO 3000
         ix = ix + 2
         GOTO 2900
      ENDIF
   ENDIF
   IF ( id==Z(ix) ) GOTO 3000
   ix = ix + 1
 2900 IF ( ix<=nxset ) GOTO 2800
   buf(1) = 10*id
!
!     NOW WRITE ENTRY.
!
 3000 CALL write(outfl,buf(1),nwds,0)
   buf(1) = id
   kwds = kwds + nwds
   GOTO retx
!
!     IF PLOTS ARE REQUESTED, READ THE CSTM INTO CORE.
!     IF FIRST VECTOR, OPEN PUGV1 AND WRITE HEADER RECORD.
!
 3100 extra = 0
   IF ( iseq/=3 .OR. Plots==0 .OR. (kcount/=1 .AND. App(1)/=Trn(1)) ) GOTO 3700
   IF ( Symflg<0 ) GOTO 3700
   File = Cstm
   CALL open(*3300,Cstm,Z(Buf5),Rdrew)
   CALL fwdrec(*5200,Cstm)
   Icstm = Ivecn + 1
   CALL read(*5200,*3200,Cstm,Z(Icstm),Buf5-Icstm,1,Ncstm)
   CALL mesage(m8,0,Nam)
 3200 CALL close(Cstm,Clsrew)
   CALL pretrs(Z(Icstm),Ncstm)
 3300 IF ( jcount==1 ) THEN
      CALL makmcb(Mcb,Pugv1,J2,2,Qtype2)
      File = Pugv1
      CALL open(*3400,Pugv1,Z(Buf4),Wrtrew)
      kplot = 1
      CALL fname(Pugv1,buf)
      CALL write(Pugv1,buf,2,1)
   ENDIF
!
!     IF PLOT FILE IS PURGED, NO PLOT FILE CAN BE PREPARED.
!     IF TRANSIENT PROBLEM, REMOVE EXTRA POINTS FROM VECTOR
!     NOW IN CORE THUS CREATING A G-SET VECTOR.
!
 3400 extra = 0
   IF ( kplot==0 ) GOTO 3700
   IF ( App(1)==Trn(1) .OR. App(1)==Frq(1) .OR. App(1)==Cei(1) ) THEN
      DO i = 1 , neqex , 2
         j = Z(i+1)/10
         k = Z(i+1) - 10*j
         IF ( k==3 ) THEN
            extra = 1
            j = Ktype*j + Ivec - Ktype
            Z(j) = 1
            IF ( Ktype==2 ) Z(j+1) = 1
         ENDIF
      ENDDO
      IF ( extra/=0 ) THEN
         j = Ivec
         DO i = Ivec , Ivecn
            IF ( Z(i)/=1 ) THEN
               Z(j) = Z(i)
               j = j + 1
            ENDIF
         ENDDO
         Ivecn = j - 1
      ENDIF
   ENDIF
!
!     PASS THE BGPDT. FOR EACH ENTRY, ROTATE THE TRANSLATION COMPONENTS
!     OF UGV TO BASIC (IF REQUIRED). WRITE THESE COMPONENTS ON PUGV1.
!
   File = Bgpdt
   CALL open(*3700,Bgpdt,Z(Buf5),Rdrew)
   CALL fwdrec(*5200,Bgpdt)
   k = 0
   i = Ivec
   pbuff(1) = Z(icc+1)
   CALL write(Pugv1,pbuff,4,1)
   l = 3*Ktype
   CALL bldpk(Qtype2,Qtype2,Pugv1,0,0)
 3500 DO
      CALL read(*5200,*3600,Bgpdt,buf(7),4,0,flag)
      itemp = 0
      DO j = 1 , l
         ll = i + j - 1
         bufr(j) = Zz(ll)
      ENDDO
      IF ( buf(7)<0 ) THEN
!
!     CHECK FOR FLUID POINTS
!
         i = i + Ktype
         IF ( buf(7)==-2 ) THEN
            Iy = (i-Ivec+k)/Ktype + 2
            Y(1) = bufr(1)
            IF ( Qtype2==3 ) Y(2) = bufr(2)
            CALL zblpki
            k = k + 5*Ktype
         ENDIF
         CYCLE
      ELSEIF ( buf(7)/=0 ) THEN
!
!     TRANSFORM TO BASIC
!
         IF ( Qtype2/=1 ) THEN
            j = buf(2)
            buf(2) = buf(3)
            buf(3) = buf(5)
            buf(5) = buf(4)
            buf(4) = j
         ENDIF
         itemp = 19
         CALL transs(bufr(7),bufr(11))
         CALL gmmats(bufr(11),3,3,0,bufr(1),3,1,0,buf(itemp+1))
         IF ( Qtype2/=1 ) THEN
            CALL gmmats(bufr(11),3,3,0,bufr(4),3,1,0,buf(itemp+4))
            j = buf(21)
            buf(21) = buf(23)
            buf(23) = buf(24)
            buf(24) = buf(22)
            buf(22) = j
         ENDIF
      ENDIF
      EXIT
   ENDDO
   Iy = (i-Ivec+k)/Ktype
   DO j = 1 , l , Ktype
      Iy = Iy + 1
      ll = itemp + j
      Y(1) = bufr(ll)
      IF ( Ktype==2 ) Y(2) = bufr(ll+1)
      CALL zblpki
   ENDDO
   i = i + 6*Ktype
   GOTO 3500
 3600 CALL bldpkn(Pugv1,0,Mcb)
   CALL close(Bgpdt,Clsrew)
!
!     CONCLUDE PROCESSING OF THIS VECTOR.
!
 3700 IF ( setno/=0 ) CALL write(outfl,0,0,1)
 3800 IF ( Branch==2 .OR. Branch==4 .OR. Branch==8 .OR. Branch==9 .OR. Branch==10 ) THEN
!
!     EIGENVALUES OR DIFF. STIFF PHASE1 OR BUCKLING PHASE 1.
!
      jlist = jlist + ktype1
   ELSEIF ( Branch==3 .OR. Branch==7 ) THEN
      GOTO 4600
   ELSEIF ( Branch==5 ) THEN
      GOTO 4000
   ELSEIF ( Branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
      IF ( iseq<=2 ) GOTO 4400
      IF ( kcount<2 ) THEN
         ireq = Ivel
         kcount = 2
      ELSEIF ( kcount==2 ) THEN
         ireq = Iacc
         kcount = 3
      ELSE
         ireq = Idispl
         kcount = 1
         GOTO 4400
      ENDIF
      GOTO 1000
   ELSE
!
!     NORMAL STATICS.
!
      IF ( jcount<nvects ) GOTO 800
      IF ( eof/=0 ) GOTO 4600
      GOTO 800
   ENDIF
 3900 IF ( jcount>=nvects ) GOTO 4600
   IF ( eof/=0 ) GOTO 1000
   GOTO 800
!
!     FREQUENCY RESPONSE.
!
 4000 IF ( iseq<=2 ) GOTO 4300
   IF ( kcount==3 ) THEN
      kcount = 1
      ireq = Idispl
      GOTO 4300
   ELSE
      n = Ivecn - 1
      IF ( extra/=0 ) THEN
         CALL bckrec(infil)
         CALL unpack(*4100,infil,Z(Ivec))
      ENDIF
      omega = Twopi*Zz(jlist)
      DO i = Ivec , n , 2
         bufr(1) = -omega*Zz(i+1)
         Zz(i+1) = omega*Zz(i)
         Zz(i) = bufr(1)
      ENDDO
      GOTO 4200
   ENDIF
 4100 DO i = Ivec , n
      Zz(i) = 0.0
   ENDDO
 4200 IF ( kcount==2 ) THEN
      ireq = Iacc
   ELSE
      ireq = Ivel
   ENDIF
   kcount = kcount + 1
   incore = 1
   GOTO 1000
 4300 incore = 0
   jlist = jlist + 2
   IF ( jlist<=nlist .AND. jcount<nvects ) GOTO 1000
   kfrq = 0
   jlist = ilist
   DO i = ilist , nlist , 2
      Z(i+1) = 0
   ENDDO
   IF ( jcount>=nvects ) GOTO 4600
   GOTO 800
 4400 jlist = jlist + 2
   IF ( jlist>nlist .OR. jcount>=nvects ) GOTO 4600
   GOTO 1000
!
!     HERE WHEN END-OF-FILE ENCOUNTERED ON CASE CONTROL.
!
 4500 eof = 1
   IF ( Branch==2 .OR. Branch==4 .OR. Branch==8 .OR. Branch==9 .OR. Branch==10 ) GOTO 3900
!
!     CONCLUDE PROCESSING OF CURRENT INPUT FILE.
!
 4600 CALL close(Casecc,Clsrew)
   CALL close(infil,Clsrew)
   CALL close(outfl,Clsrew)
   IF ( kplot/=0 ) CALL close(Pugv1,Clsrew)
   IF ( kplot/=0 ) CALL wrttrl(Mcb)
   Ocb(2) = kwds/65536
   Ocb(3) = kwds - 65536*Ocb(2)
   IF ( anyout ) CALL wrttrl(Ocb)
!
!     TEST FOR ALL INPUT FILES PROCESSED.
!
 4700 iseq = iseq + 1
   IF ( iseq<=3 ) GOTO 200
   CALL close(Casecc,Clsrew)
   RETURN
!
!     HERE IF ABNORMAL CONDITION.
!     CLOSE ALL FILES, JUST TO BE SURE
!
 4800 CALL close(outfl,Clsrew)
 4900 CALL close(infil,Clsrew)
   CALL close(Casecc,Clsrew)
   ix = iseq + 75
   CALL mesage(30,ix,0)
   GOTO 4700
!
!     BINARY SEARCH ROUTINE.
!     =====================
!
 5000 klo = 1
   khi = kn
   IF ( Axic ) buf(1) = jharm*1000000 + buf(1)
   IF ( Axif>0 ) buf(1) = jharm*500000 + buf(1)
   k = (klo+khi+1)/2
   DO
      kx = 2*k - 1
      IF ( buf(1)<Z(kx) ) THEN
         khi = k
      ELSEIF ( buf(1)==Z(kx) ) THEN
         GOTO 2600
      ELSE
         klo = k
      ENDIF
      IF ( khi-klo<1 ) THEN
         GOTO retx
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
!
!     FATAL FILE ERRORS
!
 5100 n = -1
   GOTO 5300
 5200 n = -2
 5300 DO
      CALL mesage(n,File,Nam)
   ENDDO
END SUBROUTINE sdr2c
