!*==sdr2c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2c
   USE c_blank
   USE c_condas
   USE c_names
   USE c_sdr2x1
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_system
   USE c_unpakx
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: anyout , axcosi , axsine
   INTEGER :: axif , dest , eof , extra , flag , formt , fsetno , i , ibufsv , icc , id , iflag , iheat , ilist , incore , index ,  &
            & infil , ireq , ireqx , iseq , iset , isetf , isetnf , isetno , iskip , isymn , itabl , itemp , ix , ixset , ixsetn ,  &
            & j , jcount , jharm , jlist , jtj , k , kcount , kfrq , khi , klo , kn , kplot , ktype1 , ktypex , kwds , kx , l , ll ,&
            & lsym , m , m8 , n , ncc , neqex , nlist , nset , nsetf , nvects , nwds , nxset , oharms , outfl , plots , retx ,      &
            & setno , sorc , sysbuf , time , xsetno
   INTEGER , SAVE :: blanks , mmreig , xset0
   INTEGER , DIMENSION(50) , SAVE :: buf
   REAL , DIMENSION(11) :: bufr
   REAL :: coef , diff , diff1 , omega , redner
   INTEGER , DIMENSION(3) :: date
   INTEGER , DIMENSION(4) :: pbuff
   REAL , DIMENSION(4) :: pbufr
   INTEGER , DIMENSION(12) , SAVE :: platit
   REAL , DIMENSION(1) :: zz
   EXTERNAL bckrec , bldpk , bldpkn , close , fname , fwdrec , gmmats , gopen , int2al , intpk , korsz , makmcb , mesage , open ,   &
          & pretrs , rdtrl , read , skprec , transs , unpack , write , wrttrl , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SDR2C PROCESSES OUTPUT REQUESTS FOR SINGLE-POINT FORCES OF
!     CONSTRAINT, LOADS, DISPLACEMENTS, VELOCITIES, ACCELERATIONS AND
!     EIGENVECTORS.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(15),Date(1)) , (Ksystm(18),Time) , (Ksystm(20),Plots) , (Ksystm(38),Axif) ,             &
!>>>>    & (Ksystm(56),Iheat) , (buf(1),bufr(1)) , (Z(1),Zz(1)) , (pbuff(1),pbufr(1))
   DATA buf/50*0/
   DATA blanks/4H    /
   DATA xset0/100000000/
   DATA platit/4HLOAD , 4H FAC , 4HTOR  , 9*0/
   DATA mmreig/4HMMRE/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     IF THIS IS A DYNAMIC-DATA-RECOVERY-MATRIX-METHOD REIG PROBLEM
!     THEN ALL EIGENVECTORS ARE TO BE OUTPUT FOR THE DDRMM MODULE.
!
         setno = 0
         IF ( ddrmm .AND. ireq==idispl ) setno = -1
!
!     PERFORM GENERAL INITIALIZATION
!
         buf1 = korsz(z) - sysbuf + 1
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         iseq = 1
         m8 = -8
         i2 = 1
         incr2 = 1
         kplot = 0
         extra = 0
         axsine = .FALSE.
         axcosi = .FALSE.
!
!     READ SECOND RECORD OF EQEXIN OR EQDYN INTO CORE.
!
         file = eqexin
         CALL gopen(eqexin,z(buf1),0)
         CALL skprec(eqexin,1)
         CALL read(*420,*20,eqexin,z,buf5,1,neqex)
         CALL mesage(m8,0,nam)
 20      CALL close(eqexin,clsrew)
         itabl = 1
         kn = neqex/2
         icc = neqex
         ilist = neqex + 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     INITIALIZE FOR PROCESSING SPECIFIC REQUEST.
!
         IF ( iseq<2 ) THEN
!
!     LOAD VECTOR.
!
            IF ( loads==0 .OR. app(1)==rei(1) .OR. app(1)==cei(1) .OR. app(1)==bk1(1) ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            infil = 115
            outfl = opg1
            ireq = iloads
         ELSEIF ( iseq==2 ) THEN
!
!     SINGLE-POINT FORCES OF CONSTRAINT.
!
            IF ( spcf==0 ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            infil = qg
            outfl = oqg1
            ireq = ispcf
         ELSE
!
!     DISPLACEMENT VECTOR OR EIGENVECTOR
!
            IF ( displ==0 .AND. vel==0 .AND. acc==0 .AND. plots==0 ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            infil = ugv
            outfl = ougv1
            jtj = vel + acc
            IF ( app(1)/=mmreig .OR. displ/=0 .OR. jtj==0 ) THEN
               ireq = idispl
            ELSEIF ( vel==0 ) THEN
               ireq = iacc
            ELSE
               ireq = ivel
            ENDIF
         ENDIF
!
!     READ TRAILER ON INPUT FILE. SET PARAMETERS.
!
         icb(1) = infil
         CALL rdtrl(icb)
         IF ( icb(1)/=infil ) GOTO 380
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
         CALL gopen(casecc,z(buf1),0)
         pbuff(2) = 1
         IF ( branch==1 .OR. branch==3 .OR. branch==7 .OR. branch==10 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( branch==4 .OR. branch==8 ) THEN
!
!     DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1 - SKIP 1ST DATA RECORD ON
!     CC.
!
            CALL skprec(casecc,1)
            pbuff(2) = 4
            IF ( app(1)/=bk1(1) ) THEN
               pbuff(2) = 1
               spag_nextblock_1 = 4
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
            pbuff(2) = 3
            IF ( app(1)==frq(1) ) pbuff(2) = 2
            IF ( app(1)==frq(1) .OR. app(1)==trn(1) ) ix = 2
            DO
               CALL read(*420,*80,file,buf,m,0,flag)
               z(i) = buf(m)
               z(i+1) = 0
               i = i + ix
               m = 1
            ENDDO
         ENDIF
!
!     EIGENVALUES - READ LIST OF MODE NOS. AND EIGENVALUES INTO CORE.
!
         file = eigr
         CALL gopen(eigr,z(buf2),0)
         CALL skprec(eigr,1)
         IF ( app(1)==cei(1) ) pbuff(2) = 5
         IF ( app(1)==rei(1) ) pbuff(2) = 4
         i = ilist
         m = 8 - ktype
         iskip = 0
         index = 2
         IF ( app(1)/=rei(1) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_1: DO
!
!     CHECK TO SEE IF ALL GENERALIZED MASS VALUES ARE ZERO
!
            CALL read(*420,*40,eigr,buf,m,0,flag)
            IF ( buf(6)/=0.0 ) THEN
               index = 0
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
 40      CALL skprec(eigr,-1)
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_2: DO
            CALL read(*420,*60,eigr,buf,m,0,flag)
            IF ( app(1)/=rei(1) ) EXIT SPAG_Loop_1_2
            IF ( index==2 ) EXIT SPAG_Loop_1_2
!
!     MATCH CORRECT MODE NOS. AND EIGENVALUES WITH PROPER
!     EIGENVECTORS WHEN USING GIVENS METHOD WITH F1.GT.0.0
!
            IF ( index==1 ) EXIT SPAG_Loop_1_2
            IF ( buf(6)/=0. ) THEN
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
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(eigr,clsrew)
         nlist = i - ktype1
         icc = i
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      CALL close(file,clsrew)
         nlist = i - ix
         icc = i
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPEN OUTPUT FILE. WRITE HEADER RECORD.
!
         file = outfl
         anyout = .FALSE.
         CALL open(*380,outfl,z(buf2),wrtrew)
         ocb(1) = outfl
         CALL fname(outfl,buf)
         DO i = 1 , 3
            buf(i+2) = date(i)
         ENDDO
         buf(6) = time
         buf(7) = 1
         CALL write(outfl,buf,7,1)
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
         file = infil
         CALL open(*360,infil,z(buf3),rdrew)
         CALL fwdrec(*420,infil)
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
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ A RECORD IN CASE CONTROL. SET SYMMETRY FLAG.
!
         CALL read(*340,*100,casecc,z(icc+1),buf5-icc,1,ncc)
         CALL mesage(m8,0,nam)
 100     ix = icc + isymfl
         itemp = icc + harms
!
!     OHARMS WILL BE 1 GREATER THAN THE MAXIMUM OUTPUT HARMONIC
!
         oharms = z(itemp)
         IF ( oharms<0 .AND. axif/=0 ) oharms = axif
         IF ( oharms<0 ) oharms = nharms
!
!     IF A FLUID PROBLEM CONVERT USER HARMONIC TO INTERNAL HARMONIC MAX.
!
         IF ( oharms/=0 ) THEN
            IF ( axif/=0 ) THEN
               oharms = oharms - 1
               oharms = 2*oharms + 3
            ENDIF
         ENDIF
         symflg = z(ix)
         IF ( symflg==0 ) sorc = z(icc+isorc)
         IF ( sorc==1 ) axsine = .TRUE.
         IF ( sorc==2 ) axcosi = .TRUE.
         iflag = 0
         IF ( axic .AND. axsine .AND. axcosi .AND. jcount==2 ) iflag = 1
         ivec = icc + ncc + 1
         spag_nextblock_1 = 6
      CASE (6)
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT.
!     IF NOT, TEST FOR RECORD SKIP ON INFIL  THEN GO TO END OF THIS
!     REQUEST.
!     IF SO, SET POINTERS TO SET DEFINING REQUEST.
!
         ireqx = icc + ireq
         setno = z(ireqx)
         dest = z(ireqx+1)
         formt = iabs(z(ireqx+2))
         xsetno = -1
         IF ( setno<0 ) THEN
         ELSEIF ( setno==0 ) THEN
            IF ( symflg/=0 ) THEN
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( app(1)==frq(1) ) THEN
               IF ( iseq==3 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            IF ( plots==0 ) THEN
               CALL fwdrec(*420,infil)
               jcount = jcount + 1
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            ix = icc + ilsym
            isetno = ix + z(ix) + 1
            SPAG_Loop_1_3: DO
               iset = isetno + 2
               nset = z(isetno+1) + iset - 1
               IF ( z(isetno)==setno ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET.
!
                  IF ( setno<xset0 ) EXIT SPAG_Loop_1_3
                  xsetno = dest/10
                  dest = dest - 10*xsetno
                  IF ( xsetno==0 ) EXIT SPAG_Loop_1_3
                  ixsetn = ix + z(ix) + 1
                  DO
                     ixset = ixsetn + 2
                     nxset = z(ixsetn+1) + ixset - 1
                     IF ( z(ixsetn)==xsetno ) EXIT SPAG_Loop_1_3
                     ixsetn = nxset + 1
                     IF ( ixsetn>=ivec ) THEN
                        xsetno = -1
                        EXIT SPAG_Loop_1_3
                     ENDIF
                  ENDDO
               ELSE
                  isetno = nset + 1
                  IF ( isetno>=ivec ) THEN
                     setno = -1
                     EXIT SPAG_Loop_1_3
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     UNPACK VECTOR INTO CORE (UNLESS VECTOR IS ALREADY IN CORE).
!
         IF ( incore/=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ivecn = ivec + ktype*icb(3) - 1
         IF ( ivecn>=buf5 ) CALL mesage(m8,0,nam)
         IF ( symflg==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SYMMETRY SEQUENCE - BUILD VECTOR IN CORE.
!
         ix = icc + ilsym
         lsym = z(ix)
!
!     IF SYMFLG IS NEGATIVE THIS IS A REPEAT SUBCASE. BCKREC VECTOR
!     AND READ IT INTO CORE.
!
         IF ( symflg<0 .AND. app(1)==sta(1) ) THEN
!
!     REPEAT SUBCASE
!
            jcount = jcount - 1
            CALL bckrec(infil)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( symflg<0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = ivec , ivecn
               zz(i) = 0.
            ENDDO
            DO i = 1 , lsym
               CALL bckrec(infil)
            ENDDO
            isymn = ix + lsym
            i = ix + 1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         coef = zz(i)
         CALL intpk(*120,infil,0,qtype2,0)
         SPAG_Loop_1_4: DO
            CALL zntpki
            ix = ivec + ixx - 1
            zz(ix) = zz(ix) + coef*xx(1)
            IF ( ktype==2 ) zz(ix+1) = zz(ix+1) + coef*xx(2)
            IF ( eol/=0 ) EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
 120     i = i + 1
         IF ( i<=isymn ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (9)
!
!     NOT SYMMETRY - UNPACK VECTOR.
!
         j2 = icb(3)
         IF ( jcount>=nvects ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL unpack(*140,infil,z(ivec))
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 140     DO i = ivec , ivecn
            zz(i) = 0.
         ENDDO
         spag_nextblock_1 = 10
      CASE (10)
         jcount = jcount + 1
         spag_nextblock_1 = 11
      CASE (11)
!
!     TEST FOR CONTINUATION FROM HERE.
!
         IF ( setno==0 ) THEN
            IF ( app(1)==frq(1) ) THEN
               spag_nextblock_1 = 26
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     PREPARE TO WRITE ID RECORD ON OUTPUT FILE.
!
         IF ( branch==2 .OR. branch==8 .OR. branch==9 ) THEN
!
!     EIGENVALUES OR BUCKLING PHASE 1.
!
            IF ( iseq==2 ) buf(2) = ktypex + 3
            IF ( iseq==3 ) buf(2) = ktypex + 7
            buf(5) = z(jlist)
            buf(6) = z(jlist+1)
            buf(7) = z(jlist+2)
            buf(8) = 0
!     PBUFF(2) = 2  THIS CARD WAS REMOVED SINCE LEVEL 16. NO LONGER NEED
            pbuff(3) = buf(5)
            IF ( app(1)==bk1(1) ) pbuff(3) = -buf(5)
            pbuff(4) = buf(6)
            IF ( app(1)/=bk1(1) .AND. app(1)/=cei(1) ) pbufr(4) = sqrt(abs(bufr(6)))/twopi
            IF ( app(1)==cei(1) ) pbufr(4) = abs(bufr(7))/twopi
         ELSEIF ( branch==5 ) THEN
!
!     FREQUENCY RESPONSE.
!
            ix = icc + idload
            buf(8) = z(ix)
            buf(6) = 0
            buf(7) = 0
            pbuff(2) = 2
            pbuff(3) = buf(8)
            IF ( iseq/=3 ) THEN
               buf(2) = dtype(iseq) + ktypex
            ELSEIF ( kcount<2 ) THEN
               buf(2) = 1001
            ELSEIF ( kcount==2 ) THEN
               buf(2) = 1010
            ELSE
               buf(2) = 1011
            ENDIF
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
            buf(5) = z(jlist)
            IF ( kcount<2 ) THEN
               buf(2) = 1
            ELSEIF ( kcount==2 ) THEN
               buf(2) = 10
            ELSE
               buf(2) = 11
            ENDIF
            IF ( ireq==iloads ) buf(2) = 2
            IF ( ireq==ispcf ) buf(2) = 3
            ix = icc + idload
            buf(8) = z(ix)
            buf(6) = 0
            buf(7) = 0
            pbuff(2) = 3 + 10*(kcount-1)
            pbuff(3) = buf(8)
            pbuff(4) = buf(5)
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NORMAL STATICS OR DIFF.STIFF. PHASE O OR 1 OR BUCKLING PHASE 0.
!
            buf(2) = dtype(iseq)
            ix = icc + isload
            buf(5) = z(icc+1)
            buf(6) = 0
            buf(7) = 0
            buf(8) = z(ix)
            pbuff(2) = 1
            pbuff(3) = z(ix)
            pbuff(4) = 0
            IF ( branch==10 ) THEN
               ix = icc + ittl + 84
               z(ix) = platit(1)
               z(ix+1) = platit(2)
               z(ix+2) = platit(3)
               CALL int2al(jcount,z(ix+3),platit(4))
            ENDIF
         ENDIF
         spag_nextblock_1 = 14
      CASE (12)
         IF ( kfrq==0 ) THEN
!
!     FIRST TIME FOR THIS LOAD VECTOR ONLY - MATCH LIST OF USER
!     REQUESTED FREQS WITH ACTUAL FREQS. MARK FOR OUTPUT EACH ACTUAL
!     FREQ WHICH IS CLOSEST TO USER REQUEST.
!
            kfrq = 1
            ix = icc + ifrout
            fsetno = z(ix)
            IF ( fsetno>0 ) THEN
               ix = icc + ilsym
               isetnf = ix + z(ix) + 1
               SPAG_Loop_1_5: DO
                  isetf = isetnf + 2
                  nsetf = z(isetnf+1) + isetf - 1
                  IF ( z(isetnf)==fsetno ) THEN
                     DO i = isetf , nsetf
                        k = 0
                        diff = 1.E25
                        bufr(1) = zz(i)
                        DO j = ilist , nlist , 2
                           IF ( z(j+1)==0 ) THEN
                              diff1 = abs(zz(j)-bufr(1))
                              IF ( diff1<diff ) THEN
                                 diff = diff1
                                 k = j
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( k/=0 ) z(k+1) = 1
                     ENDDO
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     isetnf = nsetf + 1
                     IF ( isetnf>=ivec ) THEN
                        fsetno = -1
                        EXIT SPAG_Loop_1_5
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_5
            ENDIF
            DO j = ilist , nlist , 2
               z(j+1) = 1
            ENDDO
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
!
!     DETERMINE IF CURRENT FREQ IS MARKED FOR OUTPUT.
!
         IF ( z(jlist+1)==0 ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         buf(5) = z(jlist)
         pbuff(4) = buf(5)
         spag_nextblock_1 = 14
      CASE (14)
!
!     WRITE ID RECORD ON OUTPUT FILE.
!
         IF ( setno==0 .AND. plots/=0 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         buf(1) = dest + 10*branch
         buf(3) = 0
!
!     IF CONICAL SHELL PROBLEM, SET MINOR ID = 1000 FOR USE BY OFP
!
         IF ( axic ) buf(3) = 1000
         buf(4) = z(icc+1)
         IF ( ddrmm ) buf(4) = 9999
         buf(9) = iabs(z(ireqx+2))
         IF ( buf(9)==1 .AND. ktype==2 ) buf(9) = 2
         formt = buf(9)
         buf(10) = nwds
         CALL write(outfl,buf,50,0)
         ix = icc + ittl
         CALL write(outfl,z(ix),96,1)
!
!     BUILD DATA RECORD ON OUTPUT FILE.
!
         IF ( setno/=-1 ) THEN
!
!     SET .NE. ALL  -  OUTPUT ONLY POINTS DEFINED IN SET.
!
            jharm = 0
            i = iset
            spag_nextblock_1 = 15
         ELSE
!
!     SET .EQ. ALL  -  OUTPUT ALL POINTS DEFINED IN EQEXIN.
!
            kx = 1
            n = neqex - 1
            ASSIGN 160 TO retx
            spag_nextblock_1 = 17
         ENDIF
         CYCLE
 160     kx = kx + 2
         IF ( kx<=n ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 22
      CASE (15)
         IF ( i==nset ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( z(i+1)>0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = -z(i+1)
         buf(1) = z(i)
         ibufsv = buf(1)
         i = i + 1
         ASSIGN 180 TO retx
         spag_nextblock_1 = 32
         CYCLE SPAG_DispatchLoop_1
 180     buf(1) = ibufsv + 1
         ibufsv = buf(1)
         IF ( buf(1)<=n ) THEN
            spag_nextblock_1 = 32
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 200
      CASE (16)
         buf(1) = z(i)
         ASSIGN 200 TO retx
         spag_nextblock_1 = 32
         CYCLE SPAG_DispatchLoop_1
 200     i = i + 1
         IF ( i<=nset ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         jharm = jharm + 1
         IF ( .NOT.axic .AND. axif==0 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jharm>oharms ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = iset
         spag_nextblock_1 = 15
      CASE (17)
!
!     PICK UP POINTER TO GRID POINT DATA AND GRID POINT TYPE.
!
         buf(1) = z(kx)
         IF ( iflag==1 .AND. buf(1)>=1000000 ) GOTO retx
         j = z(kx+1)/10
         buf(2) = z(kx+1) - 10*j
         j = ivec + ktype*(j-1)
         IF ( buf(2)==1 ) THEN
!
!     GRID POINT.
!
            flag = 0
            IF ( ktype==2 ) THEN
!
!     COMPLEX GRID POINT.
!
               DO k = 1 , 11 , 2
                  bufr(k+2) = zz(j)
                  bufr(k+3) = zz(j+1)
                  IF ( bufr(k+2)/=0. .OR. bufr(k+3)/=0. .OR. sort2>=0 ) flag = 1
                  IF ( formt==3 ) THEN
                     redner = sqrt(bufr(k+2)**2+bufr(k+3)**2)
                     IF ( redner/=0 ) THEN
                        bufr(k+3) = atan2(bufr(k+3),bufr(k+2))*raddeg
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
                  bufr(k+2) = zz(j)
                  IF ( bufr(k+2)/=0.0 .OR. sort2>=0 ) flag = 1
                  j = j + 1
               ENDDO
               IF ( iseq<=2 .AND. flag==0 ) GOTO retx
            ENDIF
         ELSE
!
!     SCALAR OR EXTRA POINT.
!
            buf(3) = z(j)
            IF ( ktype==2 ) THEN
!
!     COMPLEX SCALAR OR EXTRA POINT.
!
               buf(4) = z(j+1)
               IF ( iseq<=2 .AND. bufr(3)==0.0 .AND. bufr(4)==0.0 .AND. sort2<0 ) GOTO retx
               DO k = 5 , 14
                  buf(k) = 0
               ENDDO
               IF ( formt==3 ) THEN
                  redner = sqrt(bufr(3)**2+bufr(4)**2)
                  IF ( redner/=0 ) THEN
                     bufr(4) = atan2(bufr(4),bufr(3))*raddeg
                     IF ( bufr(4)<-0.00005 ) bufr(4) = bufr(4) + 360.0
                  ELSE
                     bufr(4) = 0.0
                  ENDIF
                  bufr(3) = redner
               ENDIF
            ELSE
               IF ( iseq<=2 .AND. bufr(3)==0.0 .AND. sort2<0 ) GOTO retx
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
         IF ( axic ) THEN
            IF ( buf(1)>=1000000 ) THEN
               itemp = buf(1)/1000000
!
!     STOP OUTPUT WHEN PRESENT HARMONIC EXCEEDS OUTPUT HARMONIC SIZE REQ
!
               IF ( itemp>oharms ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               buf(1) = buf(1) - itemp*1000000
               buf(2) = itemp - 1
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ELSE
               buf(2) = blanks
            ENDIF
         ENDIF
!
!     IF A FLUID PROBLEM THEN A CHECK IS MADE ON THE HARMONIC ID
!
         IF ( axif>0 ) THEN
            IF ( buf(1)>=500000 ) THEN
               itemp = buf(1) - mod(buf(1),500000)
               itemp = itemp/500000
!
!     STOP THE OUTPUT IF THE HARMONIC IS GREATER THAN THE OUTPUT
!     REQUEST FOR HARMONICS
!
               IF ( itemp>=oharms ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 18
      CASE (18)
!
!     DETERMINE DESTINATION FOR ENTRY.
!
         id = buf(1)
         buf(1) = 10*id + dest
         IF ( xsetno<0 ) THEN
         ELSEIF ( xsetno==0 ) THEN
            buf(1) = 10*id
         ELSE
            ix = ixset
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (19)
         IF ( ix/=nxset ) THEN
            IF ( z(ix+1)<=0 ) THEN
               IF ( id>=z(ix) .AND. id<=-z(ix+1) ) THEN
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ix = ix + 2
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==z(ix) ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ix = ix + 1
         spag_nextblock_1 = 20
      CASE (20)
         IF ( ix<=nxset ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         buf(1) = 10*id
         spag_nextblock_1 = 21
      CASE (21)
!
!     NOW WRITE ENTRY.
!
         CALL write(outfl,buf(1),nwds,0)
         buf(1) = id
         kwds = kwds + nwds
         GOTO retx
      CASE (22)
!
!     IF PLOTS ARE REQUESTED, READ THE CSTM INTO CORE.
!     IF FIRST VECTOR, OPEN PUGV1 AND WRITE HEADER RECORD.
!
         extra = 0
         IF ( iseq/=3 .OR. plots==0 .OR. (kcount/=1 .AND. app(1)/=trn(1)) ) GOTO 300
         IF ( symflg<0 ) GOTO 300
         file = cstm
         CALL open(*240,cstm,z(buf5),rdrew)
         CALL fwdrec(*420,cstm)
         icstm = ivecn + 1
         CALL read(*420,*220,cstm,z(icstm),buf5-icstm,1,ncstm)
         CALL mesage(m8,0,nam)
 220     CALL close(cstm,clsrew)
         CALL pretrs(z(icstm),ncstm)
 240     IF ( jcount==1 ) THEN
            CALL makmcb(mcb,pugv1,j2,2,qtype2)
            file = pugv1
            CALL open(*260,pugv1,z(buf4),wrtrew)
            kplot = 1
            CALL fname(pugv1,buf)
            CALL write(pugv1,buf,2,1)
         ENDIF
!
!     IF PLOT FILE IS PURGED, NO PLOT FILE CAN BE PREPARED.
!     IF TRANSIENT PROBLEM, REMOVE EXTRA POINTS FROM VECTOR
!     NOW IN CORE THUS CREATING A G-SET VECTOR.
!
 260     extra = 0
         IF ( kplot==0 ) GOTO 300
         IF ( app(1)==trn(1) .OR. app(1)==frq(1) .OR. app(1)==cei(1) ) THEN
            DO i = 1 , neqex , 2
               j = z(i+1)/10
               k = z(i+1) - 10*j
               IF ( k==3 ) THEN
                  extra = 1
                  j = ktype*j + ivec - ktype
                  z(j) = 1
                  IF ( ktype==2 ) z(j+1) = 1
               ENDIF
            ENDDO
            IF ( extra/=0 ) THEN
               j = ivec
               DO i = ivec , ivecn
                  IF ( z(i)/=1 ) THEN
                     z(j) = z(i)
                     j = j + 1
                  ENDIF
               ENDDO
               ivecn = j - 1
            ENDIF
         ENDIF
!
!     PASS THE BGPDT. FOR EACH ENTRY, ROTATE THE TRANSLATION COMPONENTS
!     OF UGV TO BASIC (IF REQUIRED). WRITE THESE COMPONENTS ON PUGV1.
!
         file = bgpdt
         CALL open(*300,bgpdt,z(buf5),rdrew)
         CALL fwdrec(*420,bgpdt)
         k = 0
         i = ivec
         pbuff(1) = z(icc+1)
         CALL write(pugv1,pbuff,4,1)
         l = 3*ktype
         CALL bldpk(qtype2,qtype2,pugv1,0,0)
         spag_nextblock_1 = 23
      CASE (23)
         SPAG_Loop_1_6: DO
            CALL read(*420,*280,bgpdt,buf(7),4,0,flag)
            itemp = 0
            DO j = 1 , l
               ll = i + j - 1
               bufr(j) = zz(ll)
            ENDDO
            IF ( buf(7)<0 ) THEN
!
!     CHECK FOR FLUID POINTS
!
               i = i + ktype
               IF ( buf(7)==-2 ) THEN
                  iy = (i-ivec+k)/ktype + 2
                  y(1) = bufr(1)
                  IF ( qtype2==3 ) y(2) = bufr(2)
                  CALL zblpki
                  k = k + 5*ktype
               ENDIF
               CYCLE
            ELSEIF ( buf(7)/=0 ) THEN
!
!     TRANSFORM TO BASIC
!
               IF ( qtype2/=1 ) THEN
                  j = buf(2)
                  buf(2) = buf(3)
                  buf(3) = buf(5)
                  buf(5) = buf(4)
                  buf(4) = j
               ENDIF
               itemp = 19
               CALL transs(bufr(7),bufr(11))
               CALL gmmats(bufr(11),3,3,0,bufr(1),3,1,0,buf(itemp+1))
               IF ( qtype2/=1 ) THEN
                  CALL gmmats(bufr(11),3,3,0,bufr(4),3,1,0,buf(itemp+4))
                  j = buf(21)
                  buf(21) = buf(23)
                  buf(23) = buf(24)
                  buf(24) = buf(22)
                  buf(22) = j
               ENDIF
            ENDIF
            EXIT SPAG_Loop_1_6
         ENDDO SPAG_Loop_1_6
         iy = (i-ivec+k)/ktype
         DO j = 1 , l , ktype
            iy = iy + 1
            ll = itemp + j
            y(1) = bufr(ll)
            IF ( ktype==2 ) y(2) = bufr(ll+1)
            CALL zblpki
         ENDDO
         i = i + 6*ktype
         spag_nextblock_1 = 23
         CYCLE SPAG_DispatchLoop_1
 280     CALL bldpkn(pugv1,0,mcb)
         CALL close(bgpdt,clsrew)
!
!     CONCLUDE PROCESSING OF THIS VECTOR.
!
 300     IF ( setno/=0 ) CALL write(outfl,0,0,1)
         spag_nextblock_1 = 24
      CASE (24)
         IF ( branch==2 .OR. branch==4 .OR. branch==8 .OR. branch==9 .OR. branch==10 ) THEN
!
!     EIGENVALUES OR DIFF. STIFF PHASE1 OR BUCKLING PHASE 1.
!
            jlist = jlist + ktype1
         ELSEIF ( branch==3 .OR. branch==7 ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( branch==5 ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
            IF ( iseq<=2 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kcount<2 ) THEN
               ireq = ivel
               kcount = 2
            ELSEIF ( kcount==2 ) THEN
               ireq = iacc
               kcount = 3
            ELSE
               ireq = idispl
               kcount = 1
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NORMAL STATICS.
!
            IF ( jcount<nvects ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( eof==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 25
      CASE (25)
         IF ( jcount>=nvects ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( eof==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (26)
!
!     FREQUENCY RESPONSE.
!
         IF ( iseq<=2 ) THEN
            spag_nextblock_1 = 28
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( kcount==3 ) THEN
            kcount = 1
            ireq = idispl
            spag_nextblock_1 = 28
         ELSE
            n = ivecn - 1
            IF ( extra/=0 ) THEN
               CALL bckrec(infil)
               CALL unpack(*320,infil,z(ivec))
            ENDIF
            omega = twopi*zz(jlist)
            DO i = ivec , n , 2
               bufr(1) = -omega*zz(i+1)
               zz(i+1) = omega*zz(i)
               zz(i) = bufr(1)
            ENDDO
            spag_nextblock_1 = 27
         ENDIF
         CYCLE
 320     DO i = ivec , n
            zz(i) = 0.0
         ENDDO
         spag_nextblock_1 = 27
      CASE (27)
         IF ( kcount==2 ) THEN
            ireq = iacc
         ELSE
            ireq = ivel
         ENDIF
         kcount = kcount + 1
         incore = 1
         spag_nextblock_1 = 6
      CASE (28)
         incore = 0
         jlist = jlist + 2
         IF ( jlist<=nlist .AND. jcount<nvects ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kfrq = 0
         jlist = ilist
         DO i = ilist , nlist , 2
            z(i+1) = 0
         ENDDO
         IF ( jcount<nvects ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 30
      CASE (29)
         jlist = jlist + 2
         IF ( jlist<=nlist .AND. jcount<nvects ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 30
         CYCLE SPAG_DispatchLoop_1
!
!     HERE WHEN END-OF-FILE ENCOUNTERED ON CASE CONTROL.
!
 340     eof = 1
         IF ( branch==2 .OR. branch==4 .OR. branch==8 .OR. branch==9 .OR. branch==10 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 30
      CASE (30)
!
!     CONCLUDE PROCESSING OF CURRENT INPUT FILE.
!
         CALL close(casecc,clsrew)
         CALL close(infil,clsrew)
         CALL close(outfl,clsrew)
         IF ( kplot/=0 ) CALL close(pugv1,clsrew)
         IF ( kplot/=0 ) CALL wrttrl(mcb)
         ocb(2) = kwds/65536
         ocb(3) = kwds - 65536*ocb(2)
         IF ( anyout ) CALL wrttrl(ocb)
         spag_nextblock_1 = 31
      CASE (31)
!
!     TEST FOR ALL INPUT FILES PROCESSED.
!
         iseq = iseq + 1
         IF ( iseq<=3 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(casecc,clsrew)
         RETURN
!
!     HERE IF ABNORMAL CONDITION.
!     CLOSE ALL FILES, JUST TO BE SURE
!
 360     CALL close(outfl,clsrew)
 380     CALL close(infil,clsrew)
         CALL close(casecc,clsrew)
         ix = iseq + 75
         CALL mesage(30,ix,0)
         spag_nextblock_1 = 31
      CASE (32)
!
!     BINARY SEARCH ROUTINE.
!     =====================
!
         klo = 1
         khi = kn
         IF ( axic ) buf(1) = jharm*1000000 + buf(1)
         IF ( axif>0 ) buf(1) = jharm*500000 + buf(1)
         k = (klo+khi+1)/2
         DO
            kx = 2*k - 1
            IF ( buf(1)<z(kx) ) THEN
               khi = k
            ELSEIF ( buf(1)==z(kx) ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
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
 400     n = -1
         spag_nextblock_1 = 33
         CYCLE SPAG_DispatchLoop_1
 420     n = -2
         spag_nextblock_1 = 33
      CASE (33)
         DO
            CALL mesage(n,file,nam)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdr2c
