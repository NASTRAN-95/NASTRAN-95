!*==vdrb.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vdrb(Infil,Outfl,Ireqq)
   USE c_bitpos
   USE c_blank
   USE c_condas
   USE c_names
   USE c_system
   USE c_two
   USE c_unpakx
   USE c_vdrcom
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Infil
   INTEGER :: Outfl
   INTEGER :: Ireqq
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: branch , code , dest , eof , file , flag , format , fsetno , gptype , i , icc , id , ilist , incore , ireq , ireqx ,  &
            & iset , isetf , isetnf , isetno , itemp , iusetd , ivec , ivecn , ix , ixset , ixsetn , j , jcount , jharm , jlist ,   &
            & k , kcount , kfrq , khi , klo , kn , ktype , ktypex , kwds , kx , l , m , m8 , mskud , mskue , n , nbrep , nbrmod ,   &
            & ncc , ncore , neqd , neqdyn , nlist , nrows , nset , nsetf , nusetd , nvects , nwds , nxset , oharms , ret , retx ,   &
            & setno , sild , word , xsetno
   REAL , DIMENSION(50) :: bufr
   REAL :: clsrrw , diff , diff1 , omega , raddeg , redner , twopi
   INTEGER , SAVE :: iese , igpf , ireig
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(1) :: zz
   EXTERNAL andf , close , fname , fwdrec , gopen , mesage , open , rdtrl , read , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     VDRB PROCESSES VECTORS IN THE ANALYSIS OR MODAL SET. IN
!     ACCORDANCE WITH OUTPUT REQUESTS IN THE CASE CONTROL DATA BLOCK,
!     THESE VECTORS ARE FORMATTED FOR INPUT TO OFP WHERE ACTUAL OUTPUT
!     WILL OCCUR.
!
   !>>>>EQUIVALENCE (Consts(2),Twopi) , (Consts(3),Raddeg) , (Buf(1),Bufr(1)) , (Z(1),Zz(1))
   DATA igpf , iese , ireig/167 , 170 , 4HREIG/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERAL INITIALIZATION.
!
         m8 = -8
         mskud = two(ud)
         mskue = two(ue)
         ilist = 1
         i2 = 1
         incr2 = 1
         ireq = Ireqq
         IF ( form(1)/=modal(1) .AND. form(1)/=direct(1) ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ TRAILER ON USETD. SET NO. OF EXTRA POINTS.
!     READ TRAILER ON INFIL. SET PARAMETERS.
!     IF MODAL PROBLEM, NO. OF MODES = NO. OF ROWS IN VECTOR - NO. XTRA
!     PTS.
!
         mcb(1) = usetd
         file = usetd
         CALL rdtrl(mcb)
         IF ( mcb(1)/=usetd ) GOTO 400
         nbrep = mcb(3)
         mcb(1) = Infil
         CALL rdtrl(mcb)
         IF ( mcb(1)/=Infil ) GOTO 380
         nvects = mcb(2)
         nrows = mcb(3)
         IF ( form(1)==modal(1) ) nbrmod = imode + nrows - nbrep - 1
         IF ( mcb(5)<=2 ) THEN
            IF ( app(1)/=frq(1) ) THEN
!
!     REAL VECTOR.
!
               ktype = 1
               qtype2 = 1
               nwds = 8
               ktypex = 0
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     COMPLEX VECTOR.
!
         ktype = 2
         qtype2 = 3
         nwds = 14
         ktypex = 1000
         spag_nextblock_1 = 2
      CASE (2)
!
!     IF DIRECT PROBLEM OR MODAL PROBLEM WITH EXTRA POINTS,
!     READ 2ND TABLE OF EQDYN INTO CORE. THEN READ USETD INTO CORE.
!
         IF ( form(1)==modal(1) .AND. nbrep==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = eqdyn
         CALL gopen(eqdyn,z(buf1),0)
         CALL fwdrec(*420,eqdyn)
         CALL read(*420,*20,eqdyn,z,buf1,1,neqd)
         CALL mesage(m8,0,nam)
 20      CALL close(eqdyn,clsrew)
         iusetd = neqd + 1
         ncore = buf1 - iusetd
         file = usetd
         CALL gopen(usetd,z(buf1),0)
         CALL read(*420,*40,usetd,z(iusetd),ncore,1,flag)
         CALL mesage(m8,0,nam)
 40      CALL close(usetd,clsrrw)
         ilist = iusetd
         neqdyn = neqd - 1
         kn = neqd/2
!
!     BRANCH ON PROBLEM TYPE.
!
         IF ( form(1)==modal(1) ) THEN
!
!     MODAL - PROCESS EACH ENTRY IN EQDYN. IF POINT IS NOT AN EXTRA
!             POINT, REPLACE SILD NO. WITH ZERO. OTHERWISE, REPLACE SILD
!             NO. WITH POSITION IN MODAL SET (I.E. ROW INDEX IN VECTOR).
!
            DO i = 1 , neqdyn , 2
               sild = z(i+1)/10
               gptype = z(i+1) - 10*sild
               IF ( gptype==3 ) THEN
                  nusetd = iusetd + sild - 1
                  IF ( andf(z(nusetd),mskue)/=0 ) THEN
                     k = nbrmod - imode + 1
                     DO j = iusetd , nusetd
                        IF ( andf(z(j),mskue)/=0 ) k = k + 1
                     ENDDO
                     z(i+1) = 10*k + 3
                     CYCLE
                  ENDIF
               ENDIF
               z(i+1) = 0
            ENDDO
         ELSE
!
!     DIRECT - PROCESS EACH ENTRY IN EQDYN. IF POINT IS NOT IN ANALYSIS
!              SET, REPLACE SILD NO. WITH ZERO. OTHERWISE, REPLACE SILD
!              NO. WITH POSITION IN ANALYSIS SET (I.E. ROW INDEX IN
!              VECTOR) AND CODE INDICATING WHICH COMPONENTS OF POINT ARE
!              IN ANALYSIS SET.
!
            DO i = 1 , neqdyn , 2
               sild = z(i+1)/10
               gptype = z(i+1) - 10*sild
               nusetd = iusetd + sild - 1
               k = 0
               m = 1
               IF ( gptype==1 ) m = 6
               j = nusetd
               DO l = 1 , m
                  IF ( andf(z(j),mskud)/=0 ) k = k + masks(l)
                  j = j + 1
               ENDDO
               IF ( k==0 ) THEN
                  z(i+1) = 0
               ELSE
                  l = 1
                  m = nusetd - 1
                  IF ( m>=iusetd ) THEN
                     DO j = iusetd , m
                        IF ( andf(z(j),mskud)/=0 ) l = l + 1
                     ENDDO
                  ENDIF
                  z(i+1) = gptype + k + 256*l
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     SET PARAMETER FOR APPROACH. THEN OPEN CASE CONTROL,
!     SKIP HEADER RECORD AND BRANCH ON APPROACH.
!
         branch = 0
         IF ( app(1)==cei(1) ) branch = 1
         IF ( app(1)==frq(1) ) branch = 2
         IF ( app(1)==trn(1) ) branch = 3
         IF ( app(1)==ireig ) branch = 4
         IF ( branch==0 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(casecc,z(buf1),0)
         IF ( branch==2 .OR. branch==3 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
            file = pp
            CALL open(*400,pp,z(buf2),rdrew)
            i = ilist
            m = 3
            ix = 1
            IF ( app(1)==frq(1) ) ix = 2
            DO
               CALL read(*420,*80,pp,buf,m,0,flag)
               z(i) = buf(m)
               z(i+1) = 0
               i = i + ix
               m = 1
            ENDDO
         ELSE
!
!     COMPLEX EIGENVALUES - READ LIST OF MODE NOS. AND VALUES INTO CORE.
!
            file = oeigs
            CALL gopen(oeigs,z(buf2),0)
            CALL fwdrec(*420,oeigs)
            i = ilist
            m = 8 - ktype
            DO
               CALL read(*420,*60,oeigs,buf,m,0,flag)
               z(i) = buf(1)
               z(i+1) = buf(3)
               z(i+2) = buf(4)
               i = i + 3
            ENDDO
         ENDIF
 60      CALL close(oeigs,clsrew)
         nlist = i - 3
         icc = i
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      CALL close(pp,clsrew)
         nlist = i - ix
         icc = i
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPEN OUTPUT FILE. WROTE HEADER RECORD.
!
         file = Outfl
         CALL open(*380,Outfl,z(buf2),wrtrew)
         mcb(1) = Outfl
         CALL fname(Outfl,buf)
         DO i = 1 , 3
            buf(i+2) = date(i)
         ENDDO
         buf(6) = time
         buf(7) = 1
         CALL write(Outfl,buf,7,1)
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
         file = Infil
         CALL open(*360,Infil,z(buf3),rdrew)
         CALL fwdrec(*420,Infil)
!
!     SET PARAMETERS TO KEEP CASE CONTROL AND VECTORS IN SYNCH.
!
         eof = 0
         jcount = 0
         kcount = 1
         jlist = ilist
         kfrq = 0
         kwds = 0
         incore = 0
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ A RECORD IN CASE CONTROL.
!
         CALL read(*340,*100,casecc,z(icc+1),buf3-icc,1,ncc)
         CALL mesage(m8,0,nam)
 100     ivec = icc + ncc + 1
         ireqx = icc + idisp
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + ivel
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + iacc
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + ispcf
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + iloads
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + istr
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + ielf
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + igpf
         IF ( z(ireqx)/=0 ) sdr2 = 1
         ireqx = icc + iese
         IF ( z(ireqx)/=0 ) sdr2 = 1
!
!     SET OUTPUT HARMONICS REQUEST WHICH IS USED IF FLUID ELEMENTS
!     ARE IN PROBLEM.
!
         oharms = z(icc+137)
         IF ( oharms<0 .AND. axif/=0 ) oharms = axif
!
!     IN THE ABOVE IF OHARMS = -1  THEN ALL IS IMPLIED. IF OHARMS = 0
!     THEN NONE IS IMPLIED AND IF OHARMS IS POSITIVE THEN THAT VALUE
!     MINUS ONE IS IMPLIED.
!
         IF ( axif/=0 ) THEN
            IF ( oharms/=0 ) THEN
               oharms = oharms - 1
               oharms = 2*oharms + 3
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT. IF NOT, TEST FOR RECORD
!     SKIP ON INFIL, THEN GO TO END OF REQUEST. IF SO, SET POINTERS
!     TO SET DEFINING REQUEST.
!
         ireqx = icc + ireq
         setno = z(ireqx)
         dest = z(ireqx+1)
         xsetno = -1
         IF ( setno<0 ) THEN
         ELSEIF ( setno==0 ) THEN
            IF ( app(1)/=frq(1) ) THEN
               CALL fwdrec(*420,Infil)
               jcount = jcount + 1
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kcount/=1 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            ix = icc + ilsym
            isetno = ix + z(ix) + 1
            SPAG_Loop_1_1: DO
               iset = isetno + 2
               nset = z(isetno+1) + iset - 1
               IF ( z(isetno)==setno ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET.
!
                  IF ( setno<xset0 ) EXIT SPAG_Loop_1_1
                  xsetno = dest/10
                  dest = dest - 10*xsetno
                  IF ( xsetno==0 ) EXIT SPAG_Loop_1_1
                  ixsetn = ix + z(ix) + 1
                  DO
                     ixset = ixsetn + 2
                     nxset = z(ixsetn+1) + ixset - 1
                     IF ( z(ixsetn)==xsetno ) EXIT SPAG_Loop_1_1
                     ixsetn = nxset + 1
                     IF ( ixsetn>=ivec ) THEN
                        xsetno = -1
                        setno = -1
                        EXIT SPAG_Loop_1_1
                     ENDIF
                  ENDDO
               ELSE
                  isetno = nset + 1
                  IF ( isetno>=ivec ) EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
!
!     UNPACK VECTOR INTO CORE (UNLESS VECTOR IS ALREADY IN CORE).
!
         IF ( incore/=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ivecn = ivec + ktype*nrows - 1
         IF ( ivecn>=buf3 ) CALL mesage(m8,0,nam)
         j2 = nrows
         CALL unpack(*120,Infil,z(ivec))
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 120     DO i = ivec , ivecn
            zz(i) = 0.
         ENDDO
         spag_nextblock_1 = 7
      CASE (7)
         jcount = jcount + 1
         spag_nextblock_1 = 8
      CASE (8)
!
!     TEST FOR CONTINUATION.
!
         IF ( app(1)==frq(1) .AND. setno==0 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PREPARE TO WRITE ID RECORD ON OUTPUT FILE.
!
         IF ( branch==2 ) THEN
!
!     FREQUENCY RESPONSE.
!
            ix = icc + idload
            buf(8) = z(ix)
            buf(6) = 0
            buf(7) = 0
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
                  SPAG_Loop_1_2: DO
                     isetf = isetnf + 2
                     nsetf = z(isetnf+1) + isetf - 1
                     IF ( z(isetnf)==fsetno ) THEN
                        DO i = isetf , nsetf
                           k = 0
                           diff = 1.E+25
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
                        GOTO 130
                     ELSE
                        isetnf = nsetf + 1
                        IF ( isetnf>=ivec ) THEN
                           fsetno = -1
                           EXIT SPAG_Loop_1_2
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_1_2
               ENDIF
               DO j = ilist , nlist , 2
                  z(j+1) = 1
               ENDDO
            ENDIF
!
!     DETERMINE IF CURRENT FREQ IS MARKED FOR OUTPUT.
!
 130        IF ( z(jlist+1)==0 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            buf(5) = z(jlist)
            buf(2) = kcount + 1014
         ELSEIF ( branch==3 ) THEN
!
!     TRANSIENT RESPONSE.
!
            buf(5) = z(jlist)
            buf(2) = kcount + 14
            IF ( ireq==ipnl ) buf(2) = 12
            ix = icc + idload
            buf(8) = z(ix)
            buf(6) = 0
            buf(7) = 0
         ELSE
!
!     COMPLEX EIGENVALUES.
!
            buf(2) = 1014
            buf(5) = z(jlist)
            buf(6) = z(jlist+1)
            buf(7) = z(jlist+2)
            buf(8) = 0
         ENDIF
!
!     WRITE ID RECORD ON OUTPUT FILE.
!
         ix = branch + 3
         IF ( app(1)==cei(1) ) ix = 9
         IF ( app(1)==ireig ) ix = 2
         buf(1) = dest + 10*ix
         buf(3) = 0
         buf(4) = z(icc+1)
         IF ( z(ireqx+2)<0 ) sort2 = +1
         format = iabs(z(ireqx+2))
         buf(9) = format
         buf(10) = nwds
         CALL write(Outfl,buf,50,0)
         ix = icc + ittl
         CALL write(Outfl,z(ix),96,1)
         output = 1
         IF ( z(ireqx+2)<0 ) sort2 = 1
!
!     BUILD DATA RECORD ON OUTPUT FILE.
!
         IF ( form(1)==modal(1) ) THEN
!
!     MODAL PROBLEM WITH SET .EQ. -ALL- OUTPUT ALL MODAL POINTS. THEN
!                                       IF EXTRA POINTS, OUTPUT THEM.
!
            IF ( setno/=-1 ) THEN
!
!     MODAL PROBLEM WITH SET .NE. -ALL- ASSUME NUMBERS IN REQUESTED SET
!                                       WHICH ARE .LE. NO. OF MODES ARE
!                                       MODAL COORDINATES AND ANY OTHERS
!                                       ARE EXTRA POINTS.
!
               jharm = 0
               i = iset
               spag_nextblock_1 = 12
            ELSE
               buf(1) = imode
               buf(2) = 4
               j = 1
               ASSIGN 220 TO retx
               spag_nextblock_1 = 15
            ENDIF
            CYCLE
         ELSEIF ( setno/=-1 ) THEN
!
!     DIRECT PROBLEM WITH SET .NE. -ALL- OUTPUT POINTS IN REQUESTED SET
!                                        WHICH ARE ALSO IN ANALYSIS SET.
!
            jharm = 0
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     DIRECT PROBLEM SET .EQ. -ALL- - OUTPUT POINTS IN ANALYSIS SET
!
            kx = 1
            ASSIGN 160 TO retx
         ENDIF
 140     word = z(kx+1)
         IF ( word==0 ) GOTO retx
         j = word/256
         buf(2) = andf(word,3)
         code = word - 256*j - buf(2)
         buf(1) = z(kx)
         IF ( buf(2)/=1 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     GRID POINT.
!
         DO k = 3 , nwds
            buf(k) = 1
         ENDDO
         j = ivec + ktype*(j-1)
         IF ( ktype==2 ) THEN
!
!     COMPLEX GRID POINT.
!
            DO k = 1 , 6
               IF ( andf(code,masks(k))/=0 ) THEN
                  bufr(k+2) = zz(j)
                  bufr(k+8) = zz(j+1)
                  j = j + 2
                  IF ( format==3 ) THEN
                     redner = sqrt(bufr(k+2)**2+bufr(k+8)**2)
                     IF ( redner/=0 ) THEN
                        bufr(k+8) = atan2(bufr(k+8),bufr(k+2))*raddeg
                        IF ( bufr(k+8)<-0.00005 ) bufr(k+8) = bufr(k+8) + 360.0
                        bufr(k+2) = redner
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ELSE
            DO k = 1 , 6
               IF ( andf(code,masks(k))/=0 ) THEN
                  bufr(k+2) = zz(j)
                  j = j + 1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 160     kx = kx + 2
         IF ( kx<=neqdyn ) GOTO 140
!
!     CONCLUDE PROCESSING OF THIS VECTOR.
!
         CALL write(Outfl,0,0,1)
         spag_nextblock_1 = 20
      CASE (9)
         i = iset
         ASSIGN 140 TO ret
         spag_nextblock_1 = 10
      CASE (10)
         buf(1) = z(i)
         IF ( i==nset ) THEN
            ASSIGN 200 TO retx
         ELSEIF ( z(i+1)>0 ) THEN
            ASSIGN 200 TO retx
         ELSE
            n = -z(i+1)
            i = i + 1
            ASSIGN 180 TO retx
         ENDIF
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 180     buf(1) = buf(1) + 1
         IF ( buf(1)<=n ) THEN
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 200     i = i + 1
         IF ( i<=nset ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( axif==0 ) THEN
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
         ELSE
            jharm = jharm + 1
            IF ( jharm<=oharms ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
         ENDIF
         CYCLE
 220     buf(1) = buf(1) + 1
         j = buf(1) - imode + 1
         IF ( buf(1)<=nbrmod ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nbrep==0 ) THEN
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            kx = 1
            ASSIGN 240 TO retx
            buf(2) = 3
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         j = z(kx+1)/10
         gptype = z(kx+1) - 10*j
         buf(1) = z(kx)
         IF ( gptype==3 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 240     kx = kx + 2
         IF ( kx<=neqdyn ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL write(Outfl,0,0,1)
         spag_nextblock_1 = 20
      CASE (12)
         buf(1) = z(i)
         IF ( i==nset ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( z(i+1)>0 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = -z(i+1)
         buf(2) = 4
         i = i + 1
         ASSIGN 260 TO retx
         spag_nextblock_1 = 13
      CASE (13)
         IF ( buf(1)<imode .OR. buf(1)>nbrmod ) THEN
            IF ( nbrep==0 ) GOTO 320
            ASSIGN 280 TO ret
            buf(2) = 3
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
            j = buf(1) - imode + 1
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 260     buf(1) = buf(1) + 1
         IF ( buf(1)<=n ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 320
 280     j = z(kx+1)/10
         gptype = z(kx+1) - 10*j
         IF ( gptype==3 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 260
      CASE (14)
         ASSIGN 320 TO retx
         IF ( buf(1)<imode .OR. buf(1)>nbrmod ) THEN
            IF ( nbrep==0 ) GOTO 320
            ASSIGN 300 TO ret
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ASSIGN 320 TO retx
            j = buf(1) - imode + 1
            buf(2) = 4
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 300     j = z(kx+1)/10
         buf(2) = z(kx+1) - 10*j
         IF ( buf(2)==3 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 320     i = i + 1
         IF ( i<=nset ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( axif==0 ) THEN
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
         ELSE
            jharm = jharm + 1
            IF ( jharm<=oharms ) THEN
               i = iset
               spag_nextblock_1 = 12
            ELSE
               CALL write(Outfl,0,0,1)
               spag_nextblock_1 = 20
            ENDIF
         ENDIF
      CASE (15)
!
!     SCALAR, EXTRA OR MODAL POINT.
!
         j = ivec + ktype*(j-1)
         bufr(3) = zz(j)
         DO k = 4 , nwds
            buf(k) = 0
         ENDDO
         IF ( ktype/=1 ) THEN
!
!     COMPLEX SCALAR, EXTRA OR MODAL POINT.
!
            bufr(9) = zz(j+1)
            IF ( format==3 ) THEN
               redner = sqrt(bufr(3)**2+bufr(9)**2)
               IF ( redner/=0 ) THEN
                  bufr(9) = atan2(bufr(9),bufr(3))*raddeg
                  IF ( bufr(9)<-0.00005 ) bufr(9) = bufr(9) + 360.0
                  bufr(3) = redner
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
!
!     DETERMINE DESTINATION FOR ENTRY.
!
!
!     IF A FLUID PROBLEM THEN A CHECK IS NOW MADE TO SEE IF THIS
!     HARMONIC IS TO BE OUTPUT
!
         IF ( axif/=0 ) THEN
            IF ( buf(1)>=500000 ) THEN
               itemp = buf(1) - mod(buf(1),500000)
               itemp = itemp/500000
               IF ( itemp>=oharms ) THEN
                  CALL write(Outfl,0,0,1)
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         id = buf(1)
         buf(1) = 10*id + dest
         IF ( xsetno<0 ) THEN
         ELSEIF ( xsetno==0 ) THEN
            buf(1) = 10*id
         ELSE
            ix = ixset
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 19
      CASE (17)
         IF ( ix/=nxset ) THEN
            IF ( z(ix+1)<=0 ) THEN
               IF ( id>=z(ix) .AND. id<=-z(ix+1) ) THEN
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ix = ix + 2
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==z(ix) ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ix = ix + 1
         spag_nextblock_1 = 18
      CASE (18)
         IF ( ix<=nxset ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         buf(1) = 10*id
         spag_nextblock_1 = 19
      CASE (19)
!
!     WRITE ENTRY ON OUTPUT FILE.
!
         CALL write(Outfl,buf,nwds,0)
         kwds = kwds + nwds
         buf(1) = id
         GOTO retx
      CASE (20)
         IF ( branch==2 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( branch==3 ) THEN
!
!     TRANSIENT RESPONSE.
!
            IF ( ireq==ipnl ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kcount<2 ) THEN
               ireq = iavel
               kcount = 2
            ELSEIF ( kcount==2 ) THEN
               ireq = iaacc
               kcount = 3
            ELSE
               ireq = iadisp
               kcount = 1
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     COMPLEX EIGENVALUES.
!
            jlist = jlist + 3
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
         IF ( jcount>=nvects ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( eof==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (22)
!
!     FREQUENCY RESPONSE.
!
         IF ( kcount==3 ) THEN
            kcount = 1
            incore = 0
            ireq = iadisp
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
            spag_nextblock_1 = 24
         ELSE
            n = ivecn - 1
            omega = twopi*zz(jlist)
            DO i = ivec , n , 2
               bufr(1) = -omega*zz(i+1)
               zz(i+1) = omega*zz(i)
               zz(i) = bufr(1)
            ENDDO
            IF ( kcount==2 ) THEN
               ireq = iaacc
            ELSE
               ireq = iavel
            ENDIF
            kcount = kcount + 1
            incore = 1
            spag_nextblock_1 = 6
         ENDIF
      CASE (23)
         jlist = jlist + 1
         IF ( jlist<=nlist .AND. jcount<nvects ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
         CYCLE SPAG_DispatchLoop_1
!
!     HERE WHEN EOF ENCOUNTERED ON CASE CONTROL.
!
 340     eof = 1
         IF ( branch==1 .OR. branch==4 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
      CASE (24)
!
!     CONCLUDE PROCESSING.
!
         CALL close(casecc,clsrew)
         CALL close(Infil,clsrew)
         CALL close(Outfl,clsrew)
         mcb(1) = Outfl
         mcb(2) = kwds/65536
         mcb(3) = kwds - 65536*mcb(2)
         mcb(4) = 0
         mcb(5) = 0
         mcb(6) = 0
         mcb(7) = 0
         CALL wrttrl(mcb)
         RETURN
!
!     HERE IF ABNORMAL CONDITION.
!
 360     CALL close(Outfl,clsrew)
 380     CALL mesage(30,78,0)
         spag_nextblock_1 = 25
      CASE (25)
         RETURN
!
!     FATAL FILE ERRORS
!
 400     n = -1
         spag_nextblock_1 = 26
         CYCLE SPAG_DispatchLoop_1
 420     n = -2
         spag_nextblock_1 = 26
      CASE (26)
         CALL mesage(n,file,nam)
         RETURN
      CASE (27)
!
!     BINARY SEARCH ROUTINE
!
         klo = 1
         khi = kn
         IF ( axif/=0 ) buf(1) = jharm*500000 + buf(1)
         k = (klo+khi+1)/2
         DO
            kx = 2*k - 1
            IF ( buf(1)<z(kx) ) THEN
               khi = k
            ELSEIF ( buf(1)==z(kx) ) THEN
               GOTO ret
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
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE vdrb
