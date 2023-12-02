!*==vdrb.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vdrb(Infil,Outfl,Ireqq)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_CONDAS
   USE C_NAMES
   USE C_SYSTEM
   USE C_TWO
   USE C_UNPAKX
   USE C_VDRCOM
   USE C_ZZZZZZ
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
         mskud = Two(Ud)
         mskue = Two(Ue)
         ilist = 1
         I2 = 1
         Incr2 = 1
         ireq = Ireqq
         IF ( Form(1)/=Modal(1) .AND. Form(1)/=Direct(1) ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ TRAILER ON USETD. SET NO. OF EXTRA POINTS.
!     READ TRAILER ON INFIL. SET PARAMETERS.
!     IF MODAL PROBLEM, NO. OF MODES = NO. OF ROWS IN VECTOR - NO. XTRA
!     PTS.
!
         mcb(1) = Usetd
         file = Usetd
         CALL rdtrl(mcb)
         IF ( mcb(1)/=Usetd ) GOTO 400
         nbrep = mcb(3)
         mcb(1) = Infil
         CALL rdtrl(mcb)
         IF ( mcb(1)/=Infil ) GOTO 380
         nvects = mcb(2)
         nrows = mcb(3)
         IF ( Form(1)==Modal(1) ) nbrmod = Imode + nrows - nbrep - 1
         IF ( mcb(5)<=2 ) THEN
            IF ( App(1)/=Frq(1) ) THEN
!
!     REAL VECTOR.
!
               ktype = 1
               Qtype2 = 1
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
         Qtype2 = 3
         nwds = 14
         ktypex = 1000
         spag_nextblock_1 = 2
      CASE (2)
!
!     IF DIRECT PROBLEM OR MODAL PROBLEM WITH EXTRA POINTS,
!     READ 2ND TABLE OF EQDYN INTO CORE. THEN READ USETD INTO CORE.
!
         IF ( Form(1)==Modal(1) .AND. nbrep==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = Eqdyn
         CALL gopen(Eqdyn,Z(Buf1),0)
         CALL fwdrec(*420,Eqdyn)
         CALL read(*420,*20,Eqdyn,Z,Buf1,1,neqd)
         CALL mesage(m8,0,Nam)
 20      CALL close(Eqdyn,Clsrew)
         iusetd = neqd + 1
         ncore = Buf1 - iusetd
         file = Usetd
         CALL gopen(Usetd,Z(Buf1),0)
         CALL read(*420,*40,Usetd,Z(iusetd),ncore,1,flag)
         CALL mesage(m8,0,Nam)
 40      CALL close(Usetd,clsrrw)
         ilist = iusetd
         neqdyn = neqd - 1
         kn = neqd/2
!
!     BRANCH ON PROBLEM TYPE.
!
         IF ( Form(1)==Modal(1) ) THEN
!
!     MODAL - PROCESS EACH ENTRY IN EQDYN. IF POINT IS NOT AN EXTRA
!             POINT, REPLACE SILD NO. WITH ZERO. OTHERWISE, REPLACE SILD
!             NO. WITH POSITION IN MODAL SET (I.E. ROW INDEX IN VECTOR).
!
            DO i = 1 , neqdyn , 2
               sild = Z(i+1)/10
               gptype = Z(i+1) - 10*sild
               IF ( gptype==3 ) THEN
                  nusetd = iusetd + sild - 1
                  IF ( andf(Z(nusetd),mskue)/=0 ) THEN
                     k = nbrmod - Imode + 1
                     DO j = iusetd , nusetd
                        IF ( andf(Z(j),mskue)/=0 ) k = k + 1
                     ENDDO
                     Z(i+1) = 10*k + 3
                     CYCLE
                  ENDIF
               ENDIF
               Z(i+1) = 0
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
               sild = Z(i+1)/10
               gptype = Z(i+1) - 10*sild
               nusetd = iusetd + sild - 1
               k = 0
               m = 1
               IF ( gptype==1 ) m = 6
               j = nusetd
               DO l = 1 , m
                  IF ( andf(Z(j),mskud)/=0 ) k = k + Masks(l)
                  j = j + 1
               ENDDO
               IF ( k==0 ) THEN
                  Z(i+1) = 0
               ELSE
                  l = 1
                  m = nusetd - 1
                  IF ( m>=iusetd ) THEN
                     DO j = iusetd , m
                        IF ( andf(Z(j),mskud)/=0 ) l = l + 1
                     ENDDO
                  ENDIF
                  Z(i+1) = gptype + k + 256*l
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
         IF ( App(1)==Cei(1) ) branch = 1
         IF ( App(1)==Frq(1) ) branch = 2
         IF ( App(1)==Trn(1) ) branch = 3
         IF ( App(1)==ireig ) branch = 4
         IF ( branch==0 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(Casecc,Z(Buf1),0)
         IF ( branch==2 .OR. branch==3 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
            file = Pp
            CALL open(*400,Pp,Z(Buf2),Rdrew)
            i = ilist
            m = 3
            ix = 1
            IF ( App(1)==Frq(1) ) ix = 2
            DO
               CALL read(*420,*80,Pp,Buf,m,0,flag)
               Z(i) = Buf(m)
               Z(i+1) = 0
               i = i + ix
               m = 1
            ENDDO
         ELSE
!
!     COMPLEX EIGENVALUES - READ LIST OF MODE NOS. AND VALUES INTO CORE.
!
            file = Oeigs
            CALL gopen(Oeigs,Z(Buf2),0)
            CALL fwdrec(*420,Oeigs)
            i = ilist
            m = 8 - ktype
            DO
               CALL read(*420,*60,Oeigs,Buf,m,0,flag)
               Z(i) = Buf(1)
               Z(i+1) = Buf(3)
               Z(i+2) = Buf(4)
               i = i + 3
            ENDDO
         ENDIF
 60      CALL close(Oeigs,Clsrew)
         nlist = i - 3
         icc = i
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      CALL close(Pp,Clsrew)
         nlist = i - ix
         icc = i
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPEN OUTPUT FILE. WROTE HEADER RECORD.
!
         file = Outfl
         CALL open(*380,Outfl,Z(Buf2),Wrtrew)
         mcb(1) = Outfl
         CALL fname(Outfl,Buf)
         DO i = 1 , 3
            Buf(i+2) = Date(i)
         ENDDO
         Buf(6) = Time
         Buf(7) = 1
         CALL write(Outfl,Buf,7,1)
!
!     OPEN INPUT FILE. SKIP HEADER RECORD.
!
         file = Infil
         CALL open(*360,Infil,Z(Buf3),Rdrew)
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
         CALL read(*340,*100,Casecc,Z(icc+1),Buf3-icc,1,ncc)
         CALL mesage(m8,0,Nam)
 100     ivec = icc + ncc + 1
         ireqx = icc + Idisp
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + Ivel
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + Iacc
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + Ispcf
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + Iloads
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + Istr
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + Ielf
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + igpf
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
         ireqx = icc + iese
         IF ( Z(ireqx)/=0 ) Sdr2 = 1
!
!     SET OUTPUT HARMONICS REQUEST WHICH IS USED IF FLUID ELEMENTS
!     ARE IN PROBLEM.
!
         oharms = Z(icc+137)
         IF ( oharms<0 .AND. Axif/=0 ) oharms = Axif
!
!     IN THE ABOVE IF OHARMS = -1  THEN ALL IS IMPLIED. IF OHARMS = 0
!     THEN NONE IS IMPLIED AND IF OHARMS IS POSITIVE THEN THAT VALUE
!     MINUS ONE IS IMPLIED.
!
         IF ( Axif/=0 ) THEN
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
         setno = Z(ireqx)
         dest = Z(ireqx+1)
         xsetno = -1
         IF ( setno<0 ) THEN
         ELSEIF ( setno==0 ) THEN
            IF ( App(1)/=Frq(1) ) THEN
               CALL fwdrec(*420,Infil)
               jcount = jcount + 1
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kcount/=1 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            ix = icc + Ilsym
            isetno = ix + Z(ix) + 1
            SPAG_Loop_1_1: DO
               iset = isetno + 2
               nset = Z(isetno+1) + iset - 1
               IF ( Z(isetno)==setno ) THEN
!
!     IF REQUIRED, LOCATE PRINT/PUNCH SUBSET.
!
                  IF ( setno<Xset0 ) EXIT SPAG_Loop_1_1
                  xsetno = dest/10
                  dest = dest - 10*xsetno
                  IF ( xsetno==0 ) EXIT SPAG_Loop_1_1
                  ixsetn = ix + Z(ix) + 1
                  DO
                     ixset = ixsetn + 2
                     nxset = Z(ixsetn+1) + ixset - 1
                     IF ( Z(ixsetn)==xsetno ) EXIT SPAG_Loop_1_1
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
         IF ( ivecn>=Buf3 ) CALL mesage(m8,0,Nam)
         J2 = nrows
         CALL unpack(*120,Infil,Z(ivec))
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
         IF ( App(1)==Frq(1) .AND. setno==0 ) THEN
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
            ix = icc + Idload
            Buf(8) = Z(ix)
            Buf(6) = 0
            Buf(7) = 0
            IF ( kfrq==0 ) THEN
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
                  SPAG_Loop_1_2: DO
                     isetf = isetnf + 2
                     nsetf = Z(isetnf+1) + isetf - 1
                     IF ( Z(isetnf)==fsetno ) THEN
                        DO i = isetf , nsetf
                           k = 0
                           diff = 1.E+25
                           bufr(1) = zz(i)
                           DO j = ilist , nlist , 2
                              IF ( Z(j+1)==0 ) THEN
                                 diff1 = abs(zz(j)-bufr(1))
                                 IF ( diff1<diff ) THEN
                                    diff = diff1
                                    k = j
                                 ENDIF
                              ENDIF
                           ENDDO
                           IF ( k/=0 ) Z(k+1) = 1
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
                  Z(j+1) = 1
               ENDDO
            ENDIF
!
!     DETERMINE IF CURRENT FREQ IS MARKED FOR OUTPUT.
!
 130        IF ( Z(jlist+1)==0 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Buf(5) = Z(jlist)
            Buf(2) = kcount + 1014
         ELSEIF ( branch==3 ) THEN
!
!     TRANSIENT RESPONSE.
!
            Buf(5) = Z(jlist)
            Buf(2) = kcount + 14
            IF ( ireq==Ipnl ) Buf(2) = 12
            ix = icc + Idload
            Buf(8) = Z(ix)
            Buf(6) = 0
            Buf(7) = 0
         ELSE
!
!     COMPLEX EIGENVALUES.
!
            Buf(2) = 1014
            Buf(5) = Z(jlist)
            Buf(6) = Z(jlist+1)
            Buf(7) = Z(jlist+2)
            Buf(8) = 0
         ENDIF
!
!     WRITE ID RECORD ON OUTPUT FILE.
!
         ix = branch + 3
         IF ( App(1)==Cei(1) ) ix = 9
         IF ( App(1)==ireig ) ix = 2
         Buf(1) = dest + 10*ix
         Buf(3) = 0
         Buf(4) = Z(icc+1)
         IF ( Z(ireqx+2)<0 ) Sort2 = +1
         format = iabs(Z(ireqx+2))
         Buf(9) = format
         Buf(10) = nwds
         CALL write(Outfl,Buf,50,0)
         ix = icc + Ittl
         CALL write(Outfl,Z(ix),96,1)
         Output = 1
         IF ( Z(ireqx+2)<0 ) Sort2 = 1
!
!     BUILD DATA RECORD ON OUTPUT FILE.
!
         IF ( Form(1)==Modal(1) ) THEN
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
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Buf(1) = Imode
               Buf(2) = 4
               j = 1
               ASSIGN 220 TO retx
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
 140     word = Z(kx+1)
         IF ( word==0 ) GOTO retx
         j = word/256
         Buf(2) = andf(word,3)
         code = word - 256*j - Buf(2)
         Buf(1) = Z(kx)
         IF ( Buf(2)/=1 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     GRID POINT.
!
         DO k = 3 , nwds
            Buf(k) = 1
         ENDDO
         j = ivec + ktype*(j-1)
         IF ( ktype==2 ) THEN
!
!     COMPLEX GRID POINT.
!
            DO k = 1 , 6
               IF ( andf(code,Masks(k))/=0 ) THEN
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
               IF ( andf(code,Masks(k))/=0 ) THEN
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
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
         i = iset
         ASSIGN 140 TO ret
         spag_nextblock_1 = 10
      CASE (10)
         Buf(1) = Z(i)
         IF ( i==nset ) THEN
            ASSIGN 200 TO retx
         ELSEIF ( Z(i+1)>0 ) THEN
            ASSIGN 200 TO retx
         ELSE
            n = -Z(i+1)
            i = i + 1
            ASSIGN 180 TO retx
         ENDIF
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 180     Buf(1) = Buf(1) + 1
         IF ( Buf(1)<=n ) THEN
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 200     i = i + 1
         IF ( i<=nset ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Axif==0 ) THEN
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            jharm = jharm + 1
            IF ( jharm<=oharms ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 220     Buf(1) = Buf(1) + 1
         j = Buf(1) - Imode + 1
         IF ( Buf(1)<=nbrmod ) THEN
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
            Buf(2) = 3
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         j = Z(kx+1)/10
         gptype = Z(kx+1) - 10*j
         Buf(1) = Z(kx)
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
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         Buf(1) = Z(i)
         IF ( i==nset ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Z(i+1)>0 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = -Z(i+1)
         Buf(2) = 4
         i = i + 1
         ASSIGN 260 TO retx
         spag_nextblock_1 = 13
      CASE (13)
         IF ( Buf(1)<Imode .OR. Buf(1)>nbrmod ) THEN
            IF ( nbrep==0 ) GOTO 320
            ASSIGN 280 TO ret
            Buf(2) = 3
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
            j = Buf(1) - Imode + 1
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 260     Buf(1) = Buf(1) + 1
         IF ( Buf(1)<=n ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 320
 280     j = Z(kx+1)/10
         gptype = Z(kx+1) - 10*j
         IF ( gptype==3 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 260
      CASE (14)
         ASSIGN 320 TO retx
         IF ( Buf(1)<Imode .OR. Buf(1)>nbrmod ) THEN
            IF ( nbrep==0 ) GOTO 320
            ASSIGN 300 TO ret
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ASSIGN 320 TO retx
            j = Buf(1) - Imode + 1
            Buf(2) = 4
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 300     j = Z(kx+1)/10
         Buf(2) = Z(kx+1) - 10*j
         IF ( Buf(2)==3 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 320     i = i + 1
         IF ( i<=nset ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Axif==0 ) THEN
            CALL write(Outfl,0,0,1)
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            jharm = jharm + 1
            IF ( jharm<=oharms ) THEN
               i = iset
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL write(Outfl,0,0,1)
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
      CASE (15)
!
!     SCALAR, EXTRA OR MODAL POINT.
!
         j = ivec + ktype*(j-1)
         bufr(3) = zz(j)
         DO k = 4 , nwds
            Buf(k) = 0
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
         IF ( Axif/=0 ) THEN
            IF ( Buf(1)>=500000 ) THEN
               itemp = Buf(1) - mod(Buf(1),500000)
               itemp = itemp/500000
               IF ( itemp>=oharms ) THEN
                  CALL write(Outfl,0,0,1)
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         id = Buf(1)
         Buf(1) = 10*id + dest
         IF ( xsetno<0 ) THEN
         ELSEIF ( xsetno==0 ) THEN
            Buf(1) = 10*id
         ELSE
            ix = ixset
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
         IF ( ix/=nxset ) THEN
            IF ( Z(ix+1)<=0 ) THEN
               IF ( id>=Z(ix) .AND. id<=-Z(ix+1) ) THEN
                  spag_nextblock_1 = 19
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ix = ix + 2
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==Z(ix) ) THEN
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
         Buf(1) = 10*id
         spag_nextblock_1 = 19
      CASE (19)
!
!     WRITE ENTRY ON OUTPUT FILE.
!
         CALL write(Outfl,Buf,nwds,0)
         kwds = kwds + nwds
         Buf(1) = id
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
            IF ( ireq==Ipnl ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kcount<2 ) THEN
               ireq = Iavel
               kcount = 2
            ELSEIF ( kcount==2 ) THEN
               ireq = Iaacc
               kcount = 3
            ELSE
               ireq = Iadisp
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
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
!
!     FREQUENCY RESPONSE.
!
         IF ( kcount==3 ) THEN
            kcount = 1
            incore = 0
            ireq = Iadisp
            jlist = jlist + 2
            IF ( jlist<=nlist .AND. jcount<nvects ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            kfrq = 0
            jlist = ilist
            DO i = ilist , nlist , 2
               Z(i+1) = 0
            ENDDO
            IF ( jcount<nvects ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ELSE
            n = ivecn - 1
            omega = twopi*zz(jlist)
            DO i = ivec , n , 2
               bufr(1) = -omega*zz(i+1)
               zz(i+1) = omega*zz(i)
               zz(i) = bufr(1)
            ENDDO
            IF ( kcount==2 ) THEN
               ireq = Iaacc
            ELSE
               ireq = Iavel
            ENDIF
            kcount = kcount + 1
            incore = 1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
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
         CALL close(Casecc,Clsrew)
         CALL close(Infil,Clsrew)
         CALL close(Outfl,Clsrew)
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
 360     CALL close(Outfl,Clsrew)
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
         CALL mesage(n,file,Nam)
         RETURN
      CASE (27)
!
!     BINARY SEARCH ROUTINE
!
         klo = 1
         khi = kn
         IF ( Axif/=0 ) Buf(1) = jharm*500000 + Buf(1)
         k = (klo+khi+1)/2
         DO
            kx = 2*k - 1
            IF ( Buf(1)<Z(kx) ) THEN
               khi = k
            ELSEIF ( Buf(1)==Z(kx) ) THEN
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
