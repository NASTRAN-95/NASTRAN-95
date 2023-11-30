
SUBROUTINE vdrb(Infil,Outfl,Ireqq)
   IMPLICIT NONE
   INTEGER App(2) , Axif , Buf(50) , Buf1 , Buf2 , Buf3 , Casecc , Cei(2) , Date(3) , Direct(2) , Eqdyn , Form(2) , Frq(2) , I2 ,   &
         & Iaacc , Iacc , Iadisp , Iavel , Idisp , Idload , Ielf , Ifrout , Iloads , Ilsym , Imode , Incr2 , Infile , Ipnl , Ispcf ,&
         & Istr , Ittl , Ivel , J2 , Masks(6) , Modal(2) , Nam(2) , Oeigs , Output , Pp , Qtype2 , Sdr2 , Sort2 , Sysbuf , Time ,   &
         & Trn(2) , Two(32) , Ud , Ue , Usetd , Vdrcom(1) , Vdrreq , Xset0 , Z(1)
   REAL Bufr(50) , Clsrew , Consts(5) , Dum19(19) , Opnl1 , Outfle , Pnl , Raddeg , Rd , Rdrew , Scr1 , Scr2 , Twopi , Ua , Uf ,    &
      & Ufe , Ug , Ul , Um , Un , Une , Uo , Up , Ur , Us , Usb , Usg , Wrt , Wrtrew , Xx(13) , Xycdb , Zz(1)
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /blank / App , Form , Sort2 , Output , Sdr2 , Imode
   COMMON /condas/ Consts
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf , Xx , Date , Time , Dum19 , Axif
   COMMON /two   / Two
   COMMON /unpakx/ Qtype2 , I2 , J2 , Incr2
   COMMON /vdrcom/ Vdrcom , Idisp , Ivel , Iacc , Ispcf , Iloads , Istr , Ielf , Iadisp , Iavel , Iaacc , Ipnl , Ittl , Ilsym ,     &
                 & Ifrout , Idload , Casecc , Eqdyn , Usetd , Infile , Oeigs , Pp , Xycdb , Pnl , Outfle , Opnl1 , Scr1 , Scr2 ,    &
                 & Buf1 , Buf2 , Buf3 , Nam , Buf , Masks , Cei , Frq , Trn , Direct , Xset0 , Vdrreq , Modal
   COMMON /zzzzzz/ Z
   INTEGER Infil , Ireqq , Outfl
   INTEGER andf
   INTEGER branch , code , dest , eof , file , flag , format , fsetno , gptype , i , icc , id , iese , igpf , ilist , incore ,      &
         & ireig , ireq , ireqx , iset , isetf , isetnf , isetno , itemp , iusetd , ivec , ivecn , ix , ixset , ixsetn , j ,        &
         & jcount , jharm , jlist , k , kcount , kfrq , khi , klo , kn , ktype , ktypex , kwds , kx , l , m , m8 , mcb(7) , mskud , &
         & mskue , n , nbrep , nbrmod , ncc , ncore , neqd , neqdyn , nlist , nrows , nset , nsetf , nusetd , nvects , nwds ,       &
         & nxset , oharms , ret , retx , setno , sild , word , xsetno
   REAL clsrrw , diff , diff1 , omega , redner
   EXTERNAL andf
!
!     VDRB PROCESSES VECTORS IN THE ANALYSIS OR MODAL SET. IN
!     ACCORDANCE WITH OUTPUT REQUESTS IN THE CASE CONTROL DATA BLOCK,
!     THESE VECTORS ARE FORMATTED FOR INPUT TO OFP WHERE ACTUAL OUTPUT
!     WILL OCCUR.
!
   !>>>>EQUIVALENCE (Consts(2),Twopi) , (Consts(3),Raddeg) , (Buf(1),Bufr(1)) , (Z(1),Zz(1))
   DATA igpf , iese , ireig/167 , 170 , 4HREIG/
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
   IF ( Form(1)/=Modal(1) .AND. Form(1)/=Direct(1) ) GOTO 4400
!
!     READ TRAILER ON USETD. SET NO. OF EXTRA POINTS.
!     READ TRAILER ON INFIL. SET PARAMETERS.
!     IF MODAL PROBLEM, NO. OF MODES = NO. OF ROWS IN VECTOR - NO. XTRA
!     PTS.
!
   mcb(1) = Usetd
   file = Usetd
   CALL rdtrl(mcb)
   IF ( mcb(1)/=Usetd ) GOTO 4500
   nbrep = mcb(3)
   mcb(1) = Infil
   CALL rdtrl(mcb)
   IF ( mcb(1)/=Infil ) GOTO 4300
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
         GOTO 100
      ENDIF
   ENDIF
!
!     COMPLEX VECTOR.
!
   ktype = 2
   Qtype2 = 3
   nwds = 14
   ktypex = 1000
!
!     IF DIRECT PROBLEM OR MODAL PROBLEM WITH EXTRA POINTS,
!     READ 2ND TABLE OF EQDYN INTO CORE. THEN READ USETD INTO CORE.
!
 100  IF ( Form(1)==Modal(1) .AND. nbrep==0 ) GOTO 400
   file = Eqdyn
   CALL gopen(Eqdyn,Z(Buf1),0)
   CALL fwdrec(*4600,Eqdyn)
   CALL read(*4600,*200,Eqdyn,Z,Buf1,1,neqd)
   CALL mesage(m8,0,Nam)
 200  CALL close(Eqdyn,Clsrew)
   iusetd = neqd + 1
   ncore = Buf1 - iusetd
   file = Usetd
   CALL gopen(Usetd,Z(Buf1),0)
   CALL read(*4600,*300,Usetd,Z(iusetd),ncore,1,flag)
   CALL mesage(m8,0,Nam)
 300  CALL close(Usetd,clsrrw)
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
!
!     SET PARAMETER FOR APPROACH. THEN OPEN CASE CONTROL,
!     SKIP HEADER RECORD AND BRANCH ON APPROACH.
!
 400  branch = 0
   IF ( App(1)==Cei(1) ) branch = 1
   IF ( App(1)==Frq(1) ) branch = 2
   IF ( App(1)==Trn(1) ) branch = 3
   IF ( App(1)==ireig ) branch = 4
   IF ( branch==0 ) GOTO 4400
   CALL gopen(Casecc,Z(Buf1),0)
   IF ( branch==2 .OR. branch==3 ) THEN
!
!     FREQUENCY OR TRANSIENT RESPONSE - READ LIST INTO CORE.
!
      file = Pp
      CALL open(*4500,Pp,Z(Buf2),Rdrew)
      i = ilist
      m = 3
      ix = 1
      IF ( App(1)==Frq(1) ) ix = 2
      DO
         CALL read(*4600,*600,Pp,Buf,m,0,flag)
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
      CALL fwdrec(*4600,Oeigs)
      i = ilist
      m = 8 - ktype
      DO
         CALL read(*4600,*500,Oeigs,Buf,m,0,flag)
         Z(i) = Buf(1)
         Z(i+1) = Buf(3)
         Z(i+2) = Buf(4)
         i = i + 3
      ENDDO
   ENDIF
 500  CALL close(Oeigs,Clsrew)
   nlist = i - 3
   icc = i
   GOTO 700
 600  CALL close(Pp,Clsrew)
   nlist = i - ix
   icc = i
!
!     OPEN OUTPUT FILE. WROTE HEADER RECORD.
!
 700  file = Outfl
   CALL open(*4300,Outfl,Z(Buf2),Wrtrew)
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
   CALL open(*4200,Infil,Z(Buf3),Rdrew)
   CALL fwdrec(*4600,Infil)
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
!
!     READ A RECORD IN CASE CONTROL.
!
 800  CALL read(*4000,*900,Casecc,Z(icc+1),Buf3-icc,1,ncc)
   CALL mesage(m8,0,Nam)
 900  ivec = icc + ncc + 1
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
!
!     DETERMINE IF OUTPUT REQUEST IS PRESENT. IF NOT, TEST FOR RECORD
!     SKIP ON INFIL, THEN GO TO END OF REQUEST. IF SO, SET POINTERS
!     TO SET DEFINING REQUEST.
!
 1000 ireqx = icc + ireq
   setno = Z(ireqx)
   dest = Z(ireqx+1)
   xsetno = -1
   IF ( setno<0 ) THEN
   ELSEIF ( setno==0 ) THEN
      IF ( App(1)/=Frq(1) ) THEN
         CALL fwdrec(*4600,Infil)
         jcount = jcount + 1
         GOTO 3600
      ELSEIF ( kcount/=1 ) THEN
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
            IF ( setno<Xset0 ) EXIT
            xsetno = dest/10
            dest = dest - 10*xsetno
            IF ( xsetno==0 ) EXIT
            ixsetn = ix + Z(ix) + 1
            DO
               ixset = ixsetn + 2
               nxset = Z(ixsetn+1) + ixset - 1
               IF ( Z(ixsetn)==xsetno ) GOTO 1100
               ixsetn = nxset + 1
               IF ( ixsetn>=ivec ) THEN
                  xsetno = -1
                  setno = -1
                  GOTO 1100
               ENDIF
            ENDDO
         ELSE
            isetno = nset + 1
            IF ( isetno>=ivec ) EXIT
         ENDIF
      ENDDO
   ENDIF
!
!     UNPACK VECTOR INTO CORE (UNLESS VECTOR IS ALREADY IN CORE).
!
 1100 IF ( incore/=0 ) GOTO 1400
   ivecn = ivec + ktype*nrows - 1
   IF ( ivecn>=Buf3 ) CALL mesage(m8,0,Nam)
   J2 = nrows
   CALL unpack(*1200,Infil,Z(ivec))
   GOTO 1300
 1200 DO i = ivec , ivecn
      Zz(i) = 0.
   ENDDO
 1300 jcount = jcount + 1
!
!     TEST FOR CONTINUATION.
!
 1400 IF ( App(1)==Frq(1) .AND. setno==0 ) GOTO 3800
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
            DO
               isetf = isetnf + 2
               nsetf = Z(isetnf+1) + isetf - 1
               IF ( Z(isetnf)==fsetno ) THEN
                  DO i = isetf , nsetf
                     k = 0
                     diff = 1.E+25
                     Bufr(1) = Zz(i)
                     DO j = ilist , nlist , 2
                        IF ( Z(j+1)==0 ) THEN
                           diff1 = abs(Zz(j)-Bufr(1))
                           IF ( diff1<diff ) THEN
                              diff = diff1
                              k = j
                           ENDIF
                        ENDIF
                     ENDDO
                     IF ( k/=0 ) Z(k+1) = 1
                  ENDDO
                  GOTO 1450
               ELSE
                  isetnf = nsetf + 1
                  IF ( isetnf>=ivec ) THEN
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
 1450 IF ( Z(jlist+1)==0 ) GOTO 3800
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
         GOTO 2400
      ELSE
         Buf(1) = Imode
         Buf(2) = 4
         j = 1
         ASSIGN 2100 TO retx
         GOTO 3100
      ENDIF
   ELSEIF ( setno/=-1 ) THEN
!
!     DIRECT PROBLEM WITH SET .NE. -ALL- OUTPUT POINTS IN REQUESTED SET
!                                        WHICH ARE ALSO IN ANALYSIS SET.
!
      jharm = 0
      GOTO 1700
   ELSE
!
!     DIRECT PROBLEM SET .EQ. -ALL- - OUTPUT POINTS IN ANALYSIS SET
!
      kx = 1
      ASSIGN 1600 TO retx
   ENDIF
 1500 word = Z(kx+1)
   IF ( word==0 ) GOTO retx
   j = word/256
   Buf(2) = andf(word,3)
   code = word - 256*j - Buf(2)
   Buf(1) = Z(kx)
   IF ( Buf(2)/=1 ) GOTO 3100
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
            Bufr(k+2) = Zz(j)
            Bufr(k+8) = Zz(j+1)
            j = j + 2
            IF ( format==3 ) THEN
               redner = sqrt(Bufr(k+2)**2+Bufr(k+8)**2)
               IF ( redner/=0 ) THEN
                  Bufr(k+8) = atan2(Bufr(k+8),Bufr(k+2))*Raddeg
                  IF ( Bufr(k+8)<-0.00005 ) Bufr(k+8) = Bufr(k+8) + 360.0
                  Bufr(k+2) = redner
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ELSE
      DO k = 1 , 6
         IF ( andf(code,Masks(k))/=0 ) THEN
            Bufr(k+2) = Zz(j)
            j = j + 1
         ENDIF
      ENDDO
   ENDIF
   GOTO 3200
 1600 kx = kx + 2
   IF ( kx<=neqdyn ) GOTO 1500
!
!     CONCLUDE PROCESSING OF THIS VECTOR.
!
   CALL write(Outfl,0,0,1)
   GOTO 3600
 1700 i = iset
   ASSIGN 1500 TO ret
 1800 Buf(1) = Z(i)
   IF ( i==nset ) THEN
      ASSIGN 2000 TO retx
   ELSEIF ( Z(i+1)>0 ) THEN
      ASSIGN 2000 TO retx
   ELSE
      n = -Z(i+1)
      i = i + 1
      ASSIGN 1900 TO retx
   ENDIF
   GOTO 4800
 1900 Buf(1) = Buf(1) + 1
   IF ( Buf(1)<=n ) GOTO 4800
 2000 i = i + 1
   IF ( i<=nset ) GOTO 1800
   IF ( Axif==0 ) THEN
      CALL write(Outfl,0,0,1)
      GOTO 3600
   ELSE
      jharm = jharm + 1
      IF ( jharm<=oharms ) GOTO 1700
      CALL write(Outfl,0,0,1)
      GOTO 3600
   ENDIF
 2100 Buf(1) = Buf(1) + 1
   j = Buf(1) - Imode + 1
   IF ( Buf(1)<=nbrmod ) GOTO 3100
   IF ( nbrep==0 ) THEN
      CALL write(Outfl,0,0,1)
      GOTO 3600
   ELSE
      kx = 1
      ASSIGN 2300 TO retx
      Buf(2) = 3
   ENDIF
 2200 j = Z(kx+1)/10
   gptype = Z(kx+1) - 10*j
   Buf(1) = Z(kx)
   IF ( gptype==3 ) GOTO 3100
 2300 kx = kx + 2
   IF ( kx<=neqdyn ) GOTO 2200
   CALL write(Outfl,0,0,1)
   GOTO 3600
 2400 Buf(1) = Z(i)
   IF ( i==nset ) GOTO 2800
   IF ( Z(i+1)>0 ) GOTO 2800
   n = -Z(i+1)
   Buf(2) = 4
   i = i + 1
   ASSIGN 2600 TO retx
 2500 IF ( Buf(1)<Imode .OR. Buf(1)>nbrmod ) THEN
      IF ( nbrep==0 ) GOTO 3000
      ASSIGN 2700 TO ret
      Buf(2) = 3
      GOTO 4800
   ELSE
      j = Buf(1) - Imode + 1
      GOTO 3100
   ENDIF
 2600 Buf(1) = Buf(1) + 1
   IF ( Buf(1)>n ) GOTO 3000
   GOTO 2500
 2700 j = Z(kx+1)/10
   gptype = Z(kx+1) - 10*j
   IF ( gptype/=3 ) GOTO 2600
   GOTO 3100
 2800 ASSIGN 3000 TO retx
   IF ( Buf(1)<Imode .OR. Buf(1)>nbrmod ) THEN
      IF ( nbrep==0 ) GOTO 3000
      ASSIGN 2900 TO ret
      GOTO 4800
   ELSE
      ASSIGN 3000 TO retx
      j = Buf(1) - Imode + 1
      Buf(2) = 4
      GOTO 3100
   ENDIF
 2900 j = Z(kx+1)/10
   Buf(2) = Z(kx+1) - 10*j
   IF ( Buf(2)==3 ) GOTO 3100
 3000 i = i + 1
   IF ( i<=nset ) GOTO 2400
   IF ( Axif==0 ) THEN
      CALL write(Outfl,0,0,1)
      GOTO 3600
   ELSE
      jharm = jharm + 1
      IF ( jharm<=oharms ) THEN
         i = iset
         GOTO 2400
      ELSE
         CALL write(Outfl,0,0,1)
         GOTO 3600
      ENDIF
   ENDIF
!
!     SCALAR, EXTRA OR MODAL POINT.
!
 3100 j = ivec + ktype*(j-1)
   Bufr(3) = Zz(j)
   DO k = 4 , nwds
      Buf(k) = 0
   ENDDO
   IF ( ktype/=1 ) THEN
!
!     COMPLEX SCALAR, EXTRA OR MODAL POINT.
!
      Bufr(9) = Zz(j+1)
      IF ( format==3 ) THEN
         redner = sqrt(Bufr(3)**2+Bufr(9)**2)
         IF ( redner/=0 ) THEN
            Bufr(9) = atan2(Bufr(9),Bufr(3))*Raddeg
            IF ( Bufr(9)<-0.00005 ) Bufr(9) = Bufr(9) + 360.0
            Bufr(3) = redner
         ENDIF
      ENDIF
   ENDIF
!
!     DETERMINE DESTINATION FOR ENTRY.
!
!
!     IF A FLUID PROBLEM THEN A CHECK IS NOW MADE TO SEE IF THIS
!     HARMONIC IS TO BE OUTPUT
!
 3200 IF ( Axif/=0 ) THEN
      IF ( Buf(1)>=500000 ) THEN
         itemp = Buf(1) - mod(Buf(1),500000)
         itemp = itemp/500000
         IF ( itemp>=oharms ) THEN
            CALL write(Outfl,0,0,1)
            GOTO 3600
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
      GOTO 3300
   ENDIF
   GOTO 3500
 3300 IF ( ix/=nxset ) THEN
      IF ( Z(ix+1)<=0 ) THEN
         IF ( id>=Z(ix) .AND. id<=-Z(ix+1) ) GOTO 3500
         ix = ix + 2
         GOTO 3400
      ENDIF
   ENDIF
   IF ( id==Z(ix) ) GOTO 3500
   ix = ix + 1
 3400 IF ( ix<=nxset ) GOTO 3300
   Buf(1) = 10*id
!
!     WRITE ENTRY ON OUTPUT FILE.
!
 3500 CALL write(Outfl,Buf,nwds,0)
   kwds = kwds + nwds
   Buf(1) = id
   GOTO retx
 3600 IF ( branch==2 ) GOTO 3800
   IF ( branch==3 ) THEN
!
!     TRANSIENT RESPONSE.
!
      IF ( ireq==Ipnl ) GOTO 3900
      IF ( kcount<2 ) THEN
         ireq = Iavel
         kcount = 2
      ELSEIF ( kcount==2 ) THEN
         ireq = Iaacc
         kcount = 3
      ELSE
         ireq = Iadisp
         kcount = 1
         GOTO 3900
      ENDIF
      GOTO 1000
   ELSE
!
!     COMPLEX EIGENVALUES.
!
      jlist = jlist + 3
   ENDIF
 3700 IF ( jcount>=nvects ) GOTO 4100
   IF ( eof/=0 ) GOTO 1000
   GOTO 800
!
!     FREQUENCY RESPONSE.
!
 3800 IF ( kcount==3 ) THEN
      kcount = 1
      incore = 0
      ireq = Iadisp
      jlist = jlist + 2
      IF ( jlist<=nlist .AND. jcount<nvects ) GOTO 1000
      kfrq = 0
      jlist = ilist
      DO i = ilist , nlist , 2
         Z(i+1) = 0
      ENDDO
      IF ( jcount>=nvects ) GOTO 4100
      GOTO 800
   ELSE
      n = ivecn - 1
      omega = Twopi*Zz(jlist)
      DO i = ivec , n , 2
         Bufr(1) = -omega*Zz(i+1)
         Zz(i+1) = omega*Zz(i)
         Zz(i) = Bufr(1)
      ENDDO
      IF ( kcount==2 ) THEN
         ireq = Iaacc
      ELSE
         ireq = Iavel
      ENDIF
      kcount = kcount + 1
      incore = 1
      GOTO 1000
   ENDIF
 3900 jlist = jlist + 1
   IF ( jlist>nlist .OR. jcount>=nvects ) GOTO 4100
   GOTO 1000
!
!     HERE WHEN EOF ENCOUNTERED ON CASE CONTROL.
!
 4000 eof = 1
   IF ( branch==1 .OR. branch==4 ) GOTO 3700
!
!     CONCLUDE PROCESSING.
!
 4100 CALL close(Casecc,Clsrew)
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
 4200 CALL close(Outfl,Clsrew)
 4300 CALL mesage(30,78,0)
 4400 RETURN
!
!     FATAL FILE ERRORS
!
 4500 n = -1
   GOTO 4700
 4600 n = -2
 4700 CALL mesage(n,file,Nam)
   RETURN
!
!     BINARY SEARCH ROUTINE
!
 4800 klo = 1
   khi = kn
   IF ( Axif/=0 ) Buf(1) = jharm*500000 + Buf(1)
   k = (klo+khi+1)/2
 4900 kx = 2*k - 1
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
      GOTO 4900
   ELSE
      k = (klo+khi+1)/2
      GOTO 4900
   ENDIF
END SUBROUTINE vdrb