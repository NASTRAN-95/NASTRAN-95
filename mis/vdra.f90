
SUBROUTINE vdra
   IMPLICIT NONE
   INTEGER App(2) , Buf(50) , Buf1 , Buf2 , Buf3 , Casecc , Clsrew , Frq(2) , Iaacc , Iacc , Iadisp , Iavel , Idisp , Idload ,      &
         & Ielf , Ifrout , Iloads , Ilsym , Imode , Infile , Ipnl , Ispcf , Istr , Ittl , Ivel , Masks(6) , Modal(2) , Nam(2) , Rd ,&
         & Rdrew , Scr1 , Scr3 , Sdr2 , Sort2 , Sysbuf , Trn(2) , Vdrcom(1) , Vdrreq , Wrt , Wrtrew , Xset0 , Xycdb , Z(1)
   REAL Cei(2) , Direct(2) , Eqdyn , Form(2) , Oeigs , Opnl1 , Outfle , Output , Pnl , Pp , Usetd
   COMMON /blank / App , Form , Sort2 , Output , Sdr2 , Imode
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf
   COMMON /vdrcom/ Vdrcom , Idisp , Ivel , Iacc , Ispcf , Iloads , Istr , Ielf , Iadisp , Iavel , Iaacc , Ipnl , Ittl , Ilsym ,     &
                 & Ifrout , Idload , Casecc , Eqdyn , Usetd , Infile , Oeigs , Pp , Xycdb , Pnl , Outfle , Opnl1 , Scr1 , Scr3 ,    &
                 & Buf1 , Buf2 , Buf3 , Nam , Buf , Masks , Cei , Frq , Trn , Direct , Xset0 , Vdrreq , Modal
   COMMON /zzzzzz/ Z
   INTEGER anynew , arg , dbname , file , format , i , icc , ilist , imstr , ireq , iset , isetno , ix , ixy , ixysc , ixyset , j , &
         & jx , k , l , last , lastxy , ll , ln , loop , master , n , ncc , nmstr , nset , nxy , nxysc , nxyset , setno , subcse ,  &
         & xsetno , xycdbf
   REAL flag
   INTEGER korsz
!
!     VDRA PROCESSES THE CASE CONTROL AND XYCDB DATA BLOCKS. IF XYCDB
!     IS PURGED, NO ACTION IS TAKEN. OTHERWISE, OUTPUT REQUESTS IN
!     CASE CONTROL ARE COMPARED WITH XY REQUESTS IN XYCDB. FOR EACH
!     SUBCASE AND EACH REQUEST TYPE, CASE CONTROL IS MODIFIED TO REFLECT
!     THE UNION OF THE REQUESTS. THE NEW CASE CONTROL IS WRITTEN ON A
!     SCRATCH FILE AND THE POINTER TO CASE CONTROL SWITCHED.
!
!
!     SET BUFFER POINTERS AND PERFORM GENERAL INITIALIZATION.
!
   Buf1 = korsz(Z) - Sysbuf
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   ixy = 1
   lastxy = 0
   anynew = 0
   Sdr2 = -1
   Vdrreq = 0
   xsetno = Xset0
   imstr = 1
   master = 1
!
!     OPEN XYCDB. IF PURGED, RETURN.
!
   CALL open(*900,Xycdb,Z(Buf1),Rdrew)
   file = Xycdb
   CALL fwdrec(*900,Xycdb)
   CALL fwdrec(*900,Xycdb)
!
!     READ FIRST LINE OF XYCDB. IF SUBCASE = 0 (MEANING DATA APPLIES
!     TO ALL SUBCASES), READ IN DATA FOR ZERO SUBCASE.
!
   last = 0
   xycdbf = Xycdb
   CALL read(*800,*800,Xycdb,Buf,6,0,flag)
   subcse = Buf(1)
   IF ( subcse/=0 ) THEN
!
!     HERE IF NO MASTER SUBCASE -- CREATE A DUMMY MASTER.
!
      nmstr = imstr
      ixysc = imstr + 2
      Z(imstr) = 9999
      Z(imstr+1) = 0
      GOTO 200
   ELSE
      i = imstr
      DO
         Z(i) = Buf(2)
         Z(i+1) = Buf(3)
         i = i + 2
         CALL read(*1800,*100,Xycdb,Buf,6,0,flag)
         IF ( Buf(1)/=0 ) THEN
            nmstr = i - 2
            ixysc = i
            GOTO 200
         ENDIF
      ENDDO
   ENDIF
!
!     HERE IF MASTER SUBCASE IS THE ONLY SUBCASE IN XYCDB.
!
 100  nmstr = i - 2
   nxysc = nmstr
   master = 0
   lastxy = 1
!
!     REDUCE LIST TO UNIQUE PAIRS
!
   IF ( imstr/=nmstr ) THEN
      nmstr = nmstr - 2
      j = imstr
      DO i = imstr , nmstr , 2
         IF ( Z(i+2)/=Z(j) .OR. Z(i+3)/=Z(j+1) ) THEN
            Z(j+2) = Z(i+2)
            Z(j+3) = Z(i+3)
            j = j + 2
         ENDIF
      ENDDO
      nmstr = j
      nxysc = nmstr
   ENDIF
!
!     OPEN CASE CONTROL AND SCRATCH FILE FOR MODIFIED CASE CONTROL
!
 200  CALL gopen(Casecc,Z(Buf2),0)
   CALL gopen(Scr3,Z(Buf3),1)
!
!     READ DATA FOR ONE SUBCASE. STORE DATA BLOCK AND ID IN OPEN CORE.
!
 300  IF ( master==0 .OR. lastxy/=0 ) GOTO 600
   subcse = Buf(1)
   i = ixysc
   DO
      Z(i) = Buf(2)
      Z(i+1) = Buf(3)
      i = i + 2
      CALL read(*800,*400,xycdbf,Buf,6,0,flag)
      IF ( Buf(1)/=subcse ) GOTO 500
   ENDDO
 400  lastxy = 1
!
!     COPY DATA FROM MASTER SUBCASE AFTER CURRENT SUBCASE.
!     THEN SORT DATA TOGETHER TO FORM SORTED UNION.
!
 500  DO j = imstr , nmstr , 2
      Z(i) = Z(j)
      Z(i+1) = Z(j+1)
      i = i + 2
   ENDDO
   n = i - ixysc
   CALL sort2k(0,0,2,1,Z(ixysc),n)
!
!     REDUCE LIST TO UNIQUE PAIRS.
!
   nxysc = i - 4
   j = ixysc
   DO i = ixysc , nxysc , 2
      IF ( Z(i+2)/=Z(j) .OR. Z(i+3)/=Z(j+1) ) THEN
         Z(j+2) = Z(i+2)
         Z(j+3) = Z(i+3)
         j = j + 2
      ENDIF
   ENDDO
   nxysc = j
!
!     READ A RECORD IN CASE CONTROL. SET POINTERS FOR XYCDB DATA TO
!     EITHER MASTER SUBCASE OR CURRENT SUBCASE IN CORE.
!
 600  icc = nxysc + 1
   CALL read(*800,*700,Casecc,Z(icc+1),Buf3-icc,1,ncc)
   CALL mesage(-8,0,Nam)
 700  IF ( master==0 .OR. Z(icc+1)/=subcse ) THEN
      ixy = imstr
      nxy = nmstr
   ELSE
      ixy = ixysc
      nxy = nxysc
   ENDIF
!
!     PICK UP POINTER TO CURRENT OUTPUT REQUEST.
!     DETERMINE IF XYCDB REQUEST EXISTS.
!
   loop = 1
   GOTO 1000
!
!     TERMINATE PROCESSING.
!
 800  CALL close(Casecc,Clsrew)
   CALL close(xycdbf,Clsrew)
   CALL close(Scr3,Clsrew)
   IF ( anynew/=0 ) Casecc = Scr3
   RETURN
!
 900  Vdrreq = 1
   CALL close(Xycdb,Clsrew)
   RETURN
 1000 dbname = loop
   ireq = icc + Vdrcom(loop+1)
   setno = Z(ireq)
   DO j = ixy , nxy , 2
      IF ( Z(j)==dbname ) GOTO 1100
   ENDDO
!
!     HERE IF NO XYCDB REQUEST EXISTS.
!
   IF ( setno/=0 ) THEN
      IF ( loop>7 ) THEN
         Vdrreq = 1
      ELSE
         Sdr2 = 1
      ENDIF
   ENDIF
   GOTO 1700
 1100 ixyset = j
   DO j = ixyset , nxy , 2
      IF ( Z(j)/=dbname ) GOTO 1200
   ENDDO
   nxyset = nxy
   GOTO 1300
 1200 nxyset = j - 2
!
!     BRANCH ON CASECC REQUEST-- NOTE, NO ACTION IF REQUEST = ALL.
!
 1300 IF ( loop>7 ) THEN
      Vdrreq = 1
      Sort2 = +1
      IF ( setno<0 ) THEN
!
!     HERE IF CASECC SET = ALL AND XY REQUEST EXISTS - TURN SORT 2 ON.
!
         Z(ireq+2) = -iabs(Z(ireq+2))
         GOTO 1700
      ELSEIF ( setno==0 ) THEN
!
!     HERE IF NO CASECC REQUEST.
!     BUILD XYCDB SET IN CASECC SET FORMAT. ADD SET TO
!     CASECC RECORD AND TURN ON CASECC REQUEST FOR SET.
!
         xsetno = xsetno + 1
         Z(ireq) = xsetno
         Z(ireq+1) = 0
         format = -2
         IF ( App(1)==Trn(1) ) format = -1
         Z(ireq+2) = format
         ix = icc + ncc + 1
         Z(ix) = xsetno
         jx = ix + 2
         Z(jx) = Z(ixyset+1)
         IF ( ixyset/=nxyset ) THEN
            ixyset = ixyset + 2
            n = 1
            DO j = ixyset , nxyset , 2
               IF ( Z(j+1)-Z(jx)==n ) THEN
                  n = n + 1
               ELSEIF ( n/=1 ) THEN
                  Z(jx+1) = -Z(j-1)
                  jx = jx + 2
                  Z(jx) = Z(j+1)
                  n = 1
               ELSE
                  jx = jx + 1
                  Z(jx) = Z(j+1)
               ENDIF
            ENDDO
            IF ( n/=1 ) THEN
               jx = jx + 1
               Z(jx) = -Z(nxyset+1)
            ENDIF
         ENDIF
         Z(ix+1) = jx - ix - 1
         ncc = ncc + Z(ix+1) + 2
         anynew = 1
         GOTO 1700
      ELSE
!
!     HERE IF CASECC SET AND XYCDB SET EXIST.
!     FIRST, LOCATE CASECC SET.
!
         ilist = icc + ncc + 3
         ix = icc + Ilsym
         isetno = ix + Z(ix) + 1
         DO
            iset = isetno + 2
            nset = Z(isetno+1) + iset - 1
            IF ( Z(isetno)==setno ) THEN
!
!     COMPARE EACH POINT IN XYCDB REQUEST WITH CASECC SET.
!     ADD ANY POINTS IN XYCDB NOT IN CASECC TO CASECC SET.
!
               i = iset
               j = ixyset
               k = ilist
               l = iset
               arg = Z(j+1)
               EXIT
            ELSE
               isetno = nset + 1
               IF ( isetno>=ilist ) GOTO 1700
            ENDIF
         ENDDO
      ENDIF
   ELSE
      Sdr2 = +1
      GOTO 1700
   ENDIF
 1400 DO
      IF ( i<nset ) THEN
         IF ( Z(i+1)>0 ) EXIT
         n = 2
         IF ( arg<Z(i) ) GOTO 1500
         IF ( arg/=Z(i) ) THEN
            IF ( arg+Z(i+1)<0 ) THEN
            ELSEIF ( arg+Z(i+1)==0 ) THEN
               i = i + n
            ELSE
               i = i + n
               CYCLE
            ENDIF
         ENDIF
         GOTO 1600
      ELSEIF ( i==nset ) THEN
         EXIT
      ELSE
         GOTO 1500
      ENDIF
   ENDDO
   n = 1
   IF ( arg<Z(i) ) THEN
   ELSEIF ( arg==Z(i) ) THEN
      i = i + n
      GOTO 1600
   ELSE
      i = i + n
      GOTO 1400
   ENDIF
 1500 IF ( l/=i ) THEN
      ln = i - 1
      ll = l
      DO l = ll , ln
         Z(k) = Z(l)
         k = k + 1
      ENDDO
      l = i
   ENDIF
   Z(k) = arg
   k = k + 1
 1600 j = j + 2
   IF ( j<=nxyset ) THEN
      arg = Z(j+1)
      GOTO 1400
   ELSE
      n = k - ilist
      IF ( n/=0 ) THEN
         IF ( l<=nset ) THEN
            DO ll = l , nset
               Z(k) = Z(ll)
               k = k + 1
            ENDDO
            n = k - ilist
         ENDIF
!
!     IF NO NEW POINTS IN SET, CURRENT CASECC SET IS UNION.
!     OTHERWISE, NEW SET IS UNION. TURN ON REQUEST FOR IT AND
!     EXTEND END OF CASECC RECORD.
!
         xsetno = xsetno + 1
         Z(ireq) = xsetno
         Z(ireq+1) = 10*setno + Z(ireq+1)
         Z(ireq+2) = -iabs(Z(ireq+2))
         Z(ilist-2) = xsetno
         Z(ilist-1) = n
         ncc = ncc + n + 2
         anynew = 1
      ENDIF
   ENDIF
!
!     TEST FOR COMPLETION OF ALL CASECC REQUESTS FOR CURRENT SUBCASE.
!     WHEN COMPLETE, WRITE CURRENT SUBCASE ON SCRATCH FILE.
!
 1700 loop = loop + 1
   IF ( loop<=11 ) GOTO 1000
   CALL write(Scr3,Z(icc+1),ncc,1)
!
!     RETURN TO READ ANOTHER RECORD IN CASE CONTROL OR ANOTHER XYCDB
!     SUBCASE
!
   IF ( master==0 ) GOTO 600
   IF ( subcse>Z(icc+1) ) GOTO 600
   GOTO 300
 1800 DO
      n = -2
!
!     FATAL FILE ERROR
!
      CALL mesage(n,file,Nam)
   ENDDO
END SUBROUTINE vdra
