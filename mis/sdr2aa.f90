
SUBROUTINE sdr2aa
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER App(2) , Casecc , Cei(2) , Clsrew , Esta , Frq(2) , Iacc , Idispl , Idload , Ieldef , Ielf , Ifrout , Iloads , Ilsym ,   &
         & Isload , Ispcf , Istr , Isymfl , Itload , Ittl , Ivel , Mpt , Rd , Rdrew , Scr3 , Sdr2x1(1) , Sort2 , Sysbuf , Trn(2) ,  &
         & Wrt , Wrtrew , Xycdb , Z(1)
   REAL Bgpdt , Bkl(4) , Cstm , Dit , Edt , Eigr , Eqexin , Est , Gptt , Gptta , Harms , Oef1 , Oeigr , Oes1 , Opg1 , Ophig , Oqg1 ,&
      & Ougv1 , Pg , Phig , Pphig , Pugv1 , Qg , Sil , Ugv , X4(72)
   COMMON /blank / App , Sort2
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /sdr2x1/ Sdr2x1 , Ieldef , Itload , Isymfl , Iloads , Idispl , Istr , Ielf , Iacc , Ivel , Ispcf , Ittl , Ilsym , Ifrout ,&
                 & Isload , Idload
   COMMON /sdr2x2/ Casecc , Cstm , Mpt , Dit , Eqexin , Sil , Gptt , Edt , Bgpdt , Pg , Qg , Ugv , Est , Phig , Eigr , Opg1 , Oqg1 ,&
                 & Ougv1 , Oes1 , Oef1 , Pugv1 , Oeigr , Ophig , Pphig , Esta , Gptta , Harms , Xycdb , Scr3
   COMMON /sdr2x4/ X4 , Frq , Trn , Bkl , Cei
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER anynew , arg , buf(10) , buf1 , buf2 , buf3 , dbname , file , formt , i , icc , ilist , imstr , ireq , iset , isetno ,   &
         & ix , ixy , ixysc , ixyset , j , jx , k , l , last , lastxy , ll , ln , loop , master , n , nam(2) , ncc , nmstr , nset , &
         & nxy , nxysc , nxyset , setno , subcse , tab(14) , xsetno , xycdbf
   REAL flag
   INTEGER korsz
!
! End of declarations
!
!
!     SDR2AA PROCESSES THE CASE CONTROL AND XYCDB DATA BLOCKS. IF XYCDB
!     IS PURGED, NO ACTION IS TAKEN. OTHERWISE, OUTPUT REQUESTS IN
!     CASE CONTROL ARE COMPARED WITH XY REQUESTS IN XYCDB. FOR EACH
!     SUBCASE AND EACH REQUEST TYPE, CASE CONTROL IS MODIFIED TO
!     REFLECT THE UNION OF THE REQUESTS. THE NEW CASE CONTROL IS
!     WRITTEN ON A SCRATCH FILE AND THE POINTER TO CASE CONTROL SWITCHED
!
   DATA tab/1 , 6 , 2 , 10 , 3 , 9 , 4 , 11 , 5 , 5 , 6 , 7 , 7 , 8/ , xsetno/100000000/ , nam/4HSDR2 , 4HAA  /
!
!     SET BUFFER POINTERS AND PERFORM GENERAL INITIALIZATION.
!
   buf1 = korsz(Z) - Sysbuf
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   imstr = 1
   master = 1
   lastxy = 0
   anynew = 0
   Sort2 = -1
!
!     OPEN XYCDB. IF PURGED, RETURN.
!
   CALL open(*900,Xycdb,Z(buf1),Rdrew)
   file = Xycdb
   CALL fwdrec(*800,Xycdb)
   CALL fwdrec(*800,Xycdb)
!
!     READ FIRST LINE OF XYCDB. IF SUBCASE = 0 (MEANING DATA APPLIES
!     TO ALL SUBCASES), READ IN DATA FOR ZERO SUBCASE.
!
   last = 0
   xycdbf = Xycdb
   CALL read(*800,*800,Xycdb,buf,6,0,flag)
   Sort2 = 0
   subcse = buf(1)
   IF ( subcse/=0 ) THEN
!
!     HERE IF NO MASTER SUBCASE -- CREATE A DUMMY MASTER.
!
      nmstr = imstr
      ixysc = imstr + 2
      Z(imstr) = 9999
      Z(imstr+1) = 0
      master = -1
      GOTO 200
   ELSE
      i = imstr
      DO
         Z(i) = buf(2)
         Z(i+1) = buf(3)
         i = i + 2
         CALL read(*1900,*100,Xycdb,buf,6,0,flag)
         IF ( buf(1)/=0 ) THEN
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
 200  CALL gopen(Casecc,Z(buf2),Rdrew)
   file = Scr3
   CALL open(*1800,Scr3,Z(buf3),Wrtrew)
   CALL fname(Casecc,buf(9))
   CALL write(Scr3,buf(9),2,1)
!
!     READ DATA FOR ONE SUBCASE. STORE DATA BLOCK AND ID IN OPEN CORE.
!
 300  IF ( master==0 .OR. lastxy/=0 ) GOTO 600
   subcse = buf(1)
   i = ixysc
   DO
      Z(i) = buf(2)
      Z(i+1) = buf(3)
      i = i + 2
      CALL read(*800,*400,xycdbf,buf,6,0,flag)
      IF ( buf(1)/=subcse ) GOTO 500
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
   CALL sort(0,0,2,-2,Z(ixysc),n)
   CALL sort(0,0,2,-1,Z(ixysc),n)
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
   CALL read(*800,*700,Casecc,Z(icc+1),buf3-icc,1,ncc)
   CALL mesage(-8,0,nam)
 700  IF ( subcse==Z(icc+1) ) THEN
      ixy = ixysc
      nxy = nxysc
!
!     PICK UP POINTER TO CURRENT OUTPUT REQUEST.
!     DETERMINE IF XYCDB REQUEST EXISTS.
!
      loop = 1
      GOTO 1000
   ELSEIF ( master/=-1 ) THEN
      ixy = imstr
      nxy = nmstr
      loop = 1
      GOTO 1000
   ELSE
      IF ( subcse>Z(icc+1) ) GOTO 600
      IF ( lastxy==0 ) GOTO 300
      IF ( anynew/=0 ) THEN
         CALL write(Scr3,Z(icc+1),ncc,1)
         GOTO 600
      ENDIF
   ENDIF
!
!     TERMINATE PROCESSING.
!
 800  CALL close(Casecc,Clsrew)
   CALL close(xycdbf,Clsrew)
   CALL close(Scr3,Clsrew)
   IF ( anynew/=0 ) Casecc = Scr3
 900  RETURN
 1000 dbname = tab(loop)
   ix = tab(loop+1)
   ireq = icc + Sdr2x1(ix)
   setno = Z(ireq)
   DO j = ixy , nxy , 2
      IF ( Z(j)==dbname ) GOTO 1100
   ENDDO
   GOTO 1700
 1100 ixyset = j
   DO j = ixyset , nxy , 2
      IF ( Z(j)/=dbname ) GOTO 1200
   ENDDO
   nxyset = nxy
   GOTO 1300
 1200 nxyset = j - 2
!
!     BRANCH ON CASECC REQUEST - NOTE, NO ACTION IF REQUEST = ALL.
!
 1300 IF ( setno<0 ) THEN
!
!     HERE IF CASECC SET = ALL AND XY REQUEST EXISTS - TURN SORT2 ON.
!
      Z(ireq+2) = -iabs(Z(ireq+2))
      Sort2 = 0
   ELSEIF ( setno==0 ) THEN
!
!     HERE IF NO CASECC REQUEST.
!     BUILD XYCDB SET IN CASECC SET FORMAT. ADD SET TO
!     CASECC RECORD AND TURN ON CASECC REQUEST FOR SET.
!
      xsetno = xsetno + 1
      Z(ireq) = xsetno
      Z(ireq+1) = 0
      formt = -2
      IF ( App(1)==Trn(1) ) formt = -1
      Z(ireq+2) = formt
      Sort2 = 0
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
            GOTO 1400
         ELSE
            isetno = nset + 1
            IF ( isetno>=ilist ) EXIT
         ENDIF
      ENDDO
   ENDIF
   GOTO 1700
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
         Sort2 = 0
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
 1700 loop = loop + 2
   IF ( loop<=13 ) GOTO 1000
   CALL write(Scr3,Z(icc+1),ncc,1)
!
!     RETURN TO READ ANOTHER RECORD IN CASE CONTROL OR ANOTHER XYCDB
!     SUBCASE
!
   IF ( master==0 ) GOTO 600
   IF ( subcse>Z(icc+1) ) GOTO 600
   GOTO 300
 1800 DO
      n = -1
!
!     FATAL FILE ERRORS
!
      CALL mesage(n,file,nam)
   ENDDO
 1900 n = -2
   CALL mesage(n,file,nam)
   GOTO 1800
END SUBROUTINE sdr2aa
