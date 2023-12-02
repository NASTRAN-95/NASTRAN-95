!*==vdra.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vdra
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_SYSTEM
   USE C_VDRCOM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: anynew , arg , dbname , file , format , i , icc , ilist , imstr , ireq , iset , isetno , ix , ixy , ixysc , ixyset ,  &
            & j , jx , k , l , last , lastxy , ll , ln , loop , master , n , ncc , nmstr , nset , nxy , nxysc , nxyset , setno ,    &
            & subcse , xsetno , xycdbf
   REAL :: flag
   EXTERNAL close , fwdrec , gopen , korsz , mesage , open , read , sort2k , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL open(*100,Xycdb,Z(Buf1),Rdrew)
         file = Xycdb
         CALL fwdrec(*100,Xycdb)
         CALL fwdrec(*100,Xycdb)
!
!     READ FIRST LINE OF XYCDB. IF SUBCASE = 0 (MEANING DATA APPLIES
!     TO ALL SUBCASES), READ IN DATA FOR ZERO SUBCASE.
!
         last = 0
         xycdbf = Xycdb
         CALL read(*80,*80,Xycdb,Buf,6,0,flag)
         subcse = Buf(1)
         IF ( subcse/=0 ) THEN
!
!     HERE IF NO MASTER SUBCASE -- CREATE A DUMMY MASTER.
!
            nmstr = imstr
            ixysc = imstr + 2
            Z(imstr) = 9999
            Z(imstr+1) = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            i = imstr
            DO
               Z(i) = Buf(2)
               Z(i+1) = Buf(3)
               i = i + 2
               CALL read(*120,*20,Xycdb,Buf,6,0,flag)
               IF ( Buf(1)/=0 ) THEN
                  nmstr = i - 2
                  ixysc = i
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     HERE IF MASTER SUBCASE IS THE ONLY SUBCASE IN XYCDB.
!
 20      nmstr = i - 2
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
         spag_nextblock_1 = 2
      CASE (2)
!
!     OPEN CASE CONTROL AND SCRATCH FILE FOR MODIFIED CASE CONTROL
!
         CALL gopen(Casecc,Z(Buf2),0)
         CALL gopen(Scr3,Z(Buf3),1)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ DATA FOR ONE SUBCASE. STORE DATA BLOCK AND ID IN OPEN CORE.
!
         IF ( master==0 .OR. lastxy/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         subcse = Buf(1)
         i = ixysc
         DO
            Z(i) = Buf(2)
            Z(i+1) = Buf(3)
            i = i + 2
            CALL read(*80,*40,xycdbf,Buf,6,0,flag)
            IF ( Buf(1)/=subcse ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 40      lastxy = 1
         spag_nextblock_1 = 4
      CASE (4)
!
!     COPY DATA FROM MASTER SUBCASE AFTER CURRENT SUBCASE.
!     THEN SORT DATA TOGETHER TO FORM SORTED UNION.
!
         DO j = imstr , nmstr , 2
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
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ A RECORD IN CASE CONTROL. SET POINTERS FOR XYCDB DATA TO
!     EITHER MASTER SUBCASE OR CURRENT SUBCASE IN CORE.
!
         icc = nxysc + 1
         CALL read(*80,*60,Casecc,Z(icc+1),Buf3-icc,1,ncc)
         CALL mesage(-8,0,Nam)
 60      IF ( master==0 .OR. Z(icc+1)/=subcse ) THEN
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
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     TERMINATE PROCESSING.
!
 80      CALL close(Casecc,Clsrew)
         CALL close(xycdbf,Clsrew)
         CALL close(Scr3,Clsrew)
         IF ( anynew/=0 ) Casecc = Scr3
         RETURN
!
 100     Vdrreq = 1
         CALL close(Xycdb,Clsrew)
         RETURN
      CASE (6)
         dbname = loop
         ireq = icc + Vdrcom(loop+1)
         setno = Z(ireq)
         DO j = ixy , nxy , 2
            IF ( Z(j)==dbname ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         ixyset = j
         DO j = ixyset , nxy , 2
            IF ( Z(j)/=dbname ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         nxyset = nxy
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         nxyset = j - 2
         spag_nextblock_1 = 9
      CASE (9)
!
!     BRANCH ON CASECC REQUEST-- NOTE, NO ACTION IF REQUEST = ALL.
!
         IF ( loop>7 ) THEN
            Vdrreq = 1
            Sort2 = +1
            IF ( setno<0 ) THEN
!
!     HERE IF CASECC SET = ALL AND XY REQUEST EXISTS - TURN SORT 2 ON.
!
               Z(ireq+2) = -iabs(Z(ireq+2))
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     HERE IF CASECC SET AND XYCDB SET EXIST.
!     FIRST, LOCATE CASECC SET.
!
               ilist = icc + ncc + 3
               ix = icc + Ilsym
               isetno = ix + Z(ix) + 1
               SPAG_Loop_1_1: DO
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
                     EXIT SPAG_Loop_1_1
                  ELSE
                     isetno = nset + 1
                     IF ( isetno>=ilist ) THEN
                        spag_nextblock_1 = 13
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ENDIF
         ELSE
            Sdr2 = +1
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         SPAG_Loop_1_2: DO
            IF ( i<nset ) THEN
               IF ( Z(i+1)>0 ) EXIT SPAG_Loop_1_2
               n = 2
               IF ( arg<Z(i) ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( arg/=Z(i) ) THEN
                  IF ( arg+Z(i+1)<0 ) THEN
                  ELSEIF ( arg+Z(i+1)==0 ) THEN
                     i = i + n
                  ELSE
                     i = i + n
                     CYCLE
                  ENDIF
               ENDIF
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==nset ) THEN
               EXIT SPAG_Loop_1_2
            ELSE
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_2
         n = 1
         IF ( arg<Z(i) ) THEN
         ELSEIF ( arg==Z(i) ) THEN
            i = i + n
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
            i = i + n
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         IF ( l/=i ) THEN
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
         spag_nextblock_1 = 12
      CASE (12)
         j = j + 2
         IF ( j<=nxyset ) THEN
            arg = Z(j+1)
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
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
         spag_nextblock_1 = 13
      CASE (13)
!
!     TEST FOR COMPLETION OF ALL CASECC REQUESTS FOR CURRENT SUBCASE.
!     WHEN COMPLETE, WRITE CURRENT SUBCASE ON SCRATCH FILE.
!
         loop = loop + 1
         IF ( loop<=11 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL write(Scr3,Z(icc+1),ncc,1)
!
!     RETURN TO READ ANOTHER RECORD IN CASE CONTROL OR ANOTHER XYCDB
!     SUBCASE
!
         IF ( master==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( subcse<=Z(icc+1) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     DO
            n = -2
!
!     FATAL FILE ERROR
!
            CALL mesage(n,file,Nam)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE vdra
