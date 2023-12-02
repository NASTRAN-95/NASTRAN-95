!*==gp3b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp3b
   IMPLICIT NONE
   USE C_BLANK
   USE C_GP3COM
   USE C_NAMES
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , flag , geomp , gptt , i , id , ifile , isave , itabl , itempd , j , k , khi , klo , kn , l , n , n1 , neqx ,   &
            & ni , nodef , nogo , ntempd , nx
   LOGICAL :: intern
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL bckrec , close , fname , fwdrec , locate , mesage , open , page2 , preloc , read , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GP3B BUILDS THE GRID POINT TEMPERATURE TABLE (GPTT).
!     TEMPD AND TEMP CARDS ARE READ.
!     THE GPTT HEADER CONTAINS THE FILE NAME PLUS 3 WORDS FOR EACH
!     TEMPERATURE SET.
!       WORD 1 = TEMP SET ID.
!       WORD 2 = DEFAULT TEMP OR -1 IF NO DEFAULT TEMP.
!       WORD 3 = RECORD NO. (AFTER HEADER RECORD) OF TEMPERATURE DATA
!                FOR THE SET, OR
!                ZERO IF ONLY A DEFAULT TEMP IS DEFINED FOR THE SET.
!     DATA RECORDS OF THE GPTT CONSIST OF PAIRS OF EXTERNAL INDEX AND
!     TEMPERATURE. EACH DATA RECORD IS SORTED ON EXTERNAL INDEX.
!
!     AN IDENTICAL SET OF RECORDS WITH INTERNAL INDICES IS APPENDED AT
!     THE END OF THE GPTT.
!
!
   !>>>>EQUIVALENCE (Geom3,Geomp) , (Gptt,Scr1)
   DATA nam/4HGP3B , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TURN NODEF FLAG ON
!
         id = 0
         nodef = 0
!
!     READ EQEXIN INTO CORE
!
         file = Eqexin
         CALL open(*160,Eqexin,Z(Buf2),Rdrew)
         CALL fwdrec(*180,Eqexin)
         CALL read(*180,*20,Eqexin,Z,Buf3,1,neqx)
         CALL mesage(-8,0,nam)
 20      CALL close(Eqexin,Clsrew)
         kn = neqx/2
         itempd = neqx + 1
         itabl = itempd
!
!     READ TEMPERATURE DEFAULT CARDS (IF PRESENT)
!
         file = geomp
         CALL preloc(*99999,Z(Buf1),geomp)
         CALL locate(*60,Z(Buf1),Tempd,flag)
         i = itempd
         nodef = 1
         Notemp = 1
         DO
            CALL read(*180,*40,geomp,Z(i),2,0,flag)
            i = i + 2
         ENDDO
 40      itabl = i
         ntempd = i - 2
         n = itabl - itempd
         CALL sort(0,0,2,1,Z(itempd),n)
!
!     READ TEMP CARDS.  DETERMINE NO. OF TEMP SETS
!     FOR EACH SET ID, LOOK UP THE DEFAULT TEMPERATURE
!     WRITE SET ID, DEFAULT TEMP (OR -1) AND RECORD NUMBER
!     OF THE TEMPERATURE DATA (OR 0) IN THE GPTT HEADER
!
 60      j = 0
         k = itempd
         i = itabl
         l = 1
         file = geomp
         CALL locate(*140,Z(Buf1),Temp,flag)
         Notemp = 1
         file = gptt
         CALL open(*160,gptt,Z(Buf2),Wrtrew)
         CALL fname(gptt,Buf)
         CALL write(gptt,Buf,2,0)
!
!     OPEN ETT AS TEMPORARY SCRATCH TO FORM IDENTICAL FILE WITH
!     INTERNAL NOTATION
!
         file = Ett
         CALL open(*160,Ett,Z(Buf3),Wrtrew)
         CALL fname(Ett,Buf)
         CALL write(Ett,Buf,2,0)
         file = geomp
         DO
            CALL read(*180,*80,geomp,Buf,3,0,flag)
            j = j + 1
            IF ( id/=Buf(1) ) THEN
               id = Buf(1)
               Z(i) = j
               i = i + 1
               IF ( nodef==0 ) THEN
                  Buf(2) = -1
               ELSE
                  DO WHILE ( k<=ntempd )
                     IF ( id<Z(k) ) THEN
                        Buf(2) = -1
                     ELSEIF ( id==Z(k) ) THEN
                        Buf(2) = Z(k+1)
                        k = k + 2
                     ELSE
                        Buf(1) = Z(k)
                        Buf(2) = Z(k+1)
                        Buf(3) = 0
                        CALL write(gptt,Buf,3,0)
                        CALL write(Ett,Buf,3,0)
                        k = k + 2
                        CYCLE
                     ENDIF
                     GOTO 65
                  ENDDO
                  Buf(2) = -1
               ENDIF
 65            Buf(3) = l
               Buf(1) = id
               l = l + 1
               CALL write(gptt,Buf,3,0)
               CALL write(Ett,Buf,3,0)
               j = 0
            ENDIF
         ENDDO
 80      IF ( nodef/=0 ) THEN
            IF ( k<=ntempd ) THEN
               Buf(3) = 0
               DO l = k , ntempd , 2
                  Buf(1) = Z(l)
                  Buf(2) = Z(l+1)
                  CALL write(Ett,Buf,3,0)
                  CALL write(gptt,Buf,3,0)
               ENDDO
            ENDIF
         ENDIF
         CALL write(gptt,0,0,1)
         CALL write(Ett,0,0,1)
         CALL bckrec(geomp)
         n = i
         Z(n) = j + 1
         i = itabl + 1
!
!     READ EACH TEMP SET
!     SORT ON EXTERNAL INDEX AND WRITE ON GPTT
!
         ifile = gptt
         intern = .FALSE.
         isave = i
         nogo = 0
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*180,*200,geomp,0,-3,0,flag)
         n1 = n + 1
         spag_nextblock_1 = 3
      CASE (3)
         j = n1
         nx = Z(i)
         ni = 1
         spag_nextblock_1 = 4
      CASE (4)
         CALL read(*180,*200,geomp,Buf,3,0,flag)
         IF ( intern ) THEN
!
!     INTERNAL BINARY SEARCH ROUTINE.
!
            klo = 1
            khi = kn
            k = (klo+khi+1)/2
            SPAG_Loop_1_1: DO
               IF ( Buf(2)<Z(2*k-1) ) THEN
                  khi = k
               ELSEIF ( Buf(2)==Z(2*k-1) ) THEN
                  Buf(2) = Z(2*k)
                  EXIT SPAG_Loop_1_1
               ELSE
                  klo = k
               ENDIF
               IF ( khi-klo<1 ) THEN
                  CALL mesage(-30,9,Buf)
                  CALL mesage(j,file,nam)
                  RETURN
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
            ENDDO SPAG_Loop_1_1
         ENDIF
         Z(j) = Buf(2)
         Z(j+1) = Buf(3)
         j = j + 2
         IF ( j>=Buf3 ) THEN
            j = -8
            CALL mesage(j,file,nam)
            RETURN
         ELSE
            ni = ni + 1
            IF ( ni<=nx ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nx = j - n1
            CALL sort(0,0,2,1,Z(n1),nx)
!
!     TEST FOR UNIQUENESS OF POINT AND TEMPERATURE
!
            khi = j - 1
            klo = n1 + 2
            k = j
            IF ( klo<khi ) THEN
               k = klo
               DO j = klo , khi , 2
                  IF ( Z(j)/=Z(j-2) ) THEN
!
!     VALID TEMPERATURE
!
                     Z(k) = Z(j)
                     Z(k+1) = Z(j+1)
                     k = k + 2
                  ELSE
!
!     NOT FATAL IF SAME TEMPERATURE
!
                     IF ( Z(j+1)/=Z(j-1) ) nogo = nogo + 1
                     IF ( .NOT.(intern) ) THEN
                        CALL page2(2)
                        WRITE (Nout,99001) Ufm , Z(j-1) , Z(j+1) , Z(j)
99001                   FORMAT (A23,' 2100, TEMPERATURE SPECIFIED HAS ',1P,E10.3,4H AND,1P,E10.3,' FOR GRID',I9)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
            nx = k - n1
            CALL write(ifile,Z(n1),nx,1)
            i = i + 1
            IF ( i<=n ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     NOW DO SAME AS ABOVE WITH OUTPUT IN INTERNAL INDEX NOTATION.
!
            IF ( nogo/=0 ) CALL mesage(-61,nogo,0)
            IF ( .NOT.(intern) ) THEN
               CALL bckrec(geomp)
               intern = .TRUE.
               ifile = Ett
               i = isave
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     NOW APPEND ENTIRE ETT FILE TO GPTT FILE
!
         file = Ett
         CALL close(Ett,Clsrew)
         CALL open(*160,Ett,Z(Buf3),Rdrew)
         spag_nextblock_1 = 6
      CASE (6)
         DO
            CALL read(*120,*100,Ett,Z,Buf3-1,0,flag)
            CALL write(gptt,Z,Buf3-1,0)
         ENDDO
 100     CALL write(gptt,Z,flag,1)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 120     CALL close(gptt,Clsrew)
         CALL close(Ett,Clsrew)
         spag_nextblock_1 = 7
      CASE (7)
         CALL close(geomp,Clsrew)
         RETURN
!
!     NO TEMP CARDS PRESENT. IF NO DEFAULT CARDS, NO GPTT.
!     OTHERWISE, GPTT IS COMPRISED ONLY OF DEFAULT TEMPERATURES.
!     WRITE THE SET IDS AND DEFAULT TEMPS IN THE HEADER RECORD.
!
 140     IF ( nodef==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = gptt
         CALL open(*160,gptt,Z(Buf2),Wrtrew)
         CALL fname(gptt,Buf)
         CALL write(gptt,Buf,2,0)
         file = Ett
         CALL open(*160,Ett,Z(Buf3),Wrtrew)
         CALL fname(Ett,Buf)
         CALL write(Ett,Buf,2,0)
         Buf(3) = 0
         DO k = itempd , ntempd , 2
            Buf(1) = Z(k)
            Buf(2) = Z(k+1)
            CALL write(gptt,Buf,3,0)
         ENDDO
         CALL write(Ett,Buf,3,0)
         CALL write(gptt,0,0,1)
         CALL write(Ett,0,0,1)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     FATAL ERROR MESAGES
!
 160     j = -1
         CALL mesage(j,file,nam)
         RETURN
 180     j = -2
         CALL mesage(j,file,nam)
         RETURN
 200     j = -3
         CALL mesage(j,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99999 END SUBROUTINE gp3b
