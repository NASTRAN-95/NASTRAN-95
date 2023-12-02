!*==gp3a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp3a
   USE c_blank
   USE c_gp3com
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , first , flag , gpoint , i , id , idcmld , ii , iii , ilist , iset , itabl , jj , jj1 , jjj , jjn , jstop , jx ,&
            & k , k1 , k2 , khi , kkk , klist , klo , kn , kset , ktabl , lset , mset , n , ncore , neqx , nkey , nlist , nogo ,    &
            & nread , nset , nskip , nwds , nwds1 , setid
   INTEGER , SAVE :: irfrc
   INTEGER , DIMENSION(80) :: ksystm
   INTEGER , DIMENSION(2) , SAVE :: nam
   LOGICAL :: piez
   EXTERNAL close , fname , fwdrec , locate , mesage , open , page , preloc , read , rewind , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     GP3A BUILDS THE STATIC LOADS TABLE (SLT).
!     FORCE, FORCE1, FORCE2, MOMENT, MOMNT1, MOMNT2, GRAV, PLOAD, SLOAD
!     AND LOAD CARDS ARE READ. EXTERNAL GRID NOS. ARE CONVERTED TO
!     INTERNAL INDICES. EACH LOAD SET ID (EXCEPT ON LOAD CARD) IS
!     WRITTEN IN THE HEADER RECORD OF THE SLT. THE SLT THEN COMPRISES
!     ONE LOGICAL RECORD PER LOAD SET. THE LAST RECORD OF THE SLT
!     CONTAINS THE LOAD CARDS. RFORCE CARD ADDED IN AUGUST, 1968.
!     PLOAD3 CARD ADDED ON HALLOWEEN 1972
!
   !>>>>EQUIVALENCE (Ksystm(1),Isb)
   DATA nam/4HGP3A , 4H    / , irfrc/9/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     READ EQEXIN INTO CORE. INITIALIZE BINARY SEARCH ROUTINE.
!
         file = eqexin
         CALL open(*140,eqexin,z(buf1),rdrew)
         CALL fwdrec(*160,eqexin)
         CALL read(*160,*20,eqexin,z,buf2,1,neqx)
         CALL mesage(-8,0,nam)
 20      CALL close(eqexin,clsrew)
         kn = neqx/2
         nogo = 0
!
!     INITIALIZE POINTERS AND OPEN SCR1 AND GEOM3.
!
         iset = buf2 - 2
         kset = iset
         ilist = neqx + 1
         klist = ilist
         ktabl = 1
         first = 1
         file = scr1
         CALL open(*140,scr1,z(buf2),wrtrew)
!
!     IF PLOAD2 CARDS PRESENT, INITIALIZE TO READ PLOAD DATA FROM SCR2
!     INSTEAD OF GEOM3.
!
         IF ( nopld2==0 ) THEN
            first = 0
         ELSE
            file = scr2
            CALL open(*140,scr2,z(buf1),rdrew)
            GOTO 40
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         file = geom3
         CALL open(*140,geom3,z(buf1),rdrew)
         CALL fwdrec(*160,geom3)
!
!     READ 3-WORD RECORD ID. IF ID BELONGS TO LOAD SET, TURN NOLOAD FLAG
!     OFF.
!     SET 1ST WORD IN STATUS ENTRY TO CURRENT POINTER IN LIST TABLE.
!     SET PARAMETERS FOR CONVERSION OF GRID NOS. TO INTERNAL INDICES.
!
 40      CALL read(*80,*40,file,buf,3,0,flag)
         DO i = 1 , ntypes , 2
            IF ( buf(1)==cardid(i) .AND. buf(2)==cardid(i+1) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         CALL fwdrec(*80,file)
         GOTO 40
      CASE (4)
         noload = 1
         IF ( first/=1 ) THEN
!
!     IF I POINTS TO PLOAD RECORD AND PLOAD2 CARDS ARE PRESENT, THEN
!     PLOAD DATA IS ALREADY PROCESSED. IN THIS CASE, SKIP PLOAD RECORD.
!     IF I POINTS TO PLOAD3 RECORD ON GEOM3, SKIP RECORD.
!
            IF ( i==ipload .AND. nopld2/=0 .AND. nopld2/=2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( i==ipld3 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         status(i) = klist - ilist + 1
         nwds = carddt(i)
         nwds1 = nwds - 1
         jx = carddt(i+1)
         jj1 = jx + 1
         jjn = jx + mask(jx)
         id = 0
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ A LOAD CARD. IF SET ID IS DIFFERENT FROM LAST READ (OR 1ST
!     ONE) STORE SET ID IN POINTER LIST AND IN SET LIST. STORE POINTER
!     IN POINTER LIST. IF NOT FIRST CARD OF TYPE, STORE WORD COUNT IN
!     POINTER LIST.
!
         CALL read(*160,*60,file,buf,nwds,0,flag)
         IF ( buf(1)/=id ) THEN
            z(klist) = buf(1)
            z(klist+1) = ktabl
            IF ( id/=0 ) z(klist-1) = n
            id = buf(1)
            n = 0
            klist = klist + 3
            z(kset) = buf(1)
            kset = kset - 1
         ENDIF
!
!     CONVERT EXTERNAL GRID NOS. ON CARD TO INTERNAL NOS. INCREMENT
!     WORD COUNT. WRITE LOAD CARD (WITHOUT SET ID) ON SCR1.
!
         IF ( jx==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         jj = jj1
         jstop = 0
         spag_nextblock_1 = 6
      CASE (6)
         IF ( jstop==0 ) THEN
            jx = mask(jj)
            IF ( jx<=0 ) THEN
               jx = -jx
               jstop = 1
            ENDIF
         ELSE
            jx = jx + 1
         ENDIF
         gpoint = buf(jx)
         piez = .FALSE.
         IF ( gpoint<0 .AND. ksystm(78)==1 ) piez = .TRUE.
         IF ( piez ) gpoint = -gpoint
         IF ( .NOT.(gpoint==-1 .AND. (cardid(i)==3209 .OR. cardid(i)==3409)) ) THEN
            IF ( gpoint/=0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         IF ( piez ) gpoint = -gpoint
         buf(jx) = gpoint
         jj = jj + 1
         IF ( jj<=jjn ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     CHECK FOR PLOAD4 CARD
!
         IF ( i==49 ) THEN
!
!     CHECK FOR THRU OPTION ON PLOAD4 CARD
!
            IF ( buf(7)==0 ) THEN
!
!     PROCESS PLOAD4 DATA FOR ALL ELEMENT IDS IMPLIED BY THE THRU OPTION
!
               iii = buf(2)
               jjj = buf(8)
               buf(7) = -1
               buf(8) = 0
               DO kkk = iii , jjj
                  buf(2) = kkk
                  CALL write(scr1,buf(2),nwds1,0)
                  n = n + nwds1
                  ktabl = ktabl + nwds1
               ENDDO
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
         CALL write(scr1,buf(2),nwds1,0)
!
         n = n + nwds1
         ktabl = ktabl + nwds1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     HERE WHEN ALL CARDS OF CURRENT CARD TYPE HAVE BEEN READ.
!     STORE WORD COUNT FOR LAST SET IN POINTER LIST. STORE POINTER
!     TO LAST ENTRY FOR CARD TYPE IN 2ND WORD OF STATUS ENTRY.
!     LOOP BACK TO READ NEXT CARD TYPE.
!
 60      z(klist-1) = n
         status(i+1) = klist - ilist - 2
         GOTO 40
 80      IF ( first==0 ) THEN
!
!     HERE WHEN END-OF-FILE ON GEOM3 ENCOUNTERED. IF ERROR CONDITION
!     NOTED, CALL PEXIT. IF NO LOAD CARDS FOUND, CLOSE FILES AND RETURN.
!
            IF ( nogo/=0 ) CALL mesage(-61,0,0)
            IF ( noload/=-1 ) THEN
!
!     IF GRAVITY LOADS WERE READ, TURN NOGRAV FLAG OFF.
!     CLOSE FILES AND MOVE POINTER LIST TO BEGINNING OF CORE.
!
               IF ( status(igrav)>0 .OR. status(irfrc)>0 ) nograv = +1
               CALL write(scr1,0,0,1)
               CALL close(geom3,clsrew)
               CALL close(scr1,clsrew)
               n = klist - ilist
               DO i = 1 , n
                  k = ilist + i
                  z(i) = z(k-1)
               ENDDO
               ilist = 1
               nlist = n - 2
!
!     CHECK UNIQUENESS OF LOAD SETS WITTH RESPECT TO GRAVITY LOAD SETS
!
               IF ( status(igrav)>=0 ) THEN
                  k1 = status(igrav)
                  k2 = status(igrav+1)
                  DO i = ilist , nlist , 3
                     IF ( i<k1 .OR. i>k2 ) THEN
                        setid = z(i)
                        DO k = k1 , k2 , 3
                           IF ( z(k)==setid ) THEN
                              nogo = 1
                              CALL mesage(30,134,setid)
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
!
!     SORT THE SET LIST AND DISCARD DUPLICATE SET NOS.
!
               n = iset - kset
               kset = kset + 1
               CALL sort(0,0,1,1,z(kset),n)
               z(iset+1) = 0
               k = nlist + 3
               DO i = kset , iset
                  IF ( z(i)/=z(i+1) ) THEN
                     z(k) = z(i)
                     k = k + 1
                  ENDIF
               ENDDO
               iset = nlist + 3
               nset = k - 1
               itabl = nset
!
!     OPEN SCRATCH FILE AND SLT FILE.
!     WRITE SET LIST IN HEADER RECORD OF THE SLT.
!
               CALL open(*140,scr1,z(buf1),rdrew)
               file = slt
               CALL open(*140,slt,z(buf2),wrtrew)
               CALL fname(slt,buf)
               CALL write(slt,buf,2,0)
               n = nset - iset + 1
               CALL write(slt,z(iset),n,1)
!
!     IF ALL LOAD CARDS WILL FIT IN CORE, READ THEM IN.
!
               nwds = ktabl - 1
               ncore = itabl + ktabl
               IF ( ncore>=buf2 ) THEN
!
!     HERE IF CORE WILL NOT HOLD ALL LOAD CARDS.
!     CODE IS SIMILAR TO THAT ABOVE EXCEPT THAT POINTER LIST NOW POINTS
!     TO THE DATA ON THE SCRATCH FILE INSTEAD OF IN CORE. THEREFORE, THE
!     SCRATCH FILE WILL HAVE TO BE PASSED ONCE FOR EACH SET IN THE SET
!     LIST.
!
                  file = scr1
                  DO k = iset , nset
                     setid = z(k)
                     ii = 1
                     nread = 0
                     DO i = 1 , ntypes , 2
                        spag_nextblock_2 = 1
                        SPAG_DispatchLoop_2: DO
                           SELECT CASE (spag_nextblock_2)
                           CASE (1)
                              IF ( status(i)>=0 ) THEN
                                 jj1 = status(i)
                                 jjn = status(i+1)
                                 DO jj = jj1 , jjn , 3
                                    IF ( z(jj)==setid ) THEN
                                       spag_nextblock_2 = 2
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
                                 ENDDO
                              ENDIF
                              spag_nextblock_2 = 3
                           CASE (2)
                              nskip = z(jj+1) - nread - 1
                              nwds = z(jj+2)
                              n = carddt(i) - 1
                              IF ( nskip<0 ) THEN
                                 spag_nextblock_1 = 14
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( nskip/=0 ) CALL read(*160,*180,scr1,0,-nskip,0,flag)
                              CALL read(*160,*180,scr1,z(itabl+1),nwds,0,flag)
                              nread = z(jj+1) + nwds - 1
                              nkey = 1
                              IF ( idno(ii)==20 ) nkey = 5
                              IF ( idno(ii)/=21 ) THEN
                                 IF ( idno(ii)<22 .OR. idno(ii)>24 ) THEN
                                    IF ( i/=ipload .AND. i/=ipld3 .AND. i/=igrav ) CALL sort(0,0,n,nkey,z(itabl+1),nwds)
                                 ENDIF
                              ENDIF
                              buf(1) = idno(ii)
                              buf(2) = nwds/n
                              CALL write(slt,buf,2,0)
                              CALL write(slt,z(itabl+1),nwds,0)
                              spag_nextblock_2 = 3
                           CASE (3)
                              ii = ii + 1
                              EXIT SPAG_DispatchLoop_2
                           END SELECT
                        ENDDO SPAG_DispatchLoop_2
                     ENDDO
                     CALL write(slt,0,0,1)
                     CALL rewind(scr1)
                  ENDDO
                  CALL close(scr1,clsrew)
               ELSE
                  file = scr1
                  CALL read(*160,*180,scr1,z(itabl+1),nwds,1,flag)
                  CALL close(scr1,clsrew)
!
!     FOR EACH LOAD SET IN THE SET LIST, LOOP THRU THE STATUS TABLE.
!     FOR EACH CARD TYPE PRESENT IN THE STATUS TABLE, PICK UP POINTERS
!     TO THE POINTER LIST. SEARCH THE POINTER LIST FOR A SET ID MATCH.
!     IF FOUND, PICK UP POINTERS TO THE DATA IN CORE. SORT THE DATA ON
!     INTERNAL INDEX (EXCEPT GRAV AND PLOAD CARDS).
!     THEN, WRITE CARD TYPE ID, NO. OF CARDS IN THE SET, AND THE DATA
!     ON THE CARDS. THUS, THE SLT IS COMPRISED OF ONE LOGICAL RECORD PER
!     SET DATA WITHIN EACH RECORD IS GROUPED BY CARD TYPE, AND, WITHIN
!     THE GROUP, IS SORTED BY INTERNAL INDEX (WHERE DEFINED).
!
                  DO k = iset , nset
                     setid = z(k)
                     ii = 1
                     DO i = 1 , ntypes , 2
                        spag_nextblock_3 = 1
                        SPAG_DispatchLoop_3: DO
                           SELECT CASE (spag_nextblock_3)
                           CASE (1)
                              IF ( status(i)>=0 ) THEN
                                 jj1 = status(i)
                                 jjn = status(i+1)
                                 DO jj = jj1 , jjn , 3
                                    IF ( z(jj)==setid ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                 ENDDO
                              ENDIF
                              spag_nextblock_3 = 3
                           CASE (2)
!
                              jx = itabl + z(jj+1)
                              nwds = z(jj+2)
                              n = carddt(i) - 1
                              nkey = 1
                              IF ( idno(ii)==20 ) nkey = 5
                              IF ( idno(ii)/=21 ) THEN
                                 IF ( idno(ii)<22 .OR. idno(ii)>24 ) THEN
                                    IF ( i/=ipload .AND. i/=ipld3 .AND. i/=igrav ) CALL sort(0,0,n,nkey,z(jx),nwds)
                                 ENDIF
                              ENDIF
                              buf(1) = idno(ii)
                              buf(2) = nwds/n
                              CALL write(slt,buf,2,0)
                              CALL write(slt,z(jx),nwds,0)
                              spag_nextblock_3 = 3
                           CASE (3)
                              ii = ii + 1
                              EXIT SPAG_DispatchLoop_3
                           END SELECT
                        ENDDO SPAG_DispatchLoop_3
                     ENDDO
                     CALL write(slt,0,0,1)
                  ENDDO
               ENDIF
!
!     IF COMBINATION LOADS ARE PRESENT, SET IDS ARE CHECKED TO ASSURE
!     THAT THEY ARE UNIQUE WITH RESPECT TO LOAD CARDS.  THE SET IDS
!     SPECIFIED ON THE LOAD CARD ARE THEN CHECKED AGAINST THOSE IN THE
!     SET LIST TO VERIFY THAT ALL ARE AVAILABLE AND AGAINST EACH OTHER
!     TO ENSURE THAT NO DUPLICATE SPECIFICATIONS EXIST.  THE COMBINATION
!     LOADS ARE WRITTEN AS THE LAST LOGICAL RECORD OF THE SLT.
!
               file = geom3
               CALL preloc(*140,z(buf1),geom3)
               CALL locate(*120,z(buf1),load,flag)
            ELSE
               CALL close(geom3,clsrew)
               CALL close(scr1,clsrew)
               RETURN
            ENDIF
         ELSE
            first = 0
            CALL close(scr2,clsrew)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         CALL read(*160,*100,geom3,buf,2,0,flag)
         CALL write(slt,buf,2,0)
         DO i = iset , nset
            IF ( buf(1)==z(i) ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 11
      CASE (10)
         nogo = 1
         CALL mesage(30,106,buf)
         spag_nextblock_1 = 11
      CASE (11)
         lset = nset + 1
         mset = nset
         idcmld = buf(1)
         spag_nextblock_1 = 12
      CASE (12)
         SPAG_Loop_1_1: DO
            CALL read(*160,*100,geom3,buf,2,0,flag)
            CALL write(slt,buf,2,0)
            IF ( buf(1)==-1 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = iset , nset
               IF ( buf(2)==z(i) ) EXIT SPAG_Loop_1_1
            ENDDO
            nogo = 1
            WRITE (iptr,99001) ufm , buf(2) , idcmld
99001       FORMAT (A23,' 3178, LOAD SET',I9,' NOT FOUND.  REQUIRED FOR ','DEFINITION OF COMBINATION LOAD',I9)
            lines = lines + 2
            IF ( lines>=nlpp ) CALL page
         ENDDO SPAG_Loop_1_1
         IF ( mset/=nset ) THEN
            DO i = lset , mset
               IF ( buf(2)==z(i) ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         mset = mset + 1
         z(mset) = buf(2)
         spag_nextblock_1 = 12
      CASE (13)
         nogo = 1
         WRITE (iptr,99002) ufm , buf(2) , idcmld
99002    FORMAT (A23,' 3179, DUPLICATE LOAD SET',I9,' FOUND IN DEFINITION',' OF COMBINATION LOAD',I9)
         lines = lines + 2
         IF ( lines>=nlpp ) CALL page
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 100     CALL write(slt,0,0,1)
 120     CALL close(geom3,clsrew)
         CALL close(slt,clsrew)
         buf(1) = slt
         buf(2) = nset - iset + 1
         DO i = 3 , 7
            buf(i) = 0
         ENDDO
         CALL wrttrl(buf)
         IF ( nogo/=0 ) CALL mesage(-61,0,0)
         RETURN
      CASE (14)
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 15
      CASE (15)
!
!     BINARY SEARCH ROUTINE
!
         klo = 1
         khi = kn
         SPAG_Loop_1_2: DO
            k = (klo+khi+1)/2
            DO
               IF ( gpoint<z(2*k-1) ) THEN
                  khi = k
               ELSEIF ( gpoint==z(2*k-1) ) THEN
                  gpoint = z(2*k)
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  klo = k
               ENDIF
               IF ( khi-klo<1 ) THEN
                  buf(2) = gpoint
                  nogo = 1
                  CALL mesage(30,8,buf)
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( khi-klo==1 ) THEN
                  IF ( k==klo ) THEN
                     k = khi
                  ELSE
                     k = klo
                  ENDIF
                  klo = khi
               ELSE
                  CYCLE SPAG_Loop_1_2
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
 140     DO
            n = -1
!
!     FATAL FILE ERRORS
!
            CALL mesage(n,file,nam)
         ENDDO
 160     n = -2
         CALL mesage(n,file,nam)
         GOTO 140
 180     n = -3
         CALL mesage(n,file,nam)
         GOTO 140
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gp3a
