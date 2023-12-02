!*==gp3a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp3a
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
         file = Eqexin
         CALL open(*140,Eqexin,Z(Buf1),Rdrew)
         CALL fwdrec(*160,Eqexin)
         CALL read(*160,*20,Eqexin,Z,Buf2,1,neqx)
         CALL mesage(-8,0,nam)
 20      CALL close(Eqexin,Clsrew)
         kn = neqx/2
         nogo = 0
!
!     INITIALIZE POINTERS AND OPEN SCR1 AND GEOM3.
!
         iset = Buf2 - 2
         kset = iset
         ilist = neqx + 1
         klist = ilist
         ktabl = 1
         first = 1
         file = Scr1
         CALL open(*140,Scr1,Z(Buf2),Wrtrew)
!
!     IF PLOAD2 CARDS PRESENT, INITIALIZE TO READ PLOAD DATA FROM SCR2
!     INSTEAD OF GEOM3.
!
         IF ( Nopld2==0 ) THEN
            first = 0
         ELSE
            file = Scr2
            CALL open(*140,Scr2,Z(Buf1),Rdrew)
            GOTO 40
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         file = Geom3
         CALL open(*140,Geom3,Z(Buf1),Rdrew)
         CALL fwdrec(*160,Geom3)
!
!     READ 3-WORD RECORD ID. IF ID BELONGS TO LOAD SET, TURN NOLOAD FLAG
!     OFF.
!     SET 1ST WORD IN STATUS ENTRY TO CURRENT POINTER IN LIST TABLE.
!     SET PARAMETERS FOR CONVERSION OF GRID NOS. TO INTERNAL INDICES.
!
 40      CALL read(*80,*40,file,Buf,3,0,flag)
         DO i = 1 , Ntypes , 2
            IF ( Buf(1)==Cardid(i) .AND. Buf(2)==Cardid(i+1) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         CALL fwdrec(*80,file)
         GOTO 40
      CASE (4)
         Noload = 1
         IF ( first/=1 ) THEN
!
!     IF I POINTS TO PLOAD RECORD AND PLOAD2 CARDS ARE PRESENT, THEN
!     PLOAD DATA IS ALREADY PROCESSED. IN THIS CASE, SKIP PLOAD RECORD.
!     IF I POINTS TO PLOAD3 RECORD ON GEOM3, SKIP RECORD.
!
            IF ( i==Ipload .AND. Nopld2/=0 .AND. Nopld2/=2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( i==Ipld3 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         Status(i) = klist - ilist + 1
         nwds = Carddt(i)
         nwds1 = nwds - 1
         jx = Carddt(i+1)
         jj1 = jx + 1
         jjn = jx + Mask(jx)
         id = 0
         spag_nextblock_1 = 5
      CASE (5)
!
!     READ A LOAD CARD. IF SET ID IS DIFFERENT FROM LAST READ (OR 1ST
!     ONE) STORE SET ID IN POINTER LIST AND IN SET LIST. STORE POINTER
!     IN POINTER LIST. IF NOT FIRST CARD OF TYPE, STORE WORD COUNT IN
!     POINTER LIST.
!
         CALL read(*160,*60,file,Buf,nwds,0,flag)
         IF ( Buf(1)/=id ) THEN
            Z(klist) = Buf(1)
            Z(klist+1) = ktabl
            IF ( id/=0 ) Z(klist-1) = n
            id = Buf(1)
            n = 0
            klist = klist + 3
            Z(kset) = Buf(1)
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
            jx = Mask(jj)
            IF ( jx<=0 ) THEN
               jx = -jx
               jstop = 1
            ENDIF
         ELSE
            jx = jx + 1
         ENDIF
         gpoint = Buf(jx)
         piez = .FALSE.
         IF ( gpoint<0 .AND. ksystm(78)==1 ) piez = .TRUE.
         IF ( piez ) gpoint = -gpoint
         IF ( .NOT.(gpoint==-1 .AND. (Cardid(i)==3209 .OR. Cardid(i)==3409)) ) THEN
            IF ( gpoint/=0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         IF ( piez ) gpoint = -gpoint
         Buf(jx) = gpoint
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
            IF ( Buf(7)==0 ) THEN
!
!     PROCESS PLOAD4 DATA FOR ALL ELEMENT IDS IMPLIED BY THE THRU OPTION
!
               iii = Buf(2)
               jjj = Buf(8)
               Buf(7) = -1
               Buf(8) = 0
               DO kkk = iii , jjj
                  Buf(2) = kkk
                  CALL write(Scr1,Buf(2),nwds1,0)
                  n = n + nwds1
                  ktabl = ktabl + nwds1
               ENDDO
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
         CALL write(Scr1,Buf(2),nwds1,0)
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
 60      Z(klist-1) = n
         Status(i+1) = klist - ilist - 2
         GOTO 40
 80      IF ( first==0 ) THEN
!
!     HERE WHEN END-OF-FILE ON GEOM3 ENCOUNTERED. IF ERROR CONDITION
!     NOTED, CALL PEXIT. IF NO LOAD CARDS FOUND, CLOSE FILES AND RETURN.
!
            IF ( nogo/=0 ) CALL mesage(-61,0,0)
            IF ( Noload/=-1 ) THEN
!
!     IF GRAVITY LOADS WERE READ, TURN NOGRAV FLAG OFF.
!     CLOSE FILES AND MOVE POINTER LIST TO BEGINNING OF CORE.
!
               IF ( Status(Igrav)>0 .OR. Status(irfrc)>0 ) Nograv = +1
               CALL write(Scr1,0,0,1)
               CALL close(Geom3,Clsrew)
               CALL close(Scr1,Clsrew)
               n = klist - ilist
               DO i = 1 , n
                  k = ilist + i
                  Z(i) = Z(k-1)
               ENDDO
               ilist = 1
               nlist = n - 2
!
!     CHECK UNIQUENESS OF LOAD SETS WITTH RESPECT TO GRAVITY LOAD SETS
!
               IF ( Status(Igrav)>=0 ) THEN
                  k1 = Status(Igrav)
                  k2 = Status(Igrav+1)
                  DO i = ilist , nlist , 3
                     IF ( i<k1 .OR. i>k2 ) THEN
                        setid = Z(i)
                        DO k = k1 , k2 , 3
                           IF ( Z(k)==setid ) THEN
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
               CALL sort(0,0,1,1,Z(kset),n)
               Z(iset+1) = 0
               k = nlist + 3
               DO i = kset , iset
                  IF ( Z(i)/=Z(i+1) ) THEN
                     Z(k) = Z(i)
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
               CALL open(*140,Scr1,Z(Buf1),Rdrew)
               file = Slt
               CALL open(*140,Slt,Z(Buf2),Wrtrew)
               CALL fname(Slt,Buf)
               CALL write(Slt,Buf,2,0)
               n = nset - iset + 1
               CALL write(Slt,Z(iset),n,1)
!
!     IF ALL LOAD CARDS WILL FIT IN CORE, READ THEM IN.
!
               nwds = ktabl - 1
               ncore = itabl + ktabl
               IF ( ncore>=Buf2 ) THEN
!
!     HERE IF CORE WILL NOT HOLD ALL LOAD CARDS.
!     CODE IS SIMILAR TO THAT ABOVE EXCEPT THAT POINTER LIST NOW POINTS
!     TO THE DATA ON THE SCRATCH FILE INSTEAD OF IN CORE. THEREFORE, THE
!     SCRATCH FILE WILL HAVE TO BE PASSED ONCE FOR EACH SET IN THE SET
!     LIST.
!
                  file = Scr1
                  DO k = iset , nset
                     setid = Z(k)
                     ii = 1
                     nread = 0
                     DO i = 1 , Ntypes , 2
                        spag_nextblock_2 = 1
                        SPAG_DispatchLoop_2: DO
                           SELECT CASE (spag_nextblock_2)
                           CASE (1)
                              IF ( Status(i)>=0 ) THEN
                                 jj1 = Status(i)
                                 jjn = Status(i+1)
                                 DO jj = jj1 , jjn , 3
                                    IF ( Z(jj)==setid ) THEN
                                       spag_nextblock_2 = 2
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
                                 ENDDO
                              ENDIF
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           CASE (2)
                              nskip = Z(jj+1) - nread - 1
                              nwds = Z(jj+2)
                              n = Carddt(i) - 1
                              IF ( nskip<0 ) THEN
                                 spag_nextblock_1 = 14
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( nskip/=0 ) CALL read(*160,*180,Scr1,0,-nskip,0,flag)
                              CALL read(*160,*180,Scr1,Z(itabl+1),nwds,0,flag)
                              nread = Z(jj+1) + nwds - 1
                              nkey = 1
                              IF ( Idno(ii)==20 ) nkey = 5
                              IF ( Idno(ii)/=21 ) THEN
                                 IF ( Idno(ii)<22 .OR. Idno(ii)>24 ) THEN
                                    IF ( i/=Ipload .AND. i/=Ipld3 .AND. i/=Igrav ) CALL sort(0,0,n,nkey,Z(itabl+1),nwds)
                                 ENDIF
                              ENDIF
                              Buf(1) = Idno(ii)
                              Buf(2) = nwds/n
                              CALL write(Slt,Buf,2,0)
                              CALL write(Slt,Z(itabl+1),nwds,0)
                              spag_nextblock_2 = 3
                           CASE (3)
                              ii = ii + 1
                              EXIT SPAG_DispatchLoop_2
                           END SELECT
                        ENDDO SPAG_DispatchLoop_2
                     ENDDO
                     CALL write(Slt,0,0,1)
                     CALL rewind(Scr1)
                  ENDDO
                  CALL close(Scr1,Clsrew)
               ELSE
                  file = Scr1
                  CALL read(*160,*180,Scr1,Z(itabl+1),nwds,1,flag)
                  CALL close(Scr1,Clsrew)
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
                     setid = Z(k)
                     ii = 1
                     DO i = 1 , Ntypes , 2
                        spag_nextblock_3 = 1
                        SPAG_DispatchLoop_3: DO
                           SELECT CASE (spag_nextblock_3)
                           CASE (1)
                              IF ( Status(i)>=0 ) THEN
                                 jj1 = Status(i)
                                 jjn = Status(i+1)
                                 DO jj = jj1 , jjn , 3
                                    IF ( Z(jj)==setid ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                 ENDDO
                              ENDIF
                              spag_nextblock_3 = 3
                              CYCLE SPAG_DispatchLoop_3
                           CASE (2)
!
                              jx = itabl + Z(jj+1)
                              nwds = Z(jj+2)
                              n = Carddt(i) - 1
                              nkey = 1
                              IF ( Idno(ii)==20 ) nkey = 5
                              IF ( Idno(ii)/=21 ) THEN
                                 IF ( Idno(ii)<22 .OR. Idno(ii)>24 ) THEN
                                    IF ( i/=Ipload .AND. i/=Ipld3 .AND. i/=Igrav ) CALL sort(0,0,n,nkey,Z(jx),nwds)
                                 ENDIF
                              ENDIF
                              Buf(1) = Idno(ii)
                              Buf(2) = nwds/n
                              CALL write(Slt,Buf,2,0)
                              CALL write(Slt,Z(jx),nwds,0)
                              spag_nextblock_3 = 3
                           CASE (3)
                              ii = ii + 1
                              EXIT SPAG_DispatchLoop_3
                           END SELECT
                        ENDDO SPAG_DispatchLoop_3
                     ENDDO
                     CALL write(Slt,0,0,1)
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
               file = Geom3
               CALL preloc(*140,Z(Buf1),Geom3)
               CALL locate(*120,Z(Buf1),Load,flag)
            ELSE
               CALL close(Geom3,Clsrew)
               CALL close(Scr1,Clsrew)
               RETURN
            ENDIF
         ELSE
            first = 0
            CALL close(Scr2,Clsrew)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         CALL read(*160,*100,Geom3,Buf,2,0,flag)
         CALL write(Slt,Buf,2,0)
         DO i = iset , nset
            IF ( Buf(1)==Z(i) ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         nogo = 1
         CALL mesage(30,106,Buf)
         spag_nextblock_1 = 11
      CASE (11)
         lset = nset + 1
         mset = nset
         idcmld = Buf(1)
         spag_nextblock_1 = 12
      CASE (12)
         SPAG_Loop_1_1: DO
            CALL read(*160,*100,Geom3,Buf,2,0,flag)
            CALL write(Slt,Buf,2,0)
            IF ( Buf(1)==-1 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = iset , nset
               IF ( Buf(2)==Z(i) ) EXIT SPAG_Loop_1_1
            ENDDO
            nogo = 1
            WRITE (Iptr,99001) Ufm , Buf(2) , idcmld
99001       FORMAT (A23,' 3178, LOAD SET',I9,' NOT FOUND.  REQUIRED FOR ','DEFINITION OF COMBINATION LOAD',I9)
            Lines = Lines + 2
            IF ( Lines>=Nlpp ) CALL page
         ENDDO SPAG_Loop_1_1
         IF ( mset/=nset ) THEN
            DO i = lset , mset
               IF ( Buf(2)==Z(i) ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         mset = mset + 1
         Z(mset) = Buf(2)
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         nogo = 1
         WRITE (Iptr,99002) Ufm , Buf(2) , idcmld
99002    FORMAT (A23,' 3179, DUPLICATE LOAD SET',I9,' FOUND IN DEFINITION',' OF COMBINATION LOAD',I9)
         Lines = Lines + 2
         IF ( Lines>=Nlpp ) CALL page
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 100     CALL write(Slt,0,0,1)
 120     CALL close(Geom3,Clsrew)
         CALL close(Slt,Clsrew)
         Buf(1) = Slt
         Buf(2) = nset - iset + 1
         DO i = 3 , 7
            Buf(i) = 0
         ENDDO
         CALL wrttrl(Buf)
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
               IF ( gpoint<Z(2*k-1) ) THEN
                  khi = k
               ELSEIF ( gpoint==Z(2*k-1) ) THEN
                  gpoint = Z(2*k)
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  klo = k
               ENDIF
               IF ( khi-klo<1 ) THEN
                  Buf(2) = gpoint
                  nogo = 1
                  CALL mesage(30,8,Buf)
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
