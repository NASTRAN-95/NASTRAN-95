
SUBROUTINE gp3a
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(50) , Buf1 , Buf2 , Carddt(60) , Cardid(60) , Clsrew , Eqexin , Geom3 , Gptt , Idm(6) , Idno(30) , Idum(2) , Igrav , &
         & Ipld3 , Ipload , Iptr , Isb , Ksystm(80) , Lines , Load(2) , Mask(60) , Nlpp , Nograv , Noload , Nopld2 , Notemp ,       &
         & Ntypes , Rd , Rdrew , Scr1 , Scr2 , Slt , Status(60) , Wrt , Wrtrew , Z(1)
   REAL Buf3 , Geom2 , Pload2(2) , Pload3(2) , Temp(2) , Tempd(2) , Tempp1(2) , Tempp2(2) , Tempp3(2) , Temprb(2)
   CHARACTER*23 Ufm
   COMMON /blank / Nograv , Noload , Notemp
   COMMON /gp3com/ Geom3 , Eqexin , Geom2 , Slt , Gptt , Scr1 , Scr2 , Buf1 , Buf2 , Buf , Cardid , Idno , Carddt , Mask , Status , &
                 & Ntypes , Ipload , Igrav , Pload2 , Load , Nopld2 , Temp , Tempd , Tempp1 , Tempp2 , Tempp3 , Temprb , Buf3 ,     &
                 & Pload3 , Ipld3
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Isb , Iptr , Idm , Nlpp , Idum , Lines
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER file , first , flag , gpoint , i , id , idcmld , ii , iii , ilist , irfrc , iset , itabl , jj , jj1 , jjj , jjn , jstop ,&
         & jx , k , k1 , k2 , khi , kkk , klist , klo , kn , kset , ktabl , lset , mset , n , nam(2) , ncore , neqx , nkey , nlist ,&
         & nogo , nread , nset , nskip , nwds , nwds1 , setid
   LOGICAL piez
!
! End of declarations
!
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
   EQUIVALENCE (Ksystm(1),Isb)
   DATA nam/4HGP3A , 4H    / , irfrc/9/
!
!     READ EQEXIN INTO CORE. INITIALIZE BINARY SEARCH ROUTINE.
!
   file = Eqexin
   CALL open(*2300,Eqexin,Z(Buf1),Rdrew)
   CALL fwdrec(*2400,Eqexin)
   CALL read(*2400,*100,Eqexin,Z,Buf2,1,neqx)
   CALL mesage(-8,0,nam)
 100  CALL close(Eqexin,Clsrew)
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
   CALL open(*2300,Scr1,Z(Buf2),Wrtrew)
!
!     IF PLOAD2 CARDS PRESENT, INITIALIZE TO READ PLOAD DATA FROM SCR2
!     INSTEAD OF GEOM3.
!
   IF ( Nopld2==0 ) THEN
      first = 0
   ELSE
      file = Scr2
      CALL open(*2300,Scr2,Z(Buf1),Rdrew)
      GOTO 300
   ENDIF
 200  file = Geom3
   CALL open(*2300,Geom3,Z(Buf1),Rdrew)
   CALL fwdrec(*2400,Geom3)
!
!     READ 3-WORD RECORD ID. IF ID BELONGS TO LOAD SET, TURN NOLOAD FLAG
!     OFF.
!     SET 1ST WORD IN STATUS ENTRY TO CURRENT POINTER IN LIST TABLE.
!     SET PARAMETERS FOR CONVERSION OF GRID NOS. TO INTERNAL INDICES.
!
 300  CALL read(*1100,*300,file,Buf,3,0,flag)
   DO i = 1 , Ntypes , 2
      IF ( Buf(1)==Cardid(i) .AND. Buf(2)==Cardid(i+1) ) GOTO 500
   ENDDO
 400  CALL fwdrec(*1100,file)
   GOTO 300
 500  Noload = 1
   IF ( first/=1 ) THEN
!
!     IF I POINTS TO PLOAD RECORD AND PLOAD2 CARDS ARE PRESENT, THEN
!     PLOAD DATA IS ALREADY PROCESSED. IN THIS CASE, SKIP PLOAD RECORD.
!     IF I POINTS TO PLOAD3 RECORD ON GEOM3, SKIP RECORD.
!
      IF ( i==Ipload .AND. Nopld2/=0 .AND. Nopld2/=2 ) GOTO 400
      IF ( i==Ipld3 ) GOTO 400
   ENDIF
   Status(i) = klist - ilist + 1
   nwds = Carddt(i)
   nwds1 = nwds - 1
   jx = Carddt(i+1)
   jj1 = jx + 1
   jjn = jx + Mask(jx)
   id = 0
!
!     READ A LOAD CARD. IF SET ID IS DIFFERENT FROM LAST READ (OR 1ST
!     ONE) STORE SET ID IN POINTER LIST AND IN SET LIST. STORE POINTER
!     IN POINTER LIST. IF NOT FIRST CARD OF TYPE, STORE WORD COUNT IN
!     POINTER LIST.
!
 600  CALL read(*2400,*1000,file,Buf,nwds,0,flag)
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
   IF ( jx==0 ) GOTO 900
   jj = jj1
   jstop = 0
 700  IF ( jstop==0 ) THEN
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
   IF ( gpoint<0 .AND. Ksystm(78)==1 ) piez = .TRUE.
   IF ( piez ) gpoint = -gpoint
   IF ( .NOT.(gpoint==-1 .AND. (Cardid(i)==3209 .OR. Cardid(i)==3409)) ) THEN
      IF ( gpoint/=0 ) GOTO 2100
   ENDIF
 800  IF ( piez ) gpoint = -gpoint
   Buf(jx) = gpoint
   jj = jj + 1
   IF ( jj<=jjn ) GOTO 700
!
!     CHECK FOR PLOAD4 CARD
!
 900  IF ( i==49 ) THEN
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
         GOTO 600
      ENDIF
   ENDIF
!
   CALL write(Scr1,Buf(2),nwds1,0)
!
   n = n + nwds1
   ktabl = ktabl + nwds1
   GOTO 600
!
!     HERE WHEN ALL CARDS OF CURRENT CARD TYPE HAVE BEEN READ.
!     STORE WORD COUNT FOR LAST SET IN POINTER LIST. STORE POINTER
!     TO LAST ENTRY FOR CARD TYPE IN 2ND WORD OF STATUS ENTRY.
!     LOOP BACK TO READ NEXT CARD TYPE.
!
 1000 Z(klist-1) = n
   Status(i+1) = klist - ilist - 2
   GOTO 300
 1100 IF ( first==0 ) THEN
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
         CALL open(*2300,Scr1,Z(Buf1),Rdrew)
         file = Slt
         CALL open(*2300,Slt,Z(Buf2),Wrtrew)
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
                  IF ( Status(i)>=0 ) THEN
                     jj1 = Status(i)
                     jjn = Status(i+1)
                     DO jj = jj1 , jjn , 3
                        IF ( Z(jj)==setid ) GOTO 1102
                     ENDDO
                  ENDIF
                  GOTO 1104
 1102             nskip = Z(jj+1) - nread - 1
                  nwds = Z(jj+2)
                  n = Carddt(i) - 1
                  IF ( nskip<0 ) GOTO 2000
                  IF ( nskip/=0 ) CALL read(*2400,*2500,Scr1,0,-nskip,0,flag)
                  CALL read(*2400,*2500,Scr1,Z(itabl+1),nwds,0,flag)
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
 1104             ii = ii + 1
               ENDDO
               CALL write(Slt,0,0,1)
               CALL rewind(Scr1)
            ENDDO
            CALL close(Scr1,Clsrew)
         ELSE
            file = Scr1
            CALL read(*2400,*2500,Scr1,Z(itabl+1),nwds,1,flag)
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
                  IF ( Status(i)>=0 ) THEN
                     jj1 = Status(i)
                     jjn = Status(i+1)
                     DO jj = jj1 , jjn , 3
                        IF ( Z(jj)==setid ) GOTO 1106
                     ENDDO
                  ENDIF
                  GOTO 1108
!
 1106             jx = itabl + Z(jj+1)
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
 1108             ii = ii + 1
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
         CALL preloc(*2300,Z(Buf1),Geom3)
         CALL locate(*1900,Z(Buf1),Load,flag)
      ELSE
         CALL close(Geom3,Clsrew)
         CALL close(Scr1,Clsrew)
         RETURN
      ENDIF
   ELSE
      first = 0
      CALL close(Scr2,Clsrew)
      GOTO 200
   ENDIF
 1200 CALL read(*2400,*1800,Geom3,Buf,2,0,flag)
   CALL write(Slt,Buf,2,0)
   DO i = iset , nset
      IF ( Buf(1)==Z(i) ) GOTO 1300
   ENDDO
   GOTO 1400
 1300 nogo = 1
   CALL mesage(30,106,Buf)
 1400 lset = nset + 1
   mset = nset
   idcmld = Buf(1)
 1500 DO
      CALL read(*2400,*1800,Geom3,Buf,2,0,flag)
      CALL write(Slt,Buf,2,0)
      IF ( Buf(1)==-1 ) GOTO 1200
      DO i = iset , nset
         IF ( Buf(2)==Z(i) ) GOTO 1600
      ENDDO
      nogo = 1
      WRITE (Iptr,99001) Ufm , Buf(2) , idcmld
99001 FORMAT (A23,' 3178, LOAD SET',I9,' NOT FOUND.  REQUIRED FOR ','DEFINITION OF COMBINATION LOAD',I9)
      Lines = Lines + 2
      IF ( Lines>=Nlpp ) CALL page
   ENDDO
 1600 IF ( mset/=nset ) THEN
      DO i = lset , mset
         IF ( Buf(2)==Z(i) ) GOTO 1700
      ENDDO
   ENDIF
   mset = mset + 1
   Z(mset) = Buf(2)
   GOTO 1500
 1700 nogo = 1
   WRITE (Iptr,99002) Ufm , Buf(2) , idcmld
99002 FORMAT (A23,' 3179, DUPLICATE LOAD SET',I9,' FOUND IN DEFINITION',' OF COMBINATION LOAD',I9)
   Lines = Lines + 2
   IF ( Lines>=Nlpp ) CALL page
   GOTO 1500
 1800 CALL write(Slt,0,0,1)
 1900 CALL close(Geom3,Clsrew)
   CALL close(Slt,Clsrew)
   Buf(1) = Slt
   Buf(2) = nset - iset + 1
   DO i = 3 , 7
      Buf(i) = 0
   ENDDO
   CALL wrttrl(Buf)
   IF ( nogo/=0 ) CALL mesage(-61,0,0)
   RETURN
 2000 CALL mesage(-61,0,0)
!
!     BINARY SEARCH ROUTINE
!
 2100 klo = 1
   khi = kn
 2200 k = (klo+khi+1)/2
   DO
      IF ( gpoint<Z(2*k-1) ) THEN
         khi = k
      ELSEIF ( gpoint==Z(2*k-1) ) THEN
         gpoint = Z(2*k)
         GOTO 800
      ELSE
         klo = k
      ENDIF
      IF ( khi-klo<1 ) THEN
         Buf(2) = gpoint
         nogo = 1
         CALL mesage(30,8,Buf)
         GOTO 800
      ELSEIF ( khi-klo==1 ) THEN
         IF ( k==klo ) THEN
            k = khi
         ELSE
            k = klo
         ENDIF
         klo = khi
      ELSE
         GOTO 2200
      ENDIF
   ENDDO
 2300 DO
      n = -1
!
!     FATAL FILE ERRORS
!
      CALL mesage(n,file,nam)
   ENDDO
 2400 n = -2
   CALL mesage(n,file,nam)
   GOTO 2300
 2500 n = -3
   CALL mesage(n,file,nam)
   GOTO 2300
END SUBROUTINE gp3a
