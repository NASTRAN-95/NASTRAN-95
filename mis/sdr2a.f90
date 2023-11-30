
SUBROUTINE sdr2a
   IMPLICIT NONE
   INTEGER Acc , All , Any , App(2) , Bk0(2) , Bk1(2) , Branch , Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Casecc , Cei(2) , Cstm , Displ ,&
         & Ds0(2) , Ds1(2) , Eldef , File , Force , Frq(2) , Iacc , Icb(7) , Icstm , Idispl , Idummy(5) , Ieigen , Ieldef , Ielf ,  &
         & Ihalf , Iloads , Ilsym , Intap , Isopl , Ispcf , Istr , Istrn , Isymfl , Isystm(175) , Itload , Ittl , Ivec , Ivecn ,    &
         & Ivel , Jhalf , Knset , Ktype , Kwdcc , Kwdedt , Kwdest , Kwdgpt , Line , Loadnn , Loads , Mach , Maxlin , Mcb(7) ,       &
         & Method , Mn , Mpcn , Mpt , Mset , Nam(2) , Ncstm , Nharms , Nogo , Nrigds , Nrings , Pla(22) , Plots , Rei(2) , Sort2 ,  &
         & Spcf , Sta(2) , Stress , Strnfl , Sysbuf , Tloads , Trn(2)
   LOGICAL Axic , Ddrmm , Strain
   REAL Bgpdt , Clsrew , Date(3) , Ddd(6) , Deform , Dit , Dtype(8) , Echo , Edt , Eigr , End , Eqexin , Est , Esta , Gptt , Gptta ,&
      & Harms , Ocb(7) , Oef1 , Oeigr , Oes1 , Opg1 , Ophig , Opte , Oqg1 , Ougv1 , Page , Pg , Phig , Pphig , Pugv1 , Qg , Rd ,    &
      & Rdrew , Sil , Spcn , Stftmp , Strspt , Symflg , Symm , Temp , Time , Tline , Ugv , Wrt , Wrtrew
   INTEGER Two(32) , Vel , Z(1)
   COMMON /blank / App , Sort2 , Istrn , Strnfl , Idummy , Strain
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /sdr2x1/ Ieigen , Ieldef , Itload , Isymfl , Iloads , Idispl , Istr , Ielf , Iacc , Ivel , Ispcf , Ittl , Ilsym
   COMMON /sdr2x2/ Casecc , Cstm , Mpt , Dit , Eqexin , Sil , Gptt , Edt , Bgpdt , Pg , Qg , Ugv , Est , Phig , Eigr , Opg1 , Oqg1 ,&
                 & Ougv1 , Oes1 , Oef1 , Pugv1 , Oeigr , Ophig , Pphig , Esta , Gptta , Harms
   COMMON /sdr2x4/ Nam , End , Mset , Icb , Ocb , Mcb , Dtype , Icstm , Ncstm , Ivec , Ivecn , Temp , Deform , File , Buf1 , Buf2 , &
                 & Buf3 , Buf4 , Buf5 , Any , All , Tloads , Eldef , Symflg , Branch , Ktype , Loads , Spcf , Displ , Vel , Acc ,   &
                 & Stress , Force , Kwdest , Kwdedt , Kwdgpt , Kwdcc , Nrigds , Sta , Rei , Ds0 , Ds1 , Frq , Trn , Bk0 , Bk1 ,     &
                 & Cei , Pla , Nrings , Nharms , Axic , Knset , Isopl , Strspt , Ddrmm
   COMMON /system/ Sysbuf , Opte , Nogo , Intap , Mpcn , Spcn , Method , Loadnn , Symm , Stftmp , Page , Line , Tline , Maxlin ,    &
                 & Date , Time , Echo , Plots , Ddd , Mn
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
   INTEGER any1 , i , ii , iset , isetno , j , jj , kn , mmreig , n , ncc , nset , pass , prevf , prevs , ret , setno , zi
   INTEGER korsz , lshift , rshift
   EXTERNAL lshift , rshift
!
!     SDR2A PROCESSES THE CASE CONTROL DATA BLOCK. DEPENDING ON THE
!     RIGID FORMAT AND THE VARIOUS OUTPUT REQUESTS, SDR2A SETS FLAGS
!     AND PARAMETERS TO CONTROL OPERATION OF THE REMAINDER OF THE PHASES
!     OF SDR2
!
   !>>>>EQUIVALENCE (Sysbuf,Isystm)
   DATA mmreig/4HMMRE/
!
!
!     CHECK FOR STRAIN OPTION
!
   Strain = .FALSE.
   IF ( Istrn>=0 ) Strain = .TRUE.
!
!     PERFORM BUFFER ALLOCATION.
!
   Buf1 = korsz(Z) - Sysbuf - 2
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Buf4 = Buf3 - Sysbuf
   Buf5 = Buf4 - Sysbuf
!
!     SET PARAMETER FOR APPROACH.
!
   n = 2*Nrigds - 1
!
!     FIRST CHECK FOR SPECIAL APPROACH FOR DYNAMIC-DATA-RECOVERY-MATRIX-
!     METHOD.  IF APPROACH IS -MMREIG- THEN DDRMM FLAG IS SET TO INSURE
!     ENOUGH OUTPUTS UNDER CERTAIN CONDITIONS.
!
   Ddrmm = .FALSE.
   IF ( App(1)/=mmreig ) THEN
!
      DO i = 1 , n , 2
         IF ( Sta(i)==App(1) ) GOTO 100
      ENDDO
      CALL mesage(-30,75,App)
   ELSE
      Ddrmm = .TRUE.
      i = 3
   ENDIF
 100  Branch = (i+1)/2
!
!    OPEN CASE CONTROL. SKIP HEADER RECORD.
!    IF DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1, SKIP 1ST CASECC RECORD
!
   CALL gopen(Casecc,Z(Buf1),Rdrew)
   IF ( App(1)==Ds1(1) .OR. App(1)==Bk1(1) ) CALL skprec(Casecc,1)
   Kwdcc = 0
!
!     INITIALIZE VARIOUS OUTPUT REQUEST FLAGS.
!
   All = 0
   Any = 0
   Displ = 0
   Vel = 0
   Acc = 0
   Spcf = 0
   Loads = 0
   Stress = 0
   Force = 0
   Tloads = 0
   Eldef = 0
   ii = 0
   prevs = 0
   prevf = 0
!
!     READ A RECORD IN CASE CONTROL.
!     IF REQUEST FOR STRESSES IS PRESENT, TURN ON STRESS FLAG.
!     IF REQUEST FOR FORCES   IS PRESENT, TURN ON FORCE  FLAG.
!     -ANY- FLAG = STRESS .OR. FORCE.
!     -ALL- FLAG = ANY REQUEST FOR ALL STRESSES OR FORCES.
!     IF ANY.NE.0 .AND ALL.EQ.0, BUILD LIST OF UNIQUE ELEMENT IDS.
!
 200  CALL read(*1300,*300,Casecc,Z,Buf5-1,1,ncc)
   CALL mesage(+8,0,Nam)
   All = 1
 300  any1 = 0
   Kwdcc = max0(Kwdcc,ncc)
   Mset = max0(Mset,Kwdcc+1)
!
!     SET DMAP FLAG FOR USE IN DISP R.F. 1
!
   IF ( Istrn<0 .AND. Strnfl<0 ) THEN
      j = 180
      IF ( Z(j)/=0 ) Strnfl = 1
   ENDIF
   Istr = 23
   IF ( Strain ) Istr = 180
   IF ( Z(Istr)<0 ) THEN
      All = 1
   ELSEIF ( Z(Istr)==0 ) THEN
      GOTO 400
   ENDIF
   Stress = 1
   any1 = 1
 400  IF ( Z(Ielf)<0 ) THEN
      All = 1
   ELSEIF ( Z(Ielf)==0 ) THEN
      GOTO 500
   ENDIF
   Force = 1
   any1 = 1
 500  IF ( All/=0 .OR. any1==0 ) GOTO 1200
!
!     INITIALIZE TO PROCESS STRESS OUTPUT REQUEST.
!     BUILD MASTER SET LIST ONLY IF CURRENT SET ID IS NEW
!
   ASSIGN 1100 TO pass
   setno = Z(Istr)
   IF ( setno==prevs ) GOTO 1100
   prevs = setno
!
!     IF REQUEST PRESENT, LOCATE SET DEFINITION IN CASE CONTROL DATA.
!
 600  IF ( setno==0 ) GOTO pass
   isetno = Ilsym + Z(Ilsym) + 1
   DO
      iset = isetno + 2
      nset = Z(isetno+1) + iset - 1
      IF ( Z(isetno)==setno ) THEN
!
!     PICK UP ELEMENT IDS IN SET. SAVE IN UNIQUE LIST.
!
         i = iset
         EXIT
      ELSE
         isetno = nset + 1
         IF ( isetno>=ncc ) THEN
            All = 1
            GOTO 1200
         ENDIF
      ENDIF
   ENDDO
 700  IF ( i==nset ) GOTO 900
   IF ( Z(i+1)>0 ) GOTO 900
   zi = Z(i)
   n = -Z(i+1)
   i = i + 1
   ASSIGN 800 TO ret
   GOTO 1400
 800  DO
      zi = zi + 1
      IF ( zi>n ) GOTO 1000
      ii = ii + 1
      IF ( ii>Buf2 ) THEN
         All = 1
         GOTO 1200
      ELSE
         Z(ii) = zi
      ENDIF
   ENDDO
 900  zi = Z(i)
   ASSIGN 1000 TO ret
   GOTO 1400
 1000 i = i + 1
   IF ( i<=nset ) GOTO 700
   GOTO pass
!
!     INITIALIZE TO PROCESS FORCE OUTPUT REQUEST.
!     BUILD MASTER SET LIST ONLY IF CURRENT SET ID IS NEW
!
 1100 setno = Z(Ielf)
   IF ( setno/=prevf ) THEN
      prevf = setno
      ASSIGN 1200 TO pass
      GOTO 600
   ENDIF
!
!     TURN ON FLAGS FOR OTHER OUTPUT REQUESTS.
!
 1200 IF ( Z(Iloads)/=0 ) Loads = 1
   IF ( Z(Ispcf)/=0 ) Spcf = 1
   IF ( Z(Idispl)/=0 ) Displ = 1
   IF ( Z(Ivel)/=0 ) Vel = 1
   IF ( Z(Iacc)/=0 ) Acc = 1
   IF ( Z(Ieldef)/=0 ) Eldef = 1
   IF ( Z(Itload)/=0 ) Tloads = 1
   IF ( Z(Iloads+2)<0 .OR. Z(Ispcf+2)<0 .OR. Z(Idispl+2)<0 .OR. Z(Ivel+2)<0 .OR. Z(Iacc+2)<0 .OR. Z(Istr+2)<0 .OR. Z(Ielf+2)<0 .OR. &
      & App(1)==Trn(1) ) Sort2 = 1
   Any = Stress + Force
!
!     CONICAL SHELL PROBLEM
!
   Axic = .FALSE.
   IF ( Mn/=0 ) THEN
      Nrings = Isystm(161)
      Nharms = Mn
      Axic = .TRUE.
   ENDIF
!
!     RETURN TO READ ANOTHER RECORD IN CASE CONTROL (UNLESS DIFF STIFF
!     PHASE 0 OR BUCKLING PHASE 0)
!
   IF ( App(1)/=Ds0(1) .AND. App(1)/=Bk0(1) ) GOTO 200
!
!     IF ALL .EQ. 0, SORT LIST OF ELEMENT IDS AND MOVE LIST TO END OF
!     CORE. AND THROW AWAY ANY DUPLICATE.
!
 1300 IF ( All/=0 .OR. Any==0 ) THEN
      Mset = Buf2 - 1
   ELSE
      kn = ii - Mset + 1
      CALL sort(0,0,1,-1,Z(Mset),kn)
      jj = Buf2 - 1
      Z(jj) = Z(ii)
      DO
         ii = ii - 1
         IF ( Z(ii)/=Z(jj) ) THEN
            jj = jj - 1
            IF ( ii>=Mset ) THEN
               Z(jj) = Z(ii)
            ELSE
               Mset = jj + 1
               Knset = Buf2 - Mset
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF
!
!     CLOSE CASE CONTROL AND RETURN
!
   CALL close(Casecc,Clsrew)
   IF ( App(1)/=Bk1(1) ) RETURN
   Eldef = 0
   Tloads = 0
   RETURN
!
!
!     SEARCH LIST OF ELEM ID. IF CURRENT ID IS IN LIST RETURN
!     OTHERWISE ADD ID TO LIST
!
!
!     ADD ELEM ID TO LIST. NO NEED TO CHECK DUPLICATE ID HERE
!
 1400 IF ( ii==0 ) ii = Mset - 1
   ii = ii + 1
   IF ( ii<Buf2 ) THEN
      Z(ii) = zi
      GOTO ret
   ELSE
      All = 1
      GOTO 1200
   ENDIF
!
END SUBROUTINE sdr2a