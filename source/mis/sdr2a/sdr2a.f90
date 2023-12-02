!*==sdr2a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2a
   IMPLICIT NONE
   USE C_BLANK
   USE C_MACHIN
   USE C_NAMES
   USE C_SDR2X1
   USE C_SDR2X2
   USE C_SDR2X4
   USE C_SYSTEM
   USE C_TWO
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: any1 , i , ii , iset , isetno , j , jj , kn , n , ncc , nset , pass , prevf , prevs , ret , setno , zi
   INTEGER , DIMENSION(175) :: isystm
   INTEGER , SAVE :: mmreig
   EXTERNAL close , gopen , korsz , mesage , read , skprec , sort
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SDR2A PROCESSES THE CASE CONTROL DATA BLOCK. DEPENDING ON THE
!     RIGID FORMAT AND THE VARIOUS OUTPUT REQUESTS, SDR2A SETS FLAGS
!     AND PARAMETERS TO CONTROL OPERATION OF THE REMAINDER OF THE PHASES
!     OF SDR2
!
   !>>>>EQUIVALENCE (Sysbuf,Isystm)
   DATA mmreig/4HMMRE/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               IF ( Sta(i)==App(1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL mesage(-30,75,App)
         ELSE
            Ddrmm = .TRUE.
            i = 3
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         Branch = (i+1)/2
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
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ A RECORD IN CASE CONTROL.
!     IF REQUEST FOR STRESSES IS PRESENT, TURN ON STRESS FLAG.
!     IF REQUEST FOR FORCES   IS PRESENT, TURN ON FORCE  FLAG.
!     -ANY- FLAG = STRESS .OR. FORCE.
!     -ALL- FLAG = ANY REQUEST FOR ALL STRESSES OR FORCES.
!     IF ANY.NE.0 .AND ALL.EQ.0, BUILD LIST OF UNIQUE ELEMENT IDS.
!
         CALL read(*120,*20,Casecc,Z,Buf5-1,1,ncc)
         CALL mesage(+8,0,Nam)
         All = 1
 20      any1 = 0
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
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Stress = 1
         any1 = 1
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Z(Ielf)<0 ) THEN
            All = 1
         ELSEIF ( Z(Ielf)==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Force = 1
         any1 = 1
         spag_nextblock_1 = 5
      CASE (5)
         IF ( All/=0 .OR. any1==0 ) GOTO 100
!
!     INITIALIZE TO PROCESS STRESS OUTPUT REQUEST.
!     BUILD MASTER SET LIST ONLY IF CURRENT SET ID IS NEW
!
         ASSIGN 80 TO pass
         setno = Z(Istr)
         IF ( setno==prevs ) GOTO 80
         prevs = setno
         spag_nextblock_1 = 6
      CASE (6)
!
!     IF REQUEST PRESENT, LOCATE SET DEFINITION IN CASE CONTROL DATA.
!
         IF ( setno==0 ) GOTO pass
         isetno = Ilsym + Z(Ilsym) + 1
         SPAG_Loop_1_1: DO
            iset = isetno + 2
            nset = Z(isetno+1) + iset - 1
            IF ( Z(isetno)==setno ) THEN
!
!     PICK UP ELEMENT IDS IN SET. SAVE IN UNIQUE LIST.
!
               i = iset
               EXIT SPAG_Loop_1_1
            ELSE
               isetno = nset + 1
               IF ( isetno>=ncc ) THEN
                  All = 1
                  GOTO 100
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 7
      CASE (7)
         IF ( i==nset ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Z(i+1)>0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         zi = Z(i)
         n = -Z(i+1)
         i = i + 1
         ASSIGN 40 TO ret
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 40      DO
            zi = zi + 1
            IF ( zi>n ) GOTO 60
            ii = ii + 1
            IF ( ii>Buf2 ) THEN
               All = 1
               GOTO 100
            ELSE
               Z(ii) = zi
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
         zi = Z(i)
         ASSIGN 60 TO ret
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 60      i = i + 1
         IF ( i<=nset ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO pass
!
!     INITIALIZE TO PROCESS FORCE OUTPUT REQUEST.
!     BUILD MASTER SET LIST ONLY IF CURRENT SET ID IS NEW
!
 80      setno = Z(Ielf)
         IF ( setno/=prevf ) THEN
            prevf = setno
            ASSIGN 100 TO pass
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TURN ON FLAGS FOR OTHER OUTPUT REQUESTS.
!
 100     IF ( Z(Iloads)/=0 ) Loads = 1
         IF ( Z(Ispcf)/=0 ) Spcf = 1
         IF ( Z(Idispl)/=0 ) Displ = 1
         IF ( Z(Ivel)/=0 ) Vel = 1
         IF ( Z(Iacc)/=0 ) Acc = 1
         IF ( Z(Ieldef)/=0 ) Eldef = 1
         IF ( Z(Itload)/=0 ) Tloads = 1
         IF ( Z(Iloads+2)<0 .OR. Z(Ispcf+2)<0 .OR. Z(Idispl+2)<0 .OR. Z(Ivel+2)<0 .OR. Z(Iacc+2)<0 .OR. Z(Istr+2)<0 .OR. Z(Ielf+2)  &
            & <0 .OR. App(1)==Trn(1) ) Sort2 = 1
         Any = Stress + Force
!
!     CONICAL SHELL PROBLEM
!
         Axic = .FALSE.
         IF ( Mn/=0 ) THEN
            Nrings = isystm(161)
            Nharms = Mn
            Axic = .TRUE.
         ENDIF
!
!     RETURN TO READ ANOTHER RECORD IN CASE CONTROL (UNLESS DIFF STIFF
!     PHASE 0 OR BUCKLING PHASE 0)
!
         IF ( App(1)/=Ds0(1) .AND. App(1)/=Bk0(1) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     IF ALL .EQ. 0, SORT LIST OF ELEMENT IDS AND MOVE LIST TO END OF
!     CORE. AND THROW AWAY ANY DUPLICATE.
!
 120     IF ( All/=0 .OR. Any==0 ) THEN
            Mset = Buf2 - 1
         ELSE
            kn = ii - Mset + 1
            CALL sort(0,0,1,-1,Z(Mset),kn)
            jj = Buf2 - 1
            Z(jj) = Z(ii)
            SPAG_Loop_1_2: DO
               ii = ii - 1
               IF ( Z(ii)/=Z(jj) ) THEN
                  jj = jj - 1
                  IF ( ii>=Mset ) THEN
                     Z(jj) = Z(ii)
                  ELSE
                     Mset = jj + 1
                     Knset = Buf2 - Mset
                     EXIT SPAG_Loop_1_2
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     CLOSE CASE CONTROL AND RETURN
!
         CALL close(Casecc,Clsrew)
         IF ( App(1)/=Bk1(1) ) RETURN
         Eldef = 0
         Tloads = 0
         RETURN
      CASE (9)
!
!
!     SEARCH LIST OF ELEM ID. IF CURRENT ID IS IN LIST RETURN
!     OTHERWISE ADD ID TO LIST
!
!
!     ADD ELEM ID TO LIST. NO NEED TO CHECK DUPLICATE ID HERE
!
         IF ( ii==0 ) ii = Mset - 1
         ii = ii + 1
         IF ( ii<Buf2 ) THEN
            Z(ii) = zi
            GOTO ret
         ELSE
            All = 1
            GOTO 100
         ENDIF
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE sdr2a
