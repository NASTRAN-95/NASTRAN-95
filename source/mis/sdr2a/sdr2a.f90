!*==sdr2a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2a
   USE c_blank
   USE c_machin
   USE c_names
   USE c_sdr2x1
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_system
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
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
         strain = .FALSE.
         IF ( istrn>=0 ) strain = .TRUE.
!
!     PERFORM BUFFER ALLOCATION.
!
         buf1 = korsz(z) - sysbuf - 2
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
!
!     SET PARAMETER FOR APPROACH.
!
         n = 2*nrigds - 1
!
!     FIRST CHECK FOR SPECIAL APPROACH FOR DYNAMIC-DATA-RECOVERY-MATRIX-
!     METHOD.  IF APPROACH IS -MMREIG- THEN DDRMM FLAG IS SET TO INSURE
!     ENOUGH OUTPUTS UNDER CERTAIN CONDITIONS.
!
         ddrmm = .FALSE.
         IF ( app(1)/=mmreig ) THEN
!
            DO i = 1 , n , 2
               IF ( sta(i)==app(1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL mesage(-30,75,app)
         ELSE
            ddrmm = .TRUE.
            i = 3
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         branch = (i+1)/2
!
!    OPEN CASE CONTROL. SKIP HEADER RECORD.
!    IF DIFF. STIFF. PHASE 1 OR BUCKLING PHASE 1, SKIP 1ST CASECC RECORD
!
         CALL gopen(casecc,z(buf1),rdrew)
         IF ( app(1)==ds1(1) .OR. app(1)==bk1(1) ) CALL skprec(casecc,1)
         kwdcc = 0
!
!     INITIALIZE VARIOUS OUTPUT REQUEST FLAGS.
!
         all = 0
         any = 0
         displ = 0
         vel = 0
         acc = 0
         spcf = 0
         loads = 0
         stress = 0
         force = 0
         tloads = 0
         eldef = 0
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
         CALL read(*120,*20,casecc,z,buf5-1,1,ncc)
         CALL mesage(+8,0,nam)
         all = 1
 20      any1 = 0
         kwdcc = max0(kwdcc,ncc)
         mset = max0(mset,kwdcc+1)
!
!     SET DMAP FLAG FOR USE IN DISP R.F. 1
!
         IF ( istrn<0 .AND. strnfl<0 ) THEN
            j = 180
            IF ( z(j)/=0 ) strnfl = 1
         ENDIF
         istr = 23
         IF ( strain ) istr = 180
         IF ( z(istr)<0 ) THEN
            all = 1
         ELSEIF ( z(istr)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         stress = 1
         any1 = 1
         spag_nextblock_1 = 4
      CASE (4)
         IF ( z(ielf)<0 ) THEN
            all = 1
         ELSEIF ( z(ielf)==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         force = 1
         any1 = 1
         spag_nextblock_1 = 5
      CASE (5)
         IF ( all/=0 .OR. any1==0 ) GOTO 100
!
!     INITIALIZE TO PROCESS STRESS OUTPUT REQUEST.
!     BUILD MASTER SET LIST ONLY IF CURRENT SET ID IS NEW
!
         ASSIGN 80 TO pass
         setno = z(istr)
         IF ( setno==prevs ) GOTO 80
         prevs = setno
         spag_nextblock_1 = 6
      CASE (6)
!
!     IF REQUEST PRESENT, LOCATE SET DEFINITION IN CASE CONTROL DATA.
!
         IF ( setno==0 ) GOTO pass
         isetno = ilsym + z(ilsym) + 1
         SPAG_Loop_1_1: DO
            iset = isetno + 2
            nset = z(isetno+1) + iset - 1
            IF ( z(isetno)==setno ) THEN
!
!     PICK UP ELEMENT IDS IN SET. SAVE IN UNIQUE LIST.
!
               i = iset
               EXIT SPAG_Loop_1_1
            ELSE
               isetno = nset + 1
               IF ( isetno>=ncc ) THEN
                  all = 1
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
         IF ( z(i+1)>0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         zi = z(i)
         n = -z(i+1)
         i = i + 1
         ASSIGN 40 TO ret
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 40      DO
            zi = zi + 1
            IF ( zi>n ) GOTO 60
            ii = ii + 1
            IF ( ii>buf2 ) THEN
               all = 1
               GOTO 100
            ELSE
               z(ii) = zi
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
         zi = z(i)
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
 80      setno = z(ielf)
         IF ( setno/=prevf ) THEN
            prevf = setno
            ASSIGN 100 TO pass
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TURN ON FLAGS FOR OTHER OUTPUT REQUESTS.
!
 100     IF ( z(iloads)/=0 ) loads = 1
         IF ( z(ispcf)/=0 ) spcf = 1
         IF ( z(idispl)/=0 ) displ = 1
         IF ( z(ivel)/=0 ) vel = 1
         IF ( z(iacc)/=0 ) acc = 1
         IF ( z(ieldef)/=0 ) eldef = 1
         IF ( z(itload)/=0 ) tloads = 1
         IF ( z(iloads+2)<0 .OR. z(ispcf+2)<0 .OR. z(idispl+2)<0 .OR. z(ivel+2)<0 .OR. z(iacc+2)<0 .OR. z(istr+2)<0 .OR. z(ielf+2)  &
            & <0 .OR. app(1)==trn(1) ) sort2 = 1
         any = stress + force
!
!     CONICAL SHELL PROBLEM
!
         axic = .FALSE.
         IF ( mn/=0 ) THEN
            nrings = isystm(161)
            nharms = mn
            axic = .TRUE.
         ENDIF
!
!     RETURN TO READ ANOTHER RECORD IN CASE CONTROL (UNLESS DIFF STIFF
!     PHASE 0 OR BUCKLING PHASE 0)
!
         IF ( app(1)/=ds0(1) .AND. app(1)/=bk0(1) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     IF ALL .EQ. 0, SORT LIST OF ELEMENT IDS AND MOVE LIST TO END OF
!     CORE. AND THROW AWAY ANY DUPLICATE.
!
 120     IF ( all/=0 .OR. any==0 ) THEN
            mset = buf2 - 1
         ELSE
            kn = ii - mset + 1
            CALL sort(0,0,1,-1,z(mset),kn)
            jj = buf2 - 1
            z(jj) = z(ii)
            SPAG_Loop_1_2: DO
               ii = ii - 1
               IF ( z(ii)/=z(jj) ) THEN
                  jj = jj - 1
                  IF ( ii>=mset ) THEN
                     z(jj) = z(ii)
                  ELSE
                     mset = jj + 1
                     knset = buf2 - mset
                     EXIT SPAG_Loop_1_2
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     CLOSE CASE CONTROL AND RETURN
!
         CALL close(casecc,clsrew)
         IF ( app(1)/=bk1(1) ) RETURN
         eldef = 0
         tloads = 0
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
         IF ( ii==0 ) ii = mset - 1
         ii = ii + 1
         IF ( ii<buf2 ) THEN
            z(ii) = zi
            GOTO ret
         ELSE
            all = 1
            GOTO 100
         ENDIF
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE sdr2a
