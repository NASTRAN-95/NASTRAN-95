!*==rcovls.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovls(Lastss)
   IMPLICIT NONE
   USE C_BLANK
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Lastss
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eog , eoi , eqss , soln , srd , swrt
   INTEGER :: i , ig0 , igs , incr , isol , istep , j , jgs , k , kgs , l1 , l2 , lsl , lvn , n , nc , nl , ns , nsl , nsll , nss , &
            & nwds , rc
   INTEGER , DIMENSION(3) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL delete , mesage , sfetch , sjump , smsg , sofcls , suread , suwrt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS ROUTINE CREATES THE SOLN ITEM FOR A LOWER LEVEL SUBSTRUCTURE,
!     LASTSS, BY EDITING THAT OF THE SOLUTION SUBSTRUCTURE FSS.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HRCOV , 4HLS  /
   DATA eqss , soln/4HEQSS , 4HSOLN/
   DATA srd , swrt/1 , 2/
   DATA eog , eoi/2 , 3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CREATE SOLN ITEM FOR THE RECOVERED SUBSTRUCTURE
!
         IF ( Rfno==3 ) THEN
!
!     MODAL SOLUTION ITEM
!
!     FOR MODAL COPY THE SOLN UNCHANGED.  IN CASE THE USER FORGOT
!     TO EDIT OUT THIS SOLN FROM A PREVIOUS RUN, DELETE IT TO AVOID
!     LOSING OR SCREWING UP THE RECOVERED DISPLACEMENTS.
!
            CALL delete(Lastss,soln,rc)
            CALL sfetch(Fss,soln,srd,rc)
            CALL suread(iz(Icore),-1,nwds,rc)
            isol = iz(Icore+2)
            IF ( isol/=Rfno ) THEN
!
!     ERROR PROCESSING
!
               WRITE (Nout,99001) Ufm , isol , Rfno
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( iz(Icore+3)>0 ) THEN
               IF ( Lcore<Icore+7*iz(Icore+3)+3 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL suread(iz(Icore+4),-1,nwds,rc)
               rc = 3
               CALL sfetch(Lastss,soln,swrt,rc)
               CALL suwrt(Z(Icore),4,eog)
               CALL suwrt(Z(Icore+4),7*iz(Icore+3),eog)
               CALL suwrt(0,0,eoi)
            ELSE
               rc = 3
               CALL sfetch(Lastss,soln,swrt,rc)
               CALL suwrt(Z(Icore),4,eog)
               CALL suwrt(0,0,eoi)
            ENDIF
         ELSE
!
!     OBTAIN LIST OF CONTRIBUTING BASIC SUBSTRUCTURES FROM EQSS.
!     STORE IN OPEN CORE AT ICORE.
!
            CALL sfetch(Lastss,eqss,srd,rc)
            CALL suread(Z(Icore),4,nwds,rc)
            nss = iz(Icore+2)
            IF ( Lcore<=Icore+2*nss-1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(Z(Icore),2*nss,nwds,rc)
!
!     CONSTRUCT SOLN GROUP 0 IN OPEN CORE AT IG0.  TWO SLOTS FOR THE
!     NUMBER OF LOADS ON EACH SUBSTRUCTURE.  FIRST IS FOR OLD FSS
!     SOLN, SECOND FOR NEW ONE.
!
            ig0 = Icore + 2*nss
            CALL sfetch(Fss,soln,srd,rc)
            IF ( rc==1 ) THEN
               CALL suread(Z(ig0),5,nwds,rc)
               isol = iz(ig0+2)
               IF ( isol/=Rfno ) THEN
                  WRITE (Nout,99001) Ufm , isol , Rfno
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  ns = iz(ig0+3)
                  nc = iz(ig0+4)
                  IF ( ig0+4+4*ns>Lcore ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO i = 1 , ns
                     CALL suread(Z(ig0+1+4*i),3,nwds,rc)
                     iz(ig0+4*i+4) = -65535
                     SPAG_Loop_2_1: DO j = 1 , nss
                        IF ( iz(ig0+4*i+1)==iz(Icore+2*j-2) ) THEN
                           IF ( iz(ig0+4*i+2)==iz(Icore+2*j-1) ) THEN
                              iz(ig0+4*i+4) = iz(ig0+4*i+3)
                              EXIT SPAG_Loop_2_1
                           ENDIF
                        ENDIF
                     ENDDO SPAG_Loop_2_1
                  ENDDO
                  IF ( Rfno==8 .OR. Rfno==9 ) THEN
!
!     DYNAMIC SOLUTION ITEM
!
!     READ IN STATIC LOAD SETS
!
                     incr = 1
                     IF ( Rfno==8 ) incr = 2
                     igs = ig0 + 4*ns + 5
                     CALL suread(Z(igs),1,nwds,rc)
                     nsl = iz(igs)
                     lsl = nsl*incr
                     nsll = 0
                     IF ( nsl/=0 ) THEN
                        IF ( igs+nsl>Lcore ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        CALL suread(Z(igs+1),nsl,nwds,rc)
!
!     FLAG THOSE STATIC LOAD IDS THAT ARE NOT IN THE LOWER LEVEL
!     SUBSTRUCTURE AND RENUMBER THOSE THAT ARE LEFT
!
                        DO j = 1 , nsl
                           spag_nextblock_2 = 1
                           SPAG_DispatchLoop_2: DO
                              SELECT CASE (spag_nextblock_2)
                              CASE (1)
                                 lvn = iz(igs+j)
                                 l1 = 0
                                 l2 = 0
                                 SPAG_Loop_2_2: DO k = 1 , ns
                                    IF ( lvn>l2+iz(ig0+4*k+3) ) THEN
                                       IF ( iz(ig0+4*k+4)<0 ) l1 = l1 + iz(ig0+4*k+3)
                                       l2 = l2 + iz(ig0+4*k+3)
                                    ELSE
                                       IF ( iz(ig0+4*k+4)<0 ) EXIT SPAG_Loop_2_2
                                       lvn = lvn - l1
                                       nsll = nsll + 1
                                       spag_nextblock_2 = 2
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
                                 ENDDO SPAG_Loop_2_2
                                 lvn = -65535
                                 spag_nextblock_2 = 2
                              CASE (2)
                                 iz(igs+j) = lvn
                                 EXIT SPAG_DispatchLoop_2
                              END SELECT
                           ENDDO SPAG_DispatchLoop_2
                        ENDDO
                     ENDIF
!
!     COPY THE FREQUENCY OR TIME STEP RECORD INTO CORE
!
                     i = 1
                     CALL sjump(i)
                     istep = igs + nsl + 1
                     IF ( istep+nc>Lcore ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL suread(iz(istep),-1,nwds,rc)
!
!     COPY IN ALL LOAD FACTOR DATA
!
                     IF ( nsll/=0 ) THEN
                        jgs = istep + nc
                        DO i = 1 , nc
                           IF ( jgs+lsl>Lcore ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL suread(Z(jgs),-1,nwds,rc)
                           jgs = jgs + lsl
                        ENDDO
                     ENDIF
!
!     WRITE THE NEW SOLN ITEM FOR THE RECOVERED SUBSTRUCTURE.  IN CASE
!     THE USER FORGOT TO EDIT OUT THIS SOLN FROM A PREVIOUS RUN,
!     DELETE IT TO AVOID LOSING OR SCREWING UP THE RECOVERED
!     DISPLACEMENTS
!
                     CALL delete(Lastss,soln,rc)
                     rc = 3
                     CALL sfetch(Lastss,soln,swrt,rc)
                     iz(ig0+3) = nss
                     CALL suwrt(Z(ig0),5,1)
                     DO i = 1 , ns
                        IF ( iz(ig0+4*i+4)>=0 ) THEN
                           CALL suwrt(Z(ig0+4*i+1),2,1)
                           CALL suwrt(Z(ig0+4*i+4),1,1)
                        ENDIF
                     ENDDO
                     CALL suwrt(nsll,1,1)
                     IF ( nsll/=0 ) THEN
                        DO i = 1 , nsl
                           IF ( Z(igs+i)>=0 ) CALL suwrt(Z(igs+i),1,1)
                        ENDDO
                     ENDIF
                     CALL suwrt(0,0,eog)
!
!     COPY THE TIME OR FREQUENCY STEP INFO TO SOF.
!
                     CALL suwrt(Z(istep),nc,eog)
!
!     COPY LOAD FACTORS FOR EACH STEP TO SOF EDITING OUT THOSE
!     THAT NO LONGER PARTICIAPTE
!
                     IF ( nsll/=0 ) THEN
                        kgs = istep + nc
                        DO i = 1 , nc
                           k = 1
                           DO j = 1 , nsl
                              IF ( Z(igs+j)>=0 ) THEN
                                 CALL suwrt(Z(kgs+k-1),incr,1)
                                 k = k + incr
                              ENDIF
                           ENDDO
                           CALL suwrt(0,0,eog)
                           kgs = kgs + lsl
                        ENDDO
                     ENDIF
!
                     CALL suwrt(0,0,eoi)
                  ELSE
                     i = 1
                     CALL sjump(i)
!
!     STATICS SOLUTION ITEM
!
!     READ ALL GROUPS OF THE OLD FSS SOLN INTO OPEN CORE AT IGS.
!     AS EACH ONE IS READ, ELIMINATE LOAD VECTORS WHICH DO NOT
!     APPLY TO THE NEW SOLN BY SETTING THEIR LOAD VECTOR
!     NUMBERS TO -65535.
!     UPDATE THE NUMBER OF LOAD VECTORS WHICH DO APPLY.
!
                     igs = ig0 + 4*ns + 5
                     jgs = igs
                     DO i = 1 , nc
                        CALL suread(Z(jgs),1,nwds,rc)
                        n = iabs(iz(jgs))
                        IF ( jgs+n*2>Lcore ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        CALL suread(Z(jgs+1),-1,nwds,rc)
                        nl = 0
                        IF ( n/=0 ) THEN
                           DO j = 1 , n
                              spag_nextblock_3 = 1
                              SPAG_DispatchLoop_3: DO
                                 SELECT CASE (spag_nextblock_3)
                                 CASE (1)
                                    lvn = iz(jgs+2*j-1)
!
!     FIND SUBSTRUCTURE WHERE LVN IS APPLIED FOR FSS SOLN ITEM.
!
                                    l1 = 0
                                    l2 = 0
                                    SPAG_Loop_3_3: DO k = 1 , ns
                                       IF ( lvn>l2+iz(ig0+4*k+3) ) THEN
                                         IF ( iz(ig0+4*k+4)<0 ) l1 = l1 + iz(ig0+4*k+3)
                                         l2 = l2 + iz(ig0+4*k+3)
                                       ELSE
                                         IF ( iz(ig0+4*k+4)<0 ) EXIT SPAG_Loop_3_3
                                         lvn = lvn - l1
                                         nl = nl + 1
                                         spag_nextblock_3 = 2
                                         CYCLE SPAG_DispatchLoop_3
                                       ENDIF
                                    ENDDO SPAG_Loop_3_3
                                    lvn = -65535
                                    spag_nextblock_3 = 2
                                 CASE (2)
                                    iz(jgs+2*j-1) = lvn
                                    EXIT SPAG_DispatchLoop_3
                                 END SELECT
                              ENDDO SPAG_DispatchLoop_3
                           ENDDO
                           IF ( iz(jgs)<0 ) nl = -nl
                           iz(jgs) = nl
                        ENDIF
                        jgs = jgs + 2*n + 1
                     ENDDO
!
!     WRITE THE NEW SOLN FOR THE RECOVERED SUBSTRUCTURE ON THE SOF.
!     IN CASE USER FORGOT TO EDIT OUT THIS SOLN FROM A PREVIOUS
!     RUN, DELETE IT TO AVOID LOSING OR SCREWING UP THE RECOVERED
!     DISPLACEMENTS.
!
                     CALL delete(Lastss,soln,rc)
                     iz(ig0+3) = nss
                     rc = 3
                     CALL sfetch(Lastss,soln,swrt,rc)
                     CALL suwrt(Z(ig0),5,1)
                     DO i = 1 , ns
                        IF ( iz(ig0+4*i+4)>=0 ) THEN
                           CALL suwrt(Z(ig0+4*i+1),2,1)
                           CALL suwrt(Z(ig0+4*i+4),1,1)
                        ENDIF
                     ENDDO
                     CALL suwrt(0,0,eog)
                     jgs = igs
                     DO i = 1 , nc
                        k = 0
                        nl = iz(jgs)
                        jgs = jgs + 1
                        CALL suwrt(nl,1,1)
                        IF ( nl/=0 ) THEN
                           nl = iabs(nl)
                           SPAG_Loop_2_4: DO
                              IF ( iz(jgs)/=-65535 ) THEN
                                 CALL suwrt(iz(jgs),2,1)
                                 k = k + 1
                              ENDIF
                              jgs = jgs + 2
                              IF ( k>=nl ) EXIT SPAG_Loop_2_4
                           ENDDO SPAG_Loop_2_4
                        ENDIF
                        DO WHILE ( iz(jgs)==-65535 )
                           jgs = jgs + 2
                        ENDDO
                        CALL suwrt(0,0,eog)
                     ENDDO
                     CALL suwrt(0,0,eoi)
                  ENDIF
               ENDIF
            ELSE
               CALL smsg(rc-2,soln,Fss)
            ENDIF
         ENDIF
!
!     NORMAL RETURN
!
         RETURN
      CASE (2)
         n = 8
         CALL mesage(n,0,name)
         spag_nextblock_1 = 3
      CASE (3)
         Iopt = -1
         CALL sofcls
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99001 FORMAT (A23,' 6369.  SOLN ITEM HAS INCORRECT RIGID FORMAT NUMBER',/31X,'SOLUTION RIGID FORMAT WAS',I5,                        &
             &' AND CURRENT NASTRAN EXECUTION RIGID FORMAT IS',I5)
END SUBROUTINE rcovls
