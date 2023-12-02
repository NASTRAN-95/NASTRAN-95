!*==rcovss.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovss
   USE c_blank
   USE c_names
   USE c_rcovcm
   USE c_rcovcr
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: casecc , loadc , name
   INTEGER , SAVE :: casess , eog , eoi , eqss , geom4 , lods , scr1 , soln , srd , swrt
   INTEGER , DIMENSION(2) :: cc
   REAL , DIMENSION(4) :: clod
   INTEGER :: file , i , icase , ilods , isc , iseq , j , jsoln , k , lcc , lodsin , lsem , lskip , n , nc , nlds , nlods , noldc , &
            & nrec , ns , nskip , nwds , rc
   INTEGER , DIMENSION(5) :: iz
   INTEGER , DIMENSION(4) :: lod
   REAL :: sfac
   EXTERNAL bckrec , close , fread , fwdrec , gopen , locate , mesage , open , preloc , read , rewind , sfetch , sjump , skpfil ,   &
          & smsg , sofcls , sort , suread , suwrt , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS ROUTINE GENERATES THE STATIC SOLUTION ITEM FOR RIGID FORMATS
!     1 AND 2
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (lod(1),clod(1))
   DATA name/4HRCOV , 4HSS  /
   DATA soln , eqss , lods/4HSOLN , 4HEQSS , 4HLODS/
   DATA casess , geom4 , scr1/101 , 102 , 301/
   DATA loadc/500 , 5/
   DATA srd , swrt , eog , eoi/1 , 2 , 2 , 3/
   DATA casecc/4HCASE , 4HCC  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CREATE SOLN FOR RIGID FORMAT 1 OR 2
!
!     GET NUMBER OF BASIC SUBSTRUCTURES (NS) FROM EQSS AND CREATE
!     GROUP 0 OF SOLN AT TOP OF OPEN CORE
!
         CALL sfetch(fss,eqss,srd,rc)
         IF ( rc==1 ) THEN
            CALL suread(z,2,nwds,rc)
            CALL suread(ns,1,nwds,rc)
            IF ( lcore<3*ns+5 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(z,1,nwds,rc)
            iz(1) = fss(1)
            iz(2) = fss(2)
            iz(3) = rfno
            iz(4) = ns
!
!     GET SUBSTRUCTURE NAMES FROM EQSS
!
            DO i = 1 , ns
               CALL suread(z(3*i+3),2,nwds,rc)
            ENDDO
!
!     COUNT NUMBER OF SUBCASES (NC) ON CASECC
!
            CALL gopen(casess,z(buf2),rdrew)
            nskip = 1
            DO
               CALL fread(casess,cc,2,1)
               nskip = nskip + 1
               IF ( cc(1)==casecc(1) ) THEN
                  IF ( cc(2)==casecc(2) ) THEN
                     nc = 0
                     DO
                        CALL fwdrec(*20,casess)
                        nc = nc + 1
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ELSE
            CALL smsg(rc-2,eqss,fss)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      CALL rewind(casess)
         iz(5) = nc
!
!     GET NUMBER OF LOAD VECTORS FOR EACH SUBSTRUCTURE FROM LODS
!
         CALL sfetch(fss,lods,srd,rc)
         IF ( rc==1 ) THEN
            j = 1
            CALL sjump(j)
            DO i = 1 , ns
               CALL suread(z(3*i+5),1,nwds,rc)
               CALL sjump(j)
            ENDDO
!
!     SOLN GROUP 0 COMPLETE.  WRITE IT ON SCR1
!
            j = 3
            CALL gopen(scr1,z(buf3),wrtrew)
            CALL write(scr1,z,3*ns+5,1)
!
!     COMPRESS SUBSTRUCTURE NAMES AT TOP OF OPEN CORE
!
            DO i = 1 , ns
               iz(2*i-1) = iz(3*i+3)
               iz(2*i) = iz(3*i+4)
            ENDDO
!
!     PREPARE TO LOOP OVER ALL SUBCASES
!
            icase = 2*ns + 1
            ilods = icase + 166
            IF ( ilods>lcore ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            lodsin = 0
            nlods = ilods - 1
            file = casess
            DO i = 1 , nskip
               CALL fwdrec(*120,casess)
            ENDDO
            noldc = 1
            CALL preloc(*40,z(buf1),geom4)
            CALL locate(*40,z(buf1),loadc,i)
            noldc = 0
         ELSE
            CALL smsg(rc-2,lods,fss)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BEGIN SUBCASE LOOP.  FOR EACH SUBCASE, BUILD ONE GROUP OF SOLN
!
 40      DO isc = 1 , nc
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL fread(casess,z(icase),166,0)
                  nlds = 0
                  IF ( iz(icase+15)/=0 ) THEN
!
!     PROCESS SYMCOM OR SUBCOM SUBCASE
!
!     READ SYMSEQ OR SUBSEQ INTO OPEN CORE AT ISEQ
!
                     lcc = iz(icase+165)
                     lskip = 167 - lcc
                     CALL fread(casess,0,lskip,0)
                     CALL fread(casess,lsem,1,0)
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     file = casess
                     CALL fwdrec(*120,casess)
                     file = geom4
!
!     PROCESS REGULAR SUBCASE.  IF LODS ITEM NOT IN CORE, GET IT.
!
                     IF ( iz(icase+3)==0 ) GOTO 42
                     IF ( noldc==1 ) GOTO 42
                     IF ( lodsin/=1 ) THEN
                        CALL sfetch(fss,lods,srd,rc)
                        i = 1
                        CALL sjump(i)
                        i = ilods
                        DO j = 1 , ns
                           CALL suread(z(i),1,nwds,rc)
                           nlods = i + iz(i)
                           IF ( nlods>lcore ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL suread(z(i+1),-1,nwds,rc)
                           i = nlods + 1
                        ENDDO
                        lodsin = 1
                     ENDIF
!
!     LODS ITEM IN CORE.  FIND MATCH ON LOADC CARD WITH LOAD SET ID
!     FROM CASECC
!
                     jsoln = nlods + 2
                     SPAG_Loop_2_1: DO
                        CALL read(*120,*42,geom4,lod,2,0,nwds)
                        IF ( lod(1)==iz(icase+3) ) THEN
!
!     FOUND MATCH ON LOADC CARD
!
                           sfac = clod(2)
                           DO
                              spag_nextblock_3 = 1
                              SPAG_DispatchLoop_3: DO
                                 SELECT CASE (spag_nextblock_3)
                                 CASE (1)
!
!     LOOP OVER BASIC SUBSTRUCTURES ON THE LOADC CARD
!
                                    CALL fread(geom4,lod,4,0)
                                    IF ( lod(4)==-1 ) EXIT SPAG_Loop_2_1
                                    IF ( jsoln+1>lcore ) THEN
                                       spag_nextblock_1 = 4
                                       CYCLE SPAG_DispatchLoop_1
                                    ENDIF
!
!     FIND BASIC SUBSTRUCTURE NUMBER BY MATCHING ITS NAME WITH THOSE
!     FROM EQSS.  THEN DETERMINE LOAD VECTOR NUMBER BY MATCHING THE
!     BASIC SUBSTRUCTURE LOAD SET ID WITH THOSE IN LODS DATA IN CORE.
!
                                    DO i = 1 , ns
                                       IF ( lod(1)==iz(2*i-1) ) THEN
                                         k = i
                                         IF ( lod(2)==iz(2*i) ) THEN
                                         spag_nextblock_3 = 2
                                         CYCLE SPAG_DispatchLoop_3
                                         ENDIF
                                       ENDIF
                                    ENDDO
                                    WRITE (nout,99001) uwm , lod(1) , lod(2) , lod(3) , fss
!
!     DIAGNOSTICS
!
99001                               FORMAT (A25,' 6315, RCOVR MODULE IS UNABLE TO FIND SUBSTRUCTURE ',2A4,                          &
                                     & ' AMONG THOSE ON EQSS.'/32X,'LOAD SET',I9,                                                   &
                                      &' FOR THAT SUBSTRUCTURE WILL BE IGNORED IN CREATING',/32X,                                   &
                                      &'THE SOLN ITEM FOR FINAL SOLUTION STRUCTURE ',2A4)
                                    spag_nextblock_3 = 2
                                 CASE (2)
                                    n = 0
                                    i = ilods
                                    j = 1
                                    DO WHILE ( j/=k )
                                       n = n + iz(i)
                                       i = i + iz(i) + 1
                                       j = j + 1
                                    ENDDO
                                    j = iz(i)
                                    DO k = 1 , j
                                       n = n + 1
                                       IF ( iz(i+k)==lod(3) ) THEN
                                         spag_nextblock_3 = 3
                                         CYCLE SPAG_DispatchLoop_3
                                       ENDIF
                                    ENDDO
                                    WRITE (nout,99002) uwm , lod(3) , lod(1) , lod(2) , fss
99002                               FORMAT (A25,' 6316, RCOVR MODULE IS UNABLE TO FIND LOAD SET',I9,' FOR SUBSTRUCTURE ',2A4,/32X,  &
                                      &'AMONG THOSE ON LODS.  ','IT WILL BE IGNORED IN CREATING THE SOLN ITEM FOR FINAL',/32X,      &
                                      &'SOLUTION STRUCTURE ',2A4)
                                    spag_nextblock_3 = 3
                                 CASE (3)
!
!     BUILD SOLN GROUP IN OPEN CORE FOLLOWING LODS DATA
!
                                    iz(jsoln) = n
                                    z(jsoln+1) = sfac*clod(4)
                                    jsoln = jsoln + 2
                                    nlds = nlds + 1
                                    EXIT SPAG_DispatchLoop_3
                                 END SELECT
                              ENDDO SPAG_DispatchLoop_3
                           ENDDO
                        ELSE
                           SPAG_Loop_3_2: DO
                              CALL fread(geom4,lod,4,0)
                              IF ( lod(4)==-1 ) EXIT SPAG_Loop_3_2
                           ENDDO SPAG_Loop_3_2
                        ENDIF
                     ENDDO SPAG_Loop_2_1
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  iz(nlods+1) = nlds
                  jsoln = nlods + 1
                  spag_nextblock_2 = 5
                  CYCLE SPAG_DispatchLoop_2
!
!     NO LOADS FOR THIS SUBCASE
!
 42               nlds = 0
                  spag_nextblock_2 = 2
               CASE (3)
                  IF ( lsem+nlods<lcore ) THEN
                     iseq = nlods + 1
                     CALL fread(casess,z(iseq),lsem,1)
!
!     READ THE PREVIOUS LSEM GROUPS OF SOLN INTO OPEN CORE FOLLOWING SEQ
!
                     jsoln = iseq + lsem
                     k = jsoln + 1
                     CALL close(scr1,eofnrw)
                     file = scr1
                     CALL open(*100,scr1,z(buf3),rd)
                     nrec = 1
                     nlds = 0
                     DO i = 1 , lsem
                        SPAG_Loop_3_3: DO
                           DO j = 1 , nrec
                              CALL bckrec(scr1)
                           ENDDO
                           CALL fread(scr1,n,1,0)
                           nrec = 2
                           IF ( n>=0 ) THEN
                              IF ( k+2*n-1<lcore ) THEN
                                 CALL fread(scr1,z(k),2*n,1)
!
!     SCALE LOAD FACTORS BY SYMSEQ OR SUBSEQ FACTORS
!
                                 DO j = 1 , n
                                    z(k+2*j-1) = z(iseq+lsem-i)*z(k+2*j-1)
                                 ENDDO
                                 k = k + 2*n
                                 nlds = nlds + n
                                 EXIT SPAG_Loop_3_3
                              ELSE
                                 IF ( lodsin==0 ) THEN
                                    spag_nextblock_1 = 4
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
!
!     SHORT OF CORE.  REPOSITION CASESS, WIPE OUT LODS DATA, AND TRY
!     AGAIN
!
                                 CALL bckrec(casess)
                                 CALL fread(casess,0,-166,0)
                                 spag_nextblock_2 = 4
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDIF
                        ENDDO SPAG_Loop_3_3
                     ENDDO
                     iz(jsoln) = -nlds
!
!     COMBINATION GROUP COMPLETE.  REPOSITION SCR1
!
                     file = scr1
                     DO
                        CALL fwdrec(*44,scr1)
                     ENDDO
                  ELSEIF ( lodsin==0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (4)
!
!     SHORT OF CORE.  WIPE OUT LODS DATA AND RE-USE SPACE
!
                  lodsin = 0
                  nlods = ilods - 1
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 44               CALL skpfil(scr1,-1)
                  CALL close(scr1,norew)
                  CALL open(*100,scr1,z(buf3),wrt)
                  spag_nextblock_2 = 5
               CASE (5)
!
!     GROUP COMPLETE IN CORE.  SORT ON LOAD VECTOR NUMBERS
!
                  CALL sort(0,0,2,1,z(jsoln+1),2*nlds)
!
!     WRITE GROUP ON SCR1 AND POSITION GEOM4 TO BEGINNING OF LOADC CARDS
!
                  CALL write(scr1,z(jsoln),2*nlds+1,1)
                  IF ( noldc/=1 ) THEN
                     CALL bckrec(geom4)
                     CALL fread(geom4,0,-3,0)
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
!     END OF LOOP OVER SUBCASES
!
         ENDDO
         CALL close(casess,rew)
         CALL close(geom4,rew)
         CALL close(scr1,rew)
!
!     COPY SOLN FROM SCR1 TO SOF
!
         CALL gopen(scr1,z(buf1),rdrew)
         rc = 3
         CALL sfetch(fss,soln,swrt,rc)
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*80,*60,scr1,z,lcore,1,nwds)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      CALL suwrt(z,nwds,eog)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      CALL close(scr1,rew)
!
!     FINISH
!
         CALL suwrt(0,0,eoi)
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
!
 100     n = 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     n = 2
         spag_nextblock_1 = 5
      CASE (4)
         n = 8
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(n,file,name)
         spag_nextblock_1 = 6
      CASE (6)
         CALL sofcls
         iopt = -1
         CALL close(casess,rew)
         CALL close(geom4,rew)
         CALL close(scr1,rew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE rcovss
