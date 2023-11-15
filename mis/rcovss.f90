
SUBROUTINE rcovss
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Buf2 , Buf3 , Dry , Eofnrw , Fss(2) , Icore , Iopt , Ireq , Iz(5) , Lbasic , Lcore , Loop , Lreq , Lui , Mrecvr , &
         & Neigv , Norew , Nosort , Nout , Rd , Rdrew , Rew , Rfno , Step , Sysbuf , Wrt , Wrtrew
   REAL Buf4 , Energy , Pa , Pthres , Qa , Qthres , Range(2) , Rss(2) , Sof1 , Sof2 , Sof3 , Ua , Uimpro , Uinms(2,5) , Uthres ,    &
      & Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER casecc(2) , casess , cc(2) , eog , eoi , eqss , file , geom4 , i , icase , ilods , isc , iseq , j , jsoln , k , lcc ,    &
         & loadc(2) , lod(4) , lods , lodsin , lsem , lskip , n , name(2) , nc , nlds , nlods , noldc , nrec , ns , nskip , nwds ,  &
         & rc , scr1 , soln , srd , swrt
   REAL clod(4) , sfac
!
! End of declarations
!
!
!     THIS ROUTINE GENERATES THE STATIC SOLUTION ITEM FOR RIGID FORMATS
!     1 AND 2
!
   EQUIVALENCE (Z(1),Iz(1)) , (lod(1),clod(1))
   DATA name/4HRCOV , 4HSS  /
   DATA soln , eqss , lods/4HSOLN , 4HEQSS , 4HLODS/
   DATA casess , geom4 , scr1/101 , 102 , 301/
   DATA loadc/500 , 5/
   DATA srd , swrt , eog , eoi/1 , 2 , 2 , 3/
   DATA casecc/4HCASE , 4HCC  /
!
!     CREATE SOLN FOR RIGID FORMAT 1 OR 2
!
!     GET NUMBER OF BASIC SUBSTRUCTURES (NS) FROM EQSS AND CREATE
!     GROUP 0 OF SOLN AT TOP OF OPEN CORE
!
   CALL sfetch(Fss,eqss,srd,rc)
   IF ( rc==1 ) THEN
      CALL suread(Z,2,nwds,rc)
      CALL suread(ns,1,nwds,rc)
      IF ( Lcore<3*ns+5 ) GOTO 1200
      CALL suread(Z,1,nwds,rc)
      Iz(1) = Fss(1)
      Iz(2) = Fss(2)
      Iz(3) = Rfno
      Iz(4) = ns
!
!     GET SUBSTRUCTURE NAMES FROM EQSS
!
      DO i = 1 , ns
         CALL suread(Z(3*i+3),2,nwds,rc)
      ENDDO
!
!     COUNT NUMBER OF SUBCASES (NC) ON CASECC
!
      CALL gopen(casess,Z(Buf2),Rdrew)
      nskip = 1
      DO
         CALL fread(casess,cc,2,1)
         nskip = nskip + 1
         IF ( cc(1)==casecc(1) ) THEN
            IF ( cc(2)==casecc(2) ) THEN
               nc = 0
               DO
                  CALL fwdrec(*100,casess)
                  nc = nc + 1
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ELSE
      CALL smsg(rc-2,eqss,Fss)
      GOTO 900
   ENDIF
 100  CALL rewind(casess)
   Iz(5) = nc
!
!     GET NUMBER OF LOAD VECTORS FOR EACH SUBSTRUCTURE FROM LODS
!
   CALL sfetch(Fss,lods,srd,rc)
   IF ( rc==1 ) THEN
      j = 1
      CALL sjump(j)
      DO i = 1 , ns
         CALL suread(Z(3*i+5),1,nwds,rc)
         CALL sjump(j)
      ENDDO
!
!     SOLN GROUP 0 COMPLETE.  WRITE IT ON SCR1
!
      j = 3
      CALL gopen(scr1,Z(Buf3),Wrtrew)
      CALL write(scr1,Z,3*ns+5,1)
!
!     COMPRESS SUBSTRUCTURE NAMES AT TOP OF OPEN CORE
!
      DO i = 1 , ns
         Iz(2*i-1) = Iz(3*i+3)
         Iz(2*i) = Iz(3*i+4)
      ENDDO
!
!     PREPARE TO LOOP OVER ALL SUBCASES
!
      icase = 2*ns + 1
      ilods = icase + 166
      IF ( ilods>Lcore ) GOTO 1200
      lodsin = 0
      nlods = ilods - 1
      file = casess
      DO i = 1 , nskip
         CALL fwdrec(*1100,casess)
      ENDDO
      noldc = 1
      CALL preloc(*200,Z(Buf1),geom4)
      CALL locate(*200,Z(Buf1),loadc,i)
      noldc = 0
   ELSE
      CALL smsg(rc-2,lods,Fss)
      GOTO 1400
   ENDIF
!
!     BEGIN SUBCASE LOOP.  FOR EACH SUBCASE, BUILD ONE GROUP OF SOLN
!
 200  DO isc = 1 , nc
      CALL fread(casess,Z(icase),166,0)
      nlds = 0
      IF ( Iz(icase+15)/=0 ) THEN
!
!     PROCESS SYMCOM OR SUBCOM SUBCASE
!
!     READ SYMSEQ OR SUBSEQ INTO OPEN CORE AT ISEQ
!
         lcc = Iz(icase+165)
         lskip = 167 - lcc
         CALL fread(casess,0,lskip,0)
         CALL fread(casess,lsem,1,0)
         GOTO 350
      ELSE
         file = casess
         CALL fwdrec(*1100,casess)
         file = geom4
!
!     PROCESS REGULAR SUBCASE.  IF LODS ITEM NOT IN CORE, GET IT.
!
         IF ( Iz(icase+3)==0 ) GOTO 300
         IF ( noldc==1 ) GOTO 300
         IF ( lodsin/=1 ) THEN
            CALL sfetch(Fss,lods,srd,rc)
            i = 1
            CALL sjump(i)
            i = ilods
            DO j = 1 , ns
               CALL suread(Z(i),1,nwds,rc)
               nlods = i + Iz(i)
               IF ( nlods>Lcore ) GOTO 1200
               CALL suread(Z(i+1),-1,nwds,rc)
               i = nlods + 1
            ENDDO
            lodsin = 1
         ENDIF
!
!     LODS ITEM IN CORE.  FIND MATCH ON LOADC CARD WITH LOAD SET ID
!     FROM CASECC
!
         jsoln = nlods + 2
         DO
            CALL read(*1100,*300,geom4,lod,2,0,nwds)
            IF ( lod(1)==Iz(icase+3) ) THEN
!
!     FOUND MATCH ON LOADC CARD
!
               sfac = clod(2)
               DO
!
!     LOOP OVER BASIC SUBSTRUCTURES ON THE LOADC CARD
!
                  CALL fread(geom4,lod,4,0)
                  IF ( lod(4)==-1 ) GOTO 250
                  IF ( jsoln+1>Lcore ) GOTO 1200
!
!     FIND BASIC SUBSTRUCTURE NUMBER BY MATCHING ITS NAME WITH THOSE
!     FROM EQSS.  THEN DETERMINE LOAD VECTOR NUMBER BY MATCHING THE
!     BASIC SUBSTRUCTURE LOAD SET ID WITH THOSE IN LODS DATA IN CORE.
!
                  DO i = 1 , ns
                     IF ( lod(1)==Iz(2*i-1) ) THEN
                        k = i
                        IF ( lod(2)==Iz(2*i) ) GOTO 202
                     ENDIF
                  ENDDO
                  WRITE (Nout,99001) Uwm , lod(1) , lod(2) , lod(3) , Fss
!
!     DIAGNOSTICS
!
99001             FORMAT (A25,' 6315, RCOVR MODULE IS UNABLE TO FIND SUBSTRUCTURE ',2A4,' AMONG THOSE ON EQSS.'/32X,'LOAD SET',I9,  &
                         &' FOR THAT SUBSTRUCTURE WILL BE IGNORED IN CREATING',/32X,'THE SOLN ITEM FOR FINAL SOLUTION STRUCTURE ',  &
                        & 2A4)
 202              n = 0
                  i = ilods
                  j = 1
                  DO WHILE ( j/=k )
                     n = n + Iz(i)
                     i = i + Iz(i) + 1
                     j = j + 1
                  ENDDO
                  j = Iz(i)
                  DO k = 1 , j
                     n = n + 1
                     IF ( Iz(i+k)==lod(3) ) GOTO 204
                  ENDDO
                  WRITE (Nout,99002) Uwm , lod(3) , lod(1) , lod(2) , Fss
99002             FORMAT (A25,' 6316, RCOVR MODULE IS UNABLE TO FIND LOAD SET',I9,' FOR SUBSTRUCTURE ',2A4,/32X,                    &
                         &'AMONG THOSE ON LODS.  ','IT WILL BE IGNORED IN CREATING THE SOLN ITEM FOR FINAL',/32X,                   &
                         &'SOLUTION STRUCTURE ',2A4)
!
!     BUILD SOLN GROUP IN OPEN CORE FOLLOWING LODS DATA
!
 204              Iz(jsoln) = n
                  Z(jsoln+1) = sfac*clod(4)
                  jsoln = jsoln + 2
                  nlds = nlds + 1
               ENDDO
            ELSE
               DO
                  CALL fread(geom4,lod,4,0)
                  IF ( lod(4)==-1 ) EXIT
               ENDDO
            ENDIF
         ENDDO
      ENDIF
 250  Iz(nlods+1) = nlds
      jsoln = nlods + 1
      GOTO 500
!
!     NO LOADS FOR THIS SUBCASE
!
 300  nlds = 0
      GOTO 250
 350  IF ( lsem+nlods<Lcore ) THEN
         iseq = nlods + 1
         CALL fread(casess,Z(iseq),lsem,1)
!
!     READ THE PREVIOUS LSEM GROUPS OF SOLN INTO OPEN CORE FOLLOWING SEQ
!
         jsoln = iseq + lsem
         k = jsoln + 1
         CALL close(scr1,Eofnrw)
         file = scr1
         CALL open(*1000,scr1,Z(Buf3),Rd)
         nrec = 1
         nlds = 0
         DO i = 1 , lsem
            DO
               DO j = 1 , nrec
                  CALL bckrec(scr1)
               ENDDO
               CALL fread(scr1,n,1,0)
               nrec = 2
               IF ( n>=0 ) THEN
                  IF ( k+2*n-1<Lcore ) THEN
                     CALL fread(scr1,Z(k),2*n,1)
!
!     SCALE LOAD FACTORS BY SYMSEQ OR SUBSEQ FACTORS
!
                     DO j = 1 , n
                        Z(k+2*j-1) = Z(iseq+lsem-i)*Z(k+2*j-1)
                     ENDDO
                     k = k + 2*n
                     nlds = nlds + n
                     EXIT
                  ELSE
                     IF ( lodsin==0 ) GOTO 1200
!
!     SHORT OF CORE.  REPOSITION CASESS, WIPE OUT LODS DATA, AND TRY
!     AGAIN
!
                     CALL bckrec(casess)
                     CALL fread(casess,0,-166,0)
                     GOTO 400
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         Iz(jsoln) = -nlds
!
!     COMBINATION GROUP COMPLETE.  REPOSITION SCR1
!
         file = scr1
         DO
            CALL fwdrec(*450,scr1)
         ENDDO
      ELSEIF ( lodsin==0 ) THEN
         GOTO 1200
      ENDIF
!
!     SHORT OF CORE.  WIPE OUT LODS DATA AND RE-USE SPACE
!
 400  lodsin = 0
      nlods = ilods - 1
      GOTO 350
 450  CALL skpfil(scr1,-1)
      CALL close(scr1,Norew)
      CALL open(*1000,scr1,Z(Buf3),Wrt)
!
!     GROUP COMPLETE IN CORE.  SORT ON LOAD VECTOR NUMBERS
!
 500  CALL sort(0,0,2,1,Z(jsoln+1),2*nlds)
!
!     WRITE GROUP ON SCR1 AND POSITION GEOM4 TO BEGINNING OF LOADC CARDS
!
      CALL write(scr1,Z(jsoln),2*nlds+1,1)
      IF ( noldc/=1 ) THEN
         CALL bckrec(geom4)
         CALL fread(geom4,0,-3,0)
      ENDIF
!
!     END OF LOOP OVER SUBCASES
!
   ENDDO
   CALL close(casess,Rew)
   CALL close(geom4,Rew)
   CALL close(scr1,Rew)
!
!     COPY SOLN FROM SCR1 TO SOF
!
   CALL gopen(scr1,Z(Buf1),Rdrew)
   rc = 3
   CALL sfetch(Fss,soln,swrt,rc)
 600  CALL read(*800,*700,scr1,Z,Lcore,1,nwds)
   GOTO 1200
 700  CALL suwrt(Z,nwds,eog)
   GOTO 600
 800  CALL close(scr1,Rew)
!
!     FINISH
!
   CALL suwrt(0,0,eoi)
 900  RETURN
!
 1000 n = 1
   GOTO 1300
 1100 n = 2
   GOTO 1300
 1200 n = 8
 1300 CALL mesage(n,file,name)
 1400 CALL sofcls
   Iopt = -1
   CALL close(casess,Rew)
   CALL close(geom4,Rew)
   CALL close(scr1,Rew)
!
END SUBROUTINE rcovss
