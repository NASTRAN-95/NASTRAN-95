
SUBROUTINE rcovds
   IMPLICIT NONE
   INTEGER Buf1 , Dry , Fss(2) , Icore , Iopt , Ireq , Iz(5) , Lbasic , Lcore , Loop , Lreq , Lui , Mrecvr , Neigv , Norew ,        &
         & Nosort , Nout , Rd , Rdrew , Rew , Rfno , Step , Sysbuf , Wrt , Wrtrew
   REAL Buf2 , Buf3 , Buf4 , Degra , Energy , Eofnrw , Pa , Pi , Pthres , Qa , Qthres , Radeg , Range(2) , Rss(2) , Sof1 , Sof2 ,   &
      & Sof3 , Twopi , Ua , Uimpro , Uinms(2,5) , Uthres , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /condas/ Pi , Twopi , Radeg , Degra
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   INTEGER casecc(2) , casess , dit , dload , dlt , eog , eoi , eqss , file , geom4 , i , i1 , i2 , idat , idat0 , idload , idlset ,&
         & ifac , ihit , ild , ildc , ildset , iload , iloadc , ilod , iold , iscale , isload , istep , isub , itab , itab0 ,       &
         & itabd , itype , ivec , j , k , l , ldload , ldlset , lldset , lload , lloadc , loadc(2) , lods , lsload , lstep , lsub , &
         & ltab , ltabd , lvec , n , name(2) , ndat , ns , nsload , nstep , nwds , rc , soln , srd , swrt , tabloc(13) , tolppf ,   &
         & trl(7) , upv
   REAL scale , tt
!
!     THIS ROUTINE GENERATES THE DYNAMIC SOLUTION ITEM FOR RIGID
!     FORMATS 8 AND 9
!
   EQUIVALENCE (Z(1),Iz(1)) , (iscale,scale)
   DATA name/4HRCOV , 4HDS  /
   DATA eqss , soln , lods/4HEQSS , 4HSOLN , 4HLODS/
   DATA srd , swrt , eog , eoi/1 , 2 , 2 , 3/
   DATA upv , dlt , casess , geom4 , tolppf , dit/106 , 108 , 101 , 102 , 111 , 107/
   DATA loadc/500 , 5/
   DATA tabloc/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   DATA casecc/4HCASE , 4HCC  /
!
!     CREATE SOLN FOR RIGID FORMAT 8 OR 9
!
!     GET NUMBER OF BASIC SUBSTRUCTURES (NS) FROM EQSS AND CREATE
!     GROUP 0 OF SOLN AT TOP OF OPEN CORE
!
   Lcore = Buf1 - 1
   CALL sfetch(Fss,eqss,srd,rc)
   IF ( rc==1 ) THEN
      CALL suread(Z,2,nwds,rc)
      CALL suread(ns,1,nwds,rc)
      IF ( Lcore<2*ns+5 ) GOTO 1000
      CALL suread(Z,1,nwds,rc)
      Iz(1) = Fss(1)
      Iz(2) = Fss(2)
      Iz(3) = Rfno
      Iz(4) = ns
!
!     GET THE BASIC SUBSTRUCTURE NAMES FROM EQSS
!
      DO i = 1 , ns
         CALL suread(Z(3*i+3),2,nwds,rc)
      ENDDO
!
!     GET THE NUMBER OF LOAD VECTORS FOR EACH SUBSTRUCTURE FORM LODS
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
!     GET THE NUMBER OF TIME OR FREQUENCY STEPS FROM UPV OR UPVC
!
         trl(1) = upv
         CALL rdtrl(trl)
         nstep = trl(2)
         IF ( Rfno==9 ) nstep = nstep/3
         Iz(5) = nstep
!
!     GET THE REQUESTED DLOAD SET FROM CASE CONTROL
!
         file = casess
         CALL gopen(casess,Z(Buf1),Rdrew)
         DO
            CALL fread(casess,trl,2,1)
            IF ( trl(1)==casecc(1) .AND. trl(2)==casecc(2) ) THEN
               CALL fread(casess,0,-12,0)
               CALL fread(casess,dload,1,0)
               CALL close(casess,Rew)
!
!     CHECK IF DLOAD SET POINTS TO A DLOAD COMBINATION CARD OR A
!     SIMPLE LOAD CARD BY LOOKING AT SET IDS IN HEADER RECORD OF DLT
!
               i = 3*ns + 6
               file = dlt
               CALL open(*800,dlt,Z(Buf1),Rdrew)
               CALL read(*900,*100,dlt,Z(i),Lcore-i,1,nwds)
               GOTO 1000
            ENDIF
         ENDDO
      ELSE
         CALL smsg(rc-2,lods,Fss)
         GOTO 1200
      ENDIF
   ELSE
      CALL smsg(rc-2,eqss,Fss)
      GOTO 700
   ENDIF
 100  idlset = i + 3
   ldlset = idlset + Iz(i+2) - 1
   ildset = ldlset + 1
   lldset = i + nwds - 1
   idload = lldset + 1
   IF ( idlset<=ldlset ) THEN
      DO i = idlset , ldlset
         IF ( Iz(i)==dload ) GOTO 200
      ENDDO
   ENDIF
!
!     NO DLOAD MATCH - MUST BE SIMPLE RLOAD OR TLOAD
!
   Z(idload) = 1.0
   Iz(idload+1) = dload
   ldload = idload + 1
   GOTO 300
 200  DO
!
!     DLOAD MATCH FOUND - READ DLOAD DATA FORM DLT RECORD 1
!
      CALL fread(dlt,trl,2,0)
      IF ( trl(1)==dload ) THEN
         i = idload
         iscale = trl(2)
         DO
            CALL fread(dlt,Z(i),2,0)
            IF ( Iz(i)==-1 ) THEN
               ldload = i - 1
               GOTO 300
            ELSE
               Z(i) = Z(i)*scale
               i = i + 2
               IF ( i>Lcore ) GOTO 1000
            ENDIF
         ENDDO
      ELSE
         DO
            CALL fread(dlt,trl,2,0)
            IF ( trl(1)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
!
!     READ THE RLOAD AND TLOAD DATA FORM DLT AND SAVE REQUESTED CARDS
!
 300  iload = ldload + 1
   l = iload
   IF ( idlset<=ldlset ) CALL fwdrec(*900,dlt)
   DO i = ildset , lldset
      DO j = idload , ldload , 2
         IF ( Iz(j+1)==Iz(i) ) GOTO 350
      ENDDO
      CALL fwdrec(*900,dlt)
      CYCLE
!
!     SAVE RLOAD DATA IF RIGID FORMAT 8
!     SAVE TLOAD DATA IF RIGID FORMAT 9
!
 350  CALL fread(dlt,itype,1,0)
      IF ( itype>2 .OR. Rfno/=8 ) THEN
         IF ( itype<3 .OR. Rfno/=9 ) THEN
            CALL fwdrec(*900,dlt)
            CYCLE
         ENDIF
      ENDIF
!
      Iz(l) = itype
      CALL fread(dlt,Iz(l+1),7,1)
      Iz(j+1) = -l
      l = l + 8
      IF ( l>Lcore ) GOTO 1000
   ENDDO
!
   lload = l - 1
   CALL close(dlt,Rew)
!
!     READ THE LOADC DATA FROM GEOM4 AND SAVE ANY THAT WAS REQUESTED
!     ON TLOAD OR RLOAD CARDS
!
!     NOTE - UNTIL A MODULE FRLG IS WRITTEN NO RLOAD CARD MAY REQUEST A
!            SCALAR LOAD
!
   nsload = 0
   iloadc = lload + 1
   lloadc = iloadc - 1
   isload = iloadc
   lsload = isload - 1
!
   IF ( Rfno==8 ) GOTO 600
!
   CALL preloc(*600,Z(Buf1),geom4)
   CALL locate(*600,Z(Buf1),loadc,i)
   iold = 0
   i1 = iloadc
   i2 = i1
 400  DO
      CALL read(*900,*500,geom4,trl(1),2,0,nwds)
      iscale = trl(2)
      IF ( iold==trl(1) ) EXIT
      ihit = 0
      DO i = iload , lload , 8
         IF ( trl(1)==Iz(i+1) ) THEN
            Iz(i+1) = -i1
            ihit = ihit + 1
         ENDIF
      ENDDO
      IF ( ihit>0 ) THEN
!
!     THIS LOADC DATA WAS REQUESTED - SAVE THE DATA AND A POINTER TO IT
!
         iold = trl(1)
         i1 = i2
         Iz(i1) = 0
         i2 = i1 + 1
         EXIT
      ELSE
         DO
            CALL fread(geom4,trl(1),4,0)
            IF ( trl(3)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
   DO
      CALL fread(geom4,Z(i2),4,0)
      IF ( Iz(i2+2)==-1 ) GOTO 400
      Iz(i1) = Iz(i1) + 1
      Z(i2+3) = Z(i2+3)*scale
      i2 = i2 + 4
      IF ( i2>Lcore ) GOTO 1000
   ENDDO
!
!     CONVERT LOADC LOAD SETS TO INTERNAL LOAD IDS BY USING THE LODS
!     ITEM
!
 500  lloadc = i2 - 1
   IF ( iloadc<=lloadc ) THEN
      CALL sfetch(Fss,lods,srd,rc)
      i = 1
      CALL sjump(i)
      ilod = 1
      idat0 = lloadc + 1
      idat = idat0 + 1
      ndat = Lcore - lloadc
      isub = 6
      lsub = 3*ns + 5
!
!     FOR EACH BASIC READ THE LODS DATA INTO CORE
!
      DO i = isub , lsub , 3
         CALL suread(Z(idat0),ndat,nwds,rc)
         IF ( rc/=2 ) GOTO 1000
         j = iloadc
         DO
            i1 = j + 1
            i2 = j + Iz(j)*4
            DO k = i1 , i2 , 4
               IF ( Iz(k)==Iz(i) .AND. Iz(k+1)==Iz(i+1) ) THEN
!
!     FOUND LOADC DATA FOR THIS BASIC - CONVERT LOAD SET ID
!
                  Iz(k) = 0
                  Iz(k+1) = 0
                  nwds = idat0 + nwds - 1
                  DO l = idat , nwds
                     IF ( Iz(l)==Iz(k+2) ) GOTO 505
                  ENDDO
                  WRITE (Nout,99001) Uwm , Iz(k+2) , Z(i) , Z(i+1) , Fss
99001             FORMAT (A25,' 6316, RCOVR MODULE IS UNABLE TO FIND LOAD SET ',I8,' FOR SUBSTRUCTURE ',2A4,/32X,                   &
                         &'AMONG THOSE ON LODS.  ','IT WILL BE IGNORED IN CREATING THE SOLN ITEM FOR FINAL',/32X,                   &
                         &'SOLUTION STRUCTURE ',2A4)
                  Iz(k+2) = -1
               ENDIF
               CYCLE
!
 505           Iz(k+2) = ilod + l - idat
!
            ENDDO
            j = i2 + 1
            IF ( j>=lloadc ) THEN
!
               ilod = ilod + Iz(idat0)
               EXIT
            ENDIF
         ENDDO
      ENDDO
!
!     CREATE A LIST OF INTERNAL LOAD VECTORS REQUESTED - ALSO CHECK IF
!     ANY BASIC NAMES WERE NOT FOUND
!
      isload = lloadc + 1
      lsload = isload - 1
      nsload = 0
      j = iloadc
      DO
         i1 = j + 1
         i2 = j + Iz(j)*4
         DO k = i1 , i2 , 4
            IF ( Iz(k)/=0 ) THEN
               WRITE (Nout,99002) Uwm , Z(k) , Z(k+1) , Fss , Iz(k+2) , Fss
!
!     DIAGNOSTICS
!
99002          FORMAT (A25,' 6315, RCOVR MODULE - SUBSTRUCTURE ',2A4,' IS NOT A',' COMPONENT OF ',2A4,/32X,'LOAD SET',I9,           &
                      &' FOR THAT ','SUBSTRUCTURE WILL BE IGNORED IN CREATING',/32X,'THE SOLN ITEM FOR FINAL SOLUTION STRUCTURE ',  &
                     & 2A4)
               Iz(k+2) = -1
            ELSEIF ( Iz(k+2)>=0 ) THEN
               IF ( nsload/=0 ) THEN
                  DO i = isload , lsload
                     IF ( Iz(i)==Iz(k+2) ) GOTO 520
                  ENDDO
               ENDIF
               nsload = nsload + 1
               lsload = lsload + 1
               IF ( lsload>Lcore ) GOTO 1000
               Iz(lsload) = Iz(k+2)
            ENDIF
 520     ENDDO
         j = i2 + 1
         IF ( j>=lloadc ) THEN
!
!     SORT LIST OF IDS
!
            CALL sort(0,0,1,1,Z(isload),nsload)
!
!     MAKE ONE MORE PASS THROUGH THE LOAC DATA CONVERTING THE
!     INTERNAL LOAD IDS TO A RELATIVE POSITION IN THE LOAD LIST
!     STARTING AT ISLOAD
!
            j = iloadc
            DO
               i1 = j + 1
               i2 = j + Iz(j)*4
               DO k = i1 , i2 , 4
                  IF ( Iz(k+2)>=0 ) THEN
                     DO l = isload , lsload
                        IF ( Iz(k+2)==Iz(l) ) GOTO 522
                     ENDDO
                  ENDIF
                  CYCLE
 522              Iz(k+2) = l - isload
               ENDDO
               j = i2 + 1
               IF ( j>=lloadc ) GOTO 600
            ENDDO
         ENDIF
      ENDDO
   ENDIF
!
!     OK - NOW WE CAN WRITE OUT GROUP 0 OF THE SOLN ITEM
!
 600  CALL close(geom4,Rew)
   rc = 3
   CALL sfetch(Fss,soln,swrt,rc)
   CALL suwrt(Z(1),3*ns+5,1)
   CALL suwrt(nsload,1,1)
   IF ( nsload>0 ) CALL suwrt(Z(isload),nsload,1)
   CALL suwrt(0,0,eog)
!
!     COPY THE FREQUENCY STEPS FROM PPF OR THE TIME STEPS FROM TOL
!     FOR GROUP 1 OF THE SOLN ITEM
!
   istep = isload
   lstep = istep + nstep - 1
   IF ( lstep>Lcore ) GOTO 1000
   file = tolppf
   CALL open(*800,tolppf,Z(Buf1),Rdrew)
   CALL fread(tolppf,trl,2,0)
   CALL fread(tolppf,Z(istep),nstep,0)
   CALL close(tolppf,Rew)
!
   CALL suwrt(Z(istep),nstep,eog)
!
!     IF ANY SCALAR LOADS EXIST CALCULATE THE SCALE FACTORS FOR EACH
!     LOAD AND WRITE THEM TO THE SOF - 1 GROUP PER TIME OR FREQUENCY
!     STEP
!
   IF ( nsload==0 ) THEN
!
!     FINISHED
!
      CALL suwrt(0,0,eoi)
   ELSE
      ivec = lstep + 1
      lvec = ivec + nsload - 1
      IF ( lvec>Lcore ) GOTO 1000
!
!     CALL PRETAB TO READ IN THE REQUIRED TABLE DATA - FIRST MAKE A
!     LIST OF REQUESTED TABLE IDS
!
      itab0 = lvec + 1
      Iz(itab0) = 0
      itab = itab0 + 1
      ltab = itab - 1
      DO j = iload , lload , 8
         IF ( Iz(j+1)<0 ) THEN
            itype = Iz(j)
            IF ( itype==3 ) THEN
               i1 = j + 2
               i2 = j + 2
            ELSEIF ( itype==4 ) THEN
               CYCLE
            ELSE
               i1 = j + 2
               i2 = j + 3
            ENDIF
            DO k = i1 , i2
               IF ( Iz(k)/=0 ) THEN
                  IF ( ltab>=itab ) THEN
                     DO l = itab , ltab
                        IF ( Iz(l)==Iz(k) ) GOTO 610
                     ENDDO
                  ENDIF
                  ltab = ltab + 1
                  IF ( ltab>Lcore ) GOTO 1000
                  Iz(ltab) = Iz(k)
                  Iz(itab0) = Iz(itab0) + 1
               ENDIF
 610        ENDDO
         ENDIF
      ENDDO
!
      IF ( Iz(itab0)/=0 ) THEN
         itabd = ltab + 1
         CALL pretab(dit,Z(itabd),Iz(itabd),Z(Buf1),Lcore-itabd,ltabd,Z(itab0),tabloc)
         ltabd = itabd + ltabd - 1
      ENDIF
!
!     LOOP OVER EACH TIME OR FREQUENCY STEP
!
      DO i = istep , lstep
!
!     ZERO A VECTOR IN CORE FOR THE SCALE FACTORS
!
         DO j = ivec , lvec
            Iz(j) = 0
         ENDDO
!
!     PASS THROUGH THE DLOAD DATA
!
         DO j = idload , ldload , 2
            IF ( Iz(j+1)<0 ) THEN
!
!     PROCESS THE TLOAD OR RLOAD DATA THIS DLOAD ENTRY POINTS TO
!
               ild = -Iz(j+1)
               IF ( Iz(ild+1)<0 ) THEN
                  itype = Iz(ild)
                  ildc = -Iz(ild+1)
!
!     CALCULATE THE SCALE FACTOR FOR THE CARD FOR THIS TIME OR FREQUENCY
!     STEP
!
                  IF ( itype==2 ) THEN
!
!     RLOAD2 DATA
!
                     scale = 0.0
                  ELSEIF ( itype==3 ) THEN
!
!     TLOAD1 DATA
!
                     CALL tab(Iz(ild+2),Z(i),scale)
                  ELSEIF ( itype==4 ) THEN
!
!     TLOAD2 DATA
!
                     scale = 0.0
                     tt = Z(i) - Z(ild+2)
                     IF ( tt==0.0 ) THEN
                        IF ( Z(ild+7)==0.0 ) scale = cos(Z(ild+5))
                     ELSEIF ( tt>=0.0 .AND. tt<=Z(ild+3) ) THEN
                        scale = tt**Z(ild+7)*exp(Z(ild+6)*tt)*cos(Twopi*Z(ild+4)*tt+Z(ild+5)*Degra)
                     ENDIF
                  ELSE
!
!     RLOAD1 DATA
!
                     scale = 0.0
                  ENDIF
!
!     NOW APPLY THIS SCALE FACTOR TO EACH LOADC ENTRY.
!     TOTAL SCALE FACTOR = T(R)LOAD FACTOR*DLOAD FACTOR*LOADC FACTOR
!
                  IF ( scale/=0.0 ) THEN
                     i1 = ildc + 1
                     i2 = ildc + Iz(ildc)*4
                     DO k = i1 , i2 , 4
                        IF ( Iz(k+2)>=0 ) THEN
                           ifac = ivec + Iz(k+2)
                           Z(ifac) = Z(ifac) + scale*Z(j)*Z(k+3)
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
!
         ENDDO
!
!     WRITE OUT THESE FACTORS TO THE NEXT GROUP OF THE SOF
!
         CALL suwrt(Z(ivec),nsload,eog)
      ENDDO
      CALL suwrt(0,0,eoi)
   ENDIF
 700  CALL sofcls
   RETURN
 800  n = 1
   GOTO 1100
 900  n = 2
   GOTO 1100
 1000 n = 8
 1100 CALL mesage(n,file,name)
 1200 CALL sofcls
   Iopt = -1
   CALL close(casess,Rew)
   CALL close(dlt,Rew)
   CALL close(geom4,Rew)
   CALL close(tolppf,Rew)
   CALL close(dit,Rew)
END SUBROUTINE rcovds
