!*==rcovds.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovds
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_NAMES
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: casecc , loadc , name
   INTEGER , SAVE :: casess , dit , dlt , eog , eoi , eqss , geom4 , lods , soln , srd , swrt , tolppf , upv
   INTEGER :: dload , file , i , i1 , i2 , idat , idat0 , idload , idlset , ifac , ihit , ild , ildc , ildset , iload , iloadc ,    &
            & ilod , iold , iscale , isload , istep , isub , itab , itab0 , itabd , itype , ivec , j , k , l , ldload , ldlset ,    &
            & lldset , lload , lloadc , lsload , lstep , lsub , ltab , ltabd , lvec , n , ndat , ns , nsload , nstep , nwds , rc
   INTEGER , DIMENSION(5) :: iz
   REAL :: scale , tt
   INTEGER , DIMENSION(13) , SAVE :: tabloc
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , fread , fwdrec , gopen , locate , mesage , open , preloc , pretab , rdtrl , read , sfetch , sjump , smsg ,      &
          & sofcls , sort , suread , suwrt , tab
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
!
!     THIS ROUTINE GENERATES THE DYNAMIC SOLUTION ITEM FOR RIGID
!     FORMATS 8 AND 9
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (iscale,scale)
   DATA name/4HRCOV , 4HDS  /
   DATA eqss , soln , lods/4HEQSS , 4HSOLN , 4HLODS/
   DATA srd , swrt , eog , eoi/1 , 2 , 2 , 3/
   DATA upv , dlt , casess , geom4 , tolppf , dit/106 , 108 , 101 , 102 , 111 , 107/
   DATA loadc/500 , 5/
   DATA tabloc/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   DATA casecc/4HCASE , 4HCC  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
            IF ( Lcore<2*ns+5 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(Z,1,nwds,rc)
            iz(1) = Fss(1)
            iz(2) = Fss(2)
            iz(3) = Rfno
            iz(4) = ns
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
               iz(5) = nstep
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
                     CALL open(*80,dlt,Z(Buf1),Rdrew)
                     CALL read(*100,*20,dlt,Z(i),Lcore-i,1,nwds)
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSE
               CALL smsg(rc-2,lods,Fss)
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            CALL smsg(rc-2,eqss,Fss)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      idlset = i + 3
         ldlset = idlset + iz(i+2) - 1
         ildset = ldlset + 1
         lldset = i + nwds - 1
         idload = lldset + 1
         IF ( idlset<=ldlset ) THEN
            DO i = idlset , ldlset
               IF ( iz(i)==dload ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     NO DLOAD MATCH - MUST BE SIMPLE RLOAD OR TLOAD
!
         Z(idload) = 1.0
         iz(idload+1) = dload
         ldload = idload + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         SPAG_Loop_1_1: DO
!
!     DLOAD MATCH FOUND - READ DLOAD DATA FORM DLT RECORD 1
!
            CALL fread(dlt,trl,2,0)
            IF ( trl(1)==dload ) THEN
               i = idload
               iscale = trl(2)
               DO
                  CALL fread(dlt,Z(i),2,0)
                  IF ( iz(i)==-1 ) THEN
                     ldload = i - 1
                     EXIT SPAG_Loop_1_1
                  ELSE
                     Z(i) = Z(i)*scale
                     i = i + 2
                     IF ( i>Lcore ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               SPAG_Loop_2_2: DO
                  CALL fread(dlt,trl,2,0)
                  IF ( trl(1)==-1 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ THE RLOAD AND TLOAD DATA FORM DLT AND SAVE REQUESTED CARDS
!
         iload = ldload + 1
         l = iload
         IF ( idlset<=ldlset ) CALL fwdrec(*100,dlt)
         DO i = ildset , lldset
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  DO j = idload , ldload , 2
                     IF ( iz(j+1)==iz(i) ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  CALL fwdrec(*100,dlt)
                  CYCLE
               CASE (2)
!
!     SAVE RLOAD DATA IF RIGID FORMAT 8
!     SAVE TLOAD DATA IF RIGID FORMAT 9
!
                  CALL fread(dlt,itype,1,0)
                  IF ( itype>2 .OR. Rfno/=8 ) THEN
                     IF ( itype<3 .OR. Rfno/=9 ) THEN
                        CALL fwdrec(*100,dlt)
                        CYCLE
                     ENDIF
                  ENDIF
!
                  iz(l) = itype
                  CALL fread(dlt,iz(l+1),7,1)
                  iz(j+1) = -l
                  l = l + 8
                  IF ( l>Lcore ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
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
         IF ( Rfno==8 ) GOTO 60
!
         CALL preloc(*60,Z(Buf1),geom4)
         CALL locate(*60,Z(Buf1),loadc,i)
         iold = 0
         i1 = iloadc
         i2 = i1
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_3: DO
            CALL read(*100,*40,geom4,trl(1),2,0,nwds)
            iscale = trl(2)
            IF ( iold==trl(1) ) EXIT SPAG_Loop_1_3
            ihit = 0
            DO i = iload , lload , 8
               IF ( trl(1)==iz(i+1) ) THEN
                  iz(i+1) = -i1
                  ihit = ihit + 1
               ENDIF
            ENDDO
            IF ( ihit>0 ) THEN
!
!     THIS LOADC DATA WAS REQUESTED - SAVE THE DATA AND A POINTER TO IT
!
               iold = trl(1)
               i1 = i2
               iz(i1) = 0
               i2 = i1 + 1
               EXIT SPAG_Loop_1_3
            ELSE
               SPAG_Loop_2_4: DO
                  CALL fread(geom4,trl(1),4,0)
                  IF ( trl(3)==-1 ) EXIT SPAG_Loop_2_4
               ENDDO SPAG_Loop_2_4
            ENDIF
         ENDDO SPAG_Loop_1_3
         DO
            CALL fread(geom4,Z(i2),4,0)
            IF ( iz(i2+2)==-1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iz(i1) = iz(i1) + 1
            Z(i2+3) = Z(i2+3)*scale
            i2 = i2 + 4
            IF ( i2>Lcore ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     CONVERT LOADC LOAD SETS TO INTERNAL LOAD IDS BY USING THE LODS
!     ITEM
!
 40      lloadc = i2 - 1
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
               IF ( rc/=2 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               j = iloadc
               SPAG_Loop_2_5: DO
                  i1 = j + 1
                  i2 = j + iz(j)*4
                  DO k = i1 , i2 , 4
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           IF ( iz(k)==iz(i) .AND. iz(k+1)==iz(i+1) ) THEN
!
!     FOUND LOADC DATA FOR THIS BASIC - CONVERT LOAD SET ID
!
                              iz(k) = 0
                              iz(k+1) = 0
                              nwds = idat0 + nwds - 1
                              DO l = idat , nwds
                                 IF ( iz(l)==iz(k+2) ) THEN
                                    spag_nextblock_3 = 2
                                    CYCLE SPAG_DispatchLoop_3
                                 ENDIF
                              ENDDO
                              WRITE (Nout,99001) Uwm , iz(k+2) , Z(i) , Z(i+1) , Fss
99001                         FORMAT (A25,' 6316, RCOVR MODULE IS UNABLE TO FIND LOAD SET ',I8,' FOR SUBSTRUCTURE ',2A4,/32X,       &
                                     &'AMONG THOSE ON LODS.  ','IT WILL BE IGNORED IN CREATING THE SOLN ITEM FOR FINAL',/32X,       &
                                     &'SOLUTION STRUCTURE ',2A4)
                              iz(k+2) = -1
                           ENDIF
                           CYCLE
                        CASE (2)
!
                           iz(k+2) = ilod + l - idat
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
!
                  ENDDO
                  j = i2 + 1
                  IF ( j>=lloadc ) THEN
!
                     ilod = ilod + iz(idat0)
                     EXIT SPAG_Loop_2_5
                  ENDIF
               ENDDO SPAG_Loop_2_5
            ENDDO
!
!     CREATE A LIST OF INTERNAL LOAD VECTORS REQUESTED - ALSO CHECK IF
!     ANY BASIC NAMES WERE NOT FOUND
!
            isload = lloadc + 1
            lsload = isload - 1
            nsload = 0
            j = iloadc
            SPAG_Loop_1_7: DO
               i1 = j + 1
               i2 = j + iz(j)*4
               SPAG_Loop_2_6: DO k = i1 , i2 , 4
                  IF ( iz(k)/=0 ) THEN
                     WRITE (Nout,99002) Uwm , Z(k) , Z(k+1) , Fss , iz(k+2) , Fss
!
!     DIAGNOSTICS
!
99002                FORMAT (A25,' 6315, RCOVR MODULE - SUBSTRUCTURE ',2A4,' IS NOT A',' COMPONENT OF ',2A4,/32X,'LOAD SET',I9,     &
                            &' FOR THAT ','SUBSTRUCTURE WILL BE IGNORED IN CREATING',/32X,                                          &
                            &'THE SOLN ITEM FOR FINAL SOLUTION STRUCTURE ',2A4)
                     iz(k+2) = -1
                  ELSEIF ( iz(k+2)>=0 ) THEN
                     IF ( nsload/=0 ) THEN
                        DO i = isload , lsload
                           IF ( iz(i)==iz(k+2) ) CYCLE SPAG_Loop_2_6
                        ENDDO
                     ENDIF
                     nsload = nsload + 1
                     lsload = lsload + 1
                     IF ( lsload>Lcore ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     iz(lsload) = iz(k+2)
                  ENDIF
               ENDDO SPAG_Loop_2_6
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
                     i2 = j + iz(j)*4
                     DO k = i1 , i2 , 4
                        spag_nextblock_4 = 1
                        SPAG_DispatchLoop_4: DO
                           SELECT CASE (spag_nextblock_4)
                           CASE (1)
                              IF ( iz(k+2)>=0 ) THEN
                                 DO l = isload , lsload
                                    IF ( iz(k+2)==iz(l) ) THEN
                                       spag_nextblock_4 = 2
                                       CYCLE SPAG_DispatchLoop_4
                                    ENDIF
                                 ENDDO
                              ENDIF
                              CYCLE
                           CASE (2)
                              iz(k+2) = l - isload
                              EXIT SPAG_DispatchLoop_4
                           END SELECT
                        ENDDO SPAG_DispatchLoop_4
                     ENDDO
                     j = i2 + 1
                     IF ( j>=lloadc ) EXIT SPAG_Loop_1_7
                  ENDDO
               ENDIF
            ENDDO SPAG_Loop_1_7
         ENDIF
!
!     OK - NOW WE CAN WRITE OUT GROUP 0 OF THE SOLN ITEM
!
 60      CALL close(geom4,Rew)
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
         IF ( lstep>Lcore ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = tolppf
         CALL open(*80,tolppf,Z(Buf1),Rdrew)
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
            IF ( lvec>Lcore ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CALL PRETAB TO READ IN THE REQUIRED TABLE DATA - FIRST MAKE A
!     LIST OF REQUESTED TABLE IDS
!
            itab0 = lvec + 1
            iz(itab0) = 0
            itab = itab0 + 1
            ltab = itab - 1
            DO j = iload , lload , 8
               IF ( iz(j+1)<0 ) THEN
                  itype = iz(j)
                  IF ( itype==3 ) THEN
                     i1 = j + 2
                     i2 = j + 2
                  ELSEIF ( itype==4 ) THEN
                     CYCLE
                  ELSE
                     i1 = j + 2
                     i2 = j + 3
                  ENDIF
                  SPAG_Loop_2_8: DO k = i1 , i2
                     IF ( iz(k)/=0 ) THEN
                        IF ( ltab>=itab ) THEN
                           DO l = itab , ltab
                              IF ( iz(l)==iz(k) ) CYCLE SPAG_Loop_2_8
                           ENDDO
                        ENDIF
                        ltab = ltab + 1
                        IF ( ltab>Lcore ) THEN
                           spag_nextblock_1 = 6
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        iz(ltab) = iz(k)
                        iz(itab0) = iz(itab0) + 1
                     ENDIF
                  ENDDO SPAG_Loop_2_8
               ENDIF
            ENDDO
!
            IF ( iz(itab0)/=0 ) THEN
               itabd = ltab + 1
               CALL pretab(dit,Z(itabd),iz(itabd),Z(Buf1),Lcore-itabd,ltabd,Z(itab0),tabloc)
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
                  iz(j) = 0
               ENDDO
!
!     PASS THROUGH THE DLOAD DATA
!
               DO j = idload , ldload , 2
                  IF ( iz(j+1)<0 ) THEN
!
!     PROCESS THE TLOAD OR RLOAD DATA THIS DLOAD ENTRY POINTS TO
!
                     ild = -iz(j+1)
                     IF ( iz(ild+1)<0 ) THEN
                        itype = iz(ild)
                        ildc = -iz(ild+1)
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
                           CALL tab(iz(ild+2),Z(i),scale)
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
                           i2 = ildc + iz(ildc)*4
                           DO k = i1 , i2 , 4
                              IF ( iz(k+2)>=0 ) THEN
                                 ifac = ivec + iz(k+2)
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
         spag_nextblock_1 = 5
      CASE (5)
         CALL sofcls
         RETURN
 80      n = 1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     n = 2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         n = 8
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(n,file,name)
         spag_nextblock_1 = 8
      CASE (8)
         CALL sofcls
         Iopt = -1
         CALL close(casess,Rew)
         CALL close(dlt,Rew)
         CALL close(geom4,Rew)
         CALL close(tolppf,Rew)
         CALL close(dit,Rew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rcovds
