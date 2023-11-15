
SUBROUTINE rcovls(Lastss)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Buf1 , Buf2 , Buf3 , Buf4 , Energy , Pa , Pthres , Qa , Qthres , Range(2) , Rss(2) , Sof1 , Sof2 , Sof3 , Sysbuf , Uimpro , &
      & Uthres , Z(1)
   INTEGER Dry , Fss(2) , Icore , Iopt , Ireq , Iz(3) , Lbasic , Lcore , Loop , Lreq , Lui , Mrecvr , Neigv , Nosort , Nout , Rfno ,&
         & Step , Ua , Uinms(2,5)
   CHARACTER*23 Ufm
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Lastss(2)
!
! Local variable declarations
!
   INTEGER eog , eoi , eqss , i , ig0 , igs , incr , isol , istep , j , jgs , k , kgs , l1 , l2 , lsl , lvn , n , name(2) , nc ,    &
         & nl , ns , nsl , nsll , nss , nwds , rc , soln , srd , swrt
!
! End of declarations
!
!
!     THIS ROUTINE CREATES THE SOLN ITEM FOR A LOWER LEVEL SUBSTRUCTURE,
!     LASTSS, BY EDITING THAT OF THE SOLUTION SUBSTRUCTURE FSS.
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HRCOV , 4HLS  /
   DATA eqss , soln/4HEQSS , 4HSOLN/
   DATA srd , swrt/1 , 2/
   DATA eog , eoi/2 , 3/
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
      CALL suread(Iz(Icore),-1,nwds,rc)
      isol = Iz(Icore+2)
      IF ( isol/=Rfno ) THEN
!
!     ERROR PROCESSING
!
         WRITE (Nout,99001) Ufm , isol , Rfno
!
99001    FORMAT (A23,' 6369.  SOLN ITEM HAS INCORRECT RIGID FORMAT NUMBER',/31X,'SOLUTION RIGID FORMAT WAS',I5,                     &
                &' AND CURRENT NASTRAN EXECUTION RIGID FORMAT IS',I5)
         GOTO 200
      ELSEIF ( Iz(Icore+3)>0 ) THEN
         IF ( Lcore<Icore+7*Iz(Icore+3)+3 ) GOTO 100
         CALL suread(Iz(Icore+4),-1,nwds,rc)
         rc = 3
         CALL sfetch(Lastss,soln,swrt,rc)
         CALL suwrt(Z(Icore),4,eog)
         CALL suwrt(Z(Icore+4),7*Iz(Icore+3),eog)
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
      nss = Iz(Icore+2)
      IF ( Lcore<=Icore+2*nss-1 ) GOTO 100
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
         isol = Iz(ig0+2)
         IF ( isol/=Rfno ) THEN
            WRITE (Nout,99001) Ufm , isol , Rfno
            GOTO 200
         ELSE
            ns = Iz(ig0+3)
            nc = Iz(ig0+4)
            IF ( ig0+4+4*ns>Lcore ) GOTO 100
            DO i = 1 , ns
               CALL suread(Z(ig0+1+4*i),3,nwds,rc)
               Iz(ig0+4*i+4) = -65535
               DO j = 1 , nss
                  IF ( Iz(ig0+4*i+1)==Iz(Icore+2*j-2) ) THEN
                     IF ( Iz(ig0+4*i+2)==Iz(Icore+2*j-1) ) THEN
                        Iz(ig0+4*i+4) = Iz(ig0+4*i+3)
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
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
               nsl = Iz(igs)
               lsl = nsl*incr
               nsll = 0
               IF ( nsl/=0 ) THEN
                  IF ( igs+nsl>Lcore ) GOTO 100
                  CALL suread(Z(igs+1),nsl,nwds,rc)
!
!     FLAG THOSE STATIC LOAD IDS THAT ARE NOT IN THE LOWER LEVEL
!     SUBSTRUCTURE AND RENUMBER THOSE THAT ARE LEFT
!
                  DO j = 1 , nsl
                     lvn = Iz(igs+j)
                     l1 = 0
                     l2 = 0
                     DO k = 1 , ns
                        IF ( lvn>l2+Iz(ig0+4*k+3) ) THEN
                           IF ( Iz(ig0+4*k+4)<0 ) l1 = l1 + Iz(ig0+4*k+3)
                           l2 = l2 + Iz(ig0+4*k+3)
                        ELSE
                           IF ( Iz(ig0+4*k+4)<0 ) EXIT
                           lvn = lvn - l1
                           nsll = nsll + 1
                           GOTO 2
                        ENDIF
                     ENDDO
                     lvn = -65535
 2                   Iz(igs+j) = lvn
                  ENDDO
               ENDIF
!
!     COPY THE FREQUENCY OR TIME STEP RECORD INTO CORE
!
               i = 1
               CALL sjump(i)
               istep = igs + nsl + 1
               IF ( istep+nc>Lcore ) GOTO 100
               CALL suread(Iz(istep),-1,nwds,rc)
!
!     COPY IN ALL LOAD FACTOR DATA
!
               IF ( nsll/=0 ) THEN
                  jgs = istep + nc
                  DO i = 1 , nc
                     IF ( jgs+lsl>Lcore ) GOTO 100
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
               Iz(ig0+3) = nss
               CALL suwrt(Z(ig0),5,1)
               DO i = 1 , ns
                  IF ( Iz(ig0+4*i+4)>=0 ) THEN
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
                  n = iabs(Iz(jgs))
                  IF ( jgs+n*2>Lcore ) GOTO 100
                  CALL suread(Z(jgs+1),-1,nwds,rc)
                  nl = 0
                  IF ( n/=0 ) THEN
                     DO j = 1 , n
                        lvn = Iz(jgs+2*j-1)
!
!     FIND SUBSTRUCTURE WHERE LVN IS APPLIED FOR FSS SOLN ITEM.
!
                        l1 = 0
                        l2 = 0
                        DO k = 1 , ns
                           IF ( lvn>l2+Iz(ig0+4*k+3) ) THEN
                              IF ( Iz(ig0+4*k+4)<0 ) l1 = l1 + Iz(ig0+4*k+3)
                              l2 = l2 + Iz(ig0+4*k+3)
                           ELSE
                              IF ( Iz(ig0+4*k+4)<0 ) EXIT
                              lvn = lvn - l1
                              nl = nl + 1
                              GOTO 4
                           ENDIF
                        ENDDO
                        lvn = -65535
 4                      Iz(jgs+2*j-1) = lvn
                     ENDDO
                     IF ( Iz(jgs)<0 ) nl = -nl
                     Iz(jgs) = nl
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
               Iz(ig0+3) = nss
               rc = 3
               CALL sfetch(Lastss,soln,swrt,rc)
               CALL suwrt(Z(ig0),5,1)
               DO i = 1 , ns
                  IF ( Iz(ig0+4*i+4)>=0 ) THEN
                     CALL suwrt(Z(ig0+4*i+1),2,1)
                     CALL suwrt(Z(ig0+4*i+4),1,1)
                  ENDIF
               ENDDO
               CALL suwrt(0,0,eog)
               jgs = igs
               DO i = 1 , nc
                  k = 0
                  nl = Iz(jgs)
                  jgs = jgs + 1
                  CALL suwrt(nl,1,1)
                  IF ( nl==0 ) GOTO 6
                  nl = iabs(nl)
                  DO
                     IF ( Iz(jgs)/=-65535 ) THEN
                        CALL suwrt(Iz(jgs),2,1)
                        k = k + 1
                     ENDIF
                     jgs = jgs + 2
                     IF ( k>=nl ) EXIT
                  ENDDO
 6                DO WHILE ( Iz(jgs)==-65535 )
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
 100  n = 8
   CALL mesage(n,0,name)
 200  Iopt = -1
   CALL sofcls
   RETURN
END SUBROUTINE rcovls
