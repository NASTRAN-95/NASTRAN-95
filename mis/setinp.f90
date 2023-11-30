
SUBROUTINE setinp
!
   IMPLICIT NONE
   INTEGER Bufsiz , El(1) , Gp(1) , Incr , Intr , Ipcdb , Last , Merr , Mset , Msetid , Ne(1) , Nin , Nogo , Nout , Nsets , Nsk(81) &
         & , Ntypes , Pcdb , Plot , Skp11 , Skp12(8) , Skp2(9) , Skp3(7) , X(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Skp11 , Nsets , Skp12 , Pcdb , Skp2 , Merr , Plot , Msetid , Skp3 , Mset , Ipcdb
   COMMON /gpta1 / Ntypes , Last , Incr , Ne
   COMMON /system/ Bufsiz , Nout , Nogo , Nin , Nsk , Intr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ X
   INTEGER all , allon , awrd(2) , b1 , b2 , b3 , b4 , blnk , card(65) , elem , elgp , encard , eor , exce , excl , go , grid , i , &
         & icont , idx , ie , ilxx , incl , inprew , iorew , iwrd , j , jc , mode , n , name(2) , nelx , ngpx , norew , nt , nw ,   &
         & org , outrew , pcard(20) , pocard(200) , poin , porg , rew , set , setid , stop , thru , to , tra , typ(100) , word , xx
   INTEGER complf , korsz , rshift
   DOUBLE PRECISION dwrd
   REAL fwrd
   EXTERNAL complf , rshift
   EQUIVALENCE (X(1),El(1),Gp(1))
   EQUIVALENCE (word,awrd(1),iwrd,fwrd,dwrd)
   DATA inprew , outrew , rew , norew , eor/0 , 1 , 1 , 3 , 1000000/
   DATA blnk , stop , go , name/4H     , 4HSTOP , 4HGO   , 4H SET , 3HINP/
   DATA set , incl , excl , elem , grid , poin , exce , to/3HSET , 4HINCL , 4HEXCL , 4HELEM , 4HGRID , 4HPOIN , 4HEXCE , 2HTO/
   DATA thru , all , ilxx/4HTHRU , 3HALL , 2HXX/
!
   CALL delset
   b1 = korsz(X) - 5*Bufsiz + 1
   b2 = b1 + Bufsiz
   b3 = b2 + Bufsiz
   b4 = b3 + Bufsiz
   Nogo = 0
   org = 0
   porg = -1
   allon = complf(0)
   pocard(200) = rshift(allon,1)
   encard = pocard(200)
!
!     OPEN ALL NECESSARY FILES
!
   iorew = inprew
   IF ( Intr>0 ) THEN
      Pcdb = Ipcdb
      iorew = outrew
   ENDIF
   CALL open(*2500,Pcdb,X(b1),iorew)
   IF ( Intr<=0 ) GOTO 300
!
   WRITE (Nout,99001)
!
99001 FORMAT (' ENTER PLOT DEFINITION OR ''GO'' IF DONE.')
 100  DO
      DO j = 1 , 20
         pcard(j) = blnk
      ENDDO
      DO j = 1 , 199
         pocard(j) = blnk
      ENDDO
      CALL xread(*200,pcard)
      IF ( pcard(1)==stop ) THEN
         Nogo = 1
         RETURN
      ELSEIF ( pcard(1)==go ) THEN
         CALL close(Pcdb,rew)
         IF ( Intr>10 ) Nout = 1
         CALL open(*2500,Pcdb,X(b1),inprew)
         GOTO 300
      ELSE
         CALL xrcard(pocard,199,pcard)
         CALL ifp1pc(1,icont,pocard,org,porg)
         IF ( Nogo==0 ) THEN
            WRITE (1,99002) pcard
99002       FORMAT (20A4)
            ie = 1
            DO j = 1 , 199
               IF ( pocard(j)==0 ) THEN
                  DO jc = 1 , 5
                     IF ( pocard(j+jc)/=blnk ) GOTO 105
                  ENDDO
                  nw = j
                  GOTO 110
               ENDIF
 105           IF ( pocard(j)==encard ) THEN
                  nw = j
                  GOTO 110
               ENDIF
            ENDDO
            nw = 80
 110        CALL write(Pcdb,pocard,nw,ie)
         ELSE
            Nogo = 0
            EXIT
         ENDIF
      ENDIF
   ENDDO
 200  WRITE (Nout,99003)
99003 FORMAT (' BAD CARD TRY AGIAN')
   GOTO 100
 300  IF ( Intr<=0 ) CALL fread(Pcdb,0,-2,1)
   CALL gopen(Plot,X(b2),outrew)
   CALL gopen(Mset,X(b3),outrew)
   CALL gopen(Msetid,X(b4),outrew)
   CALL rdmodx(Pcdb,mode,word)
 400  DO
!
!     READ MODE FLAG.  SHOULD BE ALPHABETIC
!
      CALL read(*2400,*2400,Pcdb,mode,1,0,i)
      IF ( mode<0 ) THEN
         i = 1
         IF ( mode==-4 ) i = 2
         CALL fread(Pcdb,0,-i,0)
      ELSEIF ( mode/=0 ) THEN
         IF ( mode<eor ) THEN
            mode = mode + 1
            CALL rdword(mode,word)
            CALL rdword(mode,word)
            IF ( word/=set .OR. mode/=0 ) EXIT
!
!     THIS CARD DEFINES A NEW SET
!
            ASSIGN 800 TO tra
            CALL rdmode(*2600,*500,*400,mode,word)
            GOTO 800
         ELSE
            CALL fread(Pcdb,0,0,1)
         ENDIF
      ENDIF
   ENDDO
!
!     THIS CARD IS A PLOT CONTROL CARD
!
 500  CALL bckrec(Pcdb)
 600  CALL read(*2400,*700,Pcdb,card,65,1,i)
   WRITE (Nout,99004)
99004 FORMAT ('  ARRAY CARD OF 65 TOO SAMLL')
   CALL mesage(-37,0,name)
 700  CALL write(Plot,card,i,1)
   IF ( card(i)==0 ) GOTO 600
   GOTO 400
 800  setid = iwrd
   nelx = 0
   ngpx = b1
   nt = 0
   xx = 1
   elgp = 0
!
   IF ( mode<=0 ) CALL rdmode(*1600,*900,*2300,mode,word)
 900  CALL rdword(mode,word)
!
!     CHECK FOR AN -INCLUDE- OR -EXCLUDE- CARD
!
 1000 IF ( word/=incl .AND. word/=excl .AND. word/=exce ) GOTO 1300
 1100 IF ( word==incl ) xx = 1
   IF ( word==excl ) xx = -1
   IF ( word==exce ) xx = -xx
   IF ( mode==0 ) CALL rdmode(*1600,*1200,*2300,mode,word)
 1200 CALL rdword(mode,word)
 1300 IF ( word/=grid ) THEN
      IF ( word/=elem ) GOTO 2100
!
!     ELEMENTS ARE TO BE INCLUDED OR EXCLUDED (BY ID OR TYPE)
!
      elgp = 0
      IF ( mode<0 ) THEN
      ELSEIF ( mode==0 ) THEN
!
!     A LIST OF ELEMENT OR GRID POINT ID-S CAN BE EXPLICITLY LISTED, OR
!     PREFERABLY A RANGE CAN BE SPECIFIED (SEPARATED BY THE WORD -TO-
!     OR -THRU-)
!
         CALL rdmode(*1600,*900,*2300,mode,word)
      ELSE
         GOTO 900
      ENDIF
      GOTO 1600
   ENDIF
!
!     A LIST OF GRID POINTS IS TO BE INCLUDED OR EXCLUDED (PERTAIN ONLY
!     TO DEFORMED PLOTS)
!
 1400 IF ( mode<=0 ) CALL rdmode(*1400,*1500,*2300,mode,word)
 1500 CALL rdword(mode,word)
   IF ( word/=poin .OR. mode/=0 ) GOTO 1000
   elgp = 1
   CALL rdmode(*1600,*900,*2300,mode,word)
 1600 ASSIGN 1700 TO tra
   GOTO 2600
 1700 IF ( nelx+1>=ngpx ) CALL mesage(-8,0,name)
   IF ( elgp/=0 ) THEN
      ngpx = ngpx - 1
      Gp(ngpx) = isign(iwrd,xx)
   ELSE
      nelx = nelx + 1
      El(nelx) = isign(iwrd,xx)
   ENDIF
!
   CALL rdmode(*2600,*1800,*2300,mode,word)
 1800 CALL rdword(mode,word)
   IF ( word/=to .AND. word/=thru ) GOTO 1000
   IF ( mode/=0 ) GOTO 1000
   ASSIGN 1900 TO tra
   CALL rdmode(*2600,*1000,*2300,mode,word)
 1900 IF ( nelx+2>=ngpx ) CALL mesage(-8,0,name)
   IF ( elgp/=0 ) THEN
      Gp(ngpx-1) = to
      Gp(ngpx-2) = isign(iwrd,xx)
      ngpx = ngpx - 2
   ELSE
      El(nelx+1) = to
      El(nelx+2) = iwrd
      nelx = nelx + 2
   ENDIF
   CALL rdmode(*1600,*900,*2300,mode,word)
   GOTO 1600
 2000 CALL rdword(mode,word)
 2100 IF ( word==incl .OR. word==excl .OR. word==exce ) GOTO 1100
   IF ( word==grid .OR. word==elem ) GOTO 1300
   IF ( word/=all ) THEN
!
      DO i = 1 , Ntypes
         idx = (i-1)*Incr
!
!     SKIP ELEMENTS WITH
!       1 GRID
!       SCALAR CONNECTIONS POSSIBLE
!       SPECIAL PLOTTER MNEMONIC OF -XX-
!
         IF ( Ne(idx+10)>1 .AND. Ne(idx+11)==0 ) THEN
            IF ( Ne(idx+16)/=ilxx ) THEN
               IF ( awrd(1)==Ne(idx+1) .AND. awrd(2)==Ne(idx+2) ) GOTO 2200
            ENDIF
         ENDIF
      ENDDO
      WRITE (Nout,99005) Ufm , awrd
99005 FORMAT (A23,' 699,',2A4,' ELEMENT IS INVALID')
      Nogo = 1
      elgp = 0
!
!     AN ELEMENT TYPE CAN BE INCLUDED OR EXCLUDED
!
      IF ( mode<=0 ) CALL rdmode(*1600,*2000,*2300,mode,word)
      GOTO 2000
   ELSE
      i = Ntypes + 1
   ENDIF
 2200 nt = nt + 2
!
!     SECOND WORD FOR EACH TYP LOCATES ELEMENT INCLUDE/EXCLUDE SEARCH
!     POINTER.  ELEMENT ID-S GIVEN PRIOR TO NELX ARE SKIPPED
!
   typ(nt-1) = isign(i,xx)
   typ(nt) = nelx + 1
   elgp = 0
   IF ( mode<=0 ) CALL rdmode(*1600,*2000,*2300,mode,word)
   GOTO 2000
!
!     A SET HAS BEEN COMPLETELY DEFINED.  FIRST, WRITE THE SET ID
!
 2300 IF ( nelx/=0 .OR. nt/=0 ) THEN
      CALL write(Msetid,setid,1,0)
      CALL write(Mset,setid,1,0)
!
!     WRITE THE SET OF EXPICIT ELEMENT ID-S
!
      CALL write(Mset,nelx,1,0)
      CALL write(Mset,El,nelx,0)
!
!     DELETE ALL ELEMENT TYPE DUPLICATES + WRITE REMAINING ONES
!
      n = 0
      IF ( nt/=0 ) THEN
         DO j = 1 , nt , 2
            xx = typ(j)
            IF ( xx/=0 ) THEN
               DO i = j , nt , 2
                  IF ( i/=j .AND. iabs(xx)==iabs(typ(i)) ) THEN
!
!     DELETE BOTH IF NEGATIVE OF OTHER
!
                     IF ( xx==-typ(i) ) typ(j) = 0
                     typ(i) = 0
                  ENDIF
               ENDDO
               IF ( typ(j)/=0 ) THEN
                  n = n + 2
                  typ(n-1) = xx
                  typ(n) = typ(j+1)
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      CALL write(Mset,n,1,0)
      CALL write(Mset,typ,n,0)
!
!     WRITE THE SET OF EXPLICIT GRID POINT ID-S
!
      n = b1 - ngpx
      CALL write(Mset,n,1,0)
      CALL write(Mset,Gp(ngpx),n,1)
      Nsets = Nsets + 1
   ENDIF
   GOTO 400
!
!     END OF -PCDB-
!
 2400 CALL clstab(Mset,rew)
   CALL clstab(Plot,rew)
   CALL clstab(Msetid,norew)
   CALL close(Pcdb,rew)
   IF ( Nsets==0 ) WRITE (Nout,99006) Uim
99006 FORMAT (A29,', NO SETS EXIST IN PLOT PACKAGE')
   IF ( Nogo/=0 ) CALL mesage(-61,0,0)
 2500 RETURN
!
!     READ AN INTEGER
!
 2600 IF ( mode/=-1 ) THEN
      IF ( mode==-4 ) iwrd = dwrd
      IF ( mode/=-4 ) iwrd = fwrd
   ENDIF
   GOTO tra
END SUBROUTINE setinp
