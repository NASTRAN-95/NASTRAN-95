
SUBROUTINE number(Snd,Num,Ndstk,Lvls2,Ndeg,Renum,Lvlst,Lstpt,Nflg,Ibw2,Ipf2,Ipfa,Isdir,Stka,Stkb,Stkc,Stkd,Nu,Idim)
   IMPLICIT NONE
   REAL Dum3(3) , Dums(4)
   INTEGER Ibuf , Ideg , Idpth , Maxdeg , Maxgrd , N , Ngrid , Nout
   COMMON /bandb / Dum3 , Ngrid
   COMMON /bandg / N , Idpth , Ideg
   COMMON /bands / Dums , Maxgrd , Maxdeg
   COMMON /system/ Ibuf , Nout
   INTEGER Ibw2 , Idim , Ipf2 , Isdir , Nflg , Num , Snd
   INTEGER Ipfa(1) , Lstpt(1) , Lvls2(1) , Lvlst(1) , Ndeg(1) , Ndstk(1) , Nu(1) , Renum(1) , Stka(1) , Stkb(1) , Stkc(1) , Stkd(1)
   INTEGER cx , end , i , inx , ipro , j , lnd , lst , lvln , max , nbw , nstpt , test , xa , xb , xc , xd
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     NUMBER PRODUCES THE NUMBERING OF THE GRAPH FOR MIN BANDWIDTH
!
!     SND-      ON INPUT THE NODE TO BEGIN NUMBERING ON
!     NUM-      ON INPUT AND OUTPUT, THE NEXT AVAILABLE NUMBER
!     LVLS2-    THE LEVEL STRUCTURE TO BE USED IN NUMBERING
!     RENUM-    THE ARRAY USED TO STORE THE NEW NUMBERING
!     LVLST-    ON OUTPUT CONTAINS LEVEL STRUCTURE
!     LSTPT(I)- ON OUTPUT, INDEX INTO LVLST TO FIRST NODE IN ITH LVL
!               LSTPT(I+1) - LSTPT(I) = NUMBER OF NODES IN ITH LVL
!     NFLG-     =+1 IF SND IS FORWARD END OF PSEUDO-DIAM
!               =-1 IF SND IS REVERSE END OF PSEUDO-DIAM
!     IBW2-     BANDWIDTH OF NEW NUMBERING COMPUTED BY NUMBER
!     IPF2-     PROFILE OF NEW NUMBERING COMPUTED BY NUMBER
!     IBW2 AND IPF2 HERE DO NOT INCLUDE DIAGONAL TERMS.
!     IPFA-     WORKING STORAGE USED TO COMPUTE PROFILE AND BANDWIDTH
!     ISDIR-    INDICATES STEP DIRECTION USED IN NUMBERING(+1 OR -1)
!     STACKS HAVE DIMENSION OF IDIM
!     NU-       WORK SPACE FOR BUNPAK
!
!
!     SET UP LVLST AND LSTPT FROM LVLS2
!
   DO i = 1 , N
      Ipfa(i) = 0
   ENDDO
   nstpt = 1
   DO i = 1 , Idpth
      Lstpt(i) = nstpt
      DO j = 1 , N
         IF ( Lvls2(j)==i ) THEN
            Lvlst(nstpt) = j
            nstpt = nstpt + 1
         ENDIF
      ENDDO
   ENDDO
   Lstpt(Idpth+1) = nstpt
!
!     THIS ROUTINE USES FOUR STACKS, A,B,C,AND D, WITH POINTERS
!     XA,XB,XC, AND XD.  CX IS A SPECIAL POINTER INTO STKC WHICH
!     INDICATES THE PARTICULAR NODE BEING PROCESSED.
!     LVLN KEEPS TRACK OF THE LEVEL WE ARE WORKING AT.
!     INITIALLY STKC CONTAINS ONLY THE INITIAL NODE, SND.
!
   lvln = 0
   IF ( Nflg<0 ) lvln = Idpth + 1
   xc = 1
   Stkc(xc) = Snd
 100  cx = 1
   xd = 0
   lvln = lvln + Nflg
   lst = Lstpt(lvln)
   lnd = Lstpt(lvln+1) - 1
!
!     BEGIN PROCESSING NODE STKC(CX)
!
 200  ipro = Stkc(cx)
   Renum(ipro) = Num
   Num = Num + Isdir
   end = Ndeg(ipro)
   xa = 0
   xb = 0
!
!     CHECK ALL ADJACENT NODES
!
   CALL bunpak(Ndstk,ipro,end,Nu)
   DO i = 1 , end
      test = Nu(i)
      inx = Renum(test)
!
!     ONLY NODES NOT NUMBERED OR ALREADY ON A STACK ARE ADDED
!
      IF ( inx==0 ) THEN
         Renum(test) = -1
!
!     PUT NODES ON SAME LEVEL ON STKA, ALL OTHERS ON STKB
!
         IF ( Lvls2(test)==Lvls2(ipro) ) THEN
            xa = xa + 1
            IF ( xa>Idim ) GOTO 300
            Stka(xa) = test
         ELSE
            xb = xb + 1
            IF ( xb>Idim ) GOTO 300
            Stkb(xb) = test
         ENDIF
      ELSEIF ( inx>=0 ) THEN
!
!     DO PRELIMINARY BANDWIDTH AND PROFILE CALCULATIONS
!
         nbw = (Renum(ipro)-inx)*Isdir
         IF ( Isdir>0 ) inx = Renum(ipro)
         IF ( Ipfa(inx)<nbw ) Ipfa(inx) = nbw
      ENDIF
   ENDDO
!
!     SORT STKA AND STKB INTO INCREASING DEGREE AND ADD STKA TO STKC
!     AND STKB TO STKD
!
   IF ( xa/=0 ) THEN
      IF ( xa==1 ) THEN
         xc = xc + 1
         IF ( xc>Idim ) GOTO 300
         Stkc(xc) = Stka(xa)
      ELSE
         CALL sortdg(Stkc,Stka,xc,xa,Ndeg)
      ENDIF
   ENDIF
   IF ( xb/=0 ) THEN
      IF ( xb==1 ) THEN
         xd = xd + 1
         IF ( xd>Idim ) GOTO 300
         Stkd(xd) = Stkb(xb)
      ELSE
         CALL sortdg(Stkd,Stkb,xd,xb,Ndeg)
      ENDIF
   ENDIF
!
!     BE SURE TO PROCESS ALL NODES IN STKC
!
   cx = cx + 1
   IF ( xc>=cx ) GOTO 200
!
!     WHEN STKC IS EXHAUSTED LOOK FOR MIN DEGREE NODE IN SAME LEVEL
!     WHICH HAS NOT BEEN PROCESSED
!
   max = Ideg + 1
   Snd = N + 1
   DO i = lst , lnd
      test = Lvlst(i)
      IF ( Renum(test)==0 ) THEN
         IF ( Ndeg(test)<max ) THEN
            Renum(Snd) = 0
            Renum(test) = -1
            max = Ndeg(test)
            Snd = test
         ENDIF
      ENDIF
   ENDDO
   IF ( Snd/=N+1 ) THEN
      xc = xc + 1
      IF ( xc<=Idim ) THEN
         Stkc(xc) = Snd
         GOTO 200
      ENDIF
!
!     IF STKD IS EMPTY WE ARE DONE, OTHERWISE COPY STKD ONTO STKC
!     AND BEGIN PROCESSING NEW STKC
!
   ELSEIF ( xd==0 ) THEN
!
!     DO FINAL BANDWIDTH AND PROFILE CALCULATIONS
!
      DO i = 1 , N
         IF ( Ipfa(i)>Ibw2 ) Ibw2 = Ipfa(i)
         Ipf2 = Ipf2 + Ipfa(i)
      ENDDO
      RETURN
   ELSE
      DO i = 1 , xd
         Stkc(i) = Stkd(i)
      ENDDO
      xc = xd
      GOTO 100
   ENDIF
!
!     DIMENSION EXCEEDED  . . .  STOP JOB.
!
 300  Ngrid = -3
END SUBROUTINE number
