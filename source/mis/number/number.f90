!*==number.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE number(Snd,Num,Ndstk,Lvls2,Ndeg,Renum,Lvlst,Lstpt,Nflg,Ibw2,Ipf2,Ipfa,Isdir,Stka,Stkb,Stkc,Stkd,Nu,Idim)
   IMPLICIT NONE
   USE C_BANDB
   USE C_BANDG
   USE C_BANDS
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Snd
   INTEGER :: Num
   INTEGER , DIMENSION(1) :: Ndstk
   INTEGER , DIMENSION(1) :: Lvls2
   INTEGER , DIMENSION(1) :: Ndeg
   INTEGER , DIMENSION(1) :: Renum
   INTEGER , DIMENSION(1) :: Lvlst
   INTEGER , DIMENSION(1) :: Lstpt
   INTEGER :: Nflg
   INTEGER :: Ibw2
   INTEGER :: Ipf2
   INTEGER , DIMENSION(1) :: Ipfa
   INTEGER :: Isdir
   INTEGER , DIMENSION(1) :: Stka
   INTEGER , DIMENSION(1) :: Stkb
   INTEGER , DIMENSION(1) :: Stkc
   INTEGER , DIMENSION(1) :: Stkd
   INTEGER , DIMENSION(1) :: Nu
   INTEGER :: Idim
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: cx , end , i , inx , ipro , j , lnd , lst , lvln , max , nbw , nstpt , test , xa , xb , xc , xd
   EXTERNAL bunpak , sortdg
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
         cx = 1
         xd = 0
         lvln = lvln + Nflg
         lst = Lstpt(lvln)
         lnd = Lstpt(lvln+1) - 1
         SPAG_Loop_1_1: DO
!
!     BEGIN PROCESSING NODE STKC(CX)
!
            ipro = Stkc(cx)
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
                     IF ( xa>Idim ) EXIT SPAG_Loop_1_1
                     Stka(xa) = test
                  ELSE
                     xb = xb + 1
                     IF ( xb>Idim ) EXIT SPAG_Loop_1_1
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
                  IF ( xc>Idim ) EXIT SPAG_Loop_1_1
                  Stkc(xc) = Stka(xa)
               ELSE
                  CALL sortdg(Stkc,Stka,xc,xa,Ndeg)
               ENDIF
            ENDIF
            IF ( xb/=0 ) THEN
               IF ( xb==1 ) THEN
                  xd = xd + 1
                  IF ( xd>Idim ) EXIT SPAG_Loop_1_1
                  Stkd(xd) = Stkb(xb)
               ELSE
                  CALL sortdg(Stkd,Stkb,xd,xb,Ndeg)
               ENDIF
            ENDIF
!
!     BE SURE TO PROCESS ALL NODES IN STKC
!
            cx = cx + 1
            IF ( xc<cx ) THEN
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
                     CYCLE
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
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     DIMENSION EXCEEDED  . . .  STOP JOB.
!
         Ngrid = -3
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE number
