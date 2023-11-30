
SUBROUTINE gibstk(Ndstk,Iold,Renum,Ndeg,Lvl,Lvls1,Lvls2,Ccstor,Jump,Icrit,Nhigh,Nlow,Nacum,Size,Stpt,Un,Idim)
   IMPLICIT NONE
   REAL Brms0 , Brms1 , Dum3b(3) , Dum5a(5) , Dum6s(6) , Rms0 , Rms1
   INTEGER I77 , Ibuf , Ideg , Idpth , Maxw0 , Maxw1 , Method , Mm , N , Nbw , Ncm , Ngrid , Nlpp , Nn , Nout , Np , Nzero , Obw ,  &
         & Op
   COMMON /banda / Dum5a , Method
   COMMON /bandb / Dum3b , Ngrid
   COMMON /bandd / Obw , Nbw , Op , Np , Ncm , Nzero
   COMMON /bandg / N , Idpth , Ideg
   COMMON /bands / Nn , Mm
   COMMON /bandw / Maxw0 , Rms0 , Maxw1 , Rms1 , I77 , Brms0 , Brms1
   COMMON /system/ Ibuf , Nout , Dum6s , Nlpp
   INTEGER Icrit , Idim , Jump
   INTEGER Ccstor(1) , Iold(1) , Lvl(1) , Lvls1(1) , Lvls2(1) , Nacum(1) , Ndeg(1) , Ndstk(1) , Nhigh(1) , Nlow(1) , Renum(1) ,     &
         & Size(1) , Stpt(1)
   REAL Un(1)
   REAL averwb , brmsa , brmsb , crit1 , crit2 , im1 , im2 , rmsa , rmsb
   INTEGER i , ibw1 , ibw2 , idflt , ipf1 , ipf2 , isdir , lowdg , lroot , lvlbot , lvln , lvlwth , maxb , maxlw , maxwa , maxwb ,  &
         & nflg , num , rvnode , sbnum , stnode , stnum , sumwb , xc , xcmax
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     GIBBSTOCK USES GRAPH THEORETICAL METHODS TO PRODUCE A PERMUTATION
!     OF AN INPUT ARRAY WHICH REDUCES ITS BANDWITH
!
!     THE FOLLOWING INPUT PARAMETERS ARE REQUIRED--NDSTK,N,IDEG,IOLD
!
!     THESE INTEGER ARRAYS MUST BE DIMENSIONED IN THE CALLING PROGRAM--
!     NDSTK(NR,D1),RENUM(D2+1),NDEG(D2),IOLD(D2),LVL(D2),LVLS1(D2),
!     LVLS2(D2),CCSTOR(D2)   WHERE D1 .GE. MAX DEGREE OF ANY NODE AND
!     D2 AND NR ARE .GE. THE TOTAL NUMBER OF NODES IN THE GRAPH.
!
!     EXPLANATION OF PARAMETERS--
!     NDSTK   - ADJACENCY ARRAY REPRESENTING GRAPH TO BE PROCESSED
!               NDSTK(I,J) = NODE NUMBER OF JTH CONNECTION TO NODE
!               NUMBER I.  A CONNECTION OF A NODE TO ITSELF IS NOT
!               LISTED.  EXTRA POSITIONS MUST HAVE ZERO FILL.
!     NR      - ROW DIMENSION ASSIGNED NDSTK IN CALLING PROGRAM = II1
!     IOLD(I) - RENUMBERING OF ITH NODE BEFORE GIBBSTOCK PROCESSING
!               IF NO RENUMBERING EXISTS THEN ILD(1)=1,ILD(2)=2, ETC.
!     N       - NUMBER OF NODES IN GRAPH BEING PROCESSED
!     IDEG    - MAX DEGREE OF ANY NODE IN GRAPH BEING PROCESSED
!     JUMP   IS SET TO 0 IF EITHER CRITERION IS REDUCED.
!     ICRIT   - RESEQUENCING CRITERION, SET BY BANDIT
!               1 RMS WAVEFRONT, 2 BANDWIDTH, 3 PROFILE, 4 MAX.WAVEFRONT
!
!     ON OUTPUT THESE VARIABLES CONTAIN THE FOLLOWING INFORMATION--
!     RENUM(I)- THE NEW NUMBER FOR THE ITH NODE
!     NDEG(I) - THE DEGREE OF THE ITH NODE
!     IDPTH   - NUMBER OF LEVELS IN GIBBSTOCK LEVEL STRUCTURE
!     IBW2    - THE BANDWITH AFTER RENUMBERING
!     IPF2    - THE PROFILE AFTER RENUMBERING
!
!     THE FOLLOWING ONLY HAVE MEANING IF THE GRAPH WAS ALL ONE COMPONENT
!     LVL(I)  - INDEX INTO LVLS1 TO THE FIRST NODE IN LEVEL I
!               LVL(I+1)-LVL(I)= NUMBER OF NODES IN ITH LEVEL
!     LVLS1   - LEVEL STRUCTURE CHOSEN BY GIBBSTOCK
!     LVLS2(I)- THE LEVEL ASSIGNED TO NODE I BY GIBBSTOCK
!
!
!     OLD AND NEW MAX AND RMS WAVEFRONT FOR ENTIRE PROBLEM,
!     NOT JUST GIBSTK.
!     DIMENSIONS OF NHIGH, NLOW, AND NACUM ARE IDIM EACH
!     SIZE AND STPT HAVE DIMENSION IDIM/2 AND SHOULD BE CONTIGUOUS IN
!     CORE WITH SIZE FIRST.
!     XC = NUMBER OF SUB-COMPONENTS RESULTING AFTER REMOVING DIAMETER
!     FROM ONE COMPONENT OF ORIGINAL GRAPH.
!
   xcmax = Idim/2
   Ncm = 0
   N = Nn
   ibw2 = 0
   ipf2 = 0
!
!     SET RENUM(I) = 0 FOR ALL I TO INDICATE NODE I IS UNNUMBERED
!     THEN COMPUTE DEGREE OF EACH NODE AND ORIGINAL B AND P.
!
   DO i = 1 , N
      Renum(i) = 0
   ENDDO
   CALL dgree(Ndstk,Ndeg,Iold,ibw1,ipf1,Un)
!
!     ORIGINAL ACTIVE COLUMN DATA IN MAXW1 AND RMS1, COMPUTED BY SCHEME
!
   IF ( Method/=0 ) THEN
      maxwa = Maxw0
      rmsa = Rms0
      brmsa = Brms0
   ELSE
      maxwa = Maxw1
      rmsa = Rms1
      brmsa = Brms1
   ENDIF
!
!     NUMBER THE NODES OF DEGREE ZERO
!     SBNUM = LOW  END OF AVAILABLE NUMBERS FOR RENUMBERING
!     STNUM = HIGH END OF AVAILABLE NUMBERS FOR RENUMBERING
!
   sbnum = 1
   stnum = N
   DO i = 1 , N
      IF ( Ndeg(i)<=0 ) THEN
         Renum(i) = stnum
         stnum = stnum - 1
      ENDIF
   ENDDO
!
!     NODES OF ZERO DEGREE APPEAR LAST IN NEW SEQUENCE.
!
   Nzero = N - stnum
   Ncm = Nzero
!
!     FIND AN UNNUMBERED NODE OF MIN DEGREE TO START ON
!
 100  lowdg = Ideg + 1
   Ncm = Ncm + 1
   nflg = 1
   isdir = 1
   DO i = 1 , N
      IF ( Ndeg(i)<lowdg .AND. Renum(i)<=0 ) THEN
         lowdg = Ndeg(i)
         stnode = i
      ENDIF
   ENDDO
!
!     FIND PSEUDO-DIAMETER AND ASSOCIATED LEVEL STRUCTURES.
!     STNODE AND RVNODE ARE THE ENDS OF THE DIAM AND LVLS1 AND LVLS2
!     ARE THE RESPECTIVE LEVEL STRUCTURES.
!
   CALL fndiam(stnode,rvnode,Ndstk,Ndeg,Lvl,Lvls1,Lvls2,Ccstor,idflt,Size,Un,Idim)
   IF ( Ngrid==-3 ) RETURN
   IF ( Ndeg(stnode)>Ndeg(rvnode) ) THEN
!
!     NFLG INDICATES THE END TO BEGIN NUMBERING ON
!
      nflg = -1
      stnode = rvnode
   ENDIF
   CALL rsetup(Lvl,Lvls1,Lvls2,Nacum,Idim)
!                                  NHIGH,NLOW,    <===== NEW
   IF ( Ngrid==-3 ) RETURN
!
!     FIND ALL THE CONNECTED COMPONENTS  (XC COUNTS THEM)
!
   xc = 0
   lroot = 1
   lvln = 1
   DO i = 1 , N
      IF ( Lvl(i)==0 ) THEN
         xc = xc + 1
         IF ( xc<=xcmax ) THEN
!
            Stpt(xc) = lroot
            CALL tree(i,Ndstk,Lvl,Ccstor,Ndeg,lvlwth,lvlbot,lvln,maxlw,N,Un)
            Size(xc) = lvlbot + lvlwth - lroot
            lroot = lvlbot + lvlwth
            lvln = lroot
         ELSE
!
!     DIMENSION EXCEEDED.  STOP JOB.
!
            Ngrid = -3
            RETURN
         ENDIF
      ENDIF
   ENDDO
   CALL piklvl(*200,Lvls1,Lvls2,Ccstor,idflt,isdir,xc,Nhigh,Nlow,Nacum,Size,Stpt)
!
!     ON RETURN FROM PIKLVL, ISDIR INDICATES THE DIRECTION THE LARGEST
!     COMPONENT FELL.  ISDIR IS MODIFIED NOW TO INDICATE THE NUMBERING
!     DIRECTION.  NUM IS SET TO THE PROPER VALUE FOR THIS DIRECTION.
!
 200  isdir = isdir*nflg
   num = sbnum
   IF ( isdir<0 ) num = stnum
!
   CALL number(stnode,num,Ndstk,Lvls2,Ndeg,Renum,Lvls1,Lvl,nflg,ibw2,ipf2,Ccstor,isdir,Nhigh,Nlow,Nacum,Size,Un,Idim)
   IF ( Ngrid==-3 ) RETURN
!
!     UPDATE STNUM OR SBNUM AFTER NUMBERING
!
   IF ( isdir<0 ) stnum = num
   IF ( isdir>0 ) sbnum = num
   IF ( sbnum<=stnum ) GOTO 100
!
!     COMPUTE THE NEW BANDWIDTH, PROFILE, AND WAVEFRONT.
!
   CALL wavey(Ndstk,Renum,Lvl,0,Lvls2,Lvls1,maxb,maxwb,averwb,sumwb,rmsb,brmsb,Un)
!
   ibw2 = maxb
   ipf2 = sumwb
   IF ( Nlpp>50 ) WRITE (Nout,99001) maxb , sumwb , maxwb , averwb , rmsb , brmsb
99001 FORMAT (/31X,66HAFTER RESEQUENCING BY GIBBS-POOLE-STOCKMEYER (GPS) ALGORITHM - - -,/40X,13HBANDWIDTH    ,I9,/40X,             &
             &13HPROFILE      ,I9,/40X,13HMAX WAVEFRONT,I9,/40X,13HAVG WAVEFRONT,F9.3,/40X,13HRMS WAVEFRONT,F9.3,/40X,              &
             &13HRMS BANDWIDTH,F9.3)
!
!     CHECK NEW NUMBERING AGAINST OLD NUMBERING.
!
   IF ( Icrit==2 ) THEN
      im1 = ibw1
      im2 = ipf1
      crit1 = ibw2
      crit2 = ipf2
   ELSEIF ( Icrit==3 ) THEN
      im1 = ipf1
      im2 = ibw1
      crit1 = ipf2
      crit2 = ibw2
   ELSEIF ( Icrit==4 ) THEN
      im1 = maxwa
      im2 = rmsa
      crit1 = maxwb
      crit2 = rmsb
   ELSE
      im1 = rmsa
      im2 = ipf1
      crit1 = rmsb
      crit2 = ipf2
   ENDIF
!
   IF ( crit1<im1 ) THEN
!
!     EQUATE CORRESPONDING GPS AND BANDIT VARIABLES.
!
      Jump = 0
      GOTO 300
   ELSEIF ( crit1==im1 ) THEN
      IF ( crit2<im2 ) THEN
         Jump = 0
         GOTO 300
      ENDIF
   ENDIF
!
!     IF ORIGINAL NUMBERING IS BETTER THAN NEW ONE, SET UP TO RETURN IT
!
   DO i = 1 , N
      Renum(i) = Iold(i)
   ENDDO
   ibw2 = ibw1
   ipf2 = ipf1
   maxwb = maxwa
   rmsb = rmsa
   brmsb = brmsa
 300  Nbw = ibw2
   Np = ipf2
   Maxw1 = maxwb
   Rms1 = rmsb
   Brms1 = brmsb
END SUBROUTINE gibstk
