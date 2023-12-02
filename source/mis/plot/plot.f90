!*==plot.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE plot(Mode,Buf1,B1,Setid,Deflst,Nofind)
USE C_BLANK
USE C_DRWDAT
USE C_OUTPUT
USE C_PLOTHD
USE C_PLTDAT
USE C_PLTSCR
USE C_SYSTEM
USE C_XMSSG
USE C_XXPARM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mode
   INTEGER :: Buf1
   INTEGER :: B1
   INTEGER , DIMENSION(1) :: Setid
   INTEGER , DIMENSION(2) :: Deflst
   INTEGER :: Nofind
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , both , contur , defo , elem , eor , epid , grid , gspc , imod , inprew , load , lorig , nmsg1 , nmsg2 ,  &
                   & nmsg7 , norew , poin , rang , rew , skplod , skpttl , thru , time , to
   INTEGER , DIMENSION(2) :: awrd
   REAL :: conv , forg , fwrd , maxdef , ph , ph1 , twopi , v , v1 , v2 , value
   INTEGER :: d1 , d2 , defbuf , defid , dtype , fscale , fvp , i , i1 , i2 , i3 , iapp , incom , isetd , iwrd , j , japp , keywd , &
            & lasset , loadid , lpcon , m , mag , mdef , mfile , modex , mtyp , n , n1 , n2 , ndef , nogo , nplots , pltbuf ,       &
            & plttyp , stereo , thlid , tra , word
   LOGICAL :: disp , stress
   REAL(REAL64) :: dwrd
   INTEGER , DIMENSION(17) :: err
   INTEGER , DIMENSION(10) , SAVE :: f1 , used
   INTEGER , DIMENSION(20) , SAVE :: f2
   REAL , DIMENSION(17) :: frr
   INTEGER , DIMENSION(2) , SAVE :: lag , magc , name , nf , subc
   INTEGER , DIMENSION(2,5) , SAVE :: mf1 , mf2
   INTEGER , DIMENSION(3,3) , SAVE :: mf3
   INTEGER , DIMENSION(6) , SAVE :: mf4
   INTEGER , DIMENSION(19) , SAVE :: msg1
   INTEGER , DIMENSION(17) , SAVE :: msg2 , rqst
   INTEGER , DIMENSION(13) , SAVE :: msg7
   EXTERNAL andf , bckrec , close , draw , find , fndset , fread , fwdrec , getdef , gopen , head , intlst , intvec , mesage ,      &
          & open , pltopr , rdmode , rdmodx , rdmody , rdword , read , rotat , skprec , sopen , stplot , tapbit , write , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS PLOT ROUTINE IS CALLED ONLY BY PARAM
!
   !>>>>EQUIVALENCE (err(1),frr(1)) , (word,awrd(1),iwrd,fwrd,dwrd)
   !>>>>EQUIVALENCE (Fscale,Scale(3)) , (Fvp,Vanpnt(1))
   !>>>>EQUIVALENCE (Skp19(1),Lasset)
   DATA eor , inprew , norew , rew , skpttl , skplod/1000000 , 0 , 2 , 1 , 37 , 5/
   DATA subc/4HSUBC , 4HASES/
   DATA name/4H  PL , 4HOT  /
!
   DATA nf/10 , 20/ , f1/4H(49X , 4H,4HP , 4HLOT, , 4HI9,2 , 4HX,16 , 4HHUND , 4HEFOR , 4HMED  , 4HSHAP , 4HE)  / , f2/4H(10X ,     &
       &4H,4HP , 4HLOT, , 4HI5,3 , 4HX,2( , 4HA4,A , 4H3),I , 4H6,10 , 4HH -  , 4HSUBC , 4HASE, , 4HI8,3 , 4HH -  , 4H,A4, ,        &
      & 4H1P,E , 4H15.6 , 4H,1X, , 4H6A4, , 4HE11. , 4H3)  /
!
!     DATA FOR FORMAT F2 - ORDER CORRESPONDING TO DTYPE, +10=VEL,+20=ACC
!
   DATA mf1/4HSTAT , 2HIC , 4HFREQ , 1H. , 4HTRAN , 2HS. , 4HMODA , 1HL , 4HCMOD , 2HAL/ , mf2/4HDEFO , 3HRM. , 4HVELO , 1H. ,      &
       &4HACCE , 2HL. , 4HSTRE , 2HSS , 4HSTRA , 2HIN/
   DATA imod , load/4HMODE , 4HLOAD/
   DATA mf3/4H- FR , 4HEQUE , 4HNCY  , 4H- EI , 4HGENV , 4HALUE , 4H- TI , 2HME , 1H / , mf4/4H PHA , 4HSE L , 4HAG   , 4H MAG ,    &
       &4HNITU , 2HDE/
!
   DATA nmsg1 , nmsg2 , nmsg7/19 , 17 , 13/ , msg1/4H(33X , 4H,26H , 4HAN U , 4HNREC , 4HOGNI , 4HZABL , 4HE OP , 4HTION , 4H (,2 , &
       &4HA4,3 , 4H1H)  , 4HWAS  , 4HDETE , 4HCTED , 4H ON  , 4HA -P , 4HLOT- , 4H CAR , 4HD)  / , msg2/4H(34X , 4H,21H , 4HA NO ,  &
       &4HN-EX , 4HISTE , 4HNT O , 4HRIGI , 4HN,I7 , 4H,31H , 4H  IS , 4H SPE , 4HCIFI , 4HED O , 4HN A  , 4H-PLO , 4HT- C ,        &
      & 4HARD)/ , msg7/4H(33X , 4H,41H , 4H***  , 4HINCO , 4HMPLE , 4HTE P , 4HLOT  , 4HDUE  , 4HTO I , 4HNPUT , 4H OR  , 4HFILE ,  &
       &4H.)  /
!
!     SET OPTIONS - FOLLOWING THE SET REQUEST(S)
!
   DATA rqst/4HSET  , 4HORIG , 4HSHAP , 4HSYMB , 4HLABE , 4HVECT , 4HDENS , 4HPEN  , 4HSYMM , 4HANTI , 4HMAXI , 4HOUTL , 4HHIDD ,   &
       &4HSHRI , 4HNOFI , 4HFILL , 4HOFFS/
!
   DATA used/4H(49X , 4H,6HO , 4HRIGI , 4HN,I7 , 4H,19H , 4H  US , 4HED I , 4HN TH , 4HIS P , 4HLOT)/
!
!     THE FOLLOWING ARE POSSIBLE OPTIONS ON THE PLOT CARD
!
   DATA defo/4HDEFO/ , lorig/0/ , all/3HALL/ , to/2HTO/ , thru/4HTHRU/ , rang/4HRANG/ , time/4HTIME/ , both/4HBOTH/ , grid/4HGRID/ ,&
      & poin/4HPOIN/ , elem/4HELEM/ , gspc/4HGSPC/ , lag/4HPHAS , 4HLAG / , magc/4HMAGN , 4HIT. / , epid/4HEPID/ , contur/4HCONT/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Ncntr = 10
         Icntvl = 1
         Where = 1
         lasset = 0
         Direct = 2
         Ncor = 50
         DO i = 1 , 50
            Pltsc(i) = 0
            Cntr(i) = 0
         ENDDO
         pltbuf = B1 - Pbufsz
         defbuf = pltbuf - Bufsiz
         IF ( defbuf<=0 ) THEN
!
!     INSUFFICIENT CORE TO START PROCESSING
!
            CALL mesage(-8,defbuf,name)
            GOTO 780
         ELSE
            v1 = -1.E+30
            v2 = +1.E+30
            ph = 0.0
            mag = 0
            Pcon = 0
            loadid = 0
            lpcon = 0
            Flag = 0.0
            Subcas = 0
            defid = 0
            disp = .FALSE.
            stress = .FALSE.
            twopi = 8.0*atan(1.0)
            ndef = 0
            nogo = 0
            CALL rdmodx(Parm,Mode,word)
         ENDIF
!
 20      IF ( Mode<=0 ) CALL rdmode(*20,*40,*60,Mode,word)
 40      CALL rdword(Mode,word)
!
!     CHECK FOR A DEFORMATION TYPE
!
         DO dtype = 1 , 5
            IF ( word==mf1(1,dtype) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 60      dtype = 0
         IF ( word/=contur .OR. Mode>=eor ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Pcon = 1
         plttyp = 1
         IF ( Mode<=0 ) CALL rdmode(*140,*100,*120,Mode,word)
         GOTO 100
      CASE (2)
!
!     DEFORMATION TYPE SPECIFIED. CHECK IF ALL ARE TO BE PLOTTED
!
         plttyp = 1
         IF ( Mode<=0 ) CALL rdmode(*140,*80,*120,Mode,word)
 80      CALL rdword(Mode,word)
         DO plttyp = 1 , 3
            IF ( word==mf2(1,plttyp) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         plttyp = 1
         IF ( word/=contur ) GOTO 120
         Pcon = 1
         spag_nextblock_1 = 3
      CASE (3)
!
!     ACCEL, VELOCITY ONLY ALLOWED FOR TRANS OR FREQUENCY RESPONSE.
!     NOTE THAT A COMPLEX  IGENVALUE WOULD BE NEEDED FOR -CMODAL-
!
         IF ( (dtype/=2 .AND. dtype/=3) .AND. plttyp/=1 ) THEN
            err(1) = 2
            err(2) = awrd(1)
            err(3) = awrd(2)
            CALL wrtprt(Merr,err,msg1,nmsg1)
            plttyp = 1
         ENDIF
         IF ( Mode<=0 ) CALL rdmode(*140,*100,*120,Mode,word)
 100     CALL rdword(Mode,word)
 120     ndef = 1
         Deflst(1) = all
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     THE DEFORMATIONS MAY BE EXPLICITLY LISTED AND/OR A RANGE MAY BE
!     LISTED (I.E., N1,N2 AND/OR N1 -TO/THRU- N2)
!
 140     ASSIGN 160 TO tra
         GOTO 840
 160     ndef = ndef + 1
         Deflst(ndef) = iwrd
         CALL rdmode(*840,*180,*240,Mode,word)
 180     CALL rdword(Mode,word)
         IF ( Mode/=0 .OR. (word/=to .AND. word/=thru) ) GOTO 240
         ASSIGN 200 TO tra
         CALL rdmode(*840,*220,*240,Mode,word)
 200     Deflst(ndef+1) = to
         Deflst(ndef+2) = iwrd
         ndef = ndef + 2
         CALL rdmode(*140,*220,*240,Mode,word)
 220     CALL rdword(Mode,word)
 240     IF ( ndef==1 .AND. Deflst(1)==0 ) THEN
            ndef = 2
            Deflst(2) = all
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     ALL THE LISTED DEFORMATION ID-S HAVE BEEN READ
!
         Deflst(ndef+1) = 0
         IF ( Mode>=eor ) GOTO 380
         spag_nextblock_1 = 5
      CASE (5)
!
!     TEST FOR CONTOUR REQUEST
!
         IF ( word/=contur ) THEN
!
!     TEST FOR RANGE / TIME  (UNITS=LAMDA,F, OR TIME)
!
            IF ( word/=rang .AND. word/=time ) THEN
!
!     TEST FOR PHASE LAG (COMPLEX DATA)
!
               IF ( word/=lag(1) ) THEN
!
!     TEST FOR MAGNITUDE (COMPLEX DATA)
!
                  IF ( word/=magc(1) ) GOTO 380
                  IF ( dtype==2 .OR. dtype==5 ) THEN
                     IF ( ph==0.0 ) mag = 1
                     GOTO 340
                  ENDIF
               ELSEIF ( dtype==2 .OR. dtype==5 ) THEN
                  ASSIGN 320 TO tra
                  IF ( Mode<=0 ) CALL rdmode(*860,*300,*380,Mode,word)
                  GOTO 300
               ENDIF
            ELSEIF ( Pcon/=0 .OR. dtype/=1 ) THEN
               ASSIGN 260 TO tra
               IF ( Mode<=0 ) THEN
                  CALL rdmode(*860,*360,*380,Mode,word)
                  GOTO 260
               ENDIF
            ENDIF
         ELSEIF ( Pcon==0 ) THEN
!
            Pcon = 1
            IF ( dtype==0 ) plttyp = 1
            IF ( ndef/=1 ) THEN
               IF ( Mode>0 ) GOTO 340
               err(2) = subc(1)
               err(3) = subc(2)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ndef = 0
               IF ( Mode<=0 ) CALL rdmode(*140,*100,*120,Mode,word)
               GOTO 100
            ENDIF
         ENDIF
         err(2) = awrd(1)
         err(3) = awrd(2)
         spag_nextblock_1 = 6
      CASE (6)
         err(1) = 2
         CALL wrtprt(Merr,err,msg1,nmsg1)
         GOTO 340
 260     v1 = fwrd
         ASSIGN 280 TO tra
         CALL rdmode(*860,*360,*380,Mode,word)
 280     v2 = fwrd
         GOTO 340
 300     DO
            CALL rdword(Mode,word)
            IF ( word/=lag(2) ) GOTO 380
            IF ( Mode<=0 ) CALL rdmode(*860,*300,*380,Mode,word)
         ENDDO
 320     IF ( mag==0 ) ph = fwrd
!
 340     IF ( Mode<=0 ) CALL rdmode(*340,*360,*380,Mode,word)
 360     CALL rdword(Mode,word)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     READ THE REST OF THE PLOT CARD INTO STORAGE - DEFLST(N1-N2)
!
 380     n1 = ndef + 1
         n2 = n1 + 1
         IF ( Mode<eor ) THEN
            n = 0
            DO
               Deflst(n2+1) = awrd(1)
               Deflst(n2+2) = awrd(2)
               n2 = n2 + 2
               n = n + 1
               IF ( Mode==0 ) THEN
                  n2 = n2 + 1
                  Deflst(n1+1) = n
                  CALL read(*880,*400,Parm,Deflst(n2),defbuf-n2+1,0,n)
                  CALL mesage(-8,defbuf,name)
                  GOTO 780
               ELSE
                  CALL rdword(Mode,word)
               ENDIF
            ENDDO
         ELSE
            Deflst(n2) = Mode
            n2 = n2 + 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 400     n2 = n2 + n
!
!     SAVE LENGTH OF OPEN CORE USED IN IUSED FOR HDPLOT
!
         Iused = n2 + Nsets
         IF ( Deflst(n2-1)==0 ) THEN
            CALL read(*880,*400,Parm,Deflst(n2),defbuf-n2+1,0,n)
            CALL mesage(-8,defbuf,name)
            GOTO 780
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         n2 = n2 - 1
!
!     INITIATE THE PLOTS OF THE REQUESTED DEFORMATIONS.
!
         nplots = 0
         IF ( Prnt<0 ) THEN
            IF ( dtype==0 .AND. Pcon==0 ) GOTO 820
!
!     DO THE DEFORMED PLOT
!
!     STRESS IS TRUE IF CONTOUR REQUEST IS FOR STRESS
!
            lpcon = Pcon
            IF ( .NOT.tapbit(Plttap) ) GOTO 880
            IF ( Pcon/=0 .AND. Icntvl<=9 ) stress = .TRUE.
            IF ( Pcon/=0 .AND. Icntvl>13 ) stress = .TRUE.
            IF ( (Pcon/=0 .AND. (Icntvl>9 .AND. Icntvl<14)) .OR. dtype/=0 ) disp = .TRUE.
            IF ( .NOT.disp ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            mdef = Defile(1)
            IF ( dtype>1 ) mdef = Defile(2)
            IF ( dtype>0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( dtype==0 .AND. Pcon==0 ) THEN
!
!     DO THE UNDEFORMED PLOT
!
            defid = 0
            defbuf = defbuf + Bufsiz
            IF ( Isubs==0 .AND. .NOT.tapbit(Plttap) ) GOTO 880
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Anydef = 1
            GOTO 820
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     USER SPECIFIED CONTOUR DISP AND NOT THE TYPE
!     USE FIRST NON-NULL FILE
!
         CALL open(*420,mdef,Deflst(defbuf),inprew)
         CALL skprec(mdef,1)
!
!     SET DTYPE BY MFILE
!
         CALL read(*800,*800,mdef,err(1),2,0,i)
         mfile = mod(err(2),10)
         dtype = mfile
         CALL close(mdef,rew)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 420     IF ( mdef==Defile(2) ) CALL mesage(-1,mdef,name)
         mdef = Defile(2)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     CALCULATE HEADER WORD 2 NEEDED FOR PLOT FILE CHECK
!
         mfile = dtype
         IF ( dtype==3 ) mfile = 3 + (plttyp-1)*10
!
!     OPEN OES1 AND MDEF
!
         IF ( disp ) THEN
            CALL open(*820,mdef,Deflst(defbuf),inprew)
            CALL skprec(mdef,1)
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         IF ( stress ) THEN
            CALL open(*800,Oes1,Deflst(B1),inprew)
            CALL skprec(Oes1,1)
            IF ( .NOT.disp ) plttyp = 4
            CALL fread(Oes1,i,1,0)
            CALL bckrec(Oes1)
            i = i/10
            japp = i
            IF ( dtype==0 ) THEN
               IF ( i==1 .OR. i==3 .OR. i==4 .OR. i==7 .OR. i==10 ) dtype = 1
               IF ( i==2 .OR. i==8 ) dtype = 4
               IF ( i==6 ) dtype = 3
            ENDIF
!
!     FOR STRESS PLOTS SET -FLAG- SO FNDSET KNOWS WHICH WORD TO COMPARE
!
            IF ( dtype/=1 ) THEN
               IF ( dtype>1 ) Flag = 1.0
               IF ( dtype>3 ) Flag = 2.0
            ENDIF
            IF ( dtype==0 ) GOTO 780
            IF ( .NOT.disp ) defbuf = defbuf + Bufsiz
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
!     READ THE PLOT TITLES FOR EACH DEFORMED SHAPE TO BE DRAWN
!
         Pcon = lpcon
         IF ( .NOT.disp ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
         SPAG_Loop_1_2: DO
            CALL read(*780,*780,mdef,defid,1,0,i)
            CALL fread(mdef,n,1,0)
            IF ( n==mfile ) THEN
               CALL fread(mdef,loadid,1,0)
               CALL fread(mdef,value,1,1)
               IF ( value<v1 .OR. value>v2 ) THEN
                  CALL skprec(mdef,1)
               ELSE
                  Data = value
                  Subcas = defid
                  n = 1
                  SPAG_Loop_2_1: DO WHILE ( Deflst(n)/=all )
                     CALL intlst(Deflst,n,i,d1,d2)
                     IF ( defid>=d1 .AND. defid<=d2 ) EXIT SPAG_Loop_2_1
                     IF ( n>=n1 ) THEN
                        CALL skprec(mdef,1)
                        CYCLE SPAG_Loop_1_2
                     ENDIF
                  ENDDO SPAG_Loop_2_1
                  EXIT SPAG_Loop_1_2
               ENDIF
            ELSE
               CALL skprec(mdef,1)
               CALL skprec(mdef,1)
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 13
      CASE (13)
!
!     POSITION OES1 IF NEEDED
!
         IF ( .NOT.stress ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nplots/=0 ) CALL open(*800,Oes1,Deflst(B1),norew)
         spag_nextblock_1 = 14
      CASE (14)
         CALL read(*780,*780,Oes1,iapp,1,0,i)
!
!     VERIFY OES1 IS FOR CURRENT DTYPE
!
         iapp = iapp/10
         IF ( iapp/=japp ) GOTO 780
         CALL fread(Oes1,0,-2,0)
         CALL fread(Oes1,i,1,0)
         IF ( disp ) THEN
            IF ( i/=defid ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         Subcas = i
         v = value
         CALL fread(Oes1,err(1),4,0)
         IF ( dtype==1 ) THEN
!
!     STATICS
!
            j = err(4)
         ELSEIF ( dtype>=4 ) THEN
!
!     MODAL
!
            j = err(1)
            v = frr(2)
            IF ( dtype==4 .AND. iapp==2 ) v = sqrt(abs(v))/twopi
         ELSE
!
!     TRANSIENT
!
            v = frr(1)
            j = err(4)
         ENDIF
         IF ( .NOT.disp ) THEN
            IF ( v>=v1 .AND. v<=v2 ) THEN
               Data = v
               n = 1
               SPAG_Loop_1_3: DO WHILE ( Deflst(n)/=all )
                  CALL intlst(Deflst,n,i,d1,d2)
                  IF ( Subcas>=d1 .AND. Subcas<=d2 ) EXIT SPAG_Loop_1_3
                  IF ( n>=n1 ) THEN
                     spag_nextblock_1 = 15
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_1_3
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ACCOUNT FOR ROUNDOFF
!
         ELSEIF ( abs(v-value)<=1.0E-6 ) THEN
            Data = value
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
!
!     WRONG CASE
!
         CALL fwdrec(*780,Oes1)
         CALL fwdrec(*780,Oes1)
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
!
!     LOCATED CASE TO PLOT
!
         CALL bckrec(Oes1)
         loadid = j
         defid = Subcas
         value = Data
         spag_nextblock_1 = 17
      CASE (17)
!
         CALL gopen(Casecc,Deflst(Buf1),inprew)
         DO
            CALL read(*440,*440,Casecc,n,1,0,i)
            IF ( n==defid ) THEN
               CALL fread(Casecc,0,-skplod,0)
               CALL fread(Casecc,thlid,1,0)
               IF ( loadid==0 ) loadid = thlid
               skpttl = 31
               CALL fread(Casecc,0,-skpttl,0)
               CALL fread(Casecc,Title,3*32,0)
               CALL close(Casecc,rew)
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL fread(Casecc,0,0,1)
            ENDIF
         ENDDO
 440     CALL close(Casecc,rew)
         IF ( .NOT.disp ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL fread(mdef,0,0,1)
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
!
!     IDENTIFY THE PLOT
!
         Pltnum = Pltnum + 1
         IF ( stress ) CALL close(Oes1,norew)
         CALL sopen(*820,Plttap,Deflst(pltbuf),Pbufsz)
         Ncntr = -iabs(Ncntr)
         IF ( nplots==0 ) CALL pltopr
         nplots = nplots + 1
         stereo = 0
         mtyp = 0
         err(2) = Pltnum
         IF ( .NOT.(disp .OR. stress) ) THEN
            err(1) = 1
            CALL wrtprt(Merr,err,f1,nf(1))
         ELSE
            err(3) = mf1(1,dtype)
            err(4) = mf1(2,dtype)
            IF ( Icntvl==20 ) plttyp = 4
            err(5) = mf2(1,plttyp)
            err(6) = mf2(2,plttyp)
            err(7) = defid
            err(8) = loadid
            err(9) = load
            IF ( dtype/=1 ) THEN
               err(1) = 12
               IF ( dtype>3 ) err(9) = imod
               frr(10) = value
               mtyp = 1
               IF ( dtype==3 ) mtyp = 3
               IF ( dtype==4 .AND. loadid<0 ) mtyp = 2
               IF ( mtyp==2 ) err(8) = -loadid
               err(11) = mf3(1,mtyp)
               err(12) = mf3(2,mtyp)
               err(13) = mf3(3,mtyp)
               IF ( dtype/=3 .AND. dtype/=4 ) THEN
                  err(1) = 15
                  m = 0
                  IF ( mag/=0 ) m = 3
                  err(14) = mf4(m+1)
                  err(15) = mf4(m+2)
                  err(16) = mf4(m+3)
                  IF ( mag==0 ) THEN
                     err(1) = 16
                     frr(17) = ph
                  ENDIF
               ENDIF
            ELSE
               err(1) = 8
            ENDIF
            CALL wrtprt(Merr,err,f2,nf(2))
         ENDIF
         CALL stplot(Pltnum)
         CALL head(dtype,plttyp,mtyp,err)
         spag_nextblock_1 = 19
      CASE (19)
!
!     PLOT EACH SET REQUESTED. INTERPRET THE ASSOCIATED REQUESTS.
!
         CALL rdmody(Deflst(n1+1),Mode,word)
         Mode = 0
         maxdef = 0.
         Porig = 1
         Ppen = 1
         Pset = 0
         spag_nextblock_1 = 20
      CASE (20)
         Plabel = -1
         Pcon = lpcon
         Pshape = 1
         Pvectr = 0
         Offlag = 0
         Pedge = 0
         Psymbl(1) = 0
         Psymbl(2) = 0
         Psymm(1) = 1
         Psymm(2) = 1
         Psymm(3) = 1
         Psymm(4) = 1
         Psymm(5) = 1
         Psymm(6) = 1
 460     IF ( Mode<=0 ) CALL rdmode(*460,*480,*720,Mode,word)
 480     CALL rdword(Mode,word)
         spag_nextblock_1 = 21
      CASE (21)
!
!     CHECK FOR THE KEYWORD. THIS MAY BE FOLLOWED BY QUALIFIERS
!
         DO keywd = 1 , 17
            IF ( word==rqst(keywd) ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
         IF ( keywd==1 ) THEN
!
!     SET -  SAVE FIRST ENCOUNTERED, DO PLOT WHEN EOR OR ANOTHER SET
!
            IF ( Mode/=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 640 TO tra
!
!     READ AN INTEGER VALUE FROM THE -PLOT- CARD
!
            CALL rdmode(*840,*480,*720,Mode,word)
            GOTO 840
         ELSEIF ( keywd==2 ) THEN
!
!     ORIGIN I
!
            IF ( Mode/=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 600 TO tra
            CALL rdmode(*840,*480,*720,Mode,word)
            GOTO 840
         ELSEIF ( keywd==3 ) THEN
!
!     SHAPE
!
            IF ( Pedge/=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( (.NOT.(disp .OR. stress) .AND. dtype/=0) ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( disp ) THEN
               Pshape = 2
               DO i = 1 , ndef
                  IF ( Deflst(i)==0 ) THEN
                     spag_nextblock_1 = 24
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            GOTO 460
         ELSEIF ( keywd==4 ) THEN
!
!     SYMBOL I,I
!
            Psymbl(1) = 1
            IF ( Mode/=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 620 TO tra
            i = 0
            i = i + 1
            CALL rdmode(*840,*480,*720,Mode,word)
            GOTO 840
         ELSEIF ( keywd==5 ) THEN
!
!     LABEL GRID / ELEMENTS
!
            Plabel = 0
            IF ( Mode<=0 ) CALL rdmode(*460,*520,*720,Mode,word)
            GOTO 520
         ELSEIF ( keywd==6 ) THEN
!
!     VECTOR B
!
            IF ( .NOT.disp .OR. Mode==0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(Mode,word)
            Pvectr = word
            GOTO 460
         ELSEIF ( keywd==7 .OR. keywd==8 ) THEN
!
!     DENSITY I, PEN I
!
            IF ( Mode/=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 500 TO tra
            CALL rdmode(*840,*480,*720,Mode,word)
            GOTO 840
         ELSEIF ( keywd==9 .OR. keywd==10 ) THEN
!
!     SYMMETRY B / ANTISYMMETRY B
!
            n = 1
            IF ( keywd==10 ) n = -1
            IF ( Mode<=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(Mode,word)
            CALL intvec(word)
            IF ( word<1 .OR. word>7 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = 1 , 3
               Psymm(i) = 1
               IF ( andf(word,2**(i-1))/=0 ) Psymm(i) = -1
               Psymm(i+3) = n*Psymm(i)
            ENDDO
            GOTO 460
         ELSEIF ( keywd==11 ) THEN
!
!     MAXIMUM DEFORMATION X.X
!
            ASSIGN 580 TO tra
            IF ( Mode<=0 ) CALL rdmode(*860,*560,*720,Mode,word)
            GOTO 560
         ELSEIF ( keywd==12 ) THEN
!
!     OUTLINE
!
            IF ( Pshape/=1 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Pcon==0 ) GOTO 460
            Pedge = 1
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( keywd==13 ) THEN
!
!     HIDDEN
!
            IF ( Pedge<10 ) Pedge = 2
            IF ( Pedge>=10 .AND. Pedge<=100 ) Pedge = 200 + Pedge
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( keywd==14 ) THEN
!
!     SHRINK
!
            IF ( Pedge/=2 ) Pedge = 75
            IF ( Pedge==2 ) Pedge = 75 + 200
!                           SHRINK + HIDDEN
!
            IF ( Mode>0 ) GOTO 460
            CALL rdmode(*680,*660,*720,Mode,word)
            GOTO 660
         ELSEIF ( keywd==15 ) THEN
!
!     NOFIND
!
!     COMMENTS FROM G.CHAN/UNISYS  11/1990
!     THE 'NOFIND' FEATURE IN NASTRAN PLOTTING COMMANDS IS REALLY NOT
!     NEEDED. IT ONLY LIMITS TO PREVIOUS PLOT CASE. THE FOLLOWING TWO
!     EXAMPLES GIVE EXACTLY THE SAME RESULT IN $ PLOT 2
!
!     $ PLOT 1                           $ PLOT 1
!     FIND SCALE, ORIGIN 100, SET 2      FIND SCALE, ORIGIN 100, SET 2
!     PLOT ORIGIN 100                    PLOT ORIGIN 100
!     $ PLOT 2                           $ PLOT 2
!     PLOT ORIGIN 100                    PLOT NOFIND
!       :
!     (NOTE - ORIGIN 100 IS STILL AVAILABLE
!      IN ANY FOLLOWING PLOT)
!     $ PLOT N
!     PLOT ORIGIN 100
!
            Nofind = +1
            IF ( lorig==0 ) THEN
               WRITE (Nout,99001) Uwm , lorig
99001          FORMAT (A25,' 704, NO PREVIOUS PLOT TO INITIATE NOFIND OPERATION')
               RETURN
            ELSE
               Porig = lorig
               GOTO 460
            ENDIF
         ELSEIF ( keywd==17 ) THEN
!
!     OFFSET n
!     TURN OFFSET PLOT ON  IF n IS .GE. 0. +n IS MAGNIFYING FACTOR
!     TURN OFFSET PLOT OFF IF n IS .LT. 0
!
!
            IF ( Mode/=0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 700 TO tra
            CALL rdmode(*840,*480,*720,Mode,word)
            GOTO 840
         ELSE
!
!             SET ORIG SHAP SYMB LABE VECT DENS  PEN SYMM ANTI
!    1       MAXI OUTL HIDD SHRI NOFI FILL OFFS
!
!     FILL ELEMENTS BY SET HERE
!     FILL PRESENTLY DOES NOT WORK TOGETHER WITH SHRINK AND HIDDEN
!
            Ppen = Ppen + 31
            Pedge = 100
            GOTO 460
         ENDIF
 500     Ppen = iwrd
         GOTO 460
 520     CALL rdword(Mode,word)
         IF ( word==both ) THEN
            Plabel = 6
            GOTO 460
         ELSEIF ( word==elem ) THEN
            Plabel = 3
            GOTO 460
         ELSEIF ( word/=grid ) THEN
            IF ( word==gspc ) Plabel = 1
            IF ( word==epid ) Plabel = 4
            IF ( Plabel/=0 ) GOTO 460
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( Mode<=0 ) CALL rdmode(*460,*540,*720,Mode,word)
         ENDIF
 540     CALL rdword(Mode,word)
         IF ( word/=poin ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 460
 560     CALL rdword(Mode,word)
         IF ( word/=defo .OR. Mode/=0 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ A REAL VALUE FROM THE -PLOT- CARD
!
         CALL rdmode(*860,*480,*720,Mode,word)
         GOTO 860
 580     maxdef = abs(fwrd)
         GOTO 460
 600     DO i = 1 , Org
            IF ( Origin(i)==iwrd ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         IF ( stereo==0 ) THEN
            err(1) = 1
            err(2) = iwrd
            CALL wrtprt(Merr,err,msg2,nmsg2)
         ENDIF
         GOTO 460
      CASE (23)
         Porig = i
         GOTO 460
      CASE (24)
         Pshape = 3
         GOTO 460
 620     Psymbl(i) = iwrd
         IF ( i>=2 ) GOTO 460
         i = i + 1
         CALL rdmode(*840,*480,*720,Mode,word)
         GOTO 840
 640     iwrd = iabs(iwrd)
         DO i = Setd , Nsets
            IF ( iwrd==Setid(i) ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         IF ( stereo==0 ) THEN
            WRITE (Nout,99002) Ufm , iwrd
99002       FORMAT (A23,' 700, SET',I9,' REQUESTED ON PLOT CARD HAS NOT BEEN',' DEFINED.')
            nogo = 1
         ENDIF
         iwrd = Setd
         spag_nextblock_1 = 26
         CYCLE SPAG_DispatchLoop_1
      CASE (25)
         iwrd = i
         spag_nextblock_1 = 26
      CASE (26)
         IF ( Pset/=0 ) GOTO 720
         Pset = iwrd
         GOTO 460
 660     CALL rdword(Mode,word)
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 680     IF ( Mode/=-2 .OR. fwrd<=0.0 .OR. fwrd>1.0 ) THEN
            WRITE (Nout,99003) Uwm
99003       FORMAT (A25,', INPUT VALUE ERROR FOR SHRINK.  0.85 IS SUBSTITUED')
            IF ( Mode==-1 ) WRITE (Nout,99004) iwrd
99004       FORMAT (5X,'FOR INTEGER VALUE',I5)
            fwrd = 0.85
         ENDIF
         j = fwrd*100
         IF ( j<10 ) j = 10
         IF ( j>100 ) j = 100
         IF ( Pedge/=2 ) Pedge = j
!                          SHRINK + HIDDEN
!
         IF ( Pedge==2 ) Pedge = j + 200
         spag_nextblock_1 = 27
      CASE (27)
!                                              HIDDEN + SHRINK
         IF ( disp ) THEN
            DO i = 1 , ndef
               IF ( Deflst(i)==0 ) THEN
                  spag_nextblock_1 = 28
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            Pshape = 2
         ENDIF
         GOTO 460
      CASE (28)
         Pshape = 3
         GOTO 460
 700     Offscl = iwrd
         IF ( Offscl>=0 ) Pedge = 3
         GOTO 460
      CASE (29)
!
!     UNRECOGNIZABLE OPTION ON THE -PLOT- CARD.
!
         IF ( stereo==0 ) THEN
            err(1) = 2
            err(2) = awrd(1)
            err(3) = awrd(2)
            CALL wrtprt(Merr,err,msg1,nmsg1)
         ENDIF
         GOTO 460
!
!
 720     IF ( Nofind<0 ) THEN
            IF ( fscale==0 .AND. For==0 ) THEN
               IF ( Prject==1 .OR. fvp==0 ) THEN
                  spag_nextblock_1 = 30
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            forg = 1
            fscale = 1
            isetd = Setd
            Setd = max0(Setd,Pset)
            modex = Mode
            Mode = -1
            Org = max0(1,Org)
            CALL find(Mode,Buf1,B1,Setid,Deflst)
            Nofind = +1
            Setd = isetd
            Mode = modex
         ENDIF
         spag_nextblock_1 = 30
      CASE (30)
!
!     PLOT THIS SET
!
         IF ( disp ) THEN
            IF ( Pvectr==0 .AND. Pshape==1 .AND. Pedge==0 ) THEN
               IF ( Pcon==0 .OR. Icntvl<=9 ) THEN
                  IF ( Pcon==0 .OR. Icntvl<=13 ) THEN
!
!     CREATE A DEFAULT OF SHAPE OR SHAPE + UNDERLAY
!
                     DO i = 1 , ndef
                        IF ( Deflst(i)==0 ) GOTO 722
                     ENDDO
                     Pshape = 2
                  ENDIF
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
 722              Pshape = 3
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 31
      CASE (31)
         Pset = max0(Pset,Setd)
!
!     DEFAULT OF FIRST DEFINED SET WILL BE USED
!
         CALL gopen(Gpset,Deflst(B1),inprew)
         CALL skprec(Gpset,Pset)
         CALL fread(Gpset,Ngpset,1,0)
!
!     TEST FOR CORE NEEDED FOR BOTH UNDEF, DEFOR PLOTS, GRID INDEX
!
         i1 = n2 + Ngp + 1
!
!     UNDEFORMED COORDINATES
!
         i2 = i1 + 3*Ngpset
!
!     DEFORMATION VALUES
!
         i3 = i2 + 3*Ngpset
!
!     REDUCE CORE FOR UNDEFORMED PLOTS
!
         IF ( disp ) THEN
!
!     DEFORMED PLOTS NEED X-Y LOCATIONS OF RESULTANT DEFLECTIONS ON
!     FRAME
!
            n = 2*Ngpset
         ELSE
            i3 = i2
            n = 0
         ENDIF
!
         IF ( i3+n-1>=defbuf ) THEN
            CALL mesage(-8,defbuf,name)
            GOTO 780
         ELSE
            Iused = max0(i3+n-1,Iused+Ngp)
!
            CALL fread(Gpset,Deflst(n2+1),Ngp,0)
            CALL close(Gpset,rew)
            CALL fndset(Deflst(n2+1),Deflst(i1),Buf1-n2,0)
!
            CALL gopen(Elset,Deflst(B1),inprew)
            IF ( Pset/=1 ) CALL skprec(Elset,Pset-1)
!
            IF ( stress ) THEN
               IF ( Icntvl>=4 .AND. Direct==2 ) THEN
                  i = B1 + Bufsiz
                  CALL close(Parm,norew)
                  CALL gopen(Oes1,Deflst(i),norew)
!
                  CALL rotat(Elset,Buf1-n2,Deflst(n2+1),Deflst(i1))
!
                  CALL close(Oes1,norew)
                  CALL gopen(Parm,Deflst(i),norew)
               ENDIF
            ENDIF
!
            IF ( disp ) THEN
!
!     CONVERSION FOR ACCEL OR VELOCITY
!
               conv = 1.0
               IF ( plttyp/=1 ) THEN
                  IF ( plttyp==3 .OR. plttyp==4 ) THEN
!
!     ACCEL
!
                     conv = (value*twopi)**2
                  ELSE
!
!     VELOCITY
!
                     conv = value*twopi
                  ENDIF
               ENDIF
               i = 3*Bufsiz + B1
               ph1 = ph*twopi/360.0
               CALL getdef(mdef,ph1,mag,conv,plttyp,Deflst(i),Deflst(n2+1),Deflst(i2))
!                  FILE PH  MAG   W   RESP   BUF(1)     GPLST
!                  DEFLECTION
!
!     PRINT THE MAXIMUM FOUND ON THE PLOT FILE
!
               IF ( Mode>=eor .AND. Icolor==0 ) CALL head(0,0,-1,Defmax)
               ASSIGN 740 TO incom
               IF ( maxdef/=0.0 ) Defmax = maxdef
               IF ( Defmax==0.0 .OR. Scale(4)==0.0 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
!                GPLST       ,X         ,U         ,S         ,
 740     CALL draw(Deflst(n2+1),Deflst(i1),Deflst(i2),Deflst(i3),disp,stereo,defbuf-(i3+n),Buf1-n2)
!
!     NOTE - THE NEXT TO LAST ARGUMENT, DEFBUF-(I3+N), IS THE SIZE OF
!            AVAILABLE OPEN CORE. IT IS NOT A POINTER, AND IT IS NOT AN
!            OPEN CORE ARRAY
!
!     OPEN CORE /ZZPLOT/
!     SETID NSETS NDOF      NGP 3*NGPSET 3*NGPSET SCRATCH  N
!     -----+-----+----+----+---+--------+--------+-------+--+--+-+-+-+-+
!          !          N1   N2  I1 (X)   I2 (U)   I3 (S)   DEFBUF ..BUF..
!          !(DEFLST)         /
!                       (GPLST)                      N=2*NGPSET
!
         CALL close(Elset,rew)
         IF ( Mode<eor ) THEN
            IF ( disp ) CALL bckrec(mdef)
            Pset = iwrd
            IF ( .NOT.stress ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     POSITION OES1
!
            i = 1
            ASSIGN 760 TO incom
            CALL fndset(Deflst(n2+1),Deflst(i1),Buf1-n2,i)
            IF ( i==1 ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 32
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF A DEFORMATION
!
 760     CALL stplot(-1)
         IF ( Prject==3 .AND. stereo==0 ) THEN
            stereo = 1
            CALL sopen(*820,Plttap,Deflst(pltbuf),Pbufsz)
            j = Bfrms
            Bfrms = 2
            CALL stplot(Pltnum)
            Bfrms = j
            Pltnum = Pltnum + 1
            IF ( disp ) CALL bckrec(mdef)
            IF ( .NOT.stress ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     POSITION OES1
!
            i = 1
            ASSIGN 760 TO incom
            CALL fndset(Deflst(n2+1),Deflst(i1),Buf1-n2,i)
            IF ( i/=1 ) THEN
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( disp .OR. stress ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF THIS PLOT CARD.
!
 780     IF ( stress ) CALL close(Oes1,rew)
 800     IF ( disp ) CALL close(mdef,rew)
         GOTO 820
      CASE (32)
!
!
!     INCOMPLETE PLOT RESULTED
!
         err(1) = 0
         CALL wrtprt(Merr,err,msg7,nmsg7)
         GOTO incom
!
!     FINISHING ONE PLOT
!     ECHO OUT WHICH ORIGIN WAS USED
!
 820     IF ( nogo/=0 ) CALL mesage(-61,0,0)
         IF ( Porig/=0 ) THEN
            err(1) = 1
            err(2) = Origin(Porig)
            CALL wrtprt(Merr,err,used,10)
            CALL write(Merr,0,0,1)
            lorig = Porig
            Porig = 0
         ENDIF
         RETURN
 840     IF ( Mode/=-1 ) THEN
            IF ( Mode==-4 ) THEN
               iwrd = dwrd
            ELSE
               iwrd = fwrd
            ENDIF
         ENDIF
         GOTO tra
 860     IF ( Mode==-4 ) THEN
            fwrd = dwrd
         ELSE
            IF ( Mode==-1 ) fwrd = iwrd
         ENDIF
         GOTO tra
!
 880     WRITE (Nout,99005) Ufm , Plttap
99005    FORMAT (A23,' 702, PLOT FILE ',A4,' DOES NOT EXIST.')
         nogo = 1
         GOTO 800
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE plot
