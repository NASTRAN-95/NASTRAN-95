
SUBROUTINE param(Setid,Xx,Buf4)
   IMPLICIT NONE
   INTEGER Axis(3) , Bframs , Bufsiz , Camera , Color , Daxis(3) , Direct , For , Fpltit , Fscale , Fvp , Icntvl , Ksystm(65) ,     &
         & Lasset , Layer , Merr , Model , Ncntr , Norg , Npens , Oesx , Org , Origin(11) , Paptyp(2) , Parm , Penclr(8,2) ,        &
         & Pensiz(8) , Ploter , Pltbuf , Pltitl(17) , Pltmod(2) , Pltype , Prject , Prnt , Tapden , Title(96) , Where
   REAL Chrscl , Cntin3 , Cntr(50) , Cntsin , D02 , D03 , Data , Defmax , Edge(11,4) , Flag , Maxdef , Papsiz(2) , Penpap , S0s ,   &
      & Scale(2) , Skp11(3) , Skp12(6) , Skp2(9) , Skpa(2) , Skpd1(6) , Skpd2(3) , Skpit(2) , Skpplt(17) , Skpvue(6) , Subcas ,     &
      & Vangle(3) , Vanpnt(5) , Xy(11,3)
   COMMON /blank / Skp11 , Prnt , Skp12 , Parm , Skp2 , Merr , Skpit , Oesx
   COMMON /output/ Title
   COMMON /pltdat/ Model , Ploter , Skpplt , Chrscl , Skpa , Cntsin , Skpd1 , Pltype , Skpd2 , Cntin3
   COMMON /system/ Ksystm
   COMMON /xxparm/ Pltbuf , Camera , Bframs , Pltmod , Tapden , Npens , Papsiz , Paptyp , Pensiz , Penclr , Penpap , Scale ,        &
                 & Fscale , Maxdef , Defmax , Axis , Daxis , Vangle , Skpvue , Fvp , Vanpnt , D02 , D03 , Prject , S0s , For , Org ,&
                 & Norg , Origin , Edge , Xy , Ncntr , Cntr , Icntvl , Where , Direct , Subcas , Flag , Data , Lasset , Fpltit ,    &
                 & Pltitl , Color , Layer
   INTEGER Buf4
   INTEGER Setid(1) , Xx(1)
   INTEGER anti , awrd(2) , axisd(7) , blank , both , bpi , buf1 , by , colo , comm , defo , dens , disp , eor , err(3) , even ,    &
         & fill , film , fram , hkey(19) , hmode , hplot(2) , hx , i , icnda(20) , id , iwrd , j , k , kwrd , laye , list , loca ,  &
         & max , mid , mode , modex , msg1(20) , msg2(20) , msg4(22) , msg5(16) , n , nkwd(3) , nmsg1 , nmsg2 , nmsg4 , nmsg5 ,     &
         & nofind , oes1 , oes1l , onrgy1 , pape , plan , pltnam(2) , poin , savtit(96) , sepa , size , stre , symm , tra , type ,  &
         & word , z1 , z2
   REAL blank4 , fwrd , x , y
   DOUBLE PRECISION dwrd
   LOGICAL test
!
!     THIS PARAM ROUTINE IS CALLED ONLY BY DPLOT, WHICH IS THE DRIVER
!     OF THE PLOT MODULE           ==== == =====
!
!     THE DRIVER FOR THE PARAM MODULE IS QPARAM
!
   EQUIVALENCE (Ksystm(1),Bufsiz) , (pape,hkey(10)) , (word,awrd(1),dwrd,fwrd,iwrd)
!
!     THE FOLLOWING ARE THE ALLOWABLE FIRST WORDS ON THE LOGICAL CARD.
!     THE PROJECTION DETERMINES HOW MANY WORDS ARE CHECKED.
!
!     OES1   IS THE NORMAL STRESS FILE, 111
!     OES1L  IS THE LAYER COMPOSITE STRESS FILE, 112
!     ONRGY1 IS THE ELEMENT  STRAIN ENERGY FILE, 113
!
   DATA oes1 , oes1l , onrgy1/111 , 112 , 113/
   DATA nkwd/17 , 19 , 19/ , blank4/4H    /
   DATA hkey/4HFIND , 4HVIEW , 4HAXES , 4HMAXI , 4HORTH , 4HPERS , 4HSTER , 4HCONT , 4HCAME , 4HPAPE , 4HPEN  , 4HBLAN , 4HORIG ,   &
       &4HSCAL , 4HCSCA , 4HPROJ , 4HPTIT , 4HOCUL , 4HVANT/
!
!     THE FOLLOWING ARE RECOGNIZABLE PARAMETERS
!
!
!     CONTOUR PLOTTING
!
   DATA axisd/2HMZ , 2HMY , 2HMX , 0 , 1HX , 1HY , 1HZ/ , anti/4HANTI/ , both/4HBOTH/ , bpi/4HBPI / , by/4HBY  / , colo/4HCOLO/ ,   &
      & defo/4HDEFO/ , dens/4HDENS/ , film/4HFILM/ , fram/4HFRAM/ , hmode/4HMODE/ , hplot/4HPLOT , 4HTER / , hx/4HX   / ,           &
       &plan/4HPLAN/ , poin/4HPOIN/ , sepa/4HSEPA/ , size/4HSIZE/ , symm/4HSYMM/ , type/4HTYPE/ , disp/4HDISP/ , stre/4HSTRE/ ,     &
      & even/4HEVEN/ , laye/4HLAYE/ , list/4HLIST/ , z1/2HZ1/ , z2/2HZ2/ , max/3HMAX/ , mid/3HMID/ , comm/4HCOMM/ , loca/4HLOCA/ ,  &
      & fill/4HFILL/
!
   DATA icnda/4HMAJP , 4HMINP , 4HMAXS , 4HXNOR , 4HYNOR , 4HZNOR , 4HXYSH , 4HXZSH , 4HYZSH , 4HXDIS , 4HYDIS , 4HZDIS , 4HMAGN ,  &
       &4HNRM1 , 4HNRM2 , 4HSH12 , 4HSH1Z , 4HSH2Z , 4HBDSH , 4HSTRA/
!
   DATA eor , blank/1000000 , 1H / , nmsg5 , msg5/16 , 4H(25X , 4H,31H , 4HMORE , 4H THA , 4HN 50 , 4H CON , 4HTOUR , 4HS SP ,      &
       &4HECIF , 4HIED, , 4H1P,E , 4H14.6 , 4H,9H  , 4HREJE , 4HCTED , 4H)   /
   DATA nmsg1/20/
   DATA msg1/4H(34X , 4H,45H , 4HAN A , 4HTTEM , 4HPT H , 4HAS B , 4HEEN  , 4HMADE , 4H TO  , 4HDEFI , 4HNE M , 4HORE  , 4HTHAN ,   &
       &4H ,I2 , 4H,17H , 4H DIS , 4HTINC , 4HT OR , 4HIGIN , 4HS)  /
   DATA nmsg2/20/
   DATA msg2/4H(30X , 4H,34H , 4HAN U , 4HNREC , 4HOGNI , 4HZABL , 4HE PL , 4HOT P , 4HARAM , 4HETER , 4H (,2 , 4HA4,2 , 4H9H)  ,   &
       &4HHAS  , 4HBEEN , 4H DET , 4HECTE , 4HD -  , 4HIGNO , 4HRED)/
   DATA nmsg4/22/
   DATA msg4/4H(25X , 4H,4HP , 4HEN , , 4HI4,6 , 4H9H I , 4HS NO , 4HT A  , 4HLEGA , 4HL PE , 4HN NU , 4HMBER , 4H FOR , 4H THI ,   &
       &4HS PL , 4HOTTE , 4HR. P , 4HEN 1 , 4H WIL , 4HL BE , 4H RED , 4HEFIN , 4HED.)/
   DATA test/.FALSE./
!
!
!     COMMENTS FROM G.CHAN/UNISYS ABOUT THE NOFIND FLAG       11/1990
!
!     THE NOFIND FLAG WAS TOO CONFUSING BEFORE. I'M SETTING THE NEW RULE
!     HERE
!
!     NOFIND FLAG IS USED IN PARAM AND PLOT ROUTINES ONLY. ITS USE IS
!     TO INDICATE WHETHER SUBROUTINE FIND SHOULD BE CALLED.
!     (SUBROUTINE FIND COMPUTES THE NEW ORIGIN, FRAME SIZE, NEW VIEW,
!     VANTAGE POINT ETC. DUE TO CERTAIN PLOT PARAMETERS).
!     NOFIND FLAG CAN BE SET BY USER VIA THE FIND AND NOFIND COMMANDS,
!     OR IT IS SET AUTOMATICALLY BY THIS PARAM SUBROUTINE.
!
!      NOFIND                  ACTION
!     --------    ----------------------------------------------------
!        -1       FIND ROUTINE SHOULD BE CALLED IN NEXT OPPORTUNITY
!                 BEFORE THE ACTUAL PLOTTING
!        +1       (1) A NOFIND CARD WAS ENCOUNTERED. USER WANTS TO KEEP
!                 ALL PARAMETERS AS IN THE PREVIOUS PLOT CASE, OR
!                 (2) FIND ROUTINE WAS JUST CALLED. PROGRAM SHOULD NOT
!                 CALL FIND AGAIN
!         0       THE CURRENT STATUS OF ALL PARAMETERS THAT WERE FOUND
!                 BY PREVIOUS FIND REMAIN UNCHANGED. HOWEVER, ANY
!                 CHANGE IN THE PLOT PARAMETERS BY THE USER (SCALE,
!                 CSCALE, VIEW, VENTAGE POINT, REGION, ORIGIN, PLOTTER,
!                 MAX.DEFORMATION, PROJECTION AND PAPER SIZE) WILL
!                 CHANGE NOFIND FLAG TO -1
!
!     IF A FIND COMMAND IS ENCOUNTERED, SUBROUTINE FIND IS CALLED
!     IMMEDIATELY AND UNCONDISIONALLY, THEN NOFIND FLAG IS SET TO +1
!
!     IF USER HAS ALREADY ONE OR MORE ORIGINS, AND IF HE USES A FIND
!     CARD TO FIND ANOTHER ORIGIN, BUT THE NEXT PLOT CARD DOES NOT USE
!     THIS NEWLY DEFINED ORIGIN, A WARNING MESSAGE SHOULD BE ISSUED TO
!     INFORM THE USER THAT THE DEFAULT ORIGIN, WHICH IS THE FIRST
!     DEFINDED ORIGIN, IS GOING TO BE USED, NOT THE ONE HE JUST DEFINED
!
   nofind = -1
   Lasset = 0
   CALL pltset
   buf1 = Buf4 + 3*Bufsiz
!
!     SAVE THE TITLE, SUBTITLE AND LABEL IF DEFORMED PLOTS ...
!
   IF ( Prnt>=0 ) THEN
      CALL rdmodx(Parm,mode,word)
   ELSE
      DO i = 1 , 96
         savtit(i) = Title(i)
      ENDDO
      nofind = 0
      CALL rdmodx(Parm,mode,word)
   ENDIF
 100  DO
      CALL read(*6100,*6100,Parm,mode,1,0,i)
      IF ( mode<0 ) THEN
         i = 1
         IF ( mode==-4 ) i = 2
         CALL fread(Parm,0,-i,0)
      ELSEIF ( mode/=0 ) THEN
         IF ( mode<eor ) THEN
            mode = mode + 1
            CALL rdword(mode,word)
            CALL rdword(mode,word)
            IF ( awrd(1)/=hplot(1) ) GOTO 700
            IF ( awrd(2)==blank ) THEN
!
!     PLOT
!
               IF ( test ) GOTO 400
!
!         WHEN PLOTTER OR PROJECTION WERE HIT
!              FSCALE=FOR=FVP=1
!              PROJECTION=KWRD-4, SOME NUMBER
!         WHEN SCALE IS HIT,       FSCALE SET TO 0
!         WHEN VANTAGE POINT IS HEIT, FVP SET TO 0
!         WHEN ORIGIN IS HIT,         ORG SET TO 0
!
               IF ( Fscale/=0 .OR. For/=0 ) GOTO 300
               IF ( Prject/=1 .AND. Fvp/=0 ) GOTO 300
               GOTO 400
            ELSE
               IF ( awrd(2)/=hplot(2) ) GOTO 6000
!
!     PLOTTER
!
               IF ( mode==0 ) GOTO 6000
               CALL rdword(mode,word)
               pltnam(1) = awrd(1)
               pltnam(2) = awrd(2)
               Pltmod(1) = 0
               Pltmod(2) = 0
               Camera = 2
               Fscale = 1
               Fvp = 1
               For = 1
               IF ( Org/=0 ) THEN
                  DO i = 1 , Org
                     Edge(i,1) = 0.
                     Edge(i,2) = 0.
                     Edge(i,3) = 1.
                     Edge(i,4) = 1.
                  ENDDO
                  Org = 0
               ENDIF
!
!     CHECK FOR A MODEL NUMBER
!
               ASSIGN 3700 TO tra
               j = 1
               IF ( mode<=0 ) CALL rdmode(*6200,*3500,*3800,mode,word)
               GOTO 3500
            ENDIF
         ELSE
            CALL fread(Parm,0,0,1)
         ENDIF
      ENDIF
   ENDDO
!
!     FIND
!
 200  CALL find(mode,buf1,Buf4,Setid,Xx)
   nofind = +1
   IF ( mode>=0 ) THEN
      CALL rdmodx(Parm,mode,word)
      GOTO 100
   ELSE
      mode = modex
      GOTO 400
   ENDIF
 300  modex = mode
   mode = -1
   Org = max0(1,Org)
   GOTO 200
 400  CALL plot(mode,buf1,Buf4,Setid,Xx,nofind)
   Oesx = oes1
   IF ( nofind==-1 ) Org = max0(1,Org)
   nofind = 0
   CALL rdmodx(Parm,mode,word)
   GOTO 100
!
!     PLOT PARAMETER CARD.
!
 500  IF ( mode<=0 ) CALL rdmode(*500,*600,*100,mode,word)
 600  CALL rdword(mode,word)
 700  i = nkwd(Prject)
   DO kwrd = 1 , i
      IF ( hkey(kwrd)==word ) GOTO 800
   ENDDO
   GOTO 6000
!
 800  IF ( kwrd==1 ) GOTO 200
   IF ( kwrd==2 ) THEN
!
!     VIEW
!
      IF ( mode/=0 ) GOTO 6000
      ASSIGN 4900 TO tra
      j = 4
      j = j - 1
!
!     READ A DECIMAL NUMBER ON A PARAMETER CARD
!
      CALL rdmode(*6300,*500,*100,mode,word)
      GOTO 6300
   ELSEIF ( kwrd==3 ) THEN
!
!     AXES
!
      DO j = 1 , 3
         IF ( mode==0 ) CALL rdmode(*500,*820,*100,mode,word)
 820     CALL rdword(mode,word)
         DO i = 1 , 7
            IF ( word==axisd(i) ) GOTO 840
         ENDDO
         GOTO 1100
 840     Axis(j) = i - 4
      ENDDO
      IF ( mode==0 ) CALL rdmode(*1200,*1000,*1200,mode,word)
      GOTO 1000
   ELSEIF ( kwrd==4 ) THEN
!
!     MAXIMUM DEFORMATION
!
      IF ( mode<=0 ) GOTO 6000
      CALL rdword(mode,word)
      IF ( word/=defo .OR. mode/=0 ) GOTO 6000
      ASSIGN 1800 TO tra
      CALL rdmode(*6300,*500,*100,mode,word)
      GOTO 6300
   ELSEIF ( kwrd==5 .OR. kwrd==6 .OR. kwrd==7 ) THEN
      GOTO 900
   ELSEIF ( kwrd==8 ) THEN
!
!     CONTOUR
!
!     RESTORE DEFAULTS
!
      Icntvl = 1
      Ncntr = 10
      Color = 0
      Layer = 0
      Where = 1
      Direct = 2
      Cntr(1) = 0.0
      Cntr(2) = 0.0
      GOTO 5000
   ELSEIF ( kwrd==9 ) THEN
!
!     CAMERA
!
      ASSIGN 1500 TO tra
      IF ( mode<=0 ) CALL rdmode(*6200,*1400,*100,mode,word)
      GOTO 1400
   ELSEIF ( kwrd==10 ) THEN
      GOTO 2500
   ELSEIF ( kwrd==11 ) THEN
!
!     PEN SIZE / COLOR
!
      IF ( mode/=0 ) GOTO 6000
      ASSIGN 3200 TO tra
!
!
!     READ AN INTEGER ON A PARAMETER CARD
!
      CALL rdmode(*6200,*500,*100,mode,word)
      GOTO 6200
   ELSEIF ( kwrd==12 ) THEN
!
!     BLANK FRAMES
!
      IF ( mode==0 ) GOTO 6000
      CALL rdword(mode,word)
      IF ( word/=fram .OR. mode/=0 ) GOTO 6000
      ASSIGN 1700 TO tra
      CALL rdmode(*6200,*500,*100,mode,word)
      GOTO 6200
   ELSEIF ( kwrd==13 ) THEN
!
!     ORIGIN
!
      IF ( mode/=0 ) GOTO 6000
      ASSIGN 2000 TO tra
      CALL rdmode(*6200,*500,*100,mode,word)
      GOTO 6200
   ELSEIF ( kwrd==14 ) THEN
!
!     SCALE
!
      IF ( mode/=0 ) GOTO 6000
      ASSIGN 4500 TO tra
      CALL rdmode(*6300,*500,*100,mode,word)
      GOTO 6300
   ELSEIF ( kwrd==15 ) THEN
!
!     CSCALE
!
      IF ( mode/=0 ) GOTO 6000
      ASSIGN 5900 TO tra
      CALL rdmode(*6300,*500,*100,mode,word)
      GOTO 6300
   ELSEIF ( kwrd==16 ) THEN
!
!     PROJECTION PLANE SEPARATION
!
      IF ( mode==0 ) GOTO 6000
      CALL rdword(mode,word)
!
!     USER MAY HAVE REVERSE ENGLISH
!
      IF ( word==plan ) THEN
         IF ( mode==0 ) GOTO 6000
         CALL rdword(mode,word)
         IF ( mode/=0 .OR. word/=sepa ) GOTO 6000
         ASSIGN 4400 TO tra
         CALL rdmode(*6300,*500,*100,mode,word)
         GOTO 6300
      ENDIF
   ELSEIF ( kwrd==17 ) THEN
!
!     PTITLE
!
      Fpltit = 1
      DO i = 1 , 17
         Pltitl(i) = blank4
      ENDDO
      j = Color
      DO i = 1 , 17 , 2
         CALL rdword(mode,word)
         Pltitl(i) = awrd(1)
         Pltitl(i+1) = awrd(2)
         IF ( mode==0 ) GOTO 500
      ENDDO
      Color = j
      IF ( mode/=0 ) CALL rdword(mode,word)
      GOTO 500
   ELSEIF ( kwrd==18 ) THEN
!
!     OCULAR SEPARATION
!
      IF ( mode<=0 ) GOTO 6000
      CALL rdword(mode,word)
      IF ( word/=sepa .OR. mode/=0 ) GOTO 6000
      ASSIGN 1900 TO tra
      CALL rdmode(*6300,*500,*100,mode,word)
      GOTO 6300
   ELSEIF ( kwrd==19 ) THEN
!
!     VANTAGE POINT
!
      IF ( mode==0 ) GOTO 6000
      CALL rdword(mode,word)
      IF ( word/=poin .OR. mode/=0 ) GOTO 6000
      ASSIGN 4800 TO tra
      j = 0
      GOTO 4700
   ENDIF
!
!           FIND  VIEW  AXES  MAXI  ORTH  PERS  STER  CONT  CAME  PAPE
!    1       PEN  BLAN  ORIG  SCAL  CSCA  PROJ  PTIT  OCUL  VANT
!
!
!     RECHECK IF PROJECTION CARD
!
   DO kwrd = 5 , 7
      IF ( word==hkey(kwrd) ) GOTO 900
   ENDDO
   GOTO 6000
!
!     PROJECTION
!
 900  Prject = kwrd - 4
   Vangle(1) = 0.
   Vangle(2) = -1.E10
   Vangle(3) = 34.27
   Fscale = 1
   Fvp = 1
   For = 1
   IF ( nofind==0 ) nofind = -1
   CALL rdword(mode,word)
   IF ( word==hkey(16) ) THEN
!
!     READ SECOND WORD OF ORTHO.,PERS.,OR STERO. SHOULD BE PROJECTION
!
      IF ( Org/=0 ) THEN
         DO i = 1 , Org
            Edge(i,1) = 0.
            Edge(i,2) = 0.
            Edge(i,3) = 1.
            Edge(i,4) = 1.
         ENDDO
         Org = 0
      ENDIF
   ENDIF
   GOTO 500
 1000 CALL rdword(mode,word)
 1100 IF ( word==anti ) THEN
      k = -1
      GOTO 1300
   ENDIF
 1200 k = 1
 1300 DO j = 1 , 3
      Daxis(j) = k*Axis(j)
   ENDDO
   IF ( mode>=eor ) GOTO 100
   IF ( mode>=0 .AND. word/=symm .AND. word/=anti ) GOTO 700
   GOTO 500
 1400 CALL rdword(mode,word)
   n = 2
   IF ( word==film ) n = 1
   IF ( word==pape ) n = 2
   IF ( word==both ) n = 3
   IF ( n==0 ) GOTO 6000
   GOTO 1600
 1500 n = iwrd
 1600 Camera = n
   GOTO 500
 1700 Bframs = iwrd
   GOTO 500
 1800 Maxdef = fwrd
   GOTO 500
 1900 S0s = fwrd
   GOTO 500
!
!     ORIGIN ID
!
 2000 id = iwrd
   ASSIGN 2100 TO tra
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
!
!     HORIZONTAL LOCATION (LEFT EYE - STEREO)
!
 2100 x = fwrd*Cntsin
   ASSIGN 2200 TO tra
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
!
!     VERTICAL LOCATION
!
 2200 y = fwrd*Cntsin
   IF ( Org/=0 ) THEN
      DO j = 1 , Org
         IF ( Origin(j)==id ) GOTO 2300
      ENDDO
      IF ( Org>=Norg ) THEN
         IF ( Prnt>=0 ) THEN
            err(1) = 1
            err(2) = Norg
            CALL wrtprt(Merr,err,msg1,nmsg1)
         ENDIF
         Org = Norg
         DO i = 1 , 2
            Edge(Org+1,i+0) = 0.
            Edge(Org+1,i+2) = 1.
         ENDDO
      ENDIF
   ENDIF
   Org = Org + 1
   j = Org
   Origin(j) = id
   IF ( nofind==0 ) nofind = -1
 2300 Xy(j,1) = x
   Xy(j,3) = y
   For = 0
   ASSIGN 2400 TO tra
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
!
!     HORIZONTAL LOCATION (RIGHT EYE - STEREO)
!
 2400 Xy(j,2) = fwrd*Cntsin
   GOTO 500
!
!     PAPER SIZE, TYPE
!
 2500 IF ( mode<=0 ) GOTO 6000
   CALL rdword(mode,word)
   IF ( word==type ) GOTO 3100
   IF ( word/=size .OR. mode/=0 ) GOTO 6000
   ASSIGN 2600 TO tra
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
 2600 x = fwrd
   CALL rdmode(*2800,*2700,*100,mode,word)
 2700 CALL rdword(mode,word)
   IF ( word/=by .AND. word/=hx ) GOTO 6000
   IF ( mode/=0 ) GOTO 6000
 2800 ASSIGN 2900 TO tra
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
 2900 Papsiz(1) = x
   Papsiz(2) = fwrd
   CALL pltset
   CALL rdmode(*500,*3000,*100,mode,word)
 3000 CALL rdword(mode,word)
   IF ( word/=type ) GOTO 700
!
!     PAPER TYPE
!
 3100 IF ( mode==0 ) GOTO 6000
   CALL rdword(mode,word)
   Paptyp(1) = awrd(1)
   Paptyp(2) = awrd(2)
   IF ( mode>0 ) GOTO 2500
   GOTO 500
 3200 IF ( iwrd==1 .OR. iwrd>Npens ) THEN
      err(1) = 1
      err(2) = iwrd
      CALL wrtprt(Merr,err,msg4,nmsg4)
      iwrd = 1
   ENDIF
   id = iwrd
   CALL rdmode(*500,*3300,*100,mode,word)
 3300 DO
      CALL rdword(mode,word)
      IF ( word==size ) THEN
!
!     PEN SIZE
!
         IF ( mode/=0 ) GOTO 6000
         ASSIGN 3400 TO tra
         CALL rdmode(*6200,*500,*100,mode,word)
         GOTO 6200
      ELSE
         IF ( word/=colo ) GOTO 700
         IF ( mode==0 ) GOTO 6000
         CALL rdword(mode,word)
         Penclr(id,1) = awrd(1)
         Penclr(id,2) = awrd(2)
         IF ( mode<0 ) GOTO 500
         IF ( mode==0 ) CALL rdmode(*500,*3300,*100,mode,word)
      ENDIF
   ENDDO
 3400 Pensiz(id) = iwrd
   CALL rdmode(*500,*3300,*100,mode,word)
   GOTO 3300
 3500 CALL rdword(mode,word)
   IF ( word==dens ) GOTO 3800
   IF ( word/=hmode ) GOTO 3700
   IF ( mode<=0 ) CALL rdmode(*6200,*3600,*3800,mode,word)
 3600 CALL rdword(mode,word)
   IF ( word==dens ) GOTO 3800
 3700 Pltmod(j) = word
   j = j + 1
   IF ( j==2 ) THEN
      IF ( mode<=0 ) CALL rdmode(*6200,*3600,*3800,mode,word)
      GOTO 3600
   ENDIF
 3800 CALL fndplt(id,n,Pltmod)
   Ploter = id
   Model = n
   CALL pltset
   IF ( word==dens ) GOTO 4100
   IF ( mode>=eor ) GOTO 100
!
!     TAPE DENSITY ON PLOTTER CARD
!
 3900 IF ( mode<=0 ) CALL rdmode(*3900,*4000,*100,mode,word)
 4000 CALL rdword(mode,word)
 4100 IF ( word/=dens ) GOTO 700
   IF ( mode/=0 ) GOTO 500
   ASSIGN 4200 TO tra
   CALL rdmode(*6200,*500,*100,mode,word)
   GOTO 6200
 4200 Tapden = iwrd
   CALL rdmode(*500,*4300,*100,mode,word)
 4300 CALL rdword(mode,word)
   IF ( word/=bpi ) GOTO 700
   GOTO 500
 4400 IF ( Prject==2 ) D02 = fwrd
   IF ( Prject==3 ) D03 = fwrd
   GOTO 500
 4500 IF ( fwrd/=0. ) THEN
      IF ( Prject/=3 ) Scale(1) = Cntsin*fwrd
      IF ( Prject==3 ) Scale(1) = Cntin3*fwrd
   ENDIF
   Fscale = 0
   ASSIGN 4600 TO tra
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
 4600 IF ( fwrd/=0. ) Scale(2) = fwrd
   IF ( nofind==0 ) nofind = -1
   GOTO 500
 4700 j = j + 1
   IF ( j==3 ) j = 4
   IF ( Prject==3 .AND. j==6 ) j = 3
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
 4800 Vanpnt(j) = fwrd
   IF ( (Prject/=3 .AND. j/=5) .OR. (Prject==3 .AND. j/=3) ) GOTO 4700
   Fvp = 0
   IF ( nofind==0 ) nofind = -1
   GOTO 500
 4900 Vangle(j) = fwrd
   IF ( nofind==0 ) nofind = -1
   IF ( j==1 ) GOTO 500
   j = j - 1
   CALL rdmode(*6300,*500,*100,mode,word)
   GOTO 6300
!
!     FLAG AND LASSET SET IN PLOT AND CONPLT
!
 5000 IF ( mode<=0 ) CALL rdmode(*5000,*5100,*100,mode,word)
 5100 CALL rdword(mode,word)
   IF ( word==colo .OR. word==fill .OR. word==laye ) THEN
      IF ( word==colo ) ASSIGN 5300 TO tra
      IF ( word==fill ) ASSIGN 5400 TO tra
      IF ( word==laye ) THEN
!
!     ASSIGN LAYER NUMBER HERE FOR COMPOSITS
!
         ASSIGN 5800 TO tra
!
!     SET STRESS FILE TO LAYER STRESS
!
         Oesx = oes1l
      ENDIF
      CALL rdmode(*6200,*500,*100,mode,word)
      GOTO 6200
   ELSEIF ( word/=even ) THEN
!
      IF ( word==list ) THEN
         IF ( mode>0 ) GOTO 5600
         Ncntr = 0
         ASSIGN 5500 TO tra
         CALL rdmode(*6300,*5100,*100,mode,word)
         GOTO 5500
!
      ELSEIF ( word==z1 ) THEN
         Where = 1
         GOTO 5000
      ELSEIF ( word==z2 ) THEN
         Where = -1
         GOTO 5000
      ELSEIF ( word==max ) THEN
         Where = 2
         GOTO 5000
      ELSEIF ( word==mid ) THEN
         Where = 3
         GOTO 5000
      ELSEIF ( word==comm ) THEN
         Direct = 2
         GOTO 5000
      ELSE
         IF ( word==disp ) GOTO 5000
         IF ( word==stre ) GOTO 5000
         IF ( word/=loca ) THEN
!
            DO j = 1 , 20
               IF ( word==icnda(j) ) GOTO 5700
            ENDDO
            GOTO 5600
         ELSE
            Direct = 1
            GOTO 5000
         ENDIF
      ENDIF
   ELSE
      ASSIGN 5200 TO tra
      CALL rdmode(*6200,*500,*100,mode,word)
      GOTO 6200
   ENDIF
 5200 Ncntr = min0(50,iwrd)
   GOTO 5000
 5300 Color = iwrd
   GOTO 5000
 5400 Color = -iwrd
   GOTO 5000
 5500 DO
      IF ( Ncntr<50 ) THEN
         Ncntr = Ncntr + 1
         Cntr(Ncntr) = fwrd
      ELSEIF ( Prnt>=0 ) THEN
         err(1) = 1
         err(2) = iwrd
         CALL wrtprt(Merr,err,msg5,nmsg5)
      ENDIF
      CALL rdmode(*6300,*5100,*100,mode,word)
   ENDDO
 5600 IF ( Prnt>=0 ) THEN
      err(1) = 2
      err(2) = awrd(1)
      err(3) = awrd(2)
      CALL wrtprt(Merr,err,msg2,nmsg2)
   ENDIF
   GOTO 5000
!
 5700 Icntvl = j
!
!     SET STRESS FILE TO STRAIN FILE
!
   IF ( Icntvl==20 ) Oesx = onrgy1
   GOTO 5000
 5800 Layer = iwrd
   GOTO 5000
 5900 Chrscl = fwrd
   IF ( nofind==0 ) nofind = -1
   IF ( Chrscl<1.0 ) Chrscl = 1.0
   CALL pltset
   GOTO 500
!
!     UNRECOGNIZABLE PLOT PARAMETER.
!
 6000 IF ( Prnt>=0 ) THEN
      err(1) = 2
      err(2) = awrd(1)
      err(3) = awrd(2)
      CALL wrtprt(Merr,err,msg2,nmsg2)
   ENDIF
   GOTO 500
!
!     END OF PLOT INPUT
!
 6100 IF ( Prnt<0 ) THEN
      DO i = 1 , 96
         Title(i) = savtit(i)
      ENDDO
   ENDIF
   RETURN
 6200 IF ( mode/=-1 ) THEN
      IF ( mode==-4 ) THEN
         iwrd = dwrd
      ELSE
         iwrd = fwrd
      ENDIF
   ENDIF
   GOTO tra
 6300 IF ( mode==-4 ) THEN
      fwrd = dwrd
   ELSEIF ( mode==-1 ) THEN
      fwrd = iwrd
   ENDIF
   GOTO tra
!
END SUBROUTINE param
