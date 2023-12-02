!*==param.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE param(Setid,Xx,Buf4)
   USE c_blank
   USE c_output
   USE c_pltdat
   USE c_system
   USE c_xxparm
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Setid
   INTEGER , DIMENSION(1) :: Xx
   INTEGER :: Buf4
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: anti , blank , both , bpi , by , colo , comm , defo , dens , disp , eor , even , fill , film , fram , hmode ,  &
                   & hx , laye , list , loca , max , mid , nmsg1 , nmsg2 , nmsg4 , nmsg5 , oes1 , oes1l , onrgy1 , plan , poin ,    &
                   & sepa , size , stre , symm , type , z1 , z2
   INTEGER , DIMENSION(2) :: awrd , pltnam
   INTEGER , DIMENSION(7) , SAVE :: axisd
   REAL , SAVE :: blank4
   INTEGER :: buf1 , bufsiz , i , id , iwrd , j , k , kwrd , mode , modex , n , nofind , pape , tra , word
   REAL(REAL64) :: dwrd
   INTEGER , DIMENSION(3) :: err
   REAL :: fwrd , x , y
   INTEGER , DIMENSION(19) , SAVE :: hkey
   INTEGER , DIMENSION(2) , SAVE :: hplot
   INTEGER , DIMENSION(20) , SAVE :: icnda , msg1 , msg2
   INTEGER , DIMENSION(22) , SAVE :: msg4
   INTEGER , DIMENSION(16) , SAVE :: msg5
   INTEGER , DIMENSION(3) , SAVE :: nkwd
   INTEGER , DIMENSION(96) :: savtit
   LOGICAL , SAVE :: test
   EXTERNAL find , fndplt , fread , plot , pltset , rdmode , rdmodx , rdword , read , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS PARAM ROUTINE IS CALLED ONLY BY DPLOT, WHICH IS THE DRIVER
!     OF THE PLOT MODULE           ==== == =====
!
!     THE DRIVER FOR THE PARAM MODULE IS QPARAM
!
   !>>>>EQUIVALENCE (Ksystm(1),Bufsiz) , (pape,hkey(10)) , (word,awrd(1),dwrd,fwrd,iwrd)
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         lasset = 0
         CALL pltset
         buf1 = Buf4 + 3*bufsiz
!
!     SAVE THE TITLE, SUBTITLE AND LABEL IF DEFORMED PLOTS ...
!
         IF ( prnt>=0 ) THEN
            CALL rdmodx(parm,mode,word)
         ELSE
            DO i = 1 , 96
               savtit(i) = title(i)
            ENDDO
            nofind = 0
            CALL rdmodx(parm,mode,word)
         ENDIF
 20      DO
            CALL read(*880,*880,parm,mode,1,0,i)
            IF ( mode<0 ) THEN
               i = 1
               IF ( mode==-4 ) i = 2
               CALL fread(parm,0,-i,0)
            ELSEIF ( mode/=0 ) THEN
               IF ( mode<eor ) THEN
                  mode = mode + 1
                  CALL rdword(mode,word)
                  CALL rdword(mode,word)
                  IF ( awrd(1)/=hplot(1) ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( awrd(2)==blank ) THEN
!
!     PLOT
!
                     IF ( test ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!         WHEN PLOTTER OR PROJECTION WERE HIT
!              FSCALE=FOR=FVP=1
!              PROJECTION=KWRD-4, SOME NUMBER
!         WHEN SCALE IS HIT,       FSCALE SET TO 0
!         WHEN VANTAGE POINT IS HEIT, FVP SET TO 0
!         WHEN ORIGIN IS HIT,         ORG SET TO 0
!
                     IF ( fscale/=0 .OR. for/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( prject==1 .OR. fvp==0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( awrd(2)/=hplot(2) ) THEN
                        spag_nextblock_1 = 18
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     PLOTTER
!
                     IF ( mode==0 ) THEN
                        spag_nextblock_1 = 18
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL rdword(mode,word)
                     pltnam(1) = awrd(1)
                     pltnam(2) = awrd(2)
                     pltmod(1) = 0
                     pltmod(2) = 0
                     camera = 2
                     fscale = 1
                     fvp = 1
                     for = 1
                     IF ( org/=0 ) THEN
                        DO i = 1 , org
                           edge(i,1) = 0.
                           edge(i,2) = 0.
                           edge(i,3) = 1.
                           edge(i,4) = 1.
                        ENDDO
                        org = 0
                     ENDIF
!
!     CHECK FOR A MODEL NUMBER
!
                     ASSIGN 500 TO tra
                     j = 1
                     IF ( mode<=0 ) CALL rdmode(*900,*460,*520,mode,word)
                     GOTO 460
                  ENDIF
               ELSE
                  CALL fread(parm,0,0,1)
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!     FIND
!
         CALL find(mode,buf1,Buf4,Setid,Xx)
         nofind = +1
         IF ( mode>=0 ) THEN
            CALL rdmodx(parm,mode,word)
            GOTO 20
         ELSE
            mode = modex
            spag_nextblock_1 = 4
         ENDIF
      CASE (3)
         modex = mode
         mode = -1
         org = max0(1,org)
         spag_nextblock_1 = 2
      CASE (4)
         CALL plot(mode,buf1,Buf4,Setid,Xx,nofind)
         oesx = oes1
         IF ( nofind==-1 ) org = max0(1,org)
         nofind = 0
         CALL rdmodx(parm,mode,word)
         GOTO 20
!
!     PLOT PARAMETER CARD.
!
 40      IF ( mode<=0 ) CALL rdmode(*40,*60,*20,mode,word)
 60      CALL rdword(mode,word)
         spag_nextblock_1 = 5
      CASE (5)
         i = nkwd(prject)
         DO kwrd = 1 , i
            IF ( hkey(kwrd)==word ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 18
      CASE (6)
!
         IF ( kwrd==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( kwrd==2 ) THEN
!
!     VIEW
!
            IF ( mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 700 TO tra
            j = 4
            j = j - 1
!
!     READ A DECIMAL NUMBER ON A PARAMETER CARD
!
            CALL rdmode(*920,*40,*20,mode,word)
            GOTO 920
         ELSEIF ( kwrd==3 ) THEN
!
!     AXES
!
            DO j = 1 , 3
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( mode==0 ) CALL rdmode(*40,*62,*20,mode,word)
 62                  CALL rdword(mode,word)
                     DO i = 1 , 7
                        IF ( word==axisd(i) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     spag_nextblock_1 = 8
                     CYCLE SPAG_DispatchLoop_1
                  CASE (2)
                     axis(j) = i - 4
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            IF ( mode==0 ) CALL rdmode(*100,*80,*100,mode,word)
            GOTO 80
         ELSEIF ( kwrd==4 ) THEN
!
!     MAXIMUM DEFORMATION
!
            IF ( mode<=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(mode,word)
            IF ( word/=defo .OR. mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 180 TO tra
            CALL rdmode(*920,*40,*20,mode,word)
            GOTO 920
         ELSEIF ( kwrd==5 .OR. kwrd==6 .OR. kwrd==7 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( kwrd==8 ) THEN
!
!     CONTOUR
!
!     RESTORE DEFAULTS
!
            icntvl = 1
            ncntr = 10
            color = 0
            layer = 0
            where = 1
            direct = 2
            cntr(1) = 0.0
            cntr(2) = 0.0
            GOTO 720
         ELSEIF ( kwrd==9 ) THEN
!
!     CAMERA
!
            ASSIGN 140 TO tra
            IF ( mode<=0 ) CALL rdmode(*900,*120,*20,mode,word)
            GOTO 120
         ELSEIF ( kwrd==10 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( kwrd==11 ) THEN
!
!     PEN SIZE / COLOR
!
            IF ( mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 400 TO tra
!
!
!     READ AN INTEGER ON A PARAMETER CARD
!
            CALL rdmode(*900,*40,*20,mode,word)
            GOTO 900
         ELSEIF ( kwrd==12 ) THEN
!
!     BLANK FRAMES
!
            IF ( mode==0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(mode,word)
            IF ( word/=fram .OR. mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 160 TO tra
            CALL rdmode(*900,*40,*20,mode,word)
            GOTO 900
         ELSEIF ( kwrd==13 ) THEN
!
!     ORIGIN
!
            IF ( mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 220 TO tra
            CALL rdmode(*900,*40,*20,mode,word)
            GOTO 900
         ELSEIF ( kwrd==14 ) THEN
!
!     SCALE
!
            IF ( mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 640 TO tra
            CALL rdmode(*920,*40,*20,mode,word)
            GOTO 920
         ELSEIF ( kwrd==15 ) THEN
!
!     CSCALE
!
            IF ( mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 860 TO tra
            CALL rdmode(*920,*40,*20,mode,word)
            GOTO 920
         ELSEIF ( kwrd==16 ) THEN
!
!     PROJECTION PLANE SEPARATION
!
            IF ( mode==0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(mode,word)
!
!     USER MAY HAVE REVERSE ENGLISH
!
            IF ( word==plan ) THEN
               IF ( mode==0 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL rdword(mode,word)
               IF ( mode/=0 .OR. word/=sepa ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ASSIGN 620 TO tra
               CALL rdmode(*920,*40,*20,mode,word)
               GOTO 920
            ENDIF
         ELSEIF ( kwrd==17 ) THEN
!
!     PTITLE
!
            fpltit = 1
            DO i = 1 , 17
               pltitl(i) = blank4
            ENDDO
            j = color
            DO i = 1 , 17 , 2
               CALL rdword(mode,word)
               pltitl(i) = awrd(1)
               pltitl(i+1) = awrd(2)
               IF ( mode==0 ) GOTO 40
            ENDDO
            color = j
            IF ( mode/=0 ) CALL rdword(mode,word)
            GOTO 40
         ELSEIF ( kwrd==18 ) THEN
!
!     OCULAR SEPARATION
!
            IF ( mode<=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(mode,word)
            IF ( word/=sepa .OR. mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 200 TO tra
            CALL rdmode(*920,*40,*20,mode,word)
            GOTO 920
         ELSEIF ( kwrd==19 ) THEN
!
!     VANTAGE POINT
!
            IF ( mode==0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL rdword(mode,word)
            IF ( word/=poin .OR. mode/=0 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 680 TO tra
            j = 0
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!           FIND  VIEW  AXES  MAXI  ORTH  PERS  STER  CONT  CAME  PAPE
!    1       PEN  BLAN  ORIG  SCAL  CSCA  PROJ  PTIT  OCUL  VANT
!
!
!     RECHECK IF PROJECTION CARD
!
         DO kwrd = 5 , 7
            IF ( word==hkey(kwrd) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 18
      CASE (7)
!
!     PROJECTION
!
         prject = kwrd - 4
         vangle(1) = 0.
         vangle(2) = -1.E10
         vangle(3) = 34.27
         fscale = 1
         fvp = 1
         for = 1
         IF ( nofind==0 ) nofind = -1
         CALL rdword(mode,word)
         IF ( word==hkey(16) ) THEN
!
!     READ SECOND WORD OF ORTHO.,PERS.,OR STERO. SHOULD BE PROJECTION
!
            IF ( org/=0 ) THEN
               DO i = 1 , org
                  edge(i,1) = 0.
                  edge(i,2) = 0.
                  edge(i,3) = 1.
                  edge(i,4) = 1.
               ENDDO
               org = 0
            ENDIF
         ENDIF
         GOTO 40
 80      CALL rdword(mode,word)
         spag_nextblock_1 = 8
      CASE (8)
         IF ( word==anti ) THEN
            k = -1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 100     k = 1
         spag_nextblock_1 = 9
      CASE (9)
         DO j = 1 , 3
            daxis(j) = k*axis(j)
         ENDDO
         IF ( mode>=eor ) GOTO 20
         IF ( mode<0 .OR. word==symm .OR. word==anti ) GOTO 40
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     CALL rdword(mode,word)
         n = 2
         IF ( word==film ) n = 1
         IF ( word==pape ) n = 2
         IF ( word==both ) n = 3
         IF ( n/=0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 140     n = iwrd
         spag_nextblock_1 = 10
      CASE (10)
         camera = n
         GOTO 40
 160     bframs = iwrd
         GOTO 40
 180     maxdef = fwrd
         GOTO 40
 200     s0s = fwrd
         GOTO 40
!
!     ORIGIN ID
!
 220     id = iwrd
         ASSIGN 240 TO tra
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
!
!     HORIZONTAL LOCATION (LEFT EYE - STEREO)
!
 240     x = fwrd*cntsin
         ASSIGN 260 TO tra
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
!
!     VERTICAL LOCATION
!
 260     y = fwrd*cntsin
         IF ( org/=0 ) THEN
            DO j = 1 , org
               IF ( origin(j)==id ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( org>=norg ) THEN
               IF ( prnt>=0 ) THEN
                  err(1) = 1
                  err(2) = norg
                  CALL wrtprt(merr,err,msg1,nmsg1)
               ENDIF
               org = norg
               DO i = 1 , 2
                  edge(org+1,i+0) = 0.
                  edge(org+1,i+2) = 1.
               ENDDO
            ENDIF
         ENDIF
         org = org + 1
         j = org
         origin(j) = id
         IF ( nofind==0 ) nofind = -1
         spag_nextblock_1 = 11
      CASE (11)
         xy(j,1) = x
         xy(j,3) = y
         for = 0
         ASSIGN 280 TO tra
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
!
!     HORIZONTAL LOCATION (RIGHT EYE - STEREO)
!
 280     xy(j,2) = fwrd*cntsin
         GOTO 40
      CASE (12)
!
!     PAPER SIZE, TYPE
!
         IF ( mode<=0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL rdword(mode,word)
         IF ( word==type ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( word/=size .OR. mode/=0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 300 TO tra
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
 300     x = fwrd
         CALL rdmode(*340,*320,*20,mode,word)
 320     CALL rdword(mode,word)
         IF ( word/=by .AND. word/=hx ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( mode/=0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 340     ASSIGN 360 TO tra
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
 360     papsiz(1) = x
         papsiz(2) = fwrd
         CALL pltset
         CALL rdmode(*40,*380,*20,mode,word)
 380     CALL rdword(mode,word)
         IF ( word/=type ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
!
!     PAPER TYPE
!
         IF ( mode==0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL rdword(mode,word)
         paptyp(1) = awrd(1)
         paptyp(2) = awrd(2)
         IF ( mode<=0 ) GOTO 40
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 400     IF ( iwrd==1 .OR. iwrd>npens ) THEN
            err(1) = 1
            err(2) = iwrd
            CALL wrtprt(merr,err,msg4,nmsg4)
            iwrd = 1
         ENDIF
         id = iwrd
         CALL rdmode(*40,*420,*20,mode,word)
 420     DO
            CALL rdword(mode,word)
            IF ( word==size ) THEN
!
!     PEN SIZE
!
               IF ( mode/=0 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ASSIGN 440 TO tra
               CALL rdmode(*900,*40,*20,mode,word)
               GOTO 900
            ELSE
               IF ( word/=colo ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mode==0 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL rdword(mode,word)
               penclr(id,1) = awrd(1)
               penclr(id,2) = awrd(2)
               IF ( mode<0 ) GOTO 40
               IF ( mode==0 ) CALL rdmode(*40,*420,*20,mode,word)
            ENDIF
         ENDDO
 440     pensiz(id) = iwrd
         CALL rdmode(*40,*420,*20,mode,word)
         GOTO 420
 460     CALL rdword(mode,word)
         IF ( word==dens ) GOTO 520
         IF ( word/=hmode ) GOTO 500
         IF ( mode<=0 ) CALL rdmode(*900,*480,*520,mode,word)
 480     CALL rdword(mode,word)
         IF ( word==dens ) GOTO 520
 500     pltmod(j) = word
         j = j + 1
         IF ( j==2 ) THEN
            IF ( mode<=0 ) CALL rdmode(*900,*480,*520,mode,word)
            GOTO 480
         ENDIF
 520     CALL fndplt(id,n,pltmod)
         ploter = id
         model = n
         CALL pltset
         IF ( word==dens ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( mode>=eor ) GOTO 20
!
!     TAPE DENSITY ON PLOTTER CARD
!
 540     IF ( mode<=0 ) CALL rdmode(*540,*560,*20,mode,word)
 560     CALL rdword(mode,word)
         spag_nextblock_1 = 14
      CASE (14)
         IF ( word/=dens ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( mode/=0 ) GOTO 40
         ASSIGN 580 TO tra
         CALL rdmode(*900,*40,*20,mode,word)
         GOTO 900
 580     tapden = iwrd
         CALL rdmode(*40,*600,*20,mode,word)
 600     CALL rdword(mode,word)
         IF ( word==bpi ) GOTO 40
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 620     IF ( prject==2 ) d02 = fwrd
         IF ( prject==3 ) d03 = fwrd
         GOTO 40
 640     IF ( fwrd/=0. ) THEN
            IF ( prject/=3 ) scale(1) = cntsin*fwrd
            IF ( prject==3 ) scale(1) = cntin3*fwrd
         ENDIF
         fscale = 0
         ASSIGN 660 TO tra
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
 660     IF ( fwrd/=0. ) scale(2) = fwrd
         IF ( nofind==0 ) nofind = -1
         GOTO 40
      CASE (15)
         j = j + 1
         IF ( j==3 ) j = 4
         IF ( prject==3 .AND. j==6 ) j = 3
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
 680     vanpnt(j) = fwrd
         IF ( (prject/=3 .AND. j/=5) .OR. (prject==3 .AND. j/=3) ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         fvp = 0
         IF ( nofind==0 ) nofind = -1
         GOTO 40
 700     vangle(j) = fwrd
         IF ( nofind==0 ) nofind = -1
         IF ( j==1 ) GOTO 40
         j = j - 1
         CALL rdmode(*920,*40,*20,mode,word)
         GOTO 920
!
!     FLAG AND LASSET SET IN PLOT AND CONPLT
!
 720     IF ( mode<=0 ) CALL rdmode(*720,*740,*20,mode,word)
 740     CALL rdword(mode,word)
         IF ( word==colo .OR. word==fill .OR. word==laye ) THEN
            IF ( word==colo ) ASSIGN 780 TO tra
            IF ( word==fill ) ASSIGN 800 TO tra
            IF ( word==laye ) THEN
!
!     ASSIGN LAYER NUMBER HERE FOR COMPOSITS
!
               ASSIGN 840 TO tra
!
!     SET STRESS FILE TO LAYER STRESS
!
               oesx = oes1l
            ENDIF
            CALL rdmode(*900,*40,*20,mode,word)
            GOTO 900
         ELSEIF ( word/=even ) THEN
!
            IF ( word==list ) THEN
               IF ( mode>0 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ncntr = 0
               ASSIGN 820 TO tra
               CALL rdmode(*920,*740,*20,mode,word)
               GOTO 820
!
            ELSEIF ( word==z1 ) THEN
               where = 1
               GOTO 720
            ELSEIF ( word==z2 ) THEN
               where = -1
               GOTO 720
            ELSEIF ( word==max ) THEN
               where = 2
               GOTO 720
            ELSEIF ( word==mid ) THEN
               where = 3
               GOTO 720
            ELSEIF ( word==comm ) THEN
               direct = 2
               GOTO 720
            ELSE
               IF ( word==disp ) GOTO 720
               IF ( word==stre ) GOTO 720
               IF ( word/=loca ) THEN
!
                  DO j = 1 , 20
                     IF ( word==icnda(j) ) THEN
                        spag_nextblock_1 = 17
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  direct = 1
                  GOTO 720
               ENDIF
            ENDIF
         ELSE
            ASSIGN 760 TO tra
            CALL rdmode(*900,*40,*20,mode,word)
            GOTO 900
         ENDIF
 760     ncntr = min0(50,iwrd)
         GOTO 720
 780     color = iwrd
         GOTO 720
 800     color = -iwrd
         GOTO 720
 820     DO
            IF ( ncntr<50 ) THEN
               ncntr = ncntr + 1
               cntr(ncntr) = fwrd
            ELSEIF ( prnt>=0 ) THEN
               err(1) = 1
               err(2) = iwrd
               CALL wrtprt(merr,err,msg5,nmsg5)
            ENDIF
            CALL rdmode(*920,*740,*20,mode,word)
         ENDDO
         spag_nextblock_1 = 16
      CASE (16)
         IF ( prnt>=0 ) THEN
            err(1) = 2
            err(2) = awrd(1)
            err(3) = awrd(2)
            CALL wrtprt(merr,err,msg2,nmsg2)
         ENDIF
         GOTO 720
      CASE (17)
!
         icntvl = j
!
!     SET STRESS FILE TO STRAIN FILE
!
         IF ( icntvl==20 ) oesx = onrgy1
         GOTO 720
 840     layer = iwrd
         GOTO 720
 860     chrscl = fwrd
         IF ( nofind==0 ) nofind = -1
         IF ( chrscl<1.0 ) chrscl = 1.0
         CALL pltset
         GOTO 40
      CASE (18)
!
!     UNRECOGNIZABLE PLOT PARAMETER.
!
         IF ( prnt>=0 ) THEN
            err(1) = 2
            err(2) = awrd(1)
            err(3) = awrd(2)
            CALL wrtprt(merr,err,msg2,nmsg2)
         ENDIF
         GOTO 40
!
!     END OF PLOT INPUT
!
 880     IF ( prnt<0 ) THEN
            DO i = 1 , 96
               title(i) = savtit(i)
            ENDDO
         ENDIF
         RETURN
 900     IF ( mode/=-1 ) THEN
            IF ( mode==-4 ) THEN
               iwrd = dwrd
            ELSE
               iwrd = fwrd
            ENDIF
         ENDIF
         GOTO tra
 920     IF ( mode==-4 ) THEN
            fwrd = dwrd
         ELSEIF ( mode==-1 ) THEN
            fwrd = iwrd
         ENDIF
         GOTO tra
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE param
