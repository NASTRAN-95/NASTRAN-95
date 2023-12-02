!*==ifp1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1
!
!     READS AND INTERPRETS CASE CONTROL DECK FOR NASTRAN
!
   USE c_ifp1a
   USE c_ifp1hx
   USE c_output
   USE c_system
   USE c_xifp1
   USE c_xmssg
   USE c_xsortx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , anom , anti , blank4 , card , cosi , coun , defa , dol1 , equal1 , flui , ibob , inomor , iout2 ,        &
                   & isymcm , loadn , mat , nptp , om , oneb , pcdb , plt1 , plt2 , ptit , sine , symm , t , xycb , xyou
   INTEGER :: begi , both , dol , file , flag , i , i1 , i2 , i81 , iaxic , iaxif , ibuf1 , ibuf2 , icasec , icaste , icnt , icont ,&
            & icrq , ido , ihowdy , ii , ik , il , io , iop , ip , ip1 , iret , iret1 , iret3 , iset , itype , iun , iword , iwrd , &
            & ixypl , j , jj , jumph , k , l , ll , m , maxcc , mm , mset , nogopc , none , nono , npch , nsub , nsyms , nsymsq ,   &
            & nwdsc , nz , org , outp , plot , porg , set , temp , xypl
   INTEGER , DIMENSION(200,2) :: case , xcase
   INTEGER , DIMENSION(11) , SAVE :: casen
   INTEGER , DIMENSION(9) , SAVE :: cccd , ttlcd
   INTEGER , DIMENSION(54) , SAVE :: cccds
   INTEGER , DIMENSION(7) :: core
   INTEGER , DIMENSION(401) :: corey
   INTEGER , DIMENSION(5) :: isubc
   INTEGER , DIMENSION(2) , SAVE :: name , nifp
   INTEGER , DIMENSION(15) , SAVE :: outop
   INTEGER , DIMENSION(13) , SAVE :: outpch
   LOGICAL :: plotcd , setcd
   REAL , DIMENSION(360) :: symseq
   REAL , DIMENSION(1) :: xcore
   REAL :: xintcd
   INTEGER , DIMENSION(5) , SAVE :: xyprm
   EXTERNAL andf , close , complf , eof , gopen , ifp1b , ifp1c , ifp1d , ifp1e , ifp1f , ifp1g , ifp1h , ifp1pc , ifp1xy , khrfn1 ,&
          & korsz , logfil , makmcb , mesage , mvbits , open , orf , page , read , rshift , tapbit , write , wrttrl , xrcard , xread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   !>>>>EQUIVALENCE (Corex(1),Xcase(1,1),Case(1,1),Corey(1)) , (Core(1),Xcore(1),Corey(401)) , (nono,outop(15)) , (Iaxic,Dumms(4)) ,     &
!>>>>    & (Iaxif,Dumms(15)) , (set,cccd(7)) , (plot,ttlcd(4)) , (xypl,xyprm(1)) , (outp,cccd(1)) , (begi,cccd(2)) , (both,outop(1)) ,   &
!>>>>    & (none,outop(2))
   DATA nifp/4H  IF , 4HP1  /
   DATA casen/4HC A  , 4HS E  , 4H   C , 4H O N , 4H T R , 4H O L , 4H   D , 4H E C , 4H K   , 4H E C , 4H H O/
   DATA ibob , isymcm , loadn , iout2 , inomor/0 , 0 , 1 , 0 , 0/
   DATA outpch/11 , 18 , 21 , 24 , 27 , 30 , 33 , 36 , 152 , 155 , 158 , 168 , 171/
   DATA blank4 , card , coun , t , equal1 , nptp , dol1/4H     , 4HCARD , 4HCOUN , 4HT    , 4H=    , 4HNPTP , 4H$   /
   DATA name/4HCASE , 4HCC  /
   DATA ttlcd/4HTITL , 4HSUBT , 4HLABE , 4HPLOT , 4HXTIT , 4HYTIT , 4HTCUR , 4HYTTI , 4HYBTI/
   DATA cccd/4HOUTP , 4HBEGI , 4HSYM  , 4HSUBC , 4HSYMC , 4HREPC , 4HSET  , 4HNCHE , 4HSCAN/
   DATA cccds/4HMPC  , 4HSPC  , 4HLOAD , 4HNLLO , 4HDEFO , 4HTEMP , 4HDLOA , 4HMETH , 4HFREQ , 4HIC   , 4HDISP , 4HVECT , 4HPRES ,  &
       &4HTHER , 4HSTRE , 4HELST , 4HELFO , 4HFORC , 4HACCE , 4HVELO , 4HSPCF , 4HMAXL , 4HTSTE , 4HSYMS , 4HSUBS , 4HECHO ,        &
      & 4HMODE , 4HLINE , 4HDSCO , 4HK2PP , 4HM2PP , 4HB2PP , 4HTFL  , 4HFMET , 4HOFRE , 4HOTIM , 4HCMET , 4HSDAM , 4HSDIS ,        &
      & 4HSVEC , 4HSVEL , 4HSACC , 4HNONL , 4HPLCO , 4HAXIS , 4HHARM , 4HRAND , 4HOLOA , 4HGPFO , 4HESE  , 4HMPCF , 4HAERO ,        &
      & 4HGUST , 4HSTRA/
   DATA all/4HALL / , cosi/4HCOSI/
   DATA defa/4HDEFA/ , mat/4HMATE/
   DATA om/4HOM  / , oneb/4H1   /
   DATA pcdb/4HPCDB/ , plt1/4HPLT1/
   DATA plt2/4HPLT2/ , sine/4HSINE/
   DATA xycb/4HXYCD/ , xyou/4HXYOU/
   DATA ptit/4HPTIT/ , flui/4HFLUI/
   DATA symm/4HSYMM/ , anti/4HANTI/
   DATA anom/4HANOM/
   DATA xyprm/4HXYPL , 4HXYPR , 4HXYPU , 4HXYPE , 4HXYPA/
   DATA outop/4HBOTH , 4HNONE , 4HUNSO , 4HSORT , 4HPUNC , 4HPRIN , 4HREAL , 4HIMAG , 4HPHAS , 4HNOPR , 4HMAXS , 4HVONM , 4HEXTR ,  &
       &4HLAYE , 4HNONO/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         icc = 1
         icnt = 0
         nset = 0
         nsym = 0
         isub = 1
         msst = 0
         org = 0
         porg = -1
         istr = 1
         ncpw4 = 4
         nwpc = 20
         jumph = 0
         npch = 0
         nogopc = 0
         scr1 = 301
         setcd = .FALSE.
         plotcd = .FALSE.
         blank = blank4
         bit64 = nbpw==64
         casecc = name(1)
         zzzzbb = 0
         zzzzbb = khrfn1(zzzzbb,1,zzzzbb,4)
         equal = khrfn1(zzzzbb,1,equal1,1)
         dol = khrfn1(zzzzbb,1,dol1,1)
         iben = khrfn1(zzzzbb,1,blank,1)
         is = 9999999
         ieor = rshift(complf(0),1)
         nmodes = 1
         lencc = 200
         DO j = 1 , 2
            DO i = 1 , lencc
               case(i,j) = 0
            ENDDO
         ENDDO
         case(166,1) = lencc
         DO j = 1 , 2
            DO i = 1 , 96
               case(i+38,j) = blank
            ENDDO
         ENDDO
         DO i = 1 , 5
            isubc(i) = blank
         ENDDO
         nz = korsz(core) - nwpc - 1
!
!     BLANK TITLE
!
         DO i = 1 , 96
            title(i) = blank
         ENDDO
         DO i = 1 , 11
            head1(i+9) = casen(i)
         ENDDO
         head2(4) = card
         head3(4) = coun
         head3(5) = t
!
         i81 = nwpc + 1
!
!     READ IN DATA-- STORE TITLE CARDS
!
         nz = nz - sysbuf
         icrq = i81 - nz
         IF ( i81>nz ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*40,scr1,corex(nz+1),1)
 20      SPAG_Loop_1_1: DO
            CALL xread(*300,core(1))
            CALL write(scr1,core(1),nwpc,0)
            IF ( ibuf41/=-1 ) THEN
!
!     IS THIS A TITLE SUBTITLE,LABEL,ETC CARD
!
               CALL ifp1f(*20,iword,i2)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
               ASSIGN 20 TO iret1
               istr = 0
               isub = 1
               DO i = 1 , 6
                  IF ( iword==cccd(i) ) THEN
                     IF ( i==1 ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( i==2 ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( i==3 .OR. i==4 .OR. i==5 .OR. i==6 ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
!                                    OUTP BEGI SYM  SUBC SYMC REPC
!
               ENDDO
               IF ( inomor/=1 ) THEN
                  DO i = 1 , 3
                     IF ( iword==ttlcd(i) ) THEN
                        IF ( i==1 ) GOTO 25
                        IF ( i==2 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i==3 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
!                                     TITL SUBT LABE
!
                  ENDDO
               ENDIF
               CYCLE
 25            IF ( logfl<=0 ) CALL logfil(core(1))
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         itype = 1
         spag_nextblock_1 = 8
      CASE (3)
         itype = 2
         spag_nextblock_1 = 8
      CASE (4)
         itype = 3
         spag_nextblock_1 = 8
      CASE (5)
         itype = 7
         spag_nextblock_1 = 8
      CASE (6)
!
!     STOP TITLE SEARCH
!
         inomor = 1
         GOTO 20
      CASE (7)
!
!     IDENTIFY PLOT PACKETS
!
         CALL xrcard(core(i81),nz,core(1))
         temp = core(i81+5)
         IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
         IF ( temp==plot ) THEN
!
!     SET PLOT FLAG
!
            case(135,1) = 1
            spag_nextblock_1 = 6
         ELSE
            IF ( temp==xypl .OR. temp==xyou ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            GOTO 20
         ENDIF
      CASE (8)
!
!     FIND EQUAL SIGN COPY REMAINING DATA ON CARD
!
         CALL ifp1g(itype,case(1,1),isub)
         GOTO iret1
!
!     FILE ERRORS
!
 40      ip1 = -1
         spag_nextblock_1 = 9
      CASE (9)
         CALL mesage(ip1,file,nifp)
         RETURN
!
 60      ip1 = -2
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 80      ip1 = -3
         spag_nextblock_1 = 9
      CASE (10)
         ip1 = -8
         file = icrq
         spag_nextblock_1 = 9
      CASE (11)
         CALL close(scr1,1)
!
!     START BUILDING RECORDS
!
         CALL page
         nwdsc = nwpc + 1
         ASSIGN 100 TO iret1
         ihowdy = 1
         nsym = 0
         nsyms = 0
         iun = 0
         ixypl = 0
         icasec = 0
         istr = 1
         nsub = 0
         msst = 0
         ibuf1 = nz + 1
         file = scr1
         CALL open(*40,scr1,corex(ibuf1),0)
         nz = nz - sysbuf
         ibuf2 = nz + 1
         file = casecc
         IF ( isubs==0 ) GOTO 160
!
!     IN SUBSTRUCTURES, THE CASECC FILE CONTAINS DATA ON THE FRONT.
!     SKIP FILE BEFORE WRITING.
!
         CALL open(*160,file,corex(ibuf2),3)
         CALL write(file,name,2,1)
 100     file = scr1
         icont = 0
         icrq = i81 - nz
         IF ( i81>nz ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
         CALL read(*60,*80,scr1,core(1),nwpc,0,flag)
         WRITE (otpe,99001) icc , (core(i),i=1,nwpc)
99001    FORMAT (11X,I8,6X,20A4)
         icc = icc + 1
         line = line + 1
         IF ( line>=nlpp ) CALL page
         IF ( dol==khrfn1(zzzzbb,1,core(1),1) ) GOTO 100
!
!     IS THIS TITLE SUBTITLE OR LABEL CARD
!
         CALL ifp1f(*100,iword,i2)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         DO i = 1 , 4
            IF ( iword==ttlcd(i) .AND. ibob+ixypl==0 ) THEN
               IF ( i==1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==4 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!                TITL SUBT LABE PLOT
!
         ENDDO
         IF ( iword==ptit .AND. ibob==1 ) THEN
            spag_nextblock_1 = 74
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ixypl==1 ) THEN
            DO i = 5 , 9
               IF ( iword==ttlcd(i) ) THEN
                  spag_nextblock_1 = 74
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         CALL xrcard(core(i81),nz,core(1))
         IF ( icont==1 ) THEN
            spag_nextblock_1 = 28
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         IF ( bit64 ) CALL mvbits(blank,0,32,core(i81+1),0)
         IF ( core(i81+1)==outp ) THEN
!
!     OUTPUT
!
            iout2 = 1
!
!     BLANK CHECK
!
            temp = core(i81+5)
            IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
            IF ( core(i81+3)==ieor .AND. core(i81)==1 ) GOTO 100
            IF ( temp==plot ) THEN
               ibob = 1
!
!     TURN ON TRAIL BITS FOR PLOT
!
               core(1) = pcdb
               core(2) = 0
               core(3) = 0
               core(4) = 0
               core(5) = 7777
               core(6) = 0
               core(7) = 0
               CALL wrttrl(core(1))
!
!     CHECK FOR PRESENCE OF PLOT TAPE
!     (SPLOTS COULD BE SET ALREADY BY NASTRAN PLTFLG CARD)
!
               IF ( isubs==0 .AND. .NOT.tapbit(plt1) .AND. .NOT.tapbit(plt2) ) CALL ifp1d(-618)
               IF ( splots==0 ) splots = 1
               IF ( splots<0 ) splots = -splots
               ASSIGN 140 TO iret3
               spag_nextblock_1 = 48
            ELSE
               IF ( ibob==1 .AND. .NOT.(setcd .AND. plotcd) ) CALL ifp1d(-631)
               IF ( temp==xypl .OR. temp==xyou ) THEN
!
!     X-Y PLOTTER PACKAGE
!
                  ASSIGN 280 TO iret3
                  spag_nextblock_1 = 48
               ELSE
                  il = -617
                  CALL ifp1d(il)
                  GOTO 100
               ENDIF
            ENDIF
         ELSE
            IF ( core(i81+1)==begi ) THEN
!
!     BEGIN BULK
!
               ASSIGN 220 TO iret3
               spag_nextblock_1 = 48
            ELSE
               IF ( ibob==1 ) THEN
!
!     PLOT DATA FOR BO BATA
!
                  i1 = i81
!
!     TEST FOR REQUIRED PLOT AND SET CARDS IN STRUCTURE PLOT OUTPUT PKG
!
                  temp = core(i81+2)
                  IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
                  IF ( core(i81+1)==plot .AND. temp==blank ) plotcd = .TRUE.
                  IF ( core(i81+1)==set ) setcd = .TRUE.
!
!     TEST FOR XYPLOT COMMAND CARDS IN STRUCTURE PLOT OUTPUT PACKAGE
!
                  iwrd = core(i81+1)
                  DO i = 1 , 5
                     IF ( iwrd==xyprm(i) ) CALL ifp1d(-632)
                  ENDDO
!
!     TEST FORMAT OF PLOT COMMAND CARDS
!
                  i = nogo
                  nogo = 0
                  CALL ifp1pc(i81,icnt,xintcd,org,porg)
                  IF ( nogo/=0 ) nogopc = -1
                  nogo = i
!
!     COMPUTE LENGTH OF RECORD
!
                  ik = 0
                  DO
                     IF ( core(i1)<0 ) THEN
                        ip = 2
                     ELSEIF ( core(i1)==0 ) THEN
                        CALL write(pcdb,core(i81),ik+1,1)
                        GOTO 100
                     ELSEIF ( core(i1)==ieor ) THEN
                        CALL write(pcdb,core(i81),ik+1,1)
                        GOTO 100
                     ELSE
                        ip = 2*core(i1) + 1
                     ENDIF
                     ik = ik + ip
                     i1 = i1 + ip
                  ENDDO
               ELSE
                  IF ( ixypl==1 ) THEN
!
!     PROCESS XYPLOTTER CARD
!
                     CALL ifp1xy(ihowdy,xintcd)
                     GOTO 100
                  ELSE
                     IF ( core(i81)<0 ) GOTO 104
                     iword = core(i81+1)
                     DO i = 3 , 9
                        io = i - 2
                        IF ( iword==cccd(i) ) THEN
                           IF ( io==1 ) THEN
                              spag_nextblock_1 = 24
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( io==2 ) THEN
                              spag_nextblock_1 = 42
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( io==3 ) THEN
                              spag_nextblock_1 = 56
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( io==4 ) THEN
                              spag_nextblock_1 = 65
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( io==5 ) THEN
                              spag_nextblock_1 = 40
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( io==6 ) THEN
                              spag_nextblock_1 = 35
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( io==7 ) THEN
                              spag_nextblock_1 = 41
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
!                SYM  SUBC  SYMC  REPC  SET  NCHE  SCAN
!
                     ENDDO
!
!
!     FIND VALUE AFTER EQUAL SIGN
!
                     l = 2*iabs(core(i81)) + i81
                     DO i = i81 , l
                        temp = core(i)
                        IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
                        IF ( temp==equal1 ) GOTO 102
                     ENDDO
                     il = -617
                     CALL ifp1d(il)
                     GOTO 100
 102                 i1 = i + 1
                     IF ( i==l ) i1 = i1 + 1
!
                     iword = core(i81+1)
                     IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                     DO i = 1 , 54
!
!               MPC   SPC   LOAD  NLLO  DEFO  TEMP  DLOA  METH  FREQ
!
!                IC   DISP  VECT  PRES  THER  STRE  ELST  ELFO  FORC
!
!               ACCE  VELO  SPCF  MAXL  TSTE  SYMS  SUBS  ECHO  MODE
!
!               LINE  DSCO  K2PP  M2PP  B2PP  TFL   FMET  OFRE  OTIM
!
!               CMET  SDAM  SDIS  SVEC  SVEL  SACC  NONL  PLCO  AXIS
!
!               HARM  RAND  OLOA  GPFO  ESE   MPCF  AERO  GUST  STRA
                        IF ( iword==cccds(i) ) THEN
                           IF ( i==1 ) GOTO 105
                           IF ( i==2 ) THEN
                              spag_nextblock_1 = 14
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==3 ) THEN
                              spag_nextblock_1 = 15
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==4 ) THEN
                              spag_nextblock_1 = 16
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==5 ) THEN
                              spag_nextblock_1 = 20
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==6 ) THEN
                              spag_nextblock_1 = 29
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==7 ) THEN
                              spag_nextblock_1 = 21
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==8 ) THEN
                              spag_nextblock_1 = 31
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==9 ) THEN
                              spag_nextblock_1 = 22
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==10 ) THEN
                              spag_nextblock_1 = 23
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==11 .OR. i==12 .OR. i==13 .OR. i==14 ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==15 .OR. i==16 ) THEN
                              spag_nextblock_1 = 33
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==17 .OR. i==18 ) THEN
                              spag_nextblock_1 = 34
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==19 ) THEN
                              spag_nextblock_1 = 36
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==20 ) THEN
                              spag_nextblock_1 = 37
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==21 ) THEN
                              spag_nextblock_1 = 38
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==22 ) THEN
                              spag_nextblock_1 = 25
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==23 ) THEN
                              spag_nextblock_1 = 26
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==24 .OR. i==25 ) THEN
                              spag_nextblock_1 = 27
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==26 ) THEN
                              spag_nextblock_1 = 52
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==27 ) THEN
                              spag_nextblock_1 = 55
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==28 ) THEN
                              spag_nextblock_1 = 57
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==29 ) THEN
                              spag_nextblock_1 = 58
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==30 ) THEN
                              spag_nextblock_1 = 61
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==31 ) THEN
                              spag_nextblock_1 = 63
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==32 ) THEN
                              spag_nextblock_1 = 64
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==33 ) THEN
                              spag_nextblock_1 = 66
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==34 ) THEN
                              spag_nextblock_1 = 78
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==35 .OR. i==36 ) THEN
                              spag_nextblock_1 = 67
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==37 ) THEN
                              spag_nextblock_1 = 68
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==38 ) THEN
                              spag_nextblock_1 = 69
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==39 .OR. i==40 ) THEN
                              spag_nextblock_1 = 70
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==41 ) THEN
                              spag_nextblock_1 = 71
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==42 ) THEN
                              spag_nextblock_1 = 72
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==43 ) THEN
                              spag_nextblock_1 = 73
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==44 ) THEN
                              spag_nextblock_1 = 59
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==45 ) THEN
                              spag_nextblock_1 = 75
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==46 ) THEN
                              spag_nextblock_1 = 76
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==47 ) THEN
                              spag_nextblock_1 = 77
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==48 ) THEN
                              spag_nextblock_1 = 17
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==49 ) THEN
                              spag_nextblock_1 = 79
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==50 ) THEN
                              spag_nextblock_1 = 80
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==51 ) GOTO 110
                           IF ( i==52 ) THEN
                              spag_nextblock_1 = 81
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==53 ) THEN
                              spag_nextblock_1 = 82
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( i==54 ) THEN
                              spag_nextblock_1 = 83
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
!
                     ENDDO
                  ENDIF
!
!     UNABLE TO FIND CARD TYPE
!
 104              CALL ifp1d(-601)
                  iun = iun + 1
                  IF ( iun<10 ) GOTO 100
!
!     ASSUME BEGIN BULK MISSING
!
                  CALL ifp1d(-611)
                  ASSIGN 220 TO iret3
                  spag_nextblock_1 = 48
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     MPC CARD FOUND
!
 105           ik = 2
               IF ( core(i1)<=0 ) CALL ifp1d(-617)
               spag_nextblock_1 = 18
            ENDIF
            CYCLE
!
!     MPCFORCE CARD
!
 110        ik = 173
            spag_nextblock_1 = 39
         ENDIF
      CASE (13)
!
!     TOO MANY SPECIFICATIONS
!
         CALL ifp1d(602)
         GOTO iret
      CASE (14)
!
!     SPC CARD DETECTED
!
         ik = 3
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (15)
!
!     LOAD SET SELECTION
!
         ik = 4
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (16)
!
!     PNL FOR VDR
!
         ik = 10
         spag_nextblock_1 = 39
      CASE (17)
!
!     OUTPUT LOAD SET
!
         ik = 17
         spag_nextblock_1 = 39
      CASE (18)
         ASSIGN 120 TO iret
!
!     SKIP CHECK FOR HARMONIC AS DEFAULT IS NON-ZERO
!
         IF ( case(ik,isub)/=0 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 120     case(ik,isub) = core(i1)
         spag_nextblock_1 = 19
      CASE (19)
         IF ( core(i1-1)/=-1 ) THEN
!
!     NO INTEGER IN INTEGER FIELD
!
            il = -604
            CALL ifp1d(il)
!
!     CHECK FOR END OF DATA
!
         ELSEIF ( core(i1+1)/=ieor ) THEN
!
!     DATA CARD DID NOT END PROPERLY
!
            il = -603
            CALL ifp1d(il)
         ENDIF
         GOTO 100
      CASE (20)
!
!     DEFORMATION SET
!
         ik = 6
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (21)
!
!     DLOAD CARD
!
         ik = 13
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (22)
!
!     FREQUENCY CARD
!
         ik = 14
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (23)
!
!     IC CARD
!
         ik = 9
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (24)
!
!     SYM CARD
!
         nsym = nsym + 1
         IF ( nsym<361 ) THEN
            symseq(nsym) = 1.0
         ELSEIF ( nsym==361 ) THEN
            CALL ifp1d(-633)
         ENDIF
         spag_nextblock_1 = 43
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE OPEN STUFF
!
 140     IF ( ixypl==1 ) THEN
!
!     TERMINANT XY PACKAGE
!
            ihowdy = -1
            CALL ifp1xy(ihowdy,xintcd)
            CALL close(xycb,1)
            ixypl = 0
         ENDIF
         CALL close(casecc,1)
!
!     OPEN  PCDB
!
         file = pcdb
!
!     OPEN WRITE FILE
!
 160     CALL gopen(file,corex(ibuf2),1)
         GOTO 100
      CASE (25)
!
!     MAXLINES CARD
!
         maxlin = core(i1)
         spag_nextblock_1 = 19
      CASE (26)
!
!     TIME STEP CARD
!
         ik = 38
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (27)
!
!     SYMSEQ AND SUBSEQ
!
         IF ( isymcm/=0 ) THEN
            nsymsq = 1
            nsym = 1
         ELSE
!
!     SYMSEQ  CARD WITHOUT SYMCOM
!
            il = -605
            CALL ifp1d(il)
            GOTO 100
         ENDIF
         spag_nextblock_1 = 28
      CASE (28)
         DO
            IF ( nsym<361 ) THEN
               symseq(nsym) = xcore(i1)
            ELSEIF ( nsym==361 ) THEN
               CALL ifp1d(-633)
            ENDIF
            IF ( core(i1+1)<0 ) THEN
!
!     CHECK FOR END OF DATA
!
               IF ( core(i1+1)==ieor ) GOTO 100
               nsym = nsym + 1
               i1 = i1 + 2
            ELSEIF ( core(i1+1)==0 ) THEN
!
!     CONTINUATION CARD
!
               icont = 1
               nsym = nsym + 1
               i1 = i81 + 1
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSE
               GOTO 100
            ENDIF
         ENDDO
         spag_nextblock_1 = 29
      CASE (29)
!
!     TEMPERATURE CARD
!
         IF ( core(i81)==2 ) THEN
!
!     THERMAL + STIFFNESS
!
            ASSIGN 180 TO iret
            GOTO 180
         ELSE
            temp = core(i81+5)
            IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
            IF ( temp==both ) THEN
               ASSIGN 180 TO iret
               GOTO 180
            ELSEIF ( temp==mat ) THEN
!
!     STIFNESS LOAD
!
               ik = 8
               stftem = core(i1)
               IF ( isub/=1 ) THEN
!
!     THERMAL REQUEST AT SUBCASE LEVEL
!
                  il = 606
                  CALL ifp1d(il)
                  GOTO 100
               ELSE
                  IF ( core(i1)<=0 ) CALL ifp1d(-617)
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 30
      CASE (30)
!
!     THERMAL LOAD
!
         ik = 7
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 180     case(8,isub) = core(i1)
         stftem = core(i1)
         IF ( isub==1 ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         il = 606
         CALL ifp1d(il)
         GOTO 100
      CASE (31)
!
!     METHOD
!
         ik = 5
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (32)
!
!     DISP(PLOT,1) CARD
!
         ik = 20
         spag_nextblock_1 = 39
      CASE (33)
!
!     STRESS CARD
!
         ik = 23
         spag_nextblock_1 = 39
      CASE (34)
!
!     ELFORCE CARD
!
         ik = 26
         spag_nextblock_1 = 39
      CASE (35)
!
!     NCHECK CARD
!
         ik = 146
         IF ( core(i81)==1 ) THEN
            case(ik,isub) = 5
         ELSEIF ( core(i81+5)==-1 ) THEN
            case(ik,isub) = core(i81+6)
            IF ( core(i81+7)/=ieor ) THEN
               il = -603
               CALL ifp1d(il)
            ENDIF
         ELSE
            il = -617
            CALL ifp1d(il)
         ENDIF
         GOTO 100
      CASE (36)
!
!     ACC
!
         ik = 29
         spag_nextblock_1 = 39
      CASE (37)
!
!     VEL CARD
!
         ik = 32
         spag_nextblock_1 = 39
      CASE (38)
!
!     SPC FORC
!
         ik = 35
         spag_nextblock_1 = 39
      CASE (39)
!
!     OUTPUT SPECIFICATION
!     STRESS AND FORCE FLAGS MAY BE PRE-SET TO 2 (NOPRINT) BY IFP1H
!
         ASSIGN 200 TO iret
         IF ( .NOT.((ik==23 .OR. ik==26) .AND. case(ik+1,isub)==2) ) THEN
            IF ( case(ik,isub)/=0 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     FIND EQUAL SIGN
!
 200     ido = core(i81)
         case(ik+1,isub) = 0
         case(ik+2,isub) = 1
         SPAG_Loop_1_2: DO i = 1 , ido
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  ii = i81 + 2*i
                  temp = core(ii)
                  IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
                  IF ( temp==equal1 ) EXIT SPAG_Loop_1_2
                  iwrd = core(ii-1)
                  SPAG_Loop_2_3: DO io = 4 , 14
                     IF ( bit64 ) CALL mvbits(blank,0,32,iwrd,0)
                     iop = io - 3
                     IF ( iwrd==outop(io) ) THEN
                        IF ( iop==1 ) THEN
                           spag_nextblock_2 = 9
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==2 ) THEN
                           spag_nextblock_2 = 2
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==3 ) THEN
                           spag_nextblock_2 = 3
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==4 ) THEN
                           spag_nextblock_2 = 5
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==5 ) THEN
                           spag_nextblock_2 = 6
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==6 ) THEN
                           spag_nextblock_2 = 7
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==7 ) THEN
                           spag_nextblock_2 = 4
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==8 .OR. iop==10 ) EXIT SPAG_Loop_2_3
                        IF ( iop==9 ) THEN
                           spag_nextblock_2 = 10
                           EXIT SPAG_Loop_2_3
                        ENDIF
                        IF ( iop==11 ) THEN
                           spag_nextblock_2 = 11
                           EXIT SPAG_Loop_2_3
                        ENDIF
                     ENDIF
!         SORT PUNC PRIN REAL IMAG PHAS NOPR MAXS VONM EXTR LAYE
!
                  ENDDO SPAG_Loop_2_3
               CASE (2)
!
!     PUNCH
!
                  case(ik+1,isub) = case(ik+1,isub) + 4
               CASE (3)
!
!     PRINT
!
                  case(ik+1,isub) = case(ik+1,isub) + 1
               CASE (4)
!
!     COMPUTE BUT NO PRINT
!     DEVICE CODE IS 2 (AND SUBPRESS PRINT CODE 1)
!
                  case(ik+1,isub) = case(ik+1,isub) - mod(case(ik+1,isub),2) + 2
               CASE (5)
!
!     REAL PRINT OUT FORMAT
!
                  ii = 1
                  spag_nextblock_2 = 8
               CASE (6)
!
!     REAL AND IMAGINARY
!
                  ii = 2
                  spag_nextblock_2 = 8
               CASE (7)
!
!     MAGNITUE AND PHASE ANGLE
!
                  ii = 3
                  spag_nextblock_2 = 8
               CASE (8)
                  case(ik+2,isub) = isign(ii,case(ik+2,isub))
               CASE (9)
!
!     SORT TWO REQUEST
!     (COMMENTS FORM G.C.  7/1989
!     SINCE OES2L FILE HAS NOT BEEN IMPLEMENTED IN ALL DMAPS, SORT2
!     STRESS REQUEST ON LAYERED ELEMENTS IS NOT AVAILABLE)
!
                  temp = core(ii)
                  IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
                  IF ( temp/=oneb ) THEN
                     IF ( ik==23 .AND. case(183,isub)>=2 ) CALL ifp1d(-645)
                     case(ik+2,isub) = -iabs(case(ik+2,isub))
                  ENDIF
               CASE (10)
!
!     VON MISES STRESS
!     (183 WORD ON CASECC, FIRST RIGHT-MOST BIT)
!
                  case(183,isub) = orf(case(183,isub),1)
               CASE (11)
!
!     LAYER STRESSES FOR COMPOSITE ELEMENTS
!     (183 WORD ON CASECC, SECOND RIGHT-MOST BIT)
!     (SORT2 STRESS REQUEST ON LAYERED ELEMENTS NOT AVAILABLE)
!
                  IF ( ik/=23 ) CALL ifp1d(-646)
                  IF ( ik==23 .AND. case(25,isub)<0 ) CALL ifp1d(-645)
                  case(183,isub) = orf(case(183,isub),2)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO SPAG_Loop_1_2
         IF ( case(ik+1,isub)==0 ) case(ik+1,isub) = 1
         IF ( core(ii+1)/=0 ) THEN
            temp = core(ii+1)
            IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
            IF ( temp==all ) THEN
!
!     ALL SPECIFIED -- SET SET NO. MINUS
!
               case(ik,isub) = -1
            ELSEIF ( temp==none .OR. temp==nono ) THEN
!
!     NONE SPECIFIED
!
               case(ik,isub) = none
            ELSE
               IF ( core(ii+1)==-1 ) THEN
                  i1 = ii + 2
!
!     FIND SET NUMBER
!
                  IF ( nset/=0 ) THEN
                     jj = nwdsc
                     DO il = 1 , nset
                        IF ( core(jj)==core(i1) ) GOTO 205
                        jj = jj + core(jj+1) + 3
                     ENDDO
                  ENDIF
!
!     UNDEFINED SET ID ON CARD
!
                  CALL ifp1d(-608)
               ELSE
                  il = -617
                  CALL ifp1d(il)
               ENDIF
               GOTO 100
 205           case(ik,isub) = core(i1)
            ENDIF
         ELSE
            CALL ifp1d(610)
            case(ik,isub) = -1
         ENDIF
         IF ( core(ii+3)/=ieor ) THEN
            il = -603
            CALL ifp1d(il)
         ENDIF
         GOTO 100
      CASE (40)
!
!     SET CARD
!
         nset = nset + 1
         CALL ifp1c(i81,nz)
         GOTO 100
      CASE (41)
!
!     SCAN CARD
!
         CALL ifp1h(i81,nz,jumph)
         GOTO 100
      CASE (42)
!
!     SUBCASE
!
         temp = core(i81+2)
         IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
         IF ( temp==om ) THEN
            spag_nextblock_1 = 56
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( isymcm==1 ) THEN
!
!     PUT OUT SUBCOM OR SYMCOM RECORD
!
            isymcm = 0
            spag_nextblock_1 = 49
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nsym = 0
            nsyms = nsyms + 1
            IF ( nsyms<361 ) THEN
               symseq(nsyms) = 1.0
            ELSEIF ( nsyms==361 ) THEN
               CALL ifp1d(-633)
            ENDIF
         ENDIF
         spag_nextblock_1 = 43
      CASE (43)
         ASSIGN 100 TO iret3
         IF ( isub/=2 ) THEN
            isub = 2
            loadn = core(i81+4)
            CALL ifp1f(*100,iword,i2)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            DO i = 1 , 5
               isubc(i) = core(i2)
               i2 = i2 + 1
            ENDDO
            IF ( core(i81+3)+1/=0 ) THEN
!
!     SUBCASE ID MISSING
!
               il = -609
               loadn = case(1,2)
               CALL ifp1d(il)
            ENDIF
            GOTO 100
         ENDIF
         spag_nextblock_1 = 44
      CASE (44)
!
!     TURN STRESS AND FORCE NO-PRINT FLAGS ON IF INTERACTIVE FLAG IS ON
!
         IF ( intra>=2 ) THEN
            case(24,isub) = orf(case(24,isub),8)
            case(27,isub) = orf(case(27,isub),8)
         ENDIF
!
         case(1,isub) = loadn
         IF ( core(i81+4)<=loadn+nmodes-1 ) THEN
            spag_nextblock_1 = 47
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         loadn = core(i81+4)
         IF ( core(i81+3)/=-1 ) THEN
            spag_nextblock_1 = 47
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 45
      CASE (45)
         IF ( case(137,1)==0 ) case(137,1) = 1
         CALL ifp1e(isubc(1),symseq,nwdsc,i81,icaste)
         stftem = icaste
         nsub = nsub + nmodes
!
!     CHECK SET NOS. THAT WERE SPECIFIED AFTER SCAN CARDS
!
!     FORM G.C./UNISYS   4/1990
!     IFP1H IS BY-PASSING THIS NEW CODE HERE (MSST=0) BECAUSE SET DATA
!     IS NOT AVAILABLE HERE. SAVE THIS CODE FOR FURTHER INVESTIGATION.
!
         IF ( msst/=0 ) THEN
            mm = 0
            ll = lencc + case(lencc,isub) + 1
            DO m = 1 , msst
               i = ll
               mset = misset(m)
               SPAG_Loop_2_4: DO
!
!     WRITE (6,2345) MSET,MSST,LL
!
                  iset = case(i,isub)
!
!     LX1 = I - 3
!     LX2 = I + 3
!     WRITE (6,6789) ISET,(CASE(LX,ISUB),LX=LX1,LX2)
!
                  IF ( iset==0 ) EXIT SPAG_Loop_2_4
                  IF ( mset/=iset ) THEN
                     i = i + case(i+1,isub)
                     IF ( i>=400 ) EXIT SPAG_Loop_2_4
                  ELSE
                     misset(m) = 0
                     mm = mm + 1
                     EXIT SPAG_Loop_2_4
                  ENDIF
               ENDDO SPAG_Loop_2_4
            ENDDO
            IF ( mm/=msst ) THEN
               DO m = 1 , msst
                  IF ( misset(m)/=0 ) THEN
                     WRITE (otpe,99002) ufm , misset(m)
99002                FORMAT (A23,' 608A, UNIDENTIFIED SET',I8,' WAS REQUESTED FOR ','SCAN')
                     nogo = 1
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 46
      CASE (46)
!
         GOTO iret3
      CASE (47)
         CALL ifp1d(-609)
         loadn = case(1,2)
         spag_nextblock_1 = 45
      CASE (48)
         core(i81+3) = -1
         core(i81+4) = 9999999
         IF ( icasec==1 ) THEN
            spag_nextblock_1 = 46
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icasec = 1
         IF ( isymcm==1 ) THEN
            isymcm = 0
         ELSE
            nsym = 0
            spag_nextblock_1 = 44
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 49
      CASE (49)
!
!     NO SUBSEQ OR SYMSEQ CARD
!
         IF ( nsymsq==0 .AND. nsym==0 ) nsym = nsyms
!
         nsymsq = 0
         case(lencc,2) = max0(nsym,0)
         case(16,2) = nsym
         spag_nextblock_1 = 44
         CYCLE SPAG_DispatchLoop_1
 220     CALL close(scr1,1)
         IF ( ibob/=1 .AND. ixypl/=1 ) CALL close(casecc,1)
         IF ( ibob==1 ) CALL close(pcdb,1)
         IF ( ibob==1 .AND. .NOT.(setcd .AND. plotcd) ) CALL ifp1d(-631)
         IF ( ixypl==1 ) THEN
!
!     TERMINATE XYPLOT PACKAGE
!
            ihowdy = -1
            CALL ifp1xy(ihowdy,xintcd)
            CALL close(xycb,1)
         ENDIF
!
!     PUT CASECC ON NPTP
!
         file = casecc
         CALL open(*40,casecc,corex(ibuf1),0)
         file = nptp
         maxcc = 0
         CALL open(*40,nptp,corex(ibuf2),3)
         spag_nextblock_1 = 50
      CASE (50)
         CALL read(*260,*240,casecc,core(1),nz,0,flag)
         icrq = nz
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 240     CALL write(nptp,core(1),flag,1)
         maxcc = max0(maxcc,flag)
!
!     CHECK ANY PUNCH REQUEST  ON OUTPUT DATA BLOCKS
!
         IF ( npch/=1 .AND. flag>=166 ) THEN
            DO i = 1 , 13
               j = outpch(i)
               IF ( andf(core(j),4)/=0 ) THEN
                  spag_nextblock_1 = 51
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 50
      CASE (51)
         npch = 1
         spag_nextblock_1 = 50
         CYCLE SPAG_DispatchLoop_1
 260     CALL close(casecc,1)
         CALL eof(nptp)
         CALL close(nptp,2)
         IF ( splots<0 ) splots = 0
!
!     IF THIS IS A RESTART  SET CHANGE FLAGS IN IFP1B
!
         IF ( app<0 ) CALL ifp1b
         IF ( iun/=0 ) CALL ifp1d(-612)
         CALL makmcb(core,casecc,nsub,0,0)
         core(2) = nsub
         core(4) = maxcc
         CALL wrttrl(core)
!
!     SET NOGO FLAG TO -9 IF ERROR IN BULKDATA AND PLOT COMMANDS
!     SET NOGO FLAG TO POSITIVE IF ERROR IN BULKDATA, AND NOT IN PLOT
!     SET NOGO FLAG TO NEGATIVE IF NO ERROR IN BULKDATA, BUT IN PLOT
!     PUNCH AN IDENTIFICATION CARD IF PUNCH IS REQUESTED ON OUTPUT DATA,
!     AND PRINT SCAN KEYWORDS IF ERROR FLAG (JUMPH) WAS TURNED ON
!
         IF ( nogo/=0 .AND. nogopc==-1 ) nogo = -9
         IF ( nogo==0 ) nogo = nogopc
         IF ( npch==1 ) WRITE (lpch,99003) (title(j),j=1,17)
99003    FORMAT (2H$ ,17A4)
         IF ( jumph==1 ) CALL ifp1h(0,0,2)
         RETURN
      CASE (52)
!
!     ECHO REQUEST
!
         iecho = 0
         ido = core(i81) - 2
         DO i = 1 , ido
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  iwrd = core(i1)
                  IF ( bit64 ) CALL mvbits(blank,0,32,iwrd,0)
                  DO io = 1 , 5
                     IF ( iwrd==outop(io) ) THEN
                        IF ( io==1 ) THEN
                           spag_nextblock_3 = 5
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        IF ( io==2 ) THEN
                           spag_nextblock_1 = 54
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( io==3 ) THEN
                           spag_nextblock_3 = 6
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        IF ( io==4 ) THEN
                           spag_nextblock_3 = 2
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        IF ( io==5 ) THEN
                           spag_nextblock_3 = 4
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                     ENDIF
!                                     BOTH  NONE  UNSO  SORT  PUNC
!
                  ENDDO
                  IF ( iwrd==outop(15) ) THEN
                     spag_nextblock_1 = 53
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL ifp1d(629)
                  spag_nextblock_3 = 3
               CASE (2)
!
!     SORTED ECHO
!
                  IF ( andf(iecho,2)/=0 ) CALL ifp1d(629)
                  spag_nextblock_3 = 3
               CASE (3)
                  iecho = orf(iecho,2)
                  spag_nextblock_3 = 7
               CASE (4)
!
!     PUNCH ECHO
!
                  IF ( andf(iecho,4)/=0 ) CALL ifp1d(629)
                  iecho = orf(iecho,4)
                  npch = 1
                  spag_nextblock_3 = 7
               CASE (5)
!
!     BOTH ECHO
!
                  IF ( andf(iecho,3)/=0 ) CALL ifp1d(629)
                  iecho = orf(iecho,3)
                  spag_nextblock_3 = 7
               CASE (6)
!
!     UNSORTED ECHO
!
                  IF ( andf(iecho,1)/=0 ) CALL ifp1d(629)
                  iecho = orf(iecho,1)
                  spag_nextblock_3 = 7
               CASE (7)
                  i1 = i1 + 2
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
!
         GOTO 100
      CASE (53)
!
!     NONO ECHO - ABSOLUTELY NO ECHO, NO EVEN IN RESTART
!
         io = 16
         spag_nextblock_1 = 54
      CASE (54)
!
!     NONE ECHO
!
         IF ( iecho/=0 .OR. i<ido ) CALL ifp1d(630)
         iecho = -1
         IF ( io==16 ) iecho = -2
         GOTO 100
      CASE (55)
!
!     LOOP CONTROL FOR EIGENVALUE
!
         nmodes = core(i1)
         GOTO 100
      CASE (56)
!
!     SYMCOM OR SUBCOM CARD
!
         IF ( isymcm==0 ) THEN
            isymcm = 1
            nsymsq = 0
            spag_nextblock_1 = 43
         ELSE
            ASSIGN 100 TO iret3
            spag_nextblock_1 = 49
         ENDIF
      CASE (57)
!
!     LINE CARD - NLPP BOTTOM-LIMITED TO 10
!
         IF ( core(i1-1)/=-1 ) THEN
            il = -604
            CALL ifp1d(il)
         ELSE
            IF ( iabs(core(i1))>0 ) nlpp = iabs(core(i1))
            IF ( nlpp<10 ) nlpp = 10
         ENDIF
         GOTO 100
      CASE (58)
!
!     DIFFERENTIAL STIFFNESS OR PIECEWISE LINEAR COEFFICIENT SET
!
         ik = 138
         spag_nextblock_1 = 60
      CASE (59)
         ik = 164
         spag_nextblock_1 = 60
      CASE (60)
         temp = core(i1)
         IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
         IF ( temp/=defa ) THEN
            IF ( core(i1)<=0 ) CALL ifp1d(-617)
         ELSE
            core(i1) = -1
            core(i1+1) = ieor
            core(i1-1) = -1
         ENDIF
         spag_nextblock_1 = 18
      CASE (61)
!
!     K2PP
!
         ik = 139
         spag_nextblock_1 = 62
      CASE (62)
         case(ik,isub) = core(i1)
         case(ik+1,isub) = core(i1+1)
         GOTO 100
      CASE (63)
!
!     M2PP
!
         ik = 141
         spag_nextblock_1 = 62
      CASE (64)
!
!     B2PP
!
         ik = 143
         spag_nextblock_1 = 62
      CASE (65)
!
!     REPRINT OF ABOVE CASE
!
         nsym = -1
         IF ( isub/=2 ) CALL ifp1d(-607)
         spag_nextblock_1 = 56
      CASE (66)
!
!     TRANSFER FUNCTION SELECTION
!
         ik = 15
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (67)
!
!     OUTPUT FREQUENCY LIST SET
!
         ik = 145
         temp = core(i1)
         IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
         IF ( temp/=all ) THEN
            spag_nextblock_1 = 39
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         core(i1) = -1
         core(i1-1) = -1
         core(i1+1) = ieor
         spag_nextblock_1 = 18
      CASE (68)
!
!     COMPLEX EIGENVALUE METHOD
!
         ik = 148
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (69)
!
!     STRUCTURAL DAMPING TABLE
!
         ik = 149
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (70)
!
!     INERTIA RELIEF SET SELECTION
!
!1770 IK = 150
!     GO TO 490
!
!     ANALYSIS SET FOR VDR
!
         ik = 151
         spag_nextblock_1 = 39
      CASE (71)
!
!     ANALYSIS VELOCITY
!
         ik = 154
         spag_nextblock_1 = 39
      CASE (72)
!
!     ANALYSIS ACCELERATION
!
         ik = 157
         spag_nextblock_1 = 39
      CASE (73)
!
!     NON LINEAR FORCE VECTOR FOR TRANSIENT ANALYSIS
!
         ik = 160
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 280     CALL close(casecc,1)
         DO i = 2 , 6
            core(i) = 0
         ENDDO
         core(1) = xycb
         core(7) = 1
         CALL wrttrl(core(1))
!
!     OPEN XYCB
!
         IF ( ibob==1 ) THEN
            CALL close(pcdb,1)
            ibob = 0
         ENDIF
         file = xycb
         ixypl = 1
         i81 = nwpc + 1
         GOTO 160
      CASE (74)
!
!     AXIS TITLE CARDS
!
         itype = 8
         core(1) = iword
         DO i = 1 , 32
            k = i81 + i - 1
            core(k) = blank
         ENDDO
         IF ( ibob==1 ) THEN
!
!     PLOT TITLE CARD
!
            core(i81) = 10
            core(i81+1) = iword
            core(i81+2) = blank
            CALL ifp1g(itype,core(i81+3),1)
            core(i81+21) = 9999999
            ik = 21
            CALL write(pcdb,core(i81),ik+1,1)
         ELSE
            CALL ifp1g(itype,core(i81),1)
            CALL ifp1xy(ihowdy,xintcd)
         ENDIF
         GOTO 100
      CASE (75)
!
!     DELETE SETS FOR FORCE
!
!1840 IK = 161
!     GO TO 490
!
!     AXISYM CARD
!
         temp = core(i1)
         IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
         IF ( temp==sine ) THEN
            case(136,isub) = 1
            iaxic = 1
         ELSE
            IF ( temp/=cosi ) THEN
               IF ( temp/=flui ) THEN
                  IF ( temp==symm ) THEN
                     case(136,isub) = -2
                  ELSEIF ( temp==anti ) THEN
                     case(136,isub) = -1
                  ELSE
                     IF ( temp==anom ) THEN
                        case(136,isub) = -30
                     ELSE
!
!     ILLEGAL  SPECIFICATION
!
                        il = -617
                        CALL ifp1d(il)
                     ENDIF
                     GOTO 100
                  ENDIF
                  temp = core(i1+1)
                  IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
                  IF ( temp==anom ) case(136,isub) = case(136,isub)*10
                  GOTO 100
               ENDIF
            ENDIF
            case(136,isub) = 2
            IF ( temp==cosi ) iaxic = 1
            IF ( temp==flui ) iaxif = 1
         ENDIF
         GOTO 100
      CASE (76)
!
!     HARMONIC SELECTOR
!
         ik = 137
         temp = core(i1)
         IF ( bit64 ) CALL mvbits(blank,0,32,temp,0)
         IF ( temp==all ) THEN
            core(i1) = -1
         ELSEIF ( temp==none ) THEN
            case(137,1) = 0
            core(i1) = 0
         ELSE
            core(i1) = core(i1) + 1
            IF ( core(i1)<=0 ) CALL ifp1d(-617)
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         core(i1-1) = -1
         core(i1+1) = ieor
         spag_nextblock_1 = 18
      CASE (77)
!
!     RANDOM SET SELECTION
!
         ik = 163
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (78)
!
!     FMETHOD
!
         ik = 165
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (79)
!
!     GRID POINT FORCE REQUEST
!
         ik = 167
         spag_nextblock_1 = 39
      CASE (80)
!
!     ELEMENT STRAIN ENERGY
!
         ik = 170
         spag_nextblock_1 = 39
      CASE (81)
!
!     AEROFORCE OUTPUT REQUEST
!
         ik = 176
         spag_nextblock_1 = 39
      CASE (82)
!
!     AEROELASTIC GUST LOAD REQUEST
!
         ik = 179
         IF ( core(i1)<=0 ) CALL ifp1d(-617)
         spag_nextblock_1 = 18
      CASE (83)
!
!     STRAIN CARD
!     (180 THRU 182 WORDS OF CASECC)
!
         ik = 180
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
!
!     EOF ON INPUT UNIT
!
 300     CALL ifp1d(-624)
         CALL mesage(-37,0,nifp)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifp1
