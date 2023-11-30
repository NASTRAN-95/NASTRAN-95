
SUBROUTINE ifp1
!
!     READS AND INTERPRETS CASE CONTROL DECK FOR NASTRAN
!
   IMPLICIT NONE
   INTEGER App , Blank , Case(200,2) , Casecc , Core(7) , Corex(1) , Corey(401) , Date(3) , Dmz(4) , Dumms(16) , Dummy(28) ,        &
         & Dumz(16) , Equal , Head1(32) , Head2(32) , Head3(32) , Iaxic , Iaxif , Iben , Ibuf41 , Icc , Idum , Iecho , Ieor , Intp ,&
         & Intra , Ipage , Is , Istr , Isub , Isubs , Label(32) , Lencc , Line , Loadnn , Logfl , Lpch , Lsystm , Maxlin ,          &
         & Misset(20) , Mpcn , Msst , Nbpw , Ncpw4 , Nlpp , Nmodes , Nogo , Nset , Nsym , Nwpc , Otpe , Pltid(32) , Scr1 , Spcn ,   &
         & Splots , Stftem , Subtit(32) , Sysbuf , Tim , Title(32) , Tline , Xcase(200,2) , Zzzzbb
   LOGICAL Bit64
   CHARACTER*23 Ufm
   REAL Xcore(1)
   COMMON /ifp1a / Scr1 , Casecc , Is , Nwpc , Ncpw4 , Nmodes , Icc , Nset , Nsym , Zzzzbb , Istr , Isub , Lencc , Iben , Equal ,   &
                 & Ieor
   COMMON /ifp1hx/ Msst , Misset
   COMMON /output/ Title , Subtit , Label , Head1 , Head2 , Head3 , Pltid
   COMMON /system/ Sysbuf , Otpe , Nogo , Intp , Mpcn , Spcn , Logfl , Loadnn , Nlpp , Stftem , Ipage , Line , Tline , Maxlin ,     &
                 & Date , Tim , Iecho , Splots , App , Idum , Lsystm , Dumms , Nbpw , Dummy , Isubs , Dumz , Intra , Dmz , Lpch
   COMMON /xifp1 / Blank , Bit64
   COMMON /xmssg / Ufm
   COMMON /xsortx/ Ibuf41
   COMMON /zzzzzz/ Corex
   INTEGER all , anom , anti , begi , blank4 , both , card , casen(11) , cccd(9) , cccds(54) , cosi , coun , defa , dol , dol1 ,    &
         & equal1 , file , flag , flui , i , i1 , i2 , i81 , ibob , ibuf1 , ibuf2 , icasec , icaste , icnt , icont , icrq , ido ,   &
         & ihowdy , ii , ik , il , inomor , io , iop , iout2 , ip , ip1 , iret , iret1 , iret3 , iset , isubc(5) , isymcm , itype , &
         & iun , iword , iwrd , ixypl , j , jj , jumph , k , l , ll , loadn , m , mat , maxcc , mm , mset , name(2) , nifp(2) ,     &
         & nogopc , none , nono , npch , nptp , nsub , nsyms , nsymsq , nwdsc , nz , om , oneb , org
   INTEGER andf , complf , khrfn1 , korsz , orf , rshift
   INTEGER outop(15) , outp , outpch(13) , pcdb , plot , plt1 , plt2 , porg , ptit , set , sine , symm , t , temp , ttlcd(9) ,      &
         & xycb , xyou , xypl , xyprm(5)
   LOGICAL plotcd , setcd
   REAL symseq(360) , xintcd
   LOGICAL tapbit
   EXTERNAL andf , complf , orf , rshift
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
!
!     INITIALIZE
!
   Icc = 1
   icnt = 0
   Nset = 0
   Nsym = 0
   Isub = 1
   Msst = 0
   org = 0
   porg = -1
   Istr = 1
   Ncpw4 = 4
   Nwpc = 20
   jumph = 0
   npch = 0
   nogopc = 0
   Scr1 = 301
   setcd = .FALSE.
   plotcd = .FALSE.
   Blank = blank4
   Bit64 = Nbpw==64
   Casecc = name(1)
   Zzzzbb = 0
   Zzzzbb = khrfn1(Zzzzbb,1,Zzzzbb,4)
   Equal = khrfn1(Zzzzbb,1,equal1,1)
   dol = khrfn1(Zzzzbb,1,dol1,1)
   Iben = khrfn1(Zzzzbb,1,Blank,1)
   Is = 9999999
   Ieor = rshift(complf(0),1)
   Nmodes = 1
   Lencc = 200
   DO j = 1 , 2
      DO i = 1 , Lencc
         Case(i,j) = 0
      ENDDO
   ENDDO
   Case(166,1) = Lencc
   DO j = 1 , 2
      DO i = 1 , 96
         Case(i+38,j) = Blank
      ENDDO
   ENDDO
   DO i = 1 , 5
      isubc(i) = Blank
   ENDDO
   nz = korsz(Core) - Nwpc - 1
!
!     BLANK TITLE
!
   DO i = 1 , 96
      Title(i) = Blank
   ENDDO
   DO i = 1 , 11
      Head1(i+9) = casen(i)
   ENDDO
   Head2(4) = card
   Head3(4) = coun
   Head3(5) = t
!
   i81 = Nwpc + 1
!
!     READ IN DATA-- STORE TITLE CARDS
!
   nz = nz - Sysbuf
   icrq = i81 - nz
   IF ( i81>nz ) GOTO 1300
   CALL open(*900,Scr1,Corex(nz+1),1)
 100  DO
      CALL xread(*10700,Core(1))
      CALL write(Scr1,Core(1),Nwpc,0)
      IF ( Ibuf41/=-1 ) THEN
!
!     IS THIS A TITLE SUBTITLE,LABEL,ETC CARD
!
         CALL ifp1f(*100,iword,i2)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         ASSIGN 100 TO iret1
         Istr = 0
         Isub = 1
         DO i = 1 , 6
            IF ( iword==cccd(i) ) THEN
               IF ( i==1 ) GOTO 700
               IF ( i==2 ) GOTO 1400
               IF ( i==3 .OR. i==4 .OR. i==5 .OR. i==6 ) GOTO 600
            ENDIF
!                                    OUTP BEGI SYM  SUBC SYMC REPC
!
         ENDDO
         IF ( inomor/=1 ) THEN
            DO i = 1 , 3
               IF ( iword==ttlcd(i) ) THEN
                  IF ( i==1 ) GOTO 120
                  IF ( i==2 ) GOTO 300
                  IF ( i==3 ) GOTO 400
               ENDIF
!                                     TITL SUBT LABE
!
            ENDDO
         ENDIF
         CYCLE
 120     IF ( Logfl<=0 ) CALL logfil(Core(1))
         EXIT
      ENDIF
   ENDDO
 200  itype = 1
   GOTO 800
 300  itype = 2
   GOTO 800
 400  itype = 3
   GOTO 800
 500  itype = 7
   GOTO 800
!
!     STOP TITLE SEARCH
!
 600  inomor = 1
   GOTO 100
!
!     IDENTIFY PLOT PACKETS
!
 700  CALL xrcard(Core(i81),nz,Core(1))
   temp = Core(i81+5)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp==plot ) THEN
!
!     SET PLOT FLAG
!
      Case(135,1) = 1
      GOTO 600
   ELSE
      IF ( temp/=xypl .AND. temp/=xyou ) GOTO 100
      GOTO 600
   ENDIF
!
!     FIND EQUAL SIGN COPY REMAINING DATA ON CARD
!
 800  CALL ifp1g(itype,Case(1,1),Isub)
   GOTO iret1
!
!     FILE ERRORS
!
 900  ip1 = -1
 1000 CALL mesage(ip1,file,nifp)
   RETURN
!
 1100 ip1 = -2
   GOTO 1000
 1200 ip1 = -3
   GOTO 1000
 1300 ip1 = -8
   file = icrq
   GOTO 1000
 1400 CALL close(Scr1,1)
!
!     START BUILDING RECORDS
!
   CALL page
   nwdsc = Nwpc + 1
   ASSIGN 1500 TO iret1
   ihowdy = 1
   Nsym = 0
   nsyms = 0
   iun = 0
   ixypl = 0
   icasec = 0
   Istr = 1
   nsub = 0
   Msst = 0
   ibuf1 = nz + 1
   file = Scr1
   CALL open(*900,Scr1,Corex(ibuf1),0)
   nz = nz - Sysbuf
   ibuf2 = nz + 1
   file = Casecc
   IF ( Isubs==0 ) GOTO 3100
!
!     IN SUBSTRUCTURES, THE CASECC FILE CONTAINS DATA ON THE FRONT.
!     SKIP FILE BEFORE WRITING.
!
   CALL open(*3100,file,Corex(ibuf2),3)
   CALL write(file,name,2,1)
 1500 file = Scr1
   icont = 0
   icrq = i81 - nz
   IF ( i81>nz ) GOTO 1300
 1600 CALL read(*1100,*1200,Scr1,Core(1),Nwpc,0,flag)
   WRITE (Otpe,99001) Icc , (Core(i),i=1,Nwpc)
99001 FORMAT (11X,I8,6X,20A4)
   Icc = Icc + 1
   Line = Line + 1
   IF ( Line>=Nlpp ) CALL page
   IF ( dol==khrfn1(Zzzzbb,1,Core(1),1) ) GOTO 1500
!
!     IS THIS TITLE SUBTITLE OR LABEL CARD
!
   CALL ifp1f(*1500,iword,i2)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   DO i = 1 , 4
      IF ( iword==ttlcd(i) .AND. ibob+ixypl==0 ) THEN
         IF ( i==1 ) GOTO 200
         IF ( i==2 ) GOTO 300
         IF ( i==3 ) GOTO 400
         IF ( i==4 ) GOTO 500
      ENDIF
!                TITL SUBT LABE PLOT
!
   ENDDO
   IF ( iword==ptit .AND. ibob==1 ) GOTO 9600
   IF ( ixypl==1 ) THEN
      DO i = 5 , 9
         IF ( iword==ttlcd(i) ) GOTO 9600
      ENDDO
   ENDIF
   CALL xrcard(Core(i81),nz,Core(1))
   IF ( icont==1 ) GOTO 3500
!
   IF ( Bit64 ) CALL mvbits(Blank,0,32,Core(i81+1),0)
   IF ( Core(i81+1)==outp ) THEN
!
!     OUTPUT
!
      iout2 = 1
!
!     BLANK CHECK
!
      temp = Core(i81+5)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
      IF ( Core(i81+3)==Ieor .AND. Core(i81)==1 ) GOTO 1500
      IF ( temp==plot ) THEN
         ibob = 1
!
!     TURN ON TRAIL BITS FOR PLOT
!
         Core(1) = pcdb
         Core(2) = 0
         Core(3) = 0
         Core(4) = 0
         Core(5) = 7777
         Core(6) = 0
         Core(7) = 0
         CALL wrttrl(Core(1))
!
!     CHECK FOR PRESENCE OF PLOT TAPE
!     (SPLOTS COULD BE SET ALREADY BY NASTRAN PLTFLG CARD)
!
         IF ( Isubs==0 .AND. .NOT.tapbit(plt1) .AND. .NOT.tapbit(plt2) ) CALL ifp1d(-618)
         IF ( Splots==0 ) Splots = 1
         IF ( Splots<0 ) Splots = -Splots
         ASSIGN 3000 TO iret3
         GOTO 6200
      ELSE
         IF ( ibob==1 .AND. .NOT.(setcd .AND. plotcd) ) CALL ifp1d(-631)
         IF ( temp==xypl .OR. temp==xyou ) THEN
!
!     X-Y PLOTTER PACKAGE
!
            ASSIGN 9500 TO iret3
            GOTO 6200
         ELSE
            il = -617
            CALL ifp1d(il)
            GOTO 1500
         ENDIF
      ENDIF
   ELSE
      IF ( Core(i81+1)==begi ) THEN
!
!     BEGIN BULK
!
         ASSIGN 6400 TO iret3
         GOTO 6200
      ELSE
         IF ( ibob==1 ) THEN
!
!     PLOT DATA FOR BO BATA
!
            i1 = i81
!
!     TEST FOR REQUIRED PLOT AND SET CARDS IN STRUCTURE PLOT OUTPUT PKG
!
            temp = Core(i81+2)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
            IF ( Core(i81+1)==plot .AND. temp==Blank ) plotcd = .TRUE.
            IF ( Core(i81+1)==set ) setcd = .TRUE.
!
!     TEST FOR XYPLOT COMMAND CARDS IN STRUCTURE PLOT OUTPUT PACKAGE
!
            iwrd = Core(i81+1)
            DO i = 1 , 5
               IF ( iwrd==xyprm(i) ) CALL ifp1d(-632)
            ENDDO
!
!     TEST FORMAT OF PLOT COMMAND CARDS
!
            i = Nogo
            Nogo = 0
            CALL ifp1pc(i81,icnt,xintcd,org,porg)
            IF ( Nogo/=0 ) nogopc = -1
            Nogo = i
!
!     COMPUTE LENGTH OF RECORD
!
            ik = 0
            GOTO 7600
         ELSE
            IF ( ixypl==1 ) THEN
!
!     PROCESS XYPLOTTER CARD
!
               CALL ifp1xy(ihowdy,xintcd)
               GOTO 1500
            ELSE
               IF ( Core(i81)<0 ) GOTO 1610
               iword = Core(i81+1)
               DO i = 3 , 9
                  io = i - 2
                  IF ( iword==cccd(i) ) THEN
                     IF ( io==1 ) GOTO 2900
                     IF ( io==2 ) GOTO 5600
                     IF ( io==3 ) GOTO 7700
                     IF ( io==4 ) GOTO 8600
                     IF ( io==5 ) GOTO 5400
                     IF ( io==6 ) GOTO 4300
                     IF ( io==7 ) GOTO 5500
                  ENDIF
!                SYM  SUBC  SYMC  REPC  SET  NCHE  SCAN
!
               ENDDO
!
!
!     FIND VALUE AFTER EQUAL SIGN
!
               l = 2*iabs(Core(i81)) + i81
               DO i = i81 , l
                  temp = Core(i)
                  IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
                  IF ( temp==equal1 ) GOTO 1605
               ENDDO
               il = -617
               CALL ifp1d(il)
               GOTO 1500
 1605          i1 = i + 1
               IF ( i==l ) i1 = i1 + 1
!
               iword = Core(i81+1)
               IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
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
                     IF ( i==1 ) GOTO 1620
                     IF ( i==2 ) GOTO 1800
                     IF ( i==3 ) GOTO 1900
                     IF ( i==4 ) GOTO 2000
                     IF ( i==5 ) GOTO 2500
                     IF ( i==6 ) GOTO 3600
                     IF ( i==7 ) GOTO 2600
                     IF ( i==8 ) GOTO 3900
                     IF ( i==9 ) GOTO 2700
                     IF ( i==10 ) GOTO 2800
                     IF ( i==11 .OR. i==12 .OR. i==13 .OR. i==14 ) GOTO 4000
                     IF ( i==15 .OR. i==16 ) GOTO 4100
                     IF ( i==17 .OR. i==18 ) GOTO 4200
                     IF ( i==19 ) GOTO 4400
                     IF ( i==20 ) GOTO 4500
                     IF ( i==21 ) GOTO 4600
                     IF ( i==22 ) GOTO 3200
                     IF ( i==23 ) GOTO 3300
                     IF ( i==24 .OR. i==25 ) GOTO 3400
                     IF ( i==26 ) GOTO 6900
                     IF ( i==27 ) GOTO 7500
                     IF ( i==28 ) GOTO 7800
                     IF ( i==29 ) GOTO 7900
                     IF ( i==30 ) GOTO 8200
                     IF ( i==31 ) GOTO 8400
                     IF ( i==32 ) GOTO 8500
                     IF ( i==33 ) GOTO 8700
                     IF ( i==34 ) GOTO 10100
                     IF ( i==35 .OR. i==36 ) GOTO 8800
                     IF ( i==37 ) GOTO 8900
                     IF ( i==38 ) GOTO 9000
                     IF ( i==39 .OR. i==40 ) GOTO 9100
                     IF ( i==41 ) GOTO 9200
                     IF ( i==42 ) GOTO 9300
                     IF ( i==43 ) GOTO 9400
                     IF ( i==44 ) GOTO 8000
                     IF ( i==45 ) GOTO 9700
                     IF ( i==46 ) GOTO 9900
                     IF ( i==47 ) GOTO 10000
                     IF ( i==48 ) GOTO 2100
                     IF ( i==49 ) GOTO 10200
                     IF ( i==50 ) GOTO 10300
                     IF ( i==51 ) GOTO 1650
                     IF ( i==52 ) GOTO 10400
                     IF ( i==53 ) GOTO 10500
                     IF ( i==54 ) GOTO 10600
                  ENDIF
!
               ENDDO
            ENDIF
!
!     UNABLE TO FIND CARD TYPE
!
 1610       CALL ifp1d(-601)
            iun = iun + 1
            IF ( iun<10 ) GOTO 1500
!
!     ASSUME BEGIN BULK MISSING
!
            CALL ifp1d(-611)
            ASSIGN 6400 TO iret3
            GOTO 6200
         ENDIF
!
!     MPC CARD FOUND
!
 1620    ik = 2
         IF ( Core(i1)<=0 ) CALL ifp1d(-617)
         GOTO 2200
      ENDIF
!
!     MPCFORCE CARD
!
 1650 ik = 173
      GOTO 4700
   ENDIF
!
!     TOO MANY SPECIFICATIONS
!
 1700 CALL ifp1d(602)
   GOTO iret
!
!     SPC CARD DETECTED
!
 1800 ik = 3
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     LOAD SET SELECTION
!
 1900 ik = 4
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     PNL FOR VDR
!
 2000 ik = 10
   GOTO 4700
!
!     OUTPUT LOAD SET
!
 2100 ik = 17
   GOTO 4700
 2200 ASSIGN 2300 TO iret
!
!     SKIP CHECK FOR HARMONIC AS DEFAULT IS NON-ZERO
!
   IF ( Case(ik,Isub)/=0 ) GOTO 1700
 2300 Case(ik,Isub) = Core(i1)
 2400 IF ( Core(i1-1)/=-1 ) THEN
!
!     NO INTEGER IN INTEGER FIELD
!
      il = -604
      CALL ifp1d(il)
!
!     CHECK FOR END OF DATA
!
   ELSEIF ( Core(i1+1)/=Ieor ) THEN
!
!     DATA CARD DID NOT END PROPERLY
!
      il = -603
      CALL ifp1d(il)
   ENDIF
   GOTO 1500
!
!     DEFORMATION SET
!
 2500 ik = 6
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     DLOAD CARD
!
 2600 ik = 13
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     FREQUENCY CARD
!
 2700 ik = 14
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     IC CARD
!
 2800 ik = 9
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     SYM CARD
!
 2900 Nsym = Nsym + 1
   IF ( Nsym<361 ) THEN
      symseq(Nsym) = 1.0
   ELSEIF ( Nsym==361 ) THEN
      CALL ifp1d(-633)
   ENDIF
   GOTO 5700
!
!     CLOSE OPEN STUFF
!
 3000 IF ( ixypl==1 ) THEN
!
!     TERMINANT XY PACKAGE
!
      ihowdy = -1
      CALL ifp1xy(ihowdy,xintcd)
      CALL close(xycb,1)
      ixypl = 0
   ENDIF
   CALL close(Casecc,1)
!
!     OPEN  PCDB
!
   file = pcdb
!
!     OPEN WRITE FILE
!
 3100 CALL gopen(file,Corex(ibuf2),1)
   GOTO 1500
!
!     MAXLINES CARD
!
 3200 Maxlin = Core(i1)
   GOTO 2400
!
!     TIME STEP CARD
!
 3300 ik = 38
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     SYMSEQ AND SUBSEQ
!
 3400 IF ( isymcm/=0 ) THEN
      nsymsq = 1
      Nsym = 1
   ELSE
!
!     SYMSEQ  CARD WITHOUT SYMCOM
!
      il = -605
      CALL ifp1d(il)
      GOTO 1500
   ENDIF
 3500 DO
      IF ( Nsym<361 ) THEN
         symseq(Nsym) = Xcore(i1)
      ELSEIF ( Nsym==361 ) THEN
         CALL ifp1d(-633)
      ENDIF
      IF ( Core(i1+1)<0 ) THEN
!
!     CHECK FOR END OF DATA
!
         IF ( Core(i1+1)==Ieor ) GOTO 1500
         Nsym = Nsym + 1
         i1 = i1 + 2
      ELSEIF ( Core(i1+1)==0 ) THEN
!
!     CONTINUATION CARD
!
         icont = 1
         Nsym = Nsym + 1
         i1 = i81 + 1
         GOTO 1600
      ELSE
         GOTO 1500
      ENDIF
   ENDDO
!
!     TEMPERATURE CARD
!
 3600 IF ( Core(i81)==2 ) THEN
!
!     THERMAL + STIFFNESS
!
      ASSIGN 3800 TO iret
      GOTO 3800
   ELSE
      temp = Core(i81+5)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
      IF ( temp==both ) THEN
         ASSIGN 3800 TO iret
         GOTO 3800
      ELSEIF ( temp==mat ) THEN
!
!     STIFNESS LOAD
!
         ik = 8
         Stftem = Core(i1)
         IF ( Isub/=1 ) THEN
!
!     THERMAL REQUEST AT SUBCASE LEVEL
!
            il = 606
            CALL ifp1d(il)
            GOTO 1500
         ELSE
            IF ( Core(i1)<=0 ) CALL ifp1d(-617)
            GOTO 2200
         ENDIF
      ENDIF
   ENDIF
!
!     THERMAL LOAD
!
 3700 ik = 7
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
 3800 Case(8,Isub) = Core(i1)
   Stftem = Core(i1)
   IF ( Isub==1 ) GOTO 3700
   il = 606
   CALL ifp1d(il)
   GOTO 1500
!
!     METHOD
!
 3900 ik = 5
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     DISP(PLOT,1) CARD
!
 4000 ik = 20
   GOTO 4700
!
!     STRESS CARD
!
 4100 ik = 23
   GOTO 4700
!
!     ELFORCE CARD
!
 4200 ik = 26
   GOTO 4700
!
!     NCHECK CARD
!
 4300 ik = 146
   IF ( Core(i81)==1 ) THEN
      Case(ik,Isub) = 5
   ELSEIF ( Core(i81+5)==-1 ) THEN
      Case(ik,Isub) = Core(i81+6)
      IF ( Core(i81+7)/=Ieor ) THEN
         il = -603
         CALL ifp1d(il)
      ENDIF
   ELSE
      il = -617
      CALL ifp1d(il)
   ENDIF
   GOTO 1500
!
!     ACC
!
 4400 ik = 29
   GOTO 4700
!
!     VEL CARD
!
 4500 ik = 32
   GOTO 4700
!
!     SPC FORC
!
 4600 ik = 35
!
!     OUTPUT SPECIFICATION
!     STRESS AND FORCE FLAGS MAY BE PRE-SET TO 2 (NOPRINT) BY IFP1H
!
 4700 ASSIGN 4800 TO iret
   IF ( .NOT.((ik==23 .OR. ik==26) .AND. Case(ik+1,Isub)==2) ) THEN
      IF ( Case(ik,Isub)/=0 ) GOTO 1700
   ENDIF
!
!     FIND EQUAL SIGN
!
 4800 ido = Core(i81)
   Case(ik+1,Isub) = 0
   Case(ik+2,Isub) = 1
   DO i = 1 , ido
      ii = i81 + 2*i
      temp = Core(ii)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
      IF ( temp==equal1 ) EXIT
      iwrd = Core(ii-1)
      DO io = 4 , 14
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
         iop = io - 3
         IF ( iwrd==outop(io) ) THEN
            IF ( iop==1 ) GOTO 5200
            IF ( iop==2 ) GOTO 4850
            IF ( iop==3 ) GOTO 4900
            IF ( iop==4 ) GOTO 5000
            IF ( iop==5 ) GOTO 5050
            IF ( iop==6 ) GOTO 5100
            IF ( iop==7 ) GOTO 4950
            IF ( iop==8 .OR. iop==10 ) EXIT
            IF ( iop==9 ) GOTO 5250
            IF ( iop==11 ) GOTO 5300
         ENDIF
!         SORT PUNC PRIN REAL IMAG PHAS NOPR MAXS VONM EXTR LAYE
!
      ENDDO
      CYCLE
!
!     PUNCH
!
 4850 Case(ik+1,Isub) = Case(ik+1,Isub) + 4
      CYCLE
!
!     PRINT
!
 4900 Case(ik+1,Isub) = Case(ik+1,Isub) + 1
      CYCLE
!
!     COMPUTE BUT NO PRINT
!     DEVICE CODE IS 2 (AND SUBPRESS PRINT CODE 1)
!
 4950 Case(ik+1,Isub) = Case(ik+1,Isub) - mod(Case(ik+1,Isub),2) + 2
      CYCLE
!
!     REAL PRINT OUT FORMAT
!
 5000 ii = 1
      GOTO 5150
!
!     REAL AND IMAGINARY
!
 5050 ii = 2
      GOTO 5150
!
!     MAGNITUE AND PHASE ANGLE
!
 5100 ii = 3
 5150 Case(ik+2,Isub) = isign(ii,Case(ik+2,Isub))
      CYCLE
!
!     SORT TWO REQUEST
!     (COMMENTS FORM G.C.  7/1989
!     SINCE OES2L FILE HAS NOT BEEN IMPLEMENTED IN ALL DMAPS, SORT2
!     STRESS REQUEST ON LAYERED ELEMENTS IS NOT AVAILABLE)
!
 5200 temp = Core(ii)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
      IF ( temp/=oneb ) THEN
         IF ( ik==23 .AND. Case(183,Isub)>=2 ) CALL ifp1d(-645)
         Case(ik+2,Isub) = -iabs(Case(ik+2,Isub))
      ENDIF
      CYCLE
!
!     VON MISES STRESS
!     (183 WORD ON CASECC, FIRST RIGHT-MOST BIT)
!
 5250 Case(183,Isub) = orf(Case(183,Isub),1)
      CYCLE
!
!     LAYER STRESSES FOR COMPOSITE ELEMENTS
!     (183 WORD ON CASECC, SECOND RIGHT-MOST BIT)
!     (SORT2 STRESS REQUEST ON LAYERED ELEMENTS NOT AVAILABLE)
!
 5300 IF ( ik/=23 ) CALL ifp1d(-646)
      IF ( ik==23 .AND. Case(25,Isub)<0 ) CALL ifp1d(-645)
      Case(183,Isub) = orf(Case(183,Isub),2)
!
   ENDDO
   IF ( Case(ik+1,Isub)==0 ) Case(ik+1,Isub) = 1
   IF ( Core(ii+1)/=0 ) THEN
      temp = Core(ii+1)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
      IF ( temp==all ) THEN
!
!     ALL SPECIFIED -- SET SET NO. MINUS
!
         Case(ik,Isub) = -1
      ELSEIF ( temp==none .OR. temp==nono ) THEN
!
!     NONE SPECIFIED
!
         Case(ik,Isub) = none
      ELSE
         IF ( Core(ii+1)==-1 ) THEN
            i1 = ii + 2
!
!     FIND SET NUMBER
!
            IF ( Nset/=0 ) THEN
               jj = nwdsc
               DO il = 1 , Nset
                  IF ( Core(jj)==Core(i1) ) GOTO 5320
                  jj = jj + Core(jj+1) + 3
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
         GOTO 1500
 5320    Case(ik,Isub) = Core(i1)
      ENDIF
   ELSE
      CALL ifp1d(610)
      Case(ik,Isub) = -1
   ENDIF
   IF ( Core(ii+3)/=Ieor ) THEN
      il = -603
      CALL ifp1d(il)
   ENDIF
   GOTO 1500
!
!     SET CARD
!
 5400 Nset = Nset + 1
   CALL ifp1c(i81,nz)
   GOTO 1500
!
!     SCAN CARD
!
 5500 CALL ifp1h(i81,nz,jumph)
   GOTO 1500
!
!     SUBCASE
!
 5600 temp = Core(i81+2)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp==om ) GOTO 7700
   IF ( isymcm==1 ) THEN
!
!     PUT OUT SUBCOM OR SYMCOM RECORD
!
      isymcm = 0
      GOTO 6300
   ELSE
      Nsym = 0
      nsyms = nsyms + 1
      IF ( nsyms<361 ) THEN
         symseq(nsyms) = 1.0
      ELSEIF ( nsyms==361 ) THEN
         CALL ifp1d(-633)
      ENDIF
   ENDIF
 5700 ASSIGN 1500 TO iret3
   IF ( Isub/=2 ) THEN
      Isub = 2
      loadn = Core(i81+4)
      CALL ifp1f(*1500,iword,i2)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      DO i = 1 , 5
         isubc(i) = Core(i2)
         i2 = i2 + 1
      ENDDO
      IF ( Core(i81+3)+1/=0 ) THEN
!
!     SUBCASE ID MISSING
!
         il = -609
         loadn = Case(1,2)
         CALL ifp1d(il)
      ENDIF
      GOTO 1500
   ENDIF
!
!     TURN STRESS AND FORCE NO-PRINT FLAGS ON IF INTERACTIVE FLAG IS ON
!
 5800 IF ( Intra>=2 ) THEN
      Case(24,Isub) = orf(Case(24,Isub),8)
      Case(27,Isub) = orf(Case(27,Isub),8)
   ENDIF
!
   Case(1,Isub) = loadn
   IF ( Core(i81+4)<=loadn+Nmodes-1 ) GOTO 6100
   loadn = Core(i81+4)
   IF ( Core(i81+3)/=-1 ) GOTO 6100
 5900 IF ( Case(137,1)==0 ) Case(137,1) = 1
   CALL ifp1e(isubc(1),symseq,nwdsc,i81,icaste)
   Stftem = icaste
   nsub = nsub + Nmodes
!
!     CHECK SET NOS. THAT WERE SPECIFIED AFTER SCAN CARDS
!
!     FORM G.C./UNISYS   4/1990
!     IFP1H IS BY-PASSING THIS NEW CODE HERE (MSST=0) BECAUSE SET DATA
!     IS NOT AVAILABLE HERE. SAVE THIS CODE FOR FURTHER INVESTIGATION.
!
   IF ( Msst/=0 ) THEN
      mm = 0
      ll = Lencc + Case(Lencc,Isub) + 1
      DO m = 1 , Msst
         i = ll
         mset = Misset(m)
         DO
!
!     WRITE (6,2345) MSET,MSST,LL
!
            iset = Case(i,Isub)
!
!     LX1 = I - 3
!     LX2 = I + 3
!     WRITE (6,6789) ISET,(CASE(LX,ISUB),LX=LX1,LX2)
!
            IF ( iset==0 ) EXIT
            IF ( mset/=iset ) THEN
               i = i + Case(i+1,Isub)
               IF ( i>=400 ) EXIT
            ELSE
               Misset(m) = 0
               mm = mm + 1
               EXIT
            ENDIF
         ENDDO
      ENDDO
      IF ( mm/=Msst ) THEN
         DO m = 1 , Msst
            IF ( Misset(m)/=0 ) THEN
               WRITE (Otpe,99002) Ufm , Misset(m)
99002          FORMAT (A23,' 608A, UNIDENTIFIED SET',I8,' WAS REQUESTED FOR ','SCAN')
               Nogo = 1
            ENDIF
         ENDDO
      ENDIF
   ENDIF
!
 6000 GOTO iret3
 6100 CALL ifp1d(-609)
   loadn = Case(1,2)
   GOTO 5900
 6200 Core(i81+3) = -1
   Core(i81+4) = 9999999
   IF ( icasec==1 ) GOTO 6000
   icasec = 1
   IF ( isymcm==1 ) THEN
      isymcm = 0
   ELSE
      Nsym = 0
      GOTO 5800
   ENDIF
!
!     NO SUBSEQ OR SYMSEQ CARD
!
 6300 IF ( nsymsq==0 .AND. Nsym==0 ) Nsym = nsyms
!
   nsymsq = 0
   Case(Lencc,2) = max0(Nsym,0)
   Case(16,2) = Nsym
   GOTO 5800
 6400 CALL close(Scr1,1)
   IF ( ibob/=1 .AND. ixypl/=1 ) CALL close(Casecc,1)
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
   file = Casecc
   CALL open(*900,Casecc,Corex(ibuf1),0)
   file = nptp
   maxcc = 0
   CALL open(*900,nptp,Corex(ibuf2),3)
 6500 CALL read(*6800,*6600,Casecc,Core(1),nz,0,flag)
   icrq = nz
   GOTO 1300
 6600 CALL write(nptp,Core(1),flag,1)
   maxcc = max0(maxcc,flag)
!
!     CHECK ANY PUNCH REQUEST  ON OUTPUT DATA BLOCKS
!
   IF ( npch/=1 .AND. flag>=166 ) THEN
      DO i = 1 , 13
         j = outpch(i)
         IF ( andf(Core(j),4)/=0 ) GOTO 6700
      ENDDO
   ENDIF
   GOTO 6500
 6700 npch = 1
   GOTO 6500
 6800 CALL close(Casecc,1)
   CALL eof(nptp)
   CALL close(nptp,2)
   IF ( Splots<0 ) Splots = 0
!
!     IF THIS IS A RESTART  SET CHANGE FLAGS IN IFP1B
!
   IF ( App<0 ) CALL ifp1b
   IF ( iun/=0 ) CALL ifp1d(-612)
   CALL makmcb(Core,Casecc,nsub,0,0)
   Core(2) = nsub
   Core(4) = maxcc
   CALL wrttrl(Core)
!
!     SET NOGO FLAG TO -9 IF ERROR IN BULKDATA AND PLOT COMMANDS
!     SET NOGO FLAG TO POSITIVE IF ERROR IN BULKDATA, AND NOT IN PLOT
!     SET NOGO FLAG TO NEGATIVE IF NO ERROR IN BULKDATA, BUT IN PLOT
!     PUNCH AN IDENTIFICATION CARD IF PUNCH IS REQUESTED ON OUTPUT DATA,
!     AND PRINT SCAN KEYWORDS IF ERROR FLAG (JUMPH) WAS TURNED ON
!
   IF ( Nogo/=0 .AND. nogopc==-1 ) Nogo = -9
   IF ( Nogo==0 ) Nogo = nogopc
   IF ( npch==1 ) WRITE (Lpch,99003) (Title(j),j=1,17)
99003 FORMAT (2H$ ,17A4)
   IF ( jumph==1 ) CALL ifp1h(0,0,2)
   RETURN
!
!     ECHO REQUEST
!
 6900 Iecho = 0
   ido = Core(i81) - 2
   DO i = 1 , ido
      iwrd = Core(i1)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      DO io = 1 , 5
         IF ( iwrd==outop(io) ) THEN
            IF ( io==1 ) GOTO 7100
            IF ( io==2 ) GOTO 7400
            IF ( io==3 ) GOTO 7150
            IF ( io==4 ) GOTO 6950
            IF ( io==5 ) GOTO 7050
         ENDIF
!                                     BOTH  NONE  UNSO  SORT  PUNC
!
      ENDDO
      IF ( iwrd==outop(15) ) GOTO 7300
      CALL ifp1d(629)
      GOTO 7000
!
!     SORTED ECHO
!
 6950 IF ( andf(Iecho,2)/=0 ) CALL ifp1d(629)
 7000 Iecho = orf(Iecho,2)
      GOTO 7200
!
!     PUNCH ECHO
!
 7050 IF ( andf(Iecho,4)/=0 ) CALL ifp1d(629)
      Iecho = orf(Iecho,4)
      npch = 1
      GOTO 7200
!
!     BOTH ECHO
!
 7100 IF ( andf(Iecho,3)/=0 ) CALL ifp1d(629)
      Iecho = orf(Iecho,3)
      GOTO 7200
!
!     UNSORTED ECHO
!
 7150 IF ( andf(Iecho,1)/=0 ) CALL ifp1d(629)
      Iecho = orf(Iecho,1)
 7200 i1 = i1 + 2
   ENDDO
!
   GOTO 1500
!
!     NONO ECHO - ABSOLUTELY NO ECHO, NO EVEN IN RESTART
!
 7300 io = 16
!
!     NONE ECHO
!
 7400 IF ( Iecho/=0 .OR. i<ido ) CALL ifp1d(630)
   Iecho = -1
   IF ( io==16 ) Iecho = -2
   GOTO 1500
!
!     LOOP CONTROL FOR EIGENVALUE
!
 7500 Nmodes = Core(i1)
   GOTO 1500
 7600 IF ( Core(i1)<0 ) THEN
      ip = 2
   ELSEIF ( Core(i1)==0 ) THEN
      CALL write(pcdb,Core(i81),ik+1,1)
      GOTO 1500
   ELSEIF ( Core(i1)==Ieor ) THEN
      CALL write(pcdb,Core(i81),ik+1,1)
      GOTO 1500
   ELSE
      ip = 2*Core(i1) + 1
   ENDIF
   ik = ik + ip
   i1 = i1 + ip
   GOTO 7600
!
!     SYMCOM OR SUBCOM CARD
!
 7700 IF ( isymcm==0 ) THEN
      isymcm = 1
      nsymsq = 0
      GOTO 5700
   ELSE
      ASSIGN 1500 TO iret3
      GOTO 6300
   ENDIF
!
!     LINE CARD - NLPP BOTTOM-LIMITED TO 10
!
 7800 IF ( Core(i1-1)/=-1 ) THEN
      il = -604
      CALL ifp1d(il)
   ELSE
      IF ( iabs(Core(i1))>0 ) Nlpp = iabs(Core(i1))
      IF ( Nlpp<10 ) Nlpp = 10
   ENDIF
   GOTO 1500
!
!     DIFFERENTIAL STIFFNESS OR PIECEWISE LINEAR COEFFICIENT SET
!
 7900 ik = 138
   GOTO 8100
 8000 ik = 164
 8100 temp = Core(i1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp/=defa ) THEN
      IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   ELSE
      Core(i1) = -1
      Core(i1+1) = Ieor
      Core(i1-1) = -1
   ENDIF
   GOTO 2200
!
!     K2PP
!
 8200 ik = 139
 8300 Case(ik,Isub) = Core(i1)
   Case(ik+1,Isub) = Core(i1+1)
   GOTO 1500
!
!     M2PP
!
 8400 ik = 141
   GOTO 8300
!
!     B2PP
!
 8500 ik = 143
   GOTO 8300
!
!     REPRINT OF ABOVE CASE
!
 8600 Nsym = -1
   IF ( Isub/=2 ) CALL ifp1d(-607)
   GOTO 7700
!
!     TRANSFER FUNCTION SELECTION
!
 8700 ik = 15
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     OUTPUT FREQUENCY LIST SET
!
 8800 ik = 145
   temp = Core(i1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp/=all ) GOTO 4700
   Core(i1) = -1
   Core(i1-1) = -1
   Core(i1+1) = Ieor
   GOTO 2200
!
!     COMPLEX EIGENVALUE METHOD
!
 8900 ik = 148
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     STRUCTURAL DAMPING TABLE
!
 9000 ik = 149
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     INERTIA RELIEF SET SELECTION
!
!1770 IK = 150
!     GO TO 490
!
!     ANALYSIS SET FOR VDR
!
 9100 ik = 151
   GOTO 4700
!
!     ANALYSIS VELOCITY
!
 9200 ik = 154
   GOTO 4700
!
!     ANALYSIS ACCELERATION
!
 9300 ik = 157
   GOTO 4700
!
!     NON LINEAR FORCE VECTOR FOR TRANSIENT ANALYSIS
!
 9400 ik = 160
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
 9500 CALL close(Casecc,1)
   DO i = 2 , 6
      Core(i) = 0
   ENDDO
   Core(1) = xycb
   Core(7) = 1
   CALL wrttrl(Core(1))
!
!     OPEN XYCB
!
   IF ( ibob==1 ) THEN
      CALL close(pcdb,1)
      ibob = 0
   ENDIF
   file = xycb
   ixypl = 1
   i81 = Nwpc + 1
   GOTO 3100
!
!     AXIS TITLE CARDS
!
 9600 itype = 8
   Core(1) = iword
   DO i = 1 , 32
      k = i81 + i - 1
      Core(k) = Blank
   ENDDO
   IF ( ibob==1 ) THEN
!
!     PLOT TITLE CARD
!
      Core(i81) = 10
      Core(i81+1) = iword
      Core(i81+2) = Blank
      CALL ifp1g(itype,Core(i81+3),1)
      Core(i81+21) = 9999999
      ik = 21
      CALL write(pcdb,Core(i81),ik+1,1)
   ELSE
      CALL ifp1g(itype,Core(i81),1)
      CALL ifp1xy(ihowdy,xintcd)
   ENDIF
   GOTO 1500
!
!     DELETE SETS FOR FORCE
!
!1840 IK = 161
!     GO TO 490
!
!     AXISYM CARD
!
 9700 temp = Core(i1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp==sine ) THEN
      Case(136,Isub) = 1
      Iaxic = 1
   ELSE
      IF ( temp/=cosi ) THEN
         IF ( temp/=flui ) THEN
            IF ( temp==symm ) THEN
               Case(136,Isub) = -2
               GOTO 9800
            ELSEIF ( temp==anti ) THEN
               Case(136,Isub) = -1
               GOTO 9800
            ELSE
               IF ( temp==anom ) THEN
                  Case(136,Isub) = -30
               ELSE
!
!     ILLEGAL  SPECIFICATION
!
                  il = -617
                  CALL ifp1d(il)
               ENDIF
               GOTO 1500
            ENDIF
         ENDIF
      ENDIF
      Case(136,Isub) = 2
      IF ( temp==cosi ) Iaxic = 1
      IF ( temp==flui ) Iaxif = 1
   ENDIF
   GOTO 1500
 9800 temp = Core(i1+1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp==anom ) Case(136,Isub) = Case(136,Isub)*10
   GOTO 1500
!
!     HARMONIC SELECTOR
!
 9900 ik = 137
   temp = Core(i1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,temp,0)
   IF ( temp==all ) THEN
      Core(i1) = -1
   ELSEIF ( temp==none ) THEN
      Case(137,1) = 0
      Core(i1) = 0
   ELSE
      Core(i1) = Core(i1) + 1
      IF ( Core(i1)<=0 ) CALL ifp1d(-617)
      GOTO 2200
   ENDIF
   Core(i1-1) = -1
   Core(i1+1) = Ieor
   GOTO 2200
!
!     RANDOM SET SELECTION
!
10000 ik = 163
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     FMETHOD
!
10100 ik = 165
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     GRID POINT FORCE REQUEST
!
10200 ik = 167
   GOTO 4700
!
!     ELEMENT STRAIN ENERGY
!
10300 ik = 170
   GOTO 4700
!
!     AEROFORCE OUTPUT REQUEST
!
10400 ik = 176
   GOTO 4700
!
!     AEROELASTIC GUST LOAD REQUEST
!
10500 ik = 179
   IF ( Core(i1)<=0 ) CALL ifp1d(-617)
   GOTO 2200
!
!     STRAIN CARD
!     (180 THRU 182 WORDS OF CASECC)
!
10600 ik = 180
   GOTO 4700
!
!     EOF ON INPUT UNIT
!
10700 CALL ifp1d(-624)
   CALL mesage(-37,0,nifp)
END SUBROUTINE ifp1