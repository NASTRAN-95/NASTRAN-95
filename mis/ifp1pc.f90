
SUBROUTINE ifp1pc(I81,Icont,Pocard,Org,Porg)
!
!     SUBROUTINE TO PERFORM FIRST-LEVEL CHECKING OF STRUCTURE PLOTTER
!     CONTROL CARD FORMAT.
!
   IMPLICIT NONE
   LOGICAL Bit64
   INTEGER Blank , Case(400) , Corex(1) , Corey(401) , Ilink , Intra , Isys , Nogo , Nout , Pltopt , Skp(16) , Skp63(63) , Sys21
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /system/ Isys , Nout , Nogo , Skp , Pltopt , Sys21 , Ilink , Skp63 , Intra
   COMMON /xifp1 / Blank , Bit64
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Corex
   INTEGER I81 , Icont , Org , Porg
   INTEGER Pocard(1)
   INTEGER allon , anti , axes(3) , camera(5) , cntur(20) , coord(25) , core(1) , ctype(21) , defo , eor , forg , hidd , i , iax ,  &
         & icrd , idvpr(3) , ierr , ilnk , integ , ipr1 , ipr2 , iprm , iro , irtn , isplot , istb , istt , itype , ivc , iword ,   &
         & iwrd , j , lag , lblpr(5) , magn , maxes(3) , mode , msgno , nast(2) , ncrd , nint , nopt , nreal , nro , nthru ,        &
         & origin(11) , plan , pltpr(28) , poin , porg1 , proj , sepa , setp2(12) , setpr(33) , symm , ter , thru
   INTEGER complf , rshift
   LOGICAL flag(3)
   EXTERNAL complf , rshift
   EQUIVALENCE (proj,ctype(11)) , (defo,idvpr(1)) , (symm,pltpr(13)) , (anti,pltpr(14)) , (magn,cntur(13)) , (thru,pltpr(22)) ,     &
    & (poin,lblpr(2)) , (core(1),Corey(401)) , (Corex(1),Corey(1),Case(1)) , (hidd,pltpr(24))
   DATA ctype/4HPLOT , 4HORTH , 4HPERS , 4HSTER , 4HAXES , 4HVIEW , 4HMAXI , 4HCSCA , 4HFIND , 4HCONT , 4HPROJ , 4HOCUL , 4HCAME ,  &
       &4HPAPE , 4HPEN  , 4HPTIT , 4HSCAL , 4HORIG , 4HVANT , 4HSET  , 4HREGI/
   DATA camera/4HFILM , 4HPAPE , 4HBOTH , 4HBLAN , 4HFRAM/
   DATA axes/4HX    , 4HY    , 4HZ   /
   DATA maxes/4HMX   , 4HMY   , 4HMZ  /
   DATA cntur/4HMAJP , 4HMINP , 4HMAXS , 4HXNOR , 4HYNOR , 4HZNOR , 4HXYSH , 4HXZSH , 4HYZSH , 4HXDIS , 4HYDIS , 4HZDIS , 4HMAGN ,  &
       &4HNRM1 , 4HNRM2 , 4HSH12 , 4HSH1Z , 4HSH2Z , 4HBDSH , 4HSTRA/
   DATA setpr/4HINCL , 4HEXCL , 4HEXCE , 4HELEM , 4HGRID , 4HALL  , 4HAERO , 4HAXIF , 4HBAR  , 4HCONE , 4HCONR , 4HHEXA , 4HFLUI ,  &
       &4HIHEX , 4HPLOT , 4HQDME , 4HQDPL , 4HQUAD , 4HROD  , 4HSHEA , 4HSLOT , 4HTETR , 4HTORD , 4HTRAP , 4HTRBS , 4HTRIA ,        &
      & 4HTRME , 4HTRPL , 4HTUBE , 4HTWIS , 4HVISC , 4HWEDG , 4HHBDY/
   DATA setp2/4HAX   , 4HRG   , 4H1    , 4H2    , 4H3    , 4H4    , 4HD2   , 4HD3   , 4HD4   , 4HM    , 4HM1   , 4HM2  /
   DATA pltpr/4HSET  , 4HSTAT , 4HMODA , 4HCMOD , 4HFREQ , 4HTRAN , 4HCONT , 4HRANG , 4HTIME , 4HPHAS , 4HMAGN , 4HORIG , 4HSYMM ,  &
       &4HANTI , 4HPEN  , 4HDENS , 4HSYMB , 4HLABE , 4HSHAP , 4HVECT , 4HOUTL , 4HTHRU , 4HMAXI , 4HHIDD , 4HSHRI , 4HNOFI ,        &
      & 4HFILL , 4HOFFS/
   DATA idvpr/4HDEFO , 4HVELO , 4HACCE/
   DATA coord/4HYX   , 4HZX   , 4HZY   , 4HXY   , 4HXZ   , 4HYZ   , 4HX    , 4HY    , 4HZ    , 4HXYZ  , 4HRXY  , 4HRXZ  , 4HRYZ  ,  &
       &4HR    , 4HRN   , 4HXN   , 4HYN   , 4HZN   , 4HXYN  , 4HXZN  , 4HYZN  , 4HXYZN , 4HRXYN , 4HRXZN , 4HRYZN/
   DATA lblpr/4HGRID , 4HPOIN , 4HELEM , 4HBOTH , 4HEPID/
   DATA ter/4HTER / , plan/4HPLAN/ , sepa/4HSEPA/
   DATA lag/4HLAG / , nast/4HSC   , 4HCALC/ , ilnk/4HNS01/
!
!
!     INITIALIZE
!
   IF ( Intra>1 .OR. Ilink/=ilnk ) THEN
      DO i = 1 , 200
         core(i) = Pocard(i)
      ENDDO
   ENDIF
   allon = complf(0)
   eor = rshift(allon,1)
   isplot = 0
   iwrd = I81
!
!     BRANCH FOR CONTINUATION CARD
!                                  SET   PLOT  FIND
   IF ( Icont/=0 ) THEN
      IF ( Icont==1 ) GOTO 100
      IF ( Icont==2 ) GOTO 4000
      IF ( Icont==3 ) GOTO 6100
      IF ( Icont==4 ) GOTO 2200
   ENDIF
!
   IF ( core(iwrd)<0 ) THEN
!
!     SET UP ERROR MESSAGE
!
      ASSIGN 9000 TO ierr
      msgno = 348
      GOTO 8700
   ELSEIF ( core(iwrd)==0 ) THEN
      GOTO 600
   ELSE
      GOTO 200
   ENDIF
 100  IF ( core(iwrd)<=0 ) GOTO 400
 200  IF ( core(iwrd)==eor ) GOTO 600
   mode = core(iwrd)
   iwrd = iwrd + 1
!
!     BRANCH FOR CARD TYPE
!
 300  iword = core(iwrd)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   DO i = 1 , 20
      IF ( iword==ctype(i) ) THEN
         IF ( i==1 ) GOTO 700
         IF ( i==2 .OR. i==3 .OR. i==4 ) GOTO 800
         IF ( i==5 ) GOTO 1000
         IF ( i==6 ) GOTO 1100
         IF ( i==7 ) GOTO 1200
         IF ( i==8 ) GOTO 1300
         IF ( i==9 ) GOTO 1400
         IF ( i==10 ) GOTO 2500
         IF ( i==11 ) GOTO 2600
         IF ( i==12 ) GOTO 2700
         IF ( i==13 ) GOTO 2900
         IF ( i==14 .OR. i==15 .OR. i==16 ) GOTO 400
         IF ( i==17 ) GOTO 3400
         IF ( i==18 ) GOTO 3500
         IF ( i==19 ) GOTO 3600
         IF ( i==20 ) GOTO 3700
      ENDIF
!
!    1         PLOT  ORTH  PERS  STER  AXES  VIEW  MAXI  CSCA  FIND
!    2         CONT  PROJ  OCUL  CAME  PAPE   PEN  PTIT  SCAL  ORIG
!    3         VANT   SET
!
   ENDDO
   ASSIGN 9100 TO ierr
   msgno = 349
   GOTO 8700
 400  DO WHILE ( mode>0 )
      iwrd = iwrd + 2
      mode = mode - 1
   ENDDO
 500  DO WHILE ( core(iwrd)<0 )
      IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
      iwrd = iwrd + 2
   ENDDO
   IF ( core(iwrd)/=0 .AND. core(iwrd)/=eor ) THEN
      mode = core(iwrd)
      iwrd = iwrd + 1
      GOTO 400
   ENDIF
 600  Icont = 0
   IF ( core(iwrd)==0 ) Icont = 1
   GOTO 10600
!
!     BRANCH TO PLOT OR PLOTTER
!
 700  iword = core(iwrd+1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( iword==ter ) THEN
!
!     PLOTTER CARD
!
      iword = core(iwrd+2)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( iword/=nast(1) .AND. iword/=nast(2) ) GOTO 400
      ASSIGN 9200 TO ierr
      msgno = 350
      GOTO 8800
   ELSE
      isplot = 1
!
!     PLOT COMMAND CARD
!
      iwrd = iwrd + 2
      mode = mode - 1
      GOTO 5800
   ENDIF
!
!     PROJECTION CARD
!
 800  iwrd = iwrd + 2
   mode = mode - 1
   iword = core(iwrd)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( iword/=proj ) THEN
      ASSIGN 900 TO irtn
      iprm = proj
      GOTO 7700
   ENDIF
 900  iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode>0 ) GOTO 300
   GOTO 500
 1000 DO
!
!     AXES CARD
!
      iwrd = iwrd + 2
      mode = mode - 1
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
         DO j = 1 , 3
            flag(j) = .FALSE.
         ENDDO
         i = 0
         EXIT
      ENDIF
   ENDDO
   DO
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
         DO j = 1 , 3
            IF ( iword==axes(j) .OR. iword==maxes(j) ) flag(j) = .TRUE.
         ENDDO
         i = i + 1
      ENDIF
      iwrd = iwrd + 2
      CALL anullsub(iwrd)
      mode = mode - 1
      IF ( i<3 ) THEN
         iword = core(iwrd)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      ELSE
!
         ASSIGN 400 TO irtn
         IF ( .NOT.flag(1) .OR. .NOT.flag(2) .OR. .NOT.flag(3) ) GOTO 7900
         DO
            iword = core(iwrd)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
            IF ( iword==symm .OR. iword==anti ) THEN
               iwrd = iwrd + 2
               mode = mode - 1
               IF ( mode>0 ) GOTO 300
               GOTO 500
            ELSE
               IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) GOTO 600
               IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 300
               iwrd = iwrd + 2
               mode = mode - 1
               IF ( mode<=0 ) GOTO 8000
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!     VIEW COMMAND
!
 1100 nreal = 3
   nopt = 0
   GOTO 2800
!
!     MAXIMUM DEFORMATION CARD
!
 1200 nreal = 1
   nopt = 0
   iword = core(iwrd+2)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( iword==defo ) GOTO 2800
   ASSIGN 400 TO irtn
   iprm = core(iwrd+2)
   GOTO 7800
!
!     CSCALE CARD
!
 1300 ASSIGN 400 TO irtn
   DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) THEN
         IF ( core(iwrd)+1<0 ) THEN
!
            nreal = 1
            nopt = 0
            GOTO 3100
         ELSEIF ( core(iwrd)+1==0 ) THEN
            WRITE (Nout,99001)
99001       FORMAT (/5X,'REAL VALUE, NOT INTEGER, IS NOW USED FOR CSCALE')
         ENDIF
         GOTO 8200
      ELSE
         iword = core(iwrd)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 8000
      ENDIF
   ENDDO
!
!     FIND COMMAND
!
 1400 iwrd = iwrd + 2
   mode = mode - 1
 1500 DO WHILE ( core(iwrd)/=0 .AND. core(iwrd)/=eor )
      ASSIGN 2300 TO irtn
      IF ( mode<=0 ) GOTO 8000
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)==allon .OR. iword==Blank ) GOTO 1400
      DO i = 17 , 21
         itype = i - 16
         IF ( iword==ctype(i) ) THEN
            IF ( itype==1 ) GOTO 1550
            IF ( itype==2 .OR. itype==4 ) GOTO 1600
            IF ( itype==3 ) GOTO 1800
            IF ( itype==5 ) GOTO 1900
         ENDIF
!                SCAL  ORIG  VANT   SET  REGI
!
      ENDDO
      iprm = core(iwrd)
      GOTO 7800
!
 1550 nreal = 1
      DO
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            IF ( core(iwrd)/=0 .AND. core(iwrd)/=eor ) GOTO 2000
            GOTO 2400
         ELSE
            iword = core(iwrd)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 1700
         ENDIF
      ENDDO
!
 1600 iprm = core(iwrd)
      ASSIGN 1500 TO irtn
      DO
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            integ = 1
            IF ( core(iwrd)==eor ) GOTO 8100
            IF ( core(iwrd)==-1 ) integ = 0
            IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
            IF ( itype==2 ) THEN
               forg = core(iwrd+1)
               Org = Org + 1
               origin(Org) = forg
            ENDIF
            iwrd = iwrd + 2
            IF ( Porg<0 ) THEN
               Porg = 0
               porg1 = forg
            ENDIF
            GOTO 2100
         ELSE
            iword = core(iwrd)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 8100
         ENDIF
      ENDDO
 1700 ENDDO
   GOTO 2400
!
 1800 iwrd = iwrd + 2
   mode = mode - 1
   ASSIGN 2300 TO irtn
   IF ( mode<=0 ) THEN
      iprm = poin
      GOTO 7700
   ELSE
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( iword==poin ) GOTO 1400
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
!
 1900 nreal = 4
   DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) EXIT
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 8300
   ENDDO
 2000 integ = 0
   ASSIGN 1500 TO irtn
   DO i = 1 , nreal
      IF ( core(iwrd)==-1 ) integ = 1
      IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
      iwrd = iwrd + 2
   ENDDO
 2100 IF ( integ>0 ) GOTO 8200
 2200 IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) GOTO 2400
   mode = core(iwrd)
   iwrd = iwrd + 1
   GOTO 1500
!
 2300 DO WHILE ( core(iwrd)/=0 .AND. core(iwrd)/=eor )
      iwrd = iwrd + 1
   ENDDO
 2400 Icont = 0
   IF ( core(iwrd)==0 ) Icont = 4
   GOTO 10600
!
!     CONTOUR
!
 2500 iwrd = iwrd + 2
   mode = mode - 1
   ASSIGN 400 TO irtn
   DO WHILE ( core(iwrd)/=0 .AND. core(iwrd)/=eor )
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)==allon .OR. iword==Blank ) THEN
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) GOTO 8000
      ELSE
         DO i = 1 , 20
            IF ( iword==cntur(i) ) GOTO 400
         ENDDO
         iprm = core(iwrd)
         GOTO 7800
      ENDIF
   ENDDO
   GOTO 600
!
!     PROJECTION PLANE SEPARATION
!
 2600 iwrd = iwrd + 2
   mode = mode - 1
   ASSIGN 400 TO irtn
   IF ( mode<=0 ) THEN
      iprm = plan
      GOTO 7700
   ELSE
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( iword/=plan ) THEN
         iprm = core(iwrd)
         GOTO 7800
      ELSE
         iword = core(iwrd+2)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( iword==sepa ) THEN
            nreal = 1
            nopt = 0
            GOTO 2800
         ELSE
            iprm = core(iwrd)
            GOTO 7800
         ENDIF
      ENDIF
   ENDIF
!
!     OCULAR SEPARATION
!
 2700 nreal = 1
   nopt = 0
   iword = core(iwrd+2)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( iword/=sepa ) THEN
      ASSIGN 400 TO irtn
      iprm = core(iwrd+2)
      GOTO 7800
   ENDIF
 2800 DO
!
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) GOTO 3100
   ENDDO
 2900 DO
!
!     CAMERA
!
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) EXIT
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
         IF ( core(iwrd)==eor .OR. core(iwrd)==0 ) GOTO 8400
         DO i = 1 , 4
            IF ( iword==camera(i) ) GOTO 2920
         ENDDO
         iprm = core(iwrd)
         ASSIGN 400 TO irtn
         GOTO 7800
 2920    DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) GOTO 3000
            iword = core(iwrd)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
               i = i + 1
               IF ( iword/=camera(4) .AND. iword/=camera(5) ) THEN
                  ASSIGN 400 TO irtn
                  IF ( i==4 ) GOTO 8000
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDDO
 3000 IF ( core(iwrd)==eor .OR. core(iwrd)==0 ) THEN
      IF ( i>3 ) GOTO 8400
      GOTO 600
   ELSE
      ASSIGN 400 TO irtn
      IF ( core(iwrd)+1/=0 ) GOTO 8200
      iwrd = iwrd + 2
      GOTO 100
   ENDIF
!
!     TEST FOR REAL VALUES
!
 3100 iro = 0
   nro = nreal
 3200 integ = 0
   ASSIGN 400 TO irtn
   DO i = 1 , nro
      IF ( core(iwrd)>=0 .OR. core(iwrd)<-4 ) THEN
         IF ( iro<=0 ) GOTO 8300
      ENDIF
      IF ( core(iwrd)==-1 ) integ = 1
      IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
      iwrd = iwrd + 2
   ENDDO
   IF ( integ/=0 ) THEN
      ASSIGN 3300 TO irtn
      GOTO 8200
   ENDIF
 3300 IF ( core(iwrd)<0 ) THEN
      IF ( iro==1 .OR. nopt==0 ) GOTO 8000
      iro = 1
      nro = nopt
      GOTO 3200
   ELSEIF ( core(iwrd)==0 ) THEN
      GOTO 600
   ELSE
      GOTO 200
   ENDIF
!
!     SCALE
!
 3400 nreal = 1
   nopt = 1
   GOTO 2800
!
!     ORIGIN
!
 3500 nreal = 3
   nopt = 0
   DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) EXIT
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) EXIT
   ENDDO
   IF ( core(iwrd)==-1 ) THEN
      IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
      iwrd = iwrd + 2
      ASSIGN 400 TO irtn
      IF ( core(iwrd)==eor ) GOTO 8300
      IF ( core(iwrd)<0 ) GOTO 3100
      mode = core(iwrd)
      iwrd = iwrd + 1
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 8000
      GOTO 2800
   ELSE
      iprm = ctype(18)
      ASSIGN 400 TO irtn
      GOTO 8100
   ENDIF
!
!     VANTAGE POINT
!
 3600 nreal = 3
   nopt = 1
   iword = core(iwrd+2)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( iword==poin ) GOTO 2800
   ASSIGN 400 TO irtn
   iprm = core(iwrd+2)
   GOTO 7800
!
!     SET DEFINITION CARD
!
 3700 nint = 0
   nthru = 0
   DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) THEN
         IF ( core(iwrd)==-1 ) GOTO 3900
         ASSIGN 3800 TO irtn
         GOTO 8200
      ELSE
         iword = core(iwrd)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
            iprm = ctype(20)
            ASSIGN 4300 TO irtn
            GOTO 8100
         ENDIF
      ENDIF
   ENDDO
 3800 IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
!
 3900 iwrd = iwrd + 2
   nreal = 0
 4000 IF ( core(iwrd)<0 ) THEN
      nint = nint + 1
      IF ( core(iwrd)==-1 .OR. nreal/=0 ) GOTO 3900
      ASSIGN 3900 TO irtn
      GOTO 8200
   ELSEIF ( core(iwrd)/=0 ) THEN
      GOTO 4200
   ENDIF
 4100 Icont = 2
   nthru = 0
   GOTO 10600
 4200 IF ( core(iwrd)/=eor ) THEN
      mode = core(iwrd)
      iwrd = iwrd + 1
   ELSE
      Icont = 0
      GOTO 10600
   ENDIF
 4300 DO
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
         iword = core(iwrd)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( iword/=thru ) GOTO 4500
         nthru = nthru + 1
         IF ( core(iwrd-3)==-1 .AND. core(iwrd+2)==-1 ) THEN
            IF ( nthru==1 ) EXIT
            IF ( nint>=2 .AND. core(iwrd-2)>core(iwrd-4) ) EXIT
            ASSIGN 4400 TO irtn
            ASSIGN 10200 TO ierr
            msgno = 359
            GOTO 8700
         ELSE
            ASSIGN 4400 TO irtn
            nreal = 1
            GOTO 8500
         ENDIF
      ELSE
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) GOTO 4000
      ENDIF
   ENDDO
 4400 nint = 0
   iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode<=0 ) GOTO 4000
 4500 IF ( core(iwrd)==0 ) GOTO 4100
   IF ( core(iwrd)==eor ) GOTO 4200
   iword = core(iwrd)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
      DO i = 1 , 33
         IF ( iword==setpr(i) ) THEN
            IF ( i==1 .OR. i==2 .OR. i==3 .OR. i==4 .OR. i==6 .OR. i==7 .OR. i==9 .OR. i==10 .OR. i==11 .OR. i==15 .OR. i==17 .OR.  &
               & i==19 .OR. i==20 .OR. i==22 .OR. i==23 .OR. i==25 .OR. i==27 .OR. i==28 .OR. i==29 .OR. i==30 .OR. i==31 .OR.      &
               & i==32 .OR. i==33 ) GOTO 4600
            IF ( i==5 ) GOTO 4800
            IF ( i==8 ) GOTO 4900
            IF ( i==12 .OR. i==18 ) GOTO 5000
            IF ( i==13 ) GOTO 5100
            IF ( i==14 ) GOTO 5200
            IF ( i==16 ) GOTO 5300
            IF ( i==21 ) GOTO 5400
            IF ( i==24 ) GOTO 5500
            IF ( i==26 ) GOTO 5600
         ENDIF
!
!    1          INCL  EXCL  EXCE  ELEM  GRID   ALL  AERO  AXIF   BAR
!    2          CONE  CONR  HEXA  FLUI  IHEX  PLOT  QDME  QDPL  QUAD
!    3           ROD  SHEA  SLOT  TETR  TORD  TRAP  TRBS  TRIA  TRME
!    4          TRPL  TUBE  TWIS VISCX  WEDG  HBDY
!
      ENDDO
      ASSIGN 4600 TO irtn
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
 4600 DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) EXIT
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 4500
   ENDDO
 4700 nthru = 0
   GOTO 4000
!
 4800 iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode<=0 ) THEN
      ASSIGN 4700 TO irtn
      iprm = poin
      GOTO 7700
   ELSE
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( iword==poin ) GOTO 4600
      ASSIGN 4500 TO irtn
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
!
 4900 istt = 4
   istb = 6
   GOTO 5700
!
 5000 istt = 3
   istb = 6
   GOTO 5700
!
 5100 istt = 7
   istb = 9
   GOTO 5700
!
 5200 istt = 3
   istb = 5
   GOTO 5700
!
 5300 istt = 10
   istb = 12
   GOTO 5700
!
 5400 istt = 5
   istb = 6
   GOTO 5700
!
 5500 istt = 1
   istb = 2
   GOTO 5700
!
 5600 istt = 1
   istb = 5
!
 5700 iword = core(iwrd+1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   DO i = istt , istb
      IF ( iword==setp2(i) ) GOTO 4600
   ENDDO
   ASSIGN 4600 TO irtn
   iprm = core(iwrd)
   GOTO 7800
 5800 IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) GOTO 6200
   iword = core(iwrd)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
      DO i = 1 , 28
         IF ( iword==pltpr(i) ) THEN
            IF ( i==1 .OR. i==12 .OR. i==15 .OR. i==16 .OR. i==17 .OR. i==28 ) GOTO 6000
            IF ( i==2 .OR. i==3 .OR. i==4 ) GOTO 6300
            IF ( i==5 .OR. i==6 ) GOTO 6400
            IF ( i==7 .OR. i==11 .OR. i==19 .OR. i==21 .OR. i==24 .OR. i==26 .OR. i==27 ) GOTO 5900
            IF ( i==8 .OR. i==9 ) GOTO 6500
            IF ( i==10 ) GOTO 6700
            IF ( i==13 .OR. i==14 ) GOTO 6800
            IF ( i==18 ) GOTO 7100
            IF ( i==20 ) GOTO 6900
            IF ( i==22 ) GOTO 7600
            IF ( i==23 ) GOTO 7300
            IF ( i==25 ) GOTO 7500
         ENDIF
!
!    1            SET  STAT  MODA  CMOD  FREQ  TRAN  CONT  RANG  TIME
!    2           PHAS  MAGN  ORIG  SYMM  ANTI   PEN  DENS  SYMB  LABE
!    3           SHAP  VECT  OUTL  THRU  MAXI  HIDD  SHRI  NOFI  FILL
!    4           OFFS
!
      ENDDO
      ASSIGN 5900 TO irtn
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
!
 5900 iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode>0 ) GOTO 5800
   GOTO 6100
!
 6000 iprm = core(iwrd)
   ASSIGN 5800 TO irtn
   DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) EXIT
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 8100
   ENDDO
!
 6100 DO WHILE ( core(iwrd)<0 )
      IF ( i==12 ) THEN
         Porg = core(iwrd+1)
         IF ( Org>0 ) THEN
            DO i = 1 , Org
               IF ( Porg==origin(i) ) GOTO 6150
            ENDDO
         ENDIF
         ASSIGN 10500 TO ierr
         msgno = 362
         GOTO 8800
      ENDIF
 6150 iwrd = iwrd + 2
   ENDDO
!
 6200 IF ( core(iwrd)==0 ) THEN
      Icont = 3
      GOTO 10600
   ELSEIF ( core(iwrd)/=eor ) THEN
      mode = core(iwrd)
      iwrd = iwrd + 1
      GOTO 5800
   ELSE
      Icont = 0
      GOTO 10600
   ENDIF
!
 6300 ipr1 = core(iwrd)
   ipr2 = core(iwrd+1)
   iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode<=0 ) THEN
      iprm = defo
      ASSIGN 6100 TO irtn
      GOTO 7700
   ELSE
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      DO i = 1 , 3
!                                     DEFO  VELO  ACCE
         IF ( iword==idvpr(i) ) THEN
            IF ( i==1 ) GOTO 5900
            IF ( i==2 .OR. i==3 ) GOTO 8600
         ENDIF
      ENDDO
      ASSIGN 5900 TO irtn
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
!
 6400 iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode<=0 ) THEN
      ASSIGN 6100 TO irtn
      iprm = defo
      GOTO 7700
   ELSE
!
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      DO i = 1 , 3
         IF ( iword==idvpr(i) ) GOTO 5900
      ENDDO
      ASSIGN 5900 TO irtn
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
!
 6500 nreal = 2
   ASSIGN 5800 TO irtn
 6600 DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) THEN
         integ = 0
         DO i = 1 , nreal
            IF ( core(iwrd)>=0 ) GOTO 6620
            IF ( core(iwrd)==-1 ) integ = 1
            IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
            iwrd = iwrd + 2
         ENDDO
         IF ( integ<=0 ) GOTO 6100
         ASSIGN 6100 TO irtn
         GOTO 8200
 6620    ASSIGN 6200 TO irtn
         GOTO 8300
      ELSE
         iword = core(iwrd)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( core(iwrd)/=allon .AND. iword/=Blank ) GOTO 8300
      ENDIF
   ENDDO
!
 6700 iwrd = iwrd + 2
   mode = mode - 1
   nreal = 1
   IF ( mode<=0 ) THEN
      ASSIGN 6100 TO irtn
      iprm = lag
      GOTO 7700
   ELSE
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( iword==lag ) GOTO 6600
      ASSIGN 6600 TO irtn
      iprm = core(iwrd)
      GOTO 7800
   ENDIF
!
 6800 ncrd = 9
   icrd = 1
   ivc = 0
   GOTO 7000
 6900 ncrd = 25
   icrd = 4
   ivc = 1
 7000 ASSIGN 6100 TO irtn
   iax = 0
   DO
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) GOTO 7900
      DO
         iword = core(iwrd)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( core(iwrd)==allon .OR. iword==Blank ) EXIT
         DO i = icrd , ncrd
            IF ( iword==coord(i) ) GOTO 7020
         ENDDO
         IF ( iax>0 ) GOTO 5800
         GOTO 7900
 7020    iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) GOTO 6200
         IF ( ivc>0 ) GOTO 5800
         IF ( iax>0 ) GOTO 5800
         iax = 1
      ENDDO
   ENDDO
 7100 DO
!
      iwrd = iwrd + 2
      mode = mode - 1
      IF ( mode<=0 ) GOTO 6100
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( core(iwrd)/=allon .AND. iword/=Blank ) THEN
         DO i = 1 , 5
            IF ( iword==lblpr(i) ) THEN
               IF ( i==1 ) GOTO 7200
               IF ( i==2 .OR. i==3 .OR. i==4 .OR. i==5 ) GOTO 5900
            ENDIF
!                                     GRID  POIN  ELEM  BOTH  EPID
         ENDDO
         GOTO 5800
      ENDIF
 7200 ENDDO
!
 7300 iwrd = iwrd + 2
   mode = mode - 1
   IF ( mode<=0 ) THEN
      ASSIGN 6100 TO irtn
      GOTO 8000
   ELSE
      iword = core(iwrd)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
      IF ( iword/=defo ) THEN
         ASSIGN 7400 TO irtn
         iprm = core(iwrd)
         GOTO 7800
      ENDIF
   ENDIF
 7400 nreal = 1
   GOTO 6600
!
 7500 iwrd = iwrd + 2
   iword = core(iwrd)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
   IF ( iword==hidd ) GOTO 5900
   mode = mode - 1
   IF ( mode>=0 ) GOTO 6100
   ASSIGN 6100 TO irtn
   GOTO 8000
!
 7600 IF ( core(iwrd-3)==-1 .AND. core(iwrd+2)==-1 ) GOTO 5900
   ASSIGN 5900 TO irtn
   GOTO 8500
 7700 ASSIGN 9300 TO ierr
   msgno = 351
   GOTO 8700
 7800 ASSIGN 9400 TO ierr
   msgno = 351
   GOTO 8700
 7900 ASSIGN 9500 TO ierr
   msgno = 352
   GOTO 8700
 8000 ASSIGN 9600 TO ierr
   msgno = 353
   GOTO 8700
 8100 ASSIGN 9700 TO ierr
   msgno = 354
   GOTO 8800
 8200 ASSIGN 9800 TO ierr
   msgno = 355
   GOTO 8700
 8300 ASSIGN 9900 TO ierr
   msgno = 356
   GOTO 8700
 8400 ASSIGN 10000 TO ierr
   msgno = 357
   GOTO 8800
 8500 ASSIGN 10100 TO ierr
   msgno = 358
   GOTO 8700
 8600 ASSIGN 10300 TO ierr
   msgno = 360
!
 8700 CALL page2(2)
   WRITE (Nout,99002) Ufm , msgno
99002 FORMAT (A23,I4)
   IF ( Pltopt<=2 ) Nogo = 1
   GOTO 8900
 8800 CALL page2(2)
   WRITE (Nout,99003) Uwm , msgno
99003 FORMAT (A25,I4)
!
 8900 GOTO ierr
!
 9000 WRITE (Nout,99004)
99004 FORMAT (5X,'FIRST CHARACTER ON CARD IS NUMERIC. INCORRECT FORMAT',' OR INCORRECT CONTINUATION ON PREVIOUS CARD')
   GOTO 400
!
 9100 WRITE (Nout,99005) core(iwrd)
99005 FORMAT (5X,'PLOT COMMAND ',A4,' NOT RECOGNIZED.  CHECK SPELLING ','AND FORMAT ON THIS CARD AND CONTINUATION ON PREVIOUS ONE')
   GOTO 400
 9200 WRITE (Nout,99006)
99006 FORMAT (1H+,30X,' - ONLY NASTRAN GENERAL PURPOSE PLOTTER IS ','SUPPORTED')
   GOTO 400
!
 9300 WRITE (Nout,99007) iprm
99007 FORMAT (1H+,30X,' - KEYWORD ',A4,' NOT FOUND')
   GOTO irtn
!
 9400 WRITE (Nout,99008) iprm
99008 FORMAT (1H+,30X,' - KEYWORD ',A4,' NOT RECOGNIZED')
   GOTO irtn
!
 9500 WRITE (Nout,99009)
99009 FORMAT (1H+,30X,' - COORDINATE AXES INCORRECTLY DEFINED')
   GOTO irtn
!
 9600 WRITE (Nout,99010)
99010 FORMAT (1H+,30X,' - INCORRECT FORMAT')
   GOTO irtn
!
 9700 WRITE (Nout,99011) iprm
99011 FORMAT (1H+,30X,3H - ,A4,' IDENTIFICATION NUMBER NOT DEFINED')
   GOTO irtn
!
 9800 WRITE (Nout,99012)
99012 FORMAT (1H+,30X,' - DATA TYPE IS INCORRECT')
   GOTO irtn
!
 9900 WRITE (Nout,99013)
99013 FORMAT (1H+,30X,' - ONE OR MORE REQUIRED REAL VALUES MISSING')
   GOTO irtn
!
10000 WRITE (Nout,99014)
99014 FORMAT (1H+,30X,' - CAMERA OPTION NOT SPECIFIED')
   GOTO 400
!
10100 WRITE (Nout,99015)
99015 FORMAT (1H+,30X,' - THRU MUST BE PRECEDED AND FOLLOWED BY INTEGER',' VALUES')
   GOTO irtn
!
10200 WRITE (Nout,99016)
99016 FORMAT (1H+,30X,' - THRU RANGE OVERLAPS RANGE OF PREVIOUS THRU')
   GOTO 4400
!
10300 WRITE (Nout,99017) ipr1 , ipr2
99017 FORMAT (1H+,30X,' - ONLY DEFORMATION VALID WITH ',2A4)
   GOTO 5900
!
10400 WRITE (Nout,99018) forg , Porg
99018 FORMAT (1H+,30X,' - A NEW ORIGIN',I8,' WAS DEFINED IN A FIND ','CARD, BUT IT IS NOT USED BY THE IMMEDIATE PLOT CARD',/5X,     &
             &'(ORIGIN',I8,' WILL BE USED FOR THIS PLOT)',/)
   GOTO 10700
!
10500 WRITE (Nout,99019) Porg
99019 FORMAT (1H+,30X,' - ORIGIN',I8,' IS UNDEFINED')
   GOTO 5900
!
10600 IF ( isplot==0 .OR. Porg==-1 ) RETURN
   IF ( Porg==0 ) Porg = porg1
   IF ( forg/=0 .AND. forg/=Porg ) THEN
      ASSIGN 10400 TO ierr
      msgno = 361
      GOTO 8800
   ENDIF
10700 forg = 0
   Porg = 0
END SUBROUTINE ifp1pc
