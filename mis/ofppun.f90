
SUBROUTINE ofppun(Ibuf,Buf,Nwds,Iopt,Idd,Pnched)
   IMPLICIT NONE
   REAL Core(1) , Dum34(34) , Dum53(53) , Hd(96) , Sysbuf
   INTEGER Icard , Ie(25,1) , Incr , Itherm , L , L1 , L2 , L3 , L4 , L5 , Last , Lpch , M , Nelm , Of(56)
   LOGICAL Temper
   COMMON /blank / Icard
   COMMON /gpta1 / Nelm , Last , Incr , Ie
   COMMON /ofpcom/ Temper , M
   COMMON /output/ Hd
   COMMON /system/ Sysbuf , L , Dum53 , Itherm , Dum34 , Lpch
   COMMON /zzzzzz/ Core
   INTEGER Idd , Iopt , Nwds
   LOGICAL Pnched
   REAL Buf(Nwds)
   INTEGER Ibuf(Nwds)
   INTEGER i , iapp , ic1 , id(50) , id3 , idtemp , j , ktype , n , nword , vector
   INTEGER numtyp
   REAL rid(50)
!
!     MAIN OFP PUNCH ROUTINE FOR PUNCHING OF DATA LINES ONLY
!
!  $MIXED_FORMATS
!
!     COMMON /ZZOFPX/ L1,L2,L3,L4,L5,ID(50)
   EQUIVALENCE (rid(1),id(1),Of(6)) , (L1,Of(1),Core(1)) , (L2,Of(2)) , (L3,Of(3)) , (L4,Of(4)) , (L5,Of(5))
   DATA vector , idtemp/1 , 0/
!
!
   IF ( .NOT.Pnched ) THEN
!
!
!     PUNCH HEADING CARDS
!
!
!     TITLE,SUBTITLE,AND LABEL
!
      DO i = 1 , 3
         Icard = Icard + 1
         IF ( i==2 ) THEN
            WRITE (Lpch,99001) (Hd(j),j=33,47) , Icard
99001       FORMAT (10H$SUBTITLE=,15A4,2X,I8)
         ELSEIF ( i==3 ) THEN
            WRITE (Lpch,99002) (Hd(j),j=65,79) , Icard
99002       FORMAT (10H$LABEL   =,15A4,2X,I8)
         ELSE
            WRITE (Lpch,99003) (Hd(j),j=1,15) , Icard
!
99003       FORMAT (10H$TITLE   =,15A4,2X,I8)
         ENDIF
      ENDDO
!
      ktype = id(2)/1000
      M = id(2) - (ktype)*1000
      IF ( M>=1 .AND. M<=19 ) THEN
         Icard = Icard + 1
         IF ( M==2 ) THEN
            WRITE (Lpch,99004) Icard
99004       FORMAT (7H$OLOADS,65X,I8)
         ELSEIF ( M==3 ) THEN
            WRITE (Lpch,99005) Icard
99005       FORMAT (5H$SPCF,67X,I8)
         ELSEIF ( M==4 ) THEN
            WRITE (Lpch,99006) Icard
99006       FORMAT (15H$ELEMENT FORCES,57X,I8)
         ELSEIF ( M==5 ) THEN
!
!     PUNCH ELEMENT STRESS OR GRID POINT STRESS HEADING LINE
!
            IF ( L2/=378 ) WRITE (Lpch,99007) Icard
99007       FORMAT (17H$ELEMENT STRESSES,55X,I8)
            IF ( L2==378 ) WRITE (Lpch,99008) Icard
99008       FORMAT (24H$STRESSES AT GRID POINTS,48X,I8)
         ELSEIF ( M==6 .OR. M==8 .OR. M==9 .OR. M==13 ) THEN
            Icard = Icard - 1
         ELSEIF ( M==7 ) THEN
            WRITE (Lpch,99009) Icard
99009       FORMAT (12H$EIGENVECTOR,60X,I8)
         ELSEIF ( M==10 ) THEN
            WRITE (Lpch,99010) Icard
99010       FORMAT (9H$VELOCITY,63X,I8)
         ELSEIF ( M==11 ) THEN
            WRITE (Lpch,99011) Icard
99011       FORMAT (13H$ACCELERATION,59X,I8)
         ELSEIF ( M==12 ) THEN
            WRITE (Lpch,99012) Icard
99012       FORMAT (18H$NON-LINEAR-FORCES,54X,I8)
         ELSEIF ( M==14 ) THEN
            WRITE (Lpch,99013) Icard
99013       FORMAT (27H$EIGENVECTOR (SOLUTION SET),45X,I8)
         ELSEIF ( M==15 ) THEN
            WRITE (Lpch,99014) Icard
99014       FORMAT (29H$DISPLACEMENTS (SOLUTION SET),43X,I8)
         ELSEIF ( M==16 ) THEN
            WRITE (Lpch,99015) Icard
99015       FORMAT (24H$VELOCITY (SOLUTION SET),48X,I8)
         ELSEIF ( M==17 ) THEN
            WRITE (Lpch,99016) Icard
99016       FORMAT (28H$ACCELERATION (SOLUTION SET),43X,I8)
         ELSEIF ( M==18 ) THEN
            WRITE (Lpch,99017) Icard
99017       FORMAT (23HELEMENT STRAIN ENERGIES,49X,I8)
         ELSEIF ( M==19 ) THEN
            WRITE (Lpch,99018) Icard
99018       FORMAT (24HGRID POINT FORCE BALANCE,48X,I8)
         ELSE
            WRITE (Lpch,99019) Icard
!
99019       FORMAT (14H$DISPLACEMENTS,58X,I8)
         ENDIF
      ENDIF
!
!     REAL, REAL/IMAGINARY, MAGNITUDE/PHASE
!
      Icard = Icard + 1
      IF ( ktype<1 .OR. ktype==2 ) THEN
         WRITE (Lpch,99020) Icard
99020    FORMAT (12H$REAL OUTPUT,60X,I8)
      ELSEIF ( id(9)==3 ) THEN
!
         WRITE (Lpch,99021) Icard
99021    FORMAT (23H$MAGNITUDE-PHASE OUTPUT,49X,I8)
      ELSE
         WRITE (Lpch,99022) Icard
99022    FORMAT (22H$REAL-IMAGINARY OUTPUT,50X,I8)
      ENDIF
!
!     SUBCASE NUMBER FOR SORT1 OUTPUT, OR
!     SUBCASE NUMBER FOR SORT2, FREQUENCY AND TRANSIENT RESPONSE ONLY
!
      IF ( ktype<=1 ) GOTO 600
      iapp = id(1)/10
      IF ( iapp==5 .OR. iapp==6 ) GOTO 600
      GOTO 700
   ENDIF
 100  IF ( Nwds<0 ) GOTO 99999
!
!     FIRST CARD OUT
!
   Icard = Icard + 1
   IF ( Iopt/=vector ) THEN
!
!     GENERAL 1-ST CARD (FIRST WORD OF BUF ASSUMED INTEGER)
!
      n = min0(4,Nwds)
      IF ( Idd<0 ) THEN
         IF ( Idd==-1 ) GOTO 200
      ELSEIF ( Idd==0 ) THEN
         GOTO 200
      ENDIF
      IF ( n==2 ) THEN
         WRITE (Lpch,99023,ERR=300) Buf(1) , Buf(2) , Icard
99023    FORMAT (2(1P,E18.6),36X,I8)
      ELSEIF ( n==3 ) THEN
         WRITE (Lpch,99024,ERR=300) Buf(1) , Buf(2) , Buf(3) , Icard
99024    FORMAT (3(1P,E18.6),18X,I8)
      ELSEIF ( n==4 ) THEN
         WRITE (Lpch,99025,ERR=300) Buf(1) , Buf(2) , Buf(3) , Buf(4) , Icard
99025    FORMAT (4(1P,E18.6),I8)
      ELSE
         WRITE (Lpch,99026,ERR=300) Buf(1) , Icard
99026    FORMAT (1P,E18.6,54X,I8)
      ENDIF
      GOTO 300
!
!     VECTOR 1-ST CARD (FIRST WORD INTEGER, SECOND WORD BCD)
!
   ELSEIF ( Temper ) THEN
!
!     SPECIAL PUNCH ONLY WHEN TEMPER FLAG IS ON IN A -HEAT- FORMULATION.
!
      ic1 = Ibuf(1)
      IF ( Idd/=0 .AND. Idd/=-1 ) THEN
         idtemp = idtemp + 1
         ic1 = Idd
      ENDIF
      WRITE (Lpch,99027) idtemp , ic1 , Buf(3) , Icard
99027 FORMAT (8HTEMP*   ,I16,I16,1P,E16.6,16X,I8)
      GOTO 99999
   ELSE
      IF ( Idd/=0 .AND. Idd/=-1 ) THEN
         WRITE (Lpch,99028,ERR=400) Buf(1) , Buf(2) , Buf(3) , Buf(4) , Buf(5) , Icard
99028    FORMAT (1P,E16.6,1X,A1,3(1P,E18.6),I8)
      ELSE
         WRITE (Lpch,99029,ERR=400) Ibuf(1) , Buf(2) , Buf(3) , Buf(4) , Buf(5) , Icard
99029    FORMAT (I10,7X,A1,3(1P,E18.6),I8)
      ENDIF
      GOTO 400
   ENDIF
 200  IF ( n==2 ) THEN
      WRITE (Lpch,99030,ERR=300) Ibuf(1) , Buf(2) , Icard
99030 FORMAT (I10,8X,1P,E18.6,36X,I8)
   ELSEIF ( n==3 ) THEN
      WRITE (Lpch,99031,ERR=300) Ibuf(1) , Buf(2) , Buf(3) , Icard
99031 FORMAT (I10,8X,2(1P,E18.6),18X,I8)
   ELSEIF ( n==4 ) THEN
!
!     CHECK FOR THERMAL FORCES FOR ISOPARAMETRICS
!
      IF ( Itherm/=0 .AND. M==4 ) THEN
         IF ( id(3)>=65 .AND. id(3)<=67 ) THEN
            WRITE (Lpch,99032) Ibuf(1) , Buf(2) , Ibuf(3) , Buf(4) , Icard
99032       FORMAT (I10,8X,A4,14X,I10,8X,1P,E18.6,I8)
            GOTO 300
         ENDIF
      ENDIF
!
!     CHECK FOR INTEGER IN SECOND ARGUMENT ALSO.
!
      IF ( M==19 ) THEN
         WRITE (Lpch,99033) Ibuf(1) , Ibuf(2) , Buf(3) , Buf(4) , Icard
99033    FORMAT (I10,8X,I10,8X,2A4,28X,I8)
      ELSEIF ( numtyp(Buf(2))<=1 ) THEN
         WRITE (Lpch,99034,ERR=300) Ibuf(1) , Ibuf(2) , Buf(3) , Buf(4) , Icard
99034    FORMAT (I10,8X,I10,8X,2(1P,E18.6),I8)
      ELSE
         WRITE (Lpch,99035,ERR=300) Ibuf(1) , Buf(2) , Buf(3) , Buf(4) , Icard
99035    FORMAT (I10,8X,3(1P,E18.6),I8)
      ENDIF
   ELSE
      WRITE (Lpch,99036) Ibuf(1) , Icard
!
99036 FORMAT (I10,62X,I8)
   ENDIF
 300  nword = 4
   GOTO 500
 400  nword = 5
!
!     CONTINUATION CARDS IF ANY.
!
 500  DO WHILE ( nword<Nwds )
      Icard = Icard + 1
      nword = nword + 3
      IF ( nword>Nwds ) THEN
         nword = nword - 1
         IF ( nword==Nwds ) THEN
!
!     2 WORDS OUT
!
            WRITE (Lpch,99049,ERR=99999) Buf(nword-1) , Buf(nword) , Icard
         ELSE
            nword = nword - 1
!
!     1 WORD OUT
!
            WRITE (Lpch,99037,ERR=99999) Buf(nword) , Icard
99037       FORMAT (6H-CONT-,12X,1P,E18.6,36X,I8)
         ENDIF
         EXIT
!
!     3 WORDS OUT
!
      ELSEIF ( Ibuf(nword-1)==vector ) THEN
         WRITE (Lpch,99038) Buf(nword-2) , Buf(nword) , Icard
99038    FORMAT (6H-CONT-,12X,1P,E18.6,18X,1P,E18.6,I8)
      ELSEIF ( Ibuf(nword)==vector ) THEN
         WRITE (Lpch,99049) Buf(nword-2) , Buf(nword-1) , Icard
      ELSE
         WRITE (Lpch,99039,ERR=500) Buf(nword-2) , Buf(nword-1) , Buf(nword) , Icard
99039    FORMAT (6H-CONT-,12X,3(1P,E18.6),I8)
      ENDIF
   ENDDO
   GOTO 99999
 600  Icard = Icard + 1
   WRITE (Lpch,99040) id(4) , Icard
99040 FORMAT (13H$SUBCASE ID =,I12,47X,I8)
!
!     IF ELEMENT STRESS OR FORCE PUNCH ELEMENT TYPE NUMBER
!
 700  IF ( M==4 .OR. M==5 ) THEN
      Icard = Icard + 1
      id3 = id(3)
      IF ( L2/=378 ) WRITE (Lpch,99041) id3 , Ie(1,id3) , Ie(2,id3) , Icard
99041 FORMAT (15H$ELEMENT TYPE =,I12,3X,1H(,2A4,1H),32X,I8)
      IF ( L2==378 ) WRITE (Lpch,99042) Icard
99042 FORMAT (38H$PUNCHED IN MATERIAL COORDINATE SYSTEM,34X,I8)
   ENDIF
!
!     PUNCH EIGENVALUE, FREQUENCY, POINT OR ELEMENT ID, OR TIME
!
   iapp = id(1)/10
   IF ( iapp>=1 .AND. iapp<=10 ) THEN
      IF ( iapp==1 .OR. iapp==3 .OR. iapp==4 .OR. iapp==7 .OR. iapp==10 ) THEN
      ELSEIF ( iapp==5 ) THEN
!
!     FREQUENCY OR TIME, POINT OR ELEMENT ID
!
         IF ( ktype<=1 ) THEN
            Icard = Icard + 1
            WRITE (Lpch,99043,ERR=800) rid(5) , Icard
99043       FORMAT (12H$FREQUENCY =,E16.7,44X,I8)
            GOTO 800
         ENDIF
      ELSEIF ( iapp==6 ) THEN
         IF ( ktype<=1 ) THEN
            Icard = Icard + 1
            WRITE (Lpch,99044,ERR=800) rid(5) , Icard
99044       FORMAT (7H$TIME =,E16.7,49X,I8)
            GOTO 800
         ENDIF
      ELSE
!
!     PUNCH EIGENVALUE
!
         Icard = Icard + 1
         IF ( ktype==1 ) THEN
            WRITE (Lpch,99045,ERR=800) rid(6) , rid(7) , id(5) , Icard
99045       FORMAT (15H$EIGENVALUE = (,E15.7,1H,,E15.7,8H) MODE =,I6,12X,I8)
         ELSE
            WRITE (Lpch,99046,ERR=800) rid(6) , id(5) , Icard
99046       FORMAT (13H$EIGENVALUE =,E15.7,2X,6HMODE =,I6,30X,I8)
         ENDIF
         GOTO 800
      ENDIF
      IF ( ktype>1 ) THEN
         Icard = Icard + 1
         IF ( M==4 .OR. M==5 ) THEN
            WRITE (Lpch,99047) id(5) , Icard
99047       FORMAT (13H$ELEMENT ID =,I10,49X,I8)
         ELSE
            WRITE (Lpch,99048) id(5) , Icard
99048       FORMAT (11H$POINT ID =,I12,49X,I8)
         ENDIF
      ENDIF
   ENDIF
!
!     CARD HEADING COMPLETE
!
 800  Pnched = .TRUE.
   IF ( Temper ) THEN
      idtemp = idtemp + 1
      IF ( Idd>0 ) idtemp = 0
   ENDIF
   GOTO 100
99049 FORMAT (6H-CONT-,12X,2(1P,E18.6),18X,I8)
!
99999 RETURN
END SUBROUTINE ofppun
