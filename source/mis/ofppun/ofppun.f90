!*==ofppun.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ofppun(Ibuf,Buf,Nwds,Iopt,Idd,Pnched)
   IMPLICIT NONE
   USE c_blank
   USE c_gpta1
   USE c_ofpcom
   USE c_output
   USE c_system
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(Nwds) :: Ibuf
   REAL , DIMENSION(Nwds) :: Buf
   INTEGER :: Nwds
   INTEGER :: Iopt
   INTEGER :: Idd
   LOGICAL :: Pnched
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iapp , ic1 , id3 , j , ktype , l1 , l2 , l3 , l4 , l5 , n , nword
   INTEGER , DIMENSION(50) :: id
   INTEGER , SAVE :: idtemp , vector
   INTEGER , DIMENSION(56) :: of
   REAL , DIMENSION(50) :: rid
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     MAIN OFP PUNCH ROUTINE FOR PUNCHING OF DATA LINES ONLY
!
!  $MIXED_FORMATS
!
!     COMMON /ZZOFPX/ L1,L2,L3,L4,L5,ID(50)
   !>>>>EQUIVALENCE (rid(1),id(1),Of(6)) , (L1,Of(1),Core(1)) , (L2,Of(2)) , (L3,Of(3)) , (L4,Of(4)) , (L5,Of(5))
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
         icard = icard + 1
         IF ( i==2 ) THEN
            WRITE (lpch,99001) (hd(j),j=33,47) , icard
99001       FORMAT (10H$SUBTITLE=,15A4,2X,I8)
         ELSEIF ( i==3 ) THEN
            WRITE (lpch,99002) (hd(j),j=65,79) , icard
99002       FORMAT (10H$LABEL   =,15A4,2X,I8)
         ELSE
            WRITE (lpch,99003) (hd(j),j=1,15) , icard
!
99003       FORMAT (10H$TITLE   =,15A4,2X,I8)
         ENDIF
      ENDDO
!
      ktype = id(2)/1000
      m = id(2) - (ktype)*1000
      IF ( m>=1 .AND. m<=19 ) THEN
         icard = icard + 1
         IF ( m==2 ) THEN
            WRITE (lpch,99004) icard
99004       FORMAT (7H$OLOADS,65X,I8)
         ELSEIF ( m==3 ) THEN
            WRITE (lpch,99005) icard
99005       FORMAT (5H$SPCF,67X,I8)
         ELSEIF ( m==4 ) THEN
            WRITE (lpch,99006) icard
99006       FORMAT (15H$ELEMENT FORCES,57X,I8)
         ELSEIF ( m==5 ) THEN
!
!     PUNCH ELEMENT STRESS OR GRID POINT STRESS HEADING LINE
!
            IF ( l2/=378 ) WRITE (lpch,99007) icard
99007       FORMAT (17H$ELEMENT STRESSES,55X,I8)
            IF ( l2==378 ) WRITE (lpch,99008) icard
99008       FORMAT (24H$STRESSES AT GRID POINTS,48X,I8)
         ELSEIF ( m==6 .OR. m==8 .OR. m==9 .OR. m==13 ) THEN
            icard = icard - 1
         ELSEIF ( m==7 ) THEN
            WRITE (lpch,99009) icard
99009       FORMAT (12H$EIGENVECTOR,60X,I8)
         ELSEIF ( m==10 ) THEN
            WRITE (lpch,99010) icard
99010       FORMAT (9H$VELOCITY,63X,I8)
         ELSEIF ( m==11 ) THEN
            WRITE (lpch,99011) icard
99011       FORMAT (13H$ACCELERATION,59X,I8)
         ELSEIF ( m==12 ) THEN
            WRITE (lpch,99012) icard
99012       FORMAT (18H$NON-LINEAR-FORCES,54X,I8)
         ELSEIF ( m==14 ) THEN
            WRITE (lpch,99013) icard
99013       FORMAT (27H$EIGENVECTOR (SOLUTION SET),45X,I8)
         ELSEIF ( m==15 ) THEN
            WRITE (lpch,99014) icard
99014       FORMAT (29H$DISPLACEMENTS (SOLUTION SET),43X,I8)
         ELSEIF ( m==16 ) THEN
            WRITE (lpch,99015) icard
99015       FORMAT (24H$VELOCITY (SOLUTION SET),48X,I8)
         ELSEIF ( m==17 ) THEN
            WRITE (lpch,99016) icard
99016       FORMAT (28H$ACCELERATION (SOLUTION SET),43X,I8)
         ELSEIF ( m==18 ) THEN
            WRITE (lpch,99017) icard
99017       FORMAT (23HELEMENT STRAIN ENERGIES,49X,I8)
         ELSEIF ( m==19 ) THEN
            WRITE (lpch,99018) icard
99018       FORMAT (24HGRID POINT FORCE BALANCE,48X,I8)
         ELSE
            WRITE (lpch,99019) icard
!
99019       FORMAT (14H$DISPLACEMENTS,58X,I8)
         ENDIF
      ENDIF
!
!     REAL, REAL/IMAGINARY, MAGNITUDE/PHASE
!
      icard = icard + 1
      IF ( ktype<1 .OR. ktype==2 ) THEN
         WRITE (lpch,99020) icard
99020    FORMAT (12H$REAL OUTPUT,60X,I8)
      ELSEIF ( id(9)==3 ) THEN
!
         WRITE (lpch,99021) icard
99021    FORMAT (23H$MAGNITUDE-PHASE OUTPUT,49X,I8)
      ELSE
         WRITE (lpch,99022) icard
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
   icard = icard + 1
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
         WRITE (lpch,99023,ERR=300) Buf(1) , Buf(2) , icard
99023    FORMAT (2(1P,E18.6),36X,I8)
      ELSEIF ( n==3 ) THEN
         WRITE (lpch,99024,ERR=300) Buf(1) , Buf(2) , Buf(3) , icard
99024    FORMAT (3(1P,E18.6),18X,I8)
      ELSEIF ( n==4 ) THEN
         WRITE (lpch,99025,ERR=300) Buf(1) , Buf(2) , Buf(3) , Buf(4) , icard
99025    FORMAT (4(1P,E18.6),I8)
      ELSE
         WRITE (lpch,99026,ERR=300) Buf(1) , icard
99026    FORMAT (1P,E18.6,54X,I8)
      ENDIF
      GOTO 300
!
!     VECTOR 1-ST CARD (FIRST WORD INTEGER, SECOND WORD BCD)
!
   ELSEIF ( temper ) THEN
!
!     SPECIAL PUNCH ONLY WHEN TEMPER FLAG IS ON IN A -HEAT- FORMULATION.
!
      ic1 = Ibuf(1)
      IF ( Idd/=0 .AND. Idd/=-1 ) THEN
         idtemp = idtemp + 1
         ic1 = Idd
      ENDIF
      WRITE (lpch,99027) idtemp , ic1 , Buf(3) , icard
99027 FORMAT (8HTEMP*   ,I16,I16,1P,E16.6,16X,I8)
      GOTO 99999
   ELSE
      IF ( Idd/=0 .AND. Idd/=-1 ) THEN
         WRITE (lpch,99028,ERR=400) Buf(1) , Buf(2) , Buf(3) , Buf(4) , Buf(5) , icard
99028    FORMAT (1P,E16.6,1X,A1,3(1P,E18.6),I8)
      ELSE
         WRITE (lpch,99029,ERR=400) Ibuf(1) , Buf(2) , Buf(3) , Buf(4) , Buf(5) , icard
99029    FORMAT (I10,7X,A1,3(1P,E18.6),I8)
      ENDIF
      GOTO 400
   ENDIF
 200  IF ( n==2 ) THEN
      WRITE (lpch,99030,ERR=300) Ibuf(1) , Buf(2) , icard
99030 FORMAT (I10,8X,1P,E18.6,36X,I8)
   ELSEIF ( n==3 ) THEN
      WRITE (lpch,99031,ERR=300) Ibuf(1) , Buf(2) , Buf(3) , icard
99031 FORMAT (I10,8X,2(1P,E18.6),18X,I8)
   ELSEIF ( n==4 ) THEN
!
!     CHECK FOR THERMAL FORCES FOR ISOPARAMETRICS
!
      IF ( itherm/=0 .AND. m==4 ) THEN
         IF ( id(3)>=65 .AND. id(3)<=67 ) THEN
            WRITE (lpch,99032) Ibuf(1) , Buf(2) , Ibuf(3) , Buf(4) , icard
99032       FORMAT (I10,8X,A4,14X,I10,8X,1P,E18.6,I8)
            GOTO 300
         ENDIF
      ENDIF
!
!     CHECK FOR INTEGER IN SECOND ARGUMENT ALSO.
!
      IF ( m==19 ) THEN
         WRITE (lpch,99033) Ibuf(1) , Ibuf(2) , Buf(3) , Buf(4) , icard
99033    FORMAT (I10,8X,I10,8X,2A4,28X,I8)
      ELSEIF ( numtyp(Buf(2))<=1 ) THEN
         WRITE (lpch,99034,ERR=300) Ibuf(1) , Ibuf(2) , Buf(3) , Buf(4) , icard
99034    FORMAT (I10,8X,I10,8X,2(1P,E18.6),I8)
      ELSE
         WRITE (lpch,99035,ERR=300) Ibuf(1) , Buf(2) , Buf(3) , Buf(4) , icard
99035    FORMAT (I10,8X,3(1P,E18.6),I8)
      ENDIF
   ELSE
      WRITE (lpch,99036) Ibuf(1) , icard
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
      icard = icard + 1
      nword = nword + 3
      IF ( nword>Nwds ) THEN
         nword = nword - 1
         IF ( nword==Nwds ) THEN
!
!     2 WORDS OUT
!
            WRITE (lpch,99049,ERR=99999) Buf(nword-1) , Buf(nword) , icard
         ELSE
            nword = nword - 1
!
!     1 WORD OUT
!
            WRITE (lpch,99037,ERR=99999) Buf(nword) , icard
99037       FORMAT (6H-CONT-,12X,1P,E18.6,36X,I8)
         ENDIF
         EXIT
!
!     3 WORDS OUT
!
      ELSEIF ( Ibuf(nword-1)==vector ) THEN
         WRITE (lpch,99038) Buf(nword-2) , Buf(nword) , icard
99038    FORMAT (6H-CONT-,12X,1P,E18.6,18X,1P,E18.6,I8)
      ELSEIF ( Ibuf(nword)==vector ) THEN
         WRITE (lpch,99049) Buf(nword-2) , Buf(nword-1) , icard
      ELSE
         WRITE (lpch,99039,ERR=500) Buf(nword-2) , Buf(nword-1) , Buf(nword) , icard
99039    FORMAT (6H-CONT-,12X,3(1P,E18.6),I8)
      ENDIF
   ENDDO
   GOTO 99999
 600  icard = icard + 1
   WRITE (lpch,99040) id(4) , icard
99040 FORMAT (13H$SUBCASE ID =,I12,47X,I8)
!
!     IF ELEMENT STRESS OR FORCE PUNCH ELEMENT TYPE NUMBER
!
 700  IF ( m==4 .OR. m==5 ) THEN
      icard = icard + 1
      id3 = id(3)
      IF ( l2/=378 ) WRITE (lpch,99041) id3 , ie(1,id3) , ie(2,id3) , icard
99041 FORMAT (15H$ELEMENT TYPE =,I12,3X,1H(,2A4,1H),32X,I8)
      IF ( l2==378 ) WRITE (lpch,99042) icard
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
            icard = icard + 1
            WRITE (lpch,99043,ERR=800) rid(5) , icard
99043       FORMAT (12H$FREQUENCY =,E16.7,44X,I8)
            GOTO 800
         ENDIF
      ELSEIF ( iapp==6 ) THEN
         IF ( ktype<=1 ) THEN
            icard = icard + 1
            WRITE (lpch,99044,ERR=800) rid(5) , icard
99044       FORMAT (7H$TIME =,E16.7,49X,I8)
            GOTO 800
         ENDIF
      ELSE
!
!     PUNCH EIGENVALUE
!
         icard = icard + 1
         IF ( ktype==1 ) THEN
            WRITE (lpch,99045,ERR=800) rid(6) , rid(7) , id(5) , icard
99045       FORMAT (15H$EIGENVALUE = (,E15.7,1H,,E15.7,8H) MODE =,I6,12X,I8)
         ELSE
            WRITE (lpch,99046,ERR=800) rid(6) , id(5) , icard
99046       FORMAT (13H$EIGENVALUE =,E15.7,2X,6HMODE =,I6,30X,I8)
         ENDIF
         GOTO 800
      ENDIF
      IF ( ktype>1 ) THEN
         icard = icard + 1
         IF ( m==4 .OR. m==5 ) THEN
            WRITE (lpch,99047) id(5) , icard
99047       FORMAT (13H$ELEMENT ID =,I10,49X,I8)
         ELSE
            WRITE (lpch,99048) id(5) , icard
99048       FORMAT (11H$POINT ID =,I12,49X,I8)
         ENDIF
      ENDIF
   ENDIF
!
!     CARD HEADING COMPLETE
!
 800  Pnched = .TRUE.
   IF ( temper ) THEN
      idtemp = idtemp + 1
      IF ( Idd>0 ) idtemp = 0
   ENDIF
   GOTO 100
99049 FORMAT (6H-CONT-,12X,2(1P,E18.6),18X,I8)
!
99999 END SUBROUTINE ofppun
