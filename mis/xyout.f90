
SUBROUTINE xyout(Iopt,Buf,Rbuf)
   IMPLICIT NONE
   REAL D1(6) , D2(2) , D3(78) , Dum(4) , Sysbuf
   INTEGER Icard , Icom1 , Ihalf , Ihead(96) , L , Line , Lpch , Mach , Maxlns
   COMMON /blank / Icom1 , Dum , Icard
   COMMON /machin/ Mach , Ihalf
   COMMON /output/ Ihead
   COMMON /system/ Sysbuf , L , D1 , Maxlns , D2 , Line , D3 , Lpch
   INTEGER Iopt
   INTEGER Buf(300)
   REAL Rbuf(300)
   INTEGER i , icomp , icount , id , imtd(6) , iprint , irand , itemp , itempv , itype(4) , ivg , j , k , m , model , names(44) ,   &
         & plt(2) , type(6)
   INTEGER lshift , rshift
   LOGICAL print , punch
   EXTERNAL lshift , rshift
!
!     THIS SUBROUTINE IS CALLED BY XYTRAN AND OUTPUTS TO PRINTER AND
!     PUNCH
!
   DATA names/4HDISP , 4HLACE , 4HMENT , 4H     , 4HVELO , 4HCITY , 4H     , 4H     , 4HACCE , 4HLERA , 4HTION , 4H     , 4HS P  ,  &
       &4HC F  , 4H     , 4H     , 4HLOAD , 4H     , 4H     , 4H     , 4HELEM , 4HENT- , 4HSTRE , 4HSS   , 4HELEM , 4HENT- ,        &
      & 4HFORC , 4HE    , 4HS-DI , 4HSPLA , 4HCEME , 4HNT   , 4HS-VE , 4HLOCI , 4HTY   , 4H     , 4HS-AC , 4HCELE , 4HRATI ,        &
      & 4HON   , 4HNONL , 4HINEA , 4HR-FO , 4HRCE /
   DATA type/4HWHOL , 4HE    , 4HUPPE , 4HR    , 4HLOWE , 4HR   /
   DATA irand/4HRAND/
   DATA ivg/4HVG  /
   DATA plt/4HNAST , 4HPLT /
   DATA imtd/4HFILM , 1H  , 4HTABL , 1HE , 4HDRUM , 1H /
   DATA itype/4HWITH , 4H     , 4HWITH , 4HOUT /
!
   IF ( Icom1/=ivg ) THEN
!
!     BRANCH ON OPTION
!
      IF ( Iopt<0 ) THEN
!
!     PRINT XY-OUTPUT SUMMARY
!
!
!     FILL OUT HEADING
!
         DO i = 1 , 96
            Ihead(i) = Buf(i+50)
         ENDDO
         CALL page1
         WRITE (L,99001)
!
99001    FORMAT (///44X,33HX Y - O U T P U T   S U M M A R Y)
         IF ( Icom1==irand ) THEN
            WRITE (L,99002) Rbuf(1)
99002       FORMAT (//5X,24HROOT MEAN SQUARE VALUE =,1P,E15.6)
            WRITE (L,99003) Rbuf(42)
99003       FORMAT (6X,38HFREQUENCY OF ZERO CROSSINGS (N ZERO) =,1P,E15.6)
         ELSE
            WRITE (L,99004) Buf(1)
99004       FORMAT (//5X,7HSUBCASE,I10)
         ENDIF
         itempv = 4*Buf(6) - 3
!
!     PRINT TYPE OF PLOT
!
         IF ( Buf(245)<2 ) THEN
            WRITE (L,99005)
99005       FORMAT (6X,8HRESPONSE)
         ELSEIF ( Buf(245)==2 ) THEN
            WRITE (L,99006)
99006       FORMAT (6X,38HPOWER-SPECTRAL-DENSITY-FUNCTION (PSDF))
         ELSE
            WRITE (L,99007)
99007       FORMAT (6X,15HAUTOCORRELATION)
         ENDIF
!
!     PRINT DATA TYPE AND CURVE
!
         icomp = Buf(5)
         IF ( Buf(6)/=6 .AND. Buf(6)/=7 ) icomp = Buf(5) - 2
         IF ( Buf(7)<0 ) THEN
            WRITE (L,99008) names(itempv) , names(itempv+1) , names(itempv+2) , names(itempv+3) , Buf(4) , icomp
99008       FORMAT (6X,4A4,5HCURVE,I9,4H(--,,I2,1H))
            itemp = 5
         ELSEIF ( Buf(7)==0 ) THEN
            WRITE (L,99009) names(itempv) , names(itempv+1) , names(itempv+2) , names(itempv+3) , Buf(4) , icomp
99009       FORMAT (6X,4A4,5HCURVE,I9,1H(,I2,1H))
            itemp = 1
         ELSE
            WRITE (L,99010) names(itempv) , names(itempv+1) , names(itempv+2) , names(itempv+3) , Buf(4) , icomp
99010       FORMAT (6X,4A4,5HCURVE,I9,1H(,I2,4H,--))
            itemp = 3
         ENDIF
         icount = Icard + 1
         WRITE (L,99011)
99011    FORMAT (1H )
         IF ( Buf(288)>0 ) WRITE (L,99012)
99012    FORMAT (6X,46HXY-PAIRS BETWEEN XMIN AND XMAX WILL BE PRINTED)
         IF ( Buf(290)>0 ) WRITE (L,99013) icount
99013    FORMAT (6X,64HXY-PAIRS BETWEEN XMIN AND XMAX WILL BE PUNCHED BIGINNING ON CARD,I8)
!
!     PLOTTER INFORMATION
!
         IF ( Buf(289)>0 ) THEN
            WRITE (L,99014)
99014       FORMAT (6X,44HXY-PAIRS WITHIN FRAME LIMITS WILL BE PLOTTED)
            j = rshift(Buf(284),Ihalf)
            model = Buf(284) - lshift(j,Ihalf) - 100
            m = 1
            IF ( model<0 ) m = 3
!
!   . NASPLOT...
!
            k = 2*iabs(model) - 1
            WRITE (L,99015) plt(1) , plt(2) , imtd(k) , imtd(k+1) , itype(m) , itype(m+1)
99015       FORMAT (6X,21HPLOTTER SPECIFIED IS ,3A4,A1,9H PLOTTER ,2A4,18HTYPING CAPABILITY.)
            IF ( Buf(283)<=0 ) Buf(283) = 1
!
!     WRITE CSCALE DATA OUT
!
            WRITE (L,99016) Rbuf(282)
99016       FORMAT (6X,9HCSCALE = ,F5.2)
            IF ( iabs(model)<2 ) THEN
!
!   . CAMERA, DENSITY...
!
               IF ( Buf(287)>=3 ) WRITE (L,99017)
99017          FORMAT (6X,36HCAMERA 3 USED. (PAPER AND 35MM FILM))
               IF ( Buf(287)==2 ) WRITE (L,99018)
99018          FORMAT (6X,22HCAMERA 1 USED. (PAPER))
               IF ( Buf(287)<=1 ) WRITE (L,99019)
99019          FORMAT (6X,26HCAMERA 2 USED. (35MM FILM))
               WRITE (L,99020) Buf(283)
99020          FORMAT (6X,9HDENSITY =,I3)
            ELSE
!
!   . PAPER SIZE
!     (THE LOGIC HERE IS SIMILAR TO THAT IN SUBROUTINE PLTSET)
!
               IF ( iabs(model)==2 ) THEN
!
!   . TABLE PLOTTERS
!
                  IF ( Rbuf(285)<=0.0 ) Rbuf(285) = 11.0
                  IF ( Rbuf(285)>30.0 ) Rbuf(285) = 30.0
                  IF ( Rbuf(286)<=0.0 ) Rbuf(286) = 8.5
               ELSE
!
!   . DRUM PLOTTERS
!
                  IF ( Rbuf(285)<=0.0 ) Rbuf(285) = 30.0
                  IF ( Rbuf(286)<=0.0 ) Rbuf(286) = 30.0
               ENDIF
               IF ( Rbuf(286)>30.0 ) Rbuf(286) = 30.0
               WRITE (L,99021) Rbuf(285) , Rbuf(286)
99021          FORMAT (6X,11HPAPER SIZE ,F5.2,3H X ,F5.2,18H INCHES SPECIFIED.)
!
!   . PEN SIZE
!
               WRITE (L,99022) Buf(283)
99022          FORMAT (6X,9HPENSIZE =,I3)
            ENDIF
            WRITE (L,99023) Buf(3) , type(itemp) , type(itemp+1) , Buf(2)
99023       FORMAT (//5X,13HTHIS IS CURVE,I4,4H OF ,A4,A2,5HFRAME,I5)
         ENDIF
!
!  .  PAPER PLOT
!
         IF ( Buf(289)<=0 .OR. Buf(289)==2 ) THEN
            WRITE (L,99024) Buf(281)
99024       FORMAT (6X,38HTHIS CURVE WILL BE PAPER-PLOTTED FRAME,I5)
         ENDIF
!
         WRITE (L,99025) (Buf(j),j=147,174) , (Buf(j),j=179,206) , (Buf(j),j=211,238)
99025    FORMAT (//5X,14HCURVE  TITLE =,28A4,/6X,14HX-AXIS TITLE =,28A4,/6X,14HY-AXIS TITLE =,28A4)
         WRITE (L,99026)
99026    FORMAT (/////5X,62HTHE FOLLOWING INFORMATION IS FOR THE ABOVE DEFINED CURVE ONLY.)
         WRITE (L,99027) Rbuf(11) , Rbuf(12)
99027    FORMAT (///6X,36HWITHIN THE FRAME X-LIMITS       (X =,1P,E14.6,8H TO  X =,1P,E14.6,1H))
         WRITE (L,99033) Rbuf(293) , Rbuf(294)
         WRITE (L,99034) Rbuf(295) , Rbuf(296)
         WRITE (L,99028) Rbuf(291) , Rbuf(292)
99028    FORMAT (//5X,36HWITHIN THE X-LIMITS OF ALL DATA (X =,1P,E14.6,8H TO  X =,1P,E14.6,1H))
         WRITE (L,99033) Rbuf(297) , Rbuf(298)
         WRITE (L,99034) Rbuf(299) , Rbuf(300)
         WRITE (L,99029)
99029    FORMAT (//45X,27HE N D   O F   S U M M A R Y)
         IF ( Buf(288)>0 ) WRITE (L,99030)
99030    FORMAT (//25X,69HP R I N T E D   D A T A   F O R   T H I S   C U R V E   F O L L O W S)
      ELSE
!
!     PRINT AND OR PUNCH OUTPUT
!
         iprint = iprint + 1
         IF ( punch ) THEN
            Icard = Icard + 1
            WRITE (Lpch,99031) iprint , Rbuf(1) , Rbuf(2) , Icard
99031       FORMAT (I10,10X,1P,2E20.6,12X,I8)
         ENDIF
         IF ( .NOT.print ) RETURN
         IF ( Line>=Maxlns ) THEN
            CALL page1
            WRITE (L,99032) names(itempv) , names(itempv+1) , names(itempv+2) , names(itempv+3) , id , icomp , type(itemp) ,        &
                          & type(itemp+1)
99032       FORMAT (//5X,4A4,12HCURVE   ID =,I9,5X,11HCOMPONENT =,I3,5X,A4,A2,5HFRAME,///27X,12HPRINT NUMBER,10X,7HX-VALUE,14X,     &
                   &7HY-VALUE,14X,11HCARD NUMBER)
         ENDIF
         Line = Line + 1
         IF ( punch ) GOTO 100
         WRITE (L,99035) iprint , Rbuf(1) , Rbuf(2)
         RETURN
      ENDIF
   ENDIF
   itempv = 4*Buf(6) - 3
   IF ( Buf(7)<0 ) THEN
      itemp = 5
   ELSEIF ( Buf(7)==0 ) THEN
      itemp = 1
   ELSE
      itemp = 3
   ENDIF
   iprint = 0
   id = Buf(4)
   icomp = Buf(5)
   IF ( Buf(6)/=6 .AND. Buf(6)/=7 ) icomp = Buf(5) - 2
   print = .FALSE.
   punch = .FALSE.
   IF ( Buf(290)>0 ) punch = .TRUE.
   IF ( Buf(288)>0 ) print = .TRUE.
   IF ( .NOT.print ) RETURN
   Line = Maxlns + 1
   RETURN
 100  WRITE (L,99035) iprint , Rbuf(1) , Rbuf(2) , Icard
   RETURN
99033 FORMAT (//30X,22HTHE SMALLEST Y-VALUE =,1P,E14.6,7H AT X =,E15.6)
99034 FORMAT (//30X,22HTHE LARGEST  Y-VALUE =,1P,E14.6,7H AT X =,E15.6,//)
99035 FORMAT (28X,I7,1P,E25.6,E21.6,10X,I8)
END SUBROUTINE xyout
