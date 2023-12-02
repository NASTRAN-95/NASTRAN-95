!*==ofp1c.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ofp1c(Line)
   IMPLICIT NONE
   USE c_system
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Line
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: local
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
!     THIS SUBROUTINE WAS FORMED ONLY TO REDUCE THE SIZE OF OFP1 FOR
!     COMPILATION PURPOSES.  IT IS CALLED ONLY BY OFP1.
!     THIS ROUTINE WAS PART OF OFP1B BEFORE.
!
!ZZ   COMMON /ZZOFPX/ L123(1)
!
!
!WKBR NCL93012 3/94      IF (LINE .GT. 467) GO TO 100
!WKBR SPR94001 7/94      IF (LINE .GT. 470) GO TO 100
   IF ( Line<=474 ) THEN
      local = Line - 380
!WKBR NCL93012 3/94 8       461,462,463,464,465,466,467), LOCAL
!WKBD SPR94001 7/94 8       461,462,463,464,465,466,467,100,469,470), LOCAL
!WKBNB SPR94001 7/94
      IF ( local==1 ) GOTO 200
      IF ( local==2 ) THEN
         WRITE (l,99001)
99001    FORMAT (4X,'S T R A I N S / C U R V A T U R E S   I N   G E N E ','R A L   Q U A D R I L A T E R A L   E L E M E N T S',6X,&
                &'( C Q U A D 1 )')
      ELSEIF ( local==3 ) THEN
         WRITE (l,99002)
!WKBRB NCL93012 3/94
!  883 FORMAT (2X,7HELEMENT,24X,37HSTRNS./CURVS. IN ELEMENT COORD SYSTEM,
!     1       6X,38HPRIN. STRNS./CURVS. (ZERO SHEAR/TWIST),7X,7HMAXIMUM)
!WKBRE NCL93012 3/94
99002    FORMAT (2X,7HELEMENT,8X,'STRAIN',8X,37HSTRNS./CURVS. IN ELEMENT COORD SYSTEM,6X,38HPRIN. STRNS./CURVS. (ZERO SHEAR/TWIST), &
               & 7X,7HMAXIMUM)
      ELSEIF ( local==4 ) THEN
         WRITE (l,99003)
99003    FORMAT (4X,3HID.,6X,15HID./OUTPUT CODE,5X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,7X,     &
                &11HSHEAR/TWIST)
      ELSEIF ( local==5 ) THEN
         WRITE (l,99004)
99004    FORMAT (2X,7HELEMENT,4X,16HMAT. COORD. SYS.,4X,'STRNS./CURVS. ',' IN MATERIAL COORD SYSTEM',5X,                            &
                &38HPRIN. STRNS./CURVS. (ZERO SHEAR/TWIST),7X,7HMAXIMUM)
      ELSEIF ( local==6 ) THEN
         WRITE (l,99005)
99005    FORMAT (33X,'S T R A I N S / C U R V A T U R E S   A T   G R I D','   P O I N T S')
      ELSEIF ( local==7 ) THEN
         WRITE (l,99006)
99006    FORMAT (2X,7H POINT ,4X,16HMAT. COORD. SYS.,6X,33HSTRESSES INMATERIAL COORD SYSTEM ,12X,31HPRINCIPAL STRESSES (ZERO SHEAR),&
               & 12X,3HMAX)
      ELSEIF ( local==8 ) THEN
         WRITE (l,99007)
99007    FORMAT (2X,7H POINT ,4X,16HMAT. COORD. SYS.,4X,38HSTRNS./CURVS. IN MATERIAL COORD SYSTEM,5X,                               &
                &38HPRIN. STRNS./CURVS. (ZERO SHEAR/TWIST),7X,7HMAXIMUM)
      ELSEIF ( local==9 ) THEN
         WRITE (l,99008)
99008    FORMAT (50X,30H(IN ELEMENT COORDINATE SYSTEM),/)
      ELSEIF ( local==10 ) THEN
         WRITE (l,99009)
99009    FORMAT (50X,31H(IN MATERIAL COORDINATE SYSTEM),/)
      ELSEIF ( local==11 ) THEN
         WRITE (l,99010)
!WKBRB NCL93012 3/94
!  891 FORMAT (4X,3HID.,26X,8HNORMAL-X, 7X,8HNORMAL-Y, 6X,8HSHEAR-XY,
!     1       7X,5HANGLE, 9X,5HMAJOR, 11X,5HMINOR, 7X,11HSHEAR/TWIST)
!WKBRE NCL93012 3/94
99010    FORMAT (4X,3HID.,9X,'CURVATURE',7X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,7X,            &
                &11HSHEAR/TWIST)
      ELSEIF ( local==12 ) THEN
         WRITE (l,99011)
99011    FORMAT (4X,'C O M P L E X   F O R C E S   I N   A X I S - S Y M ',                                                         &
                &'M E T R I C   T R I A N G U L A R   R I N G   E L E M E ','N T S   (CTRIAAX)',/)
      ELSEIF ( local==13 ) THEN
         WRITE (l,99012)
99012    FORMAT (2X,'C O M P L E X   S T R E S S E S   I N   A X I S - S ',                                                         &
                &'Y M M E T R I C   T R I A N G U L A R   R I N G   E L E ','M E N T S   (CTRIAAX)',/)
      ELSEIF ( local==14 ) THEN
         WRITE (l,99013)
99013    FORMAT (3X,'C O M P L E X   F O R C E S   I N   A X I S - S Y M ',                                                         &
                &'M E T R I C   T R A P E Z O I D A L   R I N G   E L E M ','E N T S   (CTRAPAX)',/)
      ELSEIF ( local==15 ) THEN
         WRITE (l,99014)
99014    FORMAT (' C O M P L E X   S T R E S S E S   I N   A X I S - S Y ',                                                         &
                &' M M E T R I C   T R A P E Z O I D A L   R I N G   E L E',' M E N T S   (CTRAPAX)',/)
      ELSEIF ( local==16 ) THEN
         WRITE (l,99015)
99015    FORMAT (3X,'SUBCASE   HARMONIC    POINT',12X,'RADIAL',12X,'CIRCUMFERENTIAL',12X,'AXIAL',16X,'CHARGE',/14X,                 &
                &'NUMBER     ANGLE',13X,'(R)',17X,'(THETA-T)',16X,'(Z)')
      ELSEIF ( local==17 ) THEN
         WRITE (l,99016)
99016    FORMAT (' SUBCASE   HARMONIC    POINT    RADIAL      AXIAL     ','CIRCUM.     SHEAR      SHEAR      SHEAR      F L U X   ',&
                &'D E N S I T I E S',/11X,'NUMBER      ANGLE     (R)',9X,'(Z)     (THETA-T)    (ZR)       (RT)       (ZT)',8X,      &
                &'(R)        (Z)        (T)')
      ELSEIF ( local==18 ) THEN
         WRITE (l,99017)
99017    FORMAT ('   FREQUENCY  HARMONIC    POINT            RADIAL',12X,'CIRCUMFERENTIAL',12X,'AXIAL',16X,'CHARGE',/14X,           &
                &'NUMBER     ANGLE',13X,'(R)',17X,'(THETA-T)',16X,'(Z)')
      ELSEIF ( local==19 ) THEN
         WRITE (l,99018)
99018    FORMAT (' FREQUENCY HARMONIC    POINT    RADIAL      AXIAL     ','CIRCUM.     SHEAR      SHEAR      SHEAR      F L U X   ',&
                &'D E N S I T I E S',/10X,'NUMBER      ANGLE     (R)',9X,'(Z)     (THETA-T)    (ZR)       (RT)       (ZT)',8X,      &
                &'(R)        (Z)        (T)')
      ELSEIF ( local==20 ) THEN
         WRITE (l,99019)
99019    FORMAT (4X,'TIME     HARMONIC    POINT            RADIAL',12X,'CIRCUMFERENTIAL',12X,'AXIAL',16X,'CHARGE',/14X,             &
                &'NUMBER     ANGLE',13X,'(R)',17X,'(THETA-T)',16X,'(Z)')
      ELSEIF ( local==21 ) THEN
         WRITE (l,99020)
99020    FORMAT (2X,'TIME     HARMONIC    POINT    RADIAL      AXIAL     ','CIRCUM.     SHEAR      SHEAR      SHEAR      F L U X   '&
               & ,'D E N S I T I E S',/11X,'NUMBER      ANGLE     (R)',9X,'(Z)     (THETA-T)    (ZR)       (RT)       (ZT)',8X,     &
                &'(R)        (Z)        (T)')
      ELSEIF ( local==22 ) THEN
         WRITE (l,99021)
99021    FORMAT (5X,4HTIME,7X,8HHARMONIC,8X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
      ELSEIF ( local==23 ) THEN
         WRITE (l,99022)
99022    FORMAT (4X,7HSUBCASE,5X,8HHARMONIC,8X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
      ELSEIF ( local==24 ) THEN
         WRITE (l,99023)
99023    FORMAT (3X,9HFREQUENCY,4X,8HHARMONIC,8X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
      ELSEIF ( local==25 ) THEN
         WRITE (l,99024)
99024    FORMAT (19X,'F I N I T E   E L E M E N T   M A G N E T I C   F I',' E L D   A N D   I N D U C T I O N',/)
      ELSEIF ( local==26 ) THEN
         WRITE (l,99025)
99025    FORMAT (4X,'ELEMENT-ID   EL-TYPE         X-FIELD',10X,'Y-FIELD',10X,'Z-FIELD        X-INDUCTION      Y-INDUCTION',6X,      &
                &'Z-INDUCTION')
      ELSEIF ( local==27 ) THEN
         WRITE (l,99026)
99026    FORMAT (28X,'G R I D   P O I N T   S T R E S S E S   F O R   I S',' 2 D 8   E L E M E N T S',/)
      ELSEIF ( local==28 ) THEN
         WRITE (l,99027)
99027    FORMAT (2X,7HELEMENT,3X,5HNO.OF,4X,5HNO.OF,7X,4HGRID,3X,6HCOORD.)
      ELSEIF ( local==29 ) THEN
         WRITE (l,99028)
99028    FORMAT (4X,3HID.,4X,9HGRID PTS.,1X,8HSTRESSES,3X,5HPOINT,2X,7HSYS ID.,5X,5HSIG-X,8X,5HSIG-Y,8X,6HTAU-XY)
      ELSEIF ( local==30 ) THEN
         WRITE (l,99029)
99029    FORMAT (12X,5HNO.OF,4X,5HNO.OF,13X,6HCOORD.)
      ELSEIF ( local==31 ) THEN
         WRITE (l,99030)
99030    FORMAT (4X,4HTIME,3X,9HGRID PTS.,1X,8HSTRESSES,2X,7HGRID PT,1X,7HSYS ID.,5X,5HSIG-X,8X,5HSIG-Y,8X,6HTAU-XY)
      ELSEIF ( local==32 ) THEN
         WRITE (l,99031)
99031    FORMAT (20X,'C O M P L E X   G R I D   P O I N T   S T R E S S E',' S   F O R   I S 2 D 8   E L E M E N T S',/)
      ELSEIF ( local==33 ) THEN
         WRITE (l,99032)
99032    FORMAT (2X,7HELEMENT,3X,5HNO.OF,4X,5HNO.OF,13X,6HCOORD.,/4X,3HID.,4X,9HGRID PTS.,1X,8HSTRESSES,2X,7HGRID PT,1X,7HSYS ID.,  &
               & 11X,5HSIG-X,22X,5HSIG-Y,22X,6HTAU-XY)
      ELSEIF ( local==34 ) THEN
         WRITE (l,99033)
99033    FORMAT (12X,5HNO.OF,4X,5HNO.OF,13X,6HCOORD.,/1X,9HFREQUENCY,1X,9HGRID PTS.,1X,8HSTRESSES,2X,7HGRID PT,1X,7HSYS ID.,11X,    &
                &5HSIG-X,22X,5HSIG-Y,22X,6HTAU-XY)
      ELSEIF ( local==35 ) THEN
         WRITE (l,99034)
99034    FORMAT (12X,5HNO.OF,4X,5HNO.OF,13X,6HCOORD.,/2X,7HSUBCASE,2X,9HGRID PTS.,1X,8HSTRESSES,2X,7HGRID PT,1X,7HSYS ID.,5X,       &
               & 5HSIG-X,8X,5HSIG-Y,8X,6HTAU-XY)
      ELSEIF ( local==36 ) THEN
         WRITE (l,99035)
99035    FORMAT (26X,'F O R C E S    I N    C U R V E D    B E A M    E L',' E M E N T S',8X,'( C E L B O W )',/)
      ELSEIF ( local==37 ) THEN
         WRITE (l,99036)
99036    FORMAT (5X,7HELEMENT,11X,16H-BENDING MOMENT-,21X,7H-SHEAR-,18X,13H-AXIAL FORCE-,7X,8H-TORQUE-)
      ELSEIF ( local==38 ) THEN
         WRITE (l,99037)
99037    FORMAT (7X,3HID.,7X,13HPLANE-1 END-A,2X,13HPLANE-2 END-A,5X,13HPLANE-1 END-A,2X,13HPLANE-2 END-A,13X,5HEND-A,13X,5HEND-A)
      ELSEIF ( local==39 ) THEN
         WRITE (l,99038)
99038    FORMAT (25X,5HEND-B,10X,5HEND-B,13X,5HEND-B,28X,5HEND-B,13X,5HEND-B)
      ELSEIF ( local==40 ) THEN
         WRITE (l,99039)
99039    FORMAT (26X,'S T R E S S E S    I N    C U R V E D    B E A M   ',' E L E M E N T S',8X,'( C E L B O W )',/)
      ELSEIF ( local==41 ) THEN
         WRITE (l,99040)
99040    FORMAT (23X,16H-BENDING MOMENT-,21X,7H-SHEAR-,18X,13H-AXIAL FORCE-,7X,8H-TORQUE-)
      ELSEIF ( local==42 ) THEN
         WRITE (l,99041)
99041    FORMAT (4X,4HTIME,9X,13HPLANE-1 END-A,2X,13HPLANE-2 END-A,5X,13HPLANE-1 END-A,8X,7HPLANE-2,13X,5HEND-A,13X,5HEND-A,/25X,   &
                &5HEND-B,10X,5HEND-B,13X,5HEND-B,28X,5HEND-B,13X,5HEND-B)
      ELSEIF ( local==43 ) THEN
         WRITE (l,99042)
99042    FORMAT (6X,7HSUBCASE,4X,13HPLANE-1 END-A,2X,13HPLANE-2 END-A,5X,13HPLANE-1 END-A,2X,13HPLANE-2 END-A,13X,5HEND-A,13X,      &
                &5HEND-A,/25X,5HEND-B,10X,5HEND-B,13X,5HEND-B,28X,5HEND-B,13X,5HEND-B)
      ELSEIF ( local==44 ) THEN
         WRITE (l,99043)
99043    FORMAT (19X,'C O M P L E X   F O R C E S   I N   C U R V E D   ','B E A M   E L E M E N T S   ( C E L B O W )',/)
      ELSEIF ( local==45 ) THEN
         WRITE (l,99044)
99044    FORMAT (7X,9HFREQUENCY,21X,14HBENDING-MOMENT,19X,11HSHEAR-FORCE,22X,5HAXIAL,10X,6HTORQUE,/35X,7HPLANE 1,8X,7HPLANE 2,11X,  &
                &7HPLANE 1,8X,7HPLANE 2,13X,5HFORCE)
      ELSEIF ( local==46 ) THEN
         WRITE (l,99045)
99045    FORMAT (18X,'C O M P L E X   S T R E S S E S   I N   C U R V E D','   B E A M   E L E M E N T S   ( C E L B O W )',/)
      ELSEIF ( local==47 ) THEN
         WRITE (l,99046)
99046    FORMAT (7X,9HELEMENT  ,21X,14HBENDING-MOMENT,19X,11HSHEAR-FORCE,22X,5HAXIAL,10X,6HTORQUE,/35X,7HPLANE 1,8X,7HPLANE 2,11X,  &
                &7HPLANE 1,8X,7HPLANE 2,13X,5HFORCE)
      ELSEIF ( local==48 ) THEN
         WRITE (l,99047)
99047    FORMAT (23X,'F O R C E S   I N   F L U I D   H E X A H E D R A L','   E L E M E N T S   ( C F H E X 2 )',/)
      ELSEIF ( local==49 ) THEN
         WRITE (l,99048)
99048    FORMAT (23X,'F O R C E S   I N   F L U I D   H E X A H E D R A L','   E L E M E N T S   ( C F H E X 1 )',/)
      ELSEIF ( local==50 ) THEN
         WRITE (l,99049)
99049    FORMAT (19X,'F O R C E S   I N   F L U I D   T E T R A H E D R A',' L   E L E M E N T S   ( C F T E T R A )',/)
      ELSEIF ( local==51 ) THEN
         WRITE (l,99050)
99050    FORMAT (26X,'F O R C E S   I N   F L U I D   W E D G E   E L E M',' E N T S    ( C F W E D G E )',/)
      ELSEIF ( local==52 ) THEN
         WRITE (l,99051)
99051    FORMAT (24X,'P O W E R   C O N V E C T E D   B Y   F T U B E   ','E L E M E N T S   ( C F T U B E )',/)
      ELSEIF ( local==53 ) THEN
         WRITE (l,99052)
99052    FORMAT (47X,4HTIME,26X,5HPOWER)
      ELSEIF ( local==54 ) THEN
         WRITE (l,99053)
99053    FORMAT (45X,10HELEMENT-ID,22X,5HPOWER)
      ELSEIF ( local==55 ) THEN
         WRITE (l,99054)
99054    FORMAT (2X,7HELEMENT,3X,16HMAT. COORD. SYS.,30X,42H- STRESSES IN MATERIAL COORDINATE SYSTEM -,/4X,3HID.,5X,                &
                &16HID./OUTPUT CODED,14X,8HNORMAL-X,26X,8HNORAML-Y,25X,8HSHEAR-XY)
      ELSEIF ( local==56 ) THEN
         WRITE (l,99055)
99055    FORMAT (16X,16HMAT. COORD. SYS.,30X,42H- STRESSES IN MATERIAL COORDINATE SYSTEM -,/4X,9HFREQUENCY,3X,15HID./OUTPUT CODE,   &
               & 14X,8HNORAML-X,26X,8HNORMAL-Y,25X,8HSHEAR-XY)
      ELSEIF ( local==57 ) THEN
         WRITE (l,99056)
99056    FORMAT (50X,29H(IN STRESS COORDINATE SYSTEM),/)
      ELSEIF ( local==58 ) THEN
         WRITE (l,99057)
99057    FORMAT (2X,7HELEMENT,6X,5HFIBRE,15X,'STRESSES IN STRESS COORD. ','SYSTEM',13X,31HPRINCIPAL STRESSES (ZERO SHEAR),12X,3HMAX)
      ELSEIF ( local==59 ) THEN
         WRITE (l,99058)
99058    FORMAT (4X,3HID.,7X,8HDISTANCE,11X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X,5HSHEAR)
      ELSEIF ( local==60 ) THEN
         WRITE (l,99059)
99059    FORMAT (20X,'F O R C E S   I N   G E N E R A L   Q U A D R I ','L A T E R A L   E L E M E N T S     ( Q U A D 4 )',/)
      ELSEIF ( local==61 ) THEN
         WRITE (l,99060)
99060    FORMAT (6X,'ELEMENT',12X,'- MEMBRANE  FORCES -',22X,'- BENDING','   MOMENTS -',11X,'- TRANSVERSE SHEAR FORCES -')
      ELSEIF ( local==62 ) THEN
         WRITE (l,99061)
99061    FORMAT (8X,'ID',10X,2HFX,12X,2HFY,12X,3HFXY,11X,2HMX,12X,2HMY,12X,3HMXY,11X,2HVX,12X,2HVY)
      ELSEIF ( local==63 ) THEN
         WRITE (l,99062)
99062    FORMAT (19X,5HFIBRE,11X,32HSTRESSES IN STRESS COORD. SYSTEM,13X,31HPRINCIPAL STRESSES (ZERO SHEAR),10X,7HMAXIMUM,/7X,      &
               & 4HTIME,7X,8HDISTANCE,7X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X,5HSHEAR)
      ELSEIF ( local==64 ) THEN
         WRITE (l,99063)
99063    FORMAT (19X,5HFIBRE,11X,32HSTRESSES IN STRESS COORD. SYSTEM,13X,31HPRINCIPAL STRESSES (ZERO SHEAR),10X,7HMAXIMUM,/5X,      &
                &7HSUBCASE,6X,8HDISTANCE,7X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X,5HSHEAR)
      ELSEIF ( local==65 ) THEN
         WRITE (l,99064)
99064    FORMAT (6X,' TIME  ',18X,'- MEMBRANE  FORCES -',22X,'- BENDING','   MOMENTS -',11X,'- TRANSVERSE SHEAR FORCES -')
      ELSEIF ( local==66 ) THEN
         WRITE (l,99065)
99065    FORMAT (26X,2HFX,12X,2HFY,12X,3HFXY,11X,2HMX,12X,2HMY,12X,3HMXY,11X,2HVX,12X,2HVY)
      ELSEIF ( local==67 ) THEN
         WRITE (l,99066)
99066    FORMAT (6X,'SUBCASE',18X,'- MEMBRANE  FORCES -',22X,'- BENDING','   MOMENTS -',11X,'- TRANSVERSE SHEAR FORCES -')
      ELSEIF ( local==68 ) THEN
         WRITE (l,99067)
99067    FORMAT (6X,'C O M P L E X   S T R E S S E S   I N   G E N E R A ','L   Q U A D R I L I A T E R A L   E L E M E N T S   ',  &
                &'( C Q U A D 4 )')
      ELSEIF ( local==69 ) THEN
         WRITE (l,99068)
99068    FORMAT (9H  ELEMENT,7X,5HFIBRE,38X,'- STRESSES IN STRESS COORDI','NATE SYSTEM -',/4X,3HID.,8X,8HDISTANCE,18X,8HNORMAL-X,   &
               & 26X,8HNORMAL-Y,25X,8HSHEAR-XY)
      ELSEIF ( local==70 ) THEN
         WRITE (l,99069)
99069    FORMAT (20X,5HFIBRE,38X,'- STRESSES IN STRESS COORDINATE SYSTEM -',/4X,9HFREQUENCY,6X,8HDISTANCE,18X,8HNORMAL-X,26X,       &
                &8HNORMAL-Y,25X,8HSHEAR-XY)
      ELSEIF ( local==71 ) THEN
         WRITE (l,99070)
99070    FORMAT (6X,7HELEMENT,15X,6HCENTER,22X,7HEDGE  1,14X,7HEDGE  2,14X,7HEDGE  3,14X,7HEDGE  4,/8X,3HID.,9X,                    &
               & 'R ------- PHI ----','-- Z',4X,4(8X,13HS ------- PHI))
      ELSEIF ( local==72 ) THEN
         WRITE (l,99071)
99071    FORMAT (28X,6HCENTER,22X,7HEDGE  1,14X,7HEDGE  2,14X,7HEDGE  3,14X,7HEDGE 4,/7X,4HTIME,9X,22HR ------- PHI ------ Z,4X,    &
               & 4(8X,13HS ------- PHI))
      ELSEIF ( local==73 ) THEN
         WRITE (l,99072)
99072    FORMAT (29X,6HCENTER,21X,7HEDGE  1,14X,7HEDGE  2,14X,7HEDGE  3,14X,7HEDGE  4,/4X,9HFREQUENCY,7X,22HR ------- PHI ------ Z, &
               & 4X,4(8X,13HS ------- PHI))
      ELSEIF ( local==74 ) THEN
         WRITE (l,99073)
99073    FORMAT (9X,'C O M P L E X   S T R E S S E S   I N   T R I A N G ','U L A R   M E M B R A N E   E L E M E N T S   ',        &
                &'( C T R I M 6 )')
      ELSEIF ( local==75 ) THEN
         WRITE (l,99074)
99074    FORMAT (11X,'C O M P L E X   F O R C E S   I N   T R I A N G U L',                                                         &
                &' A R   M E M B R A N E   E L E M E N T S   ( C T R I M 6 )')
      ELSEIF ( local==76 ) THEN
         WRITE (l,99075)
99075    FORMAT (9X,'C O M P L E X   S T R E S S E S   I N   T R I A N G ','U L A R   B E N D I N G   E L E M E N T S   ',          &
                &'( C T R P L T 1 )')
      ELSEIF ( local==77 ) THEN
         WRITE (l,99076)
99076    FORMAT (11X,'C O M P L E X   F O R C E S   I N   T R I A N G U L',                                                         &
                &' A R   B E N D I N G   E L E M E N T S   ( C T R P L T 1 )')
      ELSEIF ( local==78 ) THEN
         WRITE (l,99077)
99077    FORMAT (12X,'C O M P L E X   S T R E S S E S   I N   T R I A N G',                                                         &
                &' U L A R   S H E L L   E L E M E N T S   ( C T R S H L )')
      ELSEIF ( local==79 ) THEN
         WRITE (l,99078)
99078    FORMAT (14X,'C O M P L E X   F O R C E S   I N   T R I A N G U L',' A R   S H E L L   E L E M E N T S   ( C T R S H L )')
      ELSEIF ( local==80 ) THEN
         WRITE (l,99079)
99079    FORMAT (9X,'C O M P L E X   F O R C E S   I N   G E N E R A L   ','Q U A D R I L A T E R A L   E L E M E N T S   ',        &
                &'( C Q U A D 4 )')
      ELSEIF ( local==81 ) THEN
         WRITE (l,99080)
99080    FORMAT (3X,'FREQUENCY',14X,'- MEMBRANE  FORCES -',23X,'- BENDING','   MOMENTS -',10X,'- TRANSVERSE SHEAR FORCES -',/22X,   &
                &2HFX,12X,2HFY,11X,3HFXY,13X,2HMX,12X,2HMY,11X,3HMXY,13X,2HVX,12X,2HVY)
      ELSEIF ( local==82 ) THEN
         WRITE (l,99081)
99081    FORMAT (16X,4HGRID,11X,35HSTRESSES IN BASIC COORDINATE SYSTEM,13X,12HDIR. COSINES,/3X,9HFREQUENCY,3X,5HPOINT,5X,8HNORMAL-X,&
               & 9X,8HNORMAL-Y,9X,8HNORMAL-Z,9X,8HSHEAR-XY,9X,8HSHEAR-YZ,9X,8HSHEAR-ZX)
      ELSEIF ( local==83 ) THEN
         WRITE (l,99082)
99082    FORMAT (22X,'F O R C E S   I N   G E N E R A L   T R I A N G ','U L A R   E L E M E N T S     ( C T R I A 3 )',/)
      ELSEIF ( local==84 ) THEN
         WRITE (l,99083)
99083    FORMAT (12X,'C O M P L E X   F O R C E S   I N   G E N E R A L  ',                                                         &
                &' T R I A N G U L A R   E L E M E N T S   ( C T R I A 3 )')
      ELSEIF ( local==85 ) THEN
         WRITE (l,99084)
99084    FORMAT (21X,'S T R E S S E S   I N   G E N E R A L   T R I A N G',' U L A R   E L E M E N T S',6X,'( C T R I A 3 )')
      ELSEIF ( local==86 ) THEN
         WRITE (l,99085)
99085    FORMAT (9X,'C O M P L E X   S T R E S S E S   I N   G E N E R A ','L   T R I A N G U L A R   E L E M E N T S   ',          &
                &'( C T R I A 3 )')
      ELSEIF ( local==87 ) THEN
         WRITE (l,99086)
99086    FORMAT (107X,22HOCTAHEDRAL    PRESSURE,/6X,10H   SUBCASE,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,6HTAU-XZ,&
               & 8X,6HTAU-XY,8X,5HTAU-0,10X,1HP)
      ELSEIF ( local==89 ) THEN
!WKBNB NCL93012 3/94
         WRITE (l,99087)
!WKBNB NCL93012 3/94
99087    FORMAT (4X,'S T R A I N S / C U R V A T U R E S   I N   G E N ','E R A L   Q U A D R I L A T E R A L   E L E M E N T S ',  &
               & 6X,'( Q U A D 4 )')
      ELSEIF ( local==90 ) THEN
         WRITE (l,99088)
99088    FORMAT (4X,'S T R A I N S / C U R V A T U R E S   I N   G E N ','E R A L   T R I A N G U L A R   E L E M E N T S ',6X,     &
                &'( T R I A 3 )')
      ELSEIF ( local==91 ) THEN
!WKBNE NCL93012 3/94
!WKBNB SPR94001 7/94
         WRITE (l,99089)
!WKBNE NCL93012 3/94
!WKBNB SPR94001 7/94
99089    FORMAT (' SUBCASE',5X,'STRESS',15X,'RADIAL',16X,'CIRCUMFERENTIAL',16X,'AXIAL',21X,'SHEAR')
      ELSEIF ( local==92 ) THEN
         WRITE (l,99090)
99090    FORMAT (5X,'NO ',6X,'POINT',17X,'(X)',21X,'(THETA)',21X,'(Z)',23X,'(ZX)')
      ELSEIF ( local==93 ) THEN
         WRITE (l,99091)
99091    FORMAT (' SUBCASE   CORNER',18X,'RADIAL',26X,'CIRCUMFERENTIAL',26X,'AXIAL')
      ELSEIF ( local==94 ) THEN
         WRITE (l,99092)
99092    FORMAT ('     NO     POINT',20X,'(X)',31X,'(THETA)',31X,'(Z)')
      ELSE
         GOTO 100
      ENDIF
      GOTO 99999
   ENDIF
!WKBNE SPR94001 7/94
!
 100  WRITE (l,99093) Line
99093 FORMAT ('0*** OFP ERROR/OFP1C,  LINE=',I9)
   CALL mesage(-61,0,0)
!
 200  WRITE (l,99094)
!
!     ******************************************************************
!
99094 FORMAT (4X,'S T R A I N S / C U R V A T U R E S   I N   G E N E ','R A L   Q U A D R I L A T E R A L   E L E M E N T S',6X,   &
             &'( C Q U A D 2 )')
!WKBNE SPR94001 7/94
!WKBNE SPR94001 7/94
99999 END SUBROUTINE ofp1c
