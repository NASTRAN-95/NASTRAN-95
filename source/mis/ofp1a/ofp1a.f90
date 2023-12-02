!*==ofp1a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofp1a(Line)
   IMPLICIT NONE
   USE C_OFP1ID
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Line
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cycfrq , f
   REAL , DIMENSION(50) :: fid
   INTEGER , DIMENSION(50) :: id
   INTEGER :: iflag , j , k , l , linet , local , nogo
   INTEGER , DIMENSION(5) :: l123
   INTEGER , DIMENSION(6) :: of
   REAL , DIMENSION(8,15) , SAVE :: rt
   REAL , DIMENSION(2) , SAVE :: sectn
   REAL , SAVE :: twopi
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE WAS NAMED OFP1 BEFORE.
!
   !>>>>EQUIVALENCE (Ksys(2),L) , (Ksys(12),Linet) , (Ksys(33),Iflag) , (Ksys(3),Nogo) , (Core(1),Of(1),L123(1)) , (Fid(1),Id(1),Of(6))
!
   DATA rt/4HSING , 4HULAR , 4HITIE , 4HS EN , 4HCOUN , 4HTERE , 4HD.   , 4H     , 4H4 SH , 4HIFT  , 4HPTS. , 4HPER  , 4HROOT ,     &
       &4H EXC , 4HEEDE , 4HD.   , 4HALL  , 4HEIGE , 4HNVAL , 4HUES  , 4HFOUN , 4HD IN , 4H RAN , 4HGE.  , 4H3X E , 4HST.R ,        &
      & 4HOOTS , 4H IN  , 4HRANG , 4HE SP , 4HECIF , 4HIED. , 4HNO M , 4HORE  , 4HEIGE , 4HNVAL , 4HUES  , 4HIN P , 4HROBL ,        &
      & 4HEM.  , 4HNO.  , 4HOF R , 4HOOTS , 4H DES , 4HIRED , 4H WER , 4HE FO , 4HUND. , 4H1 OR , 4H MOR , 4HE RO , 4HOT O ,        &
      & 4HUTSI , 4HDE F , 4HR.RA , 4HNGE. , 4HINSU , 4HFFIC , 4HIENT , 4H TIM , 4HE FO , 4HR NE , 4HXT R , 4HOOT. , 4HUNAB ,        &
      & 4HLE T , 4HO CO , 4HNVER , 4HGE.  , 4H     , 4H     , 4H     , 4HNORM , 4HAL T , 4HERMI , 4HNATI , 4HON   , 4H     ,        &
      & 4H     , 4H     , 4HEIGE , 4HNVAL , 4HUES  , 4HOUTS , 4HIDE  , 4HFREQ , 4H. RA , 4HNGE  , 4HINSU , 4HFFIC , 4HIENT ,        &
      & 4H TIM , 4HE RE , 4HMAIN , 4HING  , 4H     , 4HFEWE , 4HR TH , 4HAN R , 4HEQUE , 4HSTED , 4H ROO , 4HTS F , 4HOUND ,        &
      & 4HROOT , 4HS FO , 4HUND  , 4HWITH , 4H REQ , 4H. AC , 4HCURA , 4HCY   , 4HNO R , 4HOOTS , 4H FOU , 4HND,  , 4HNONE ,        &
      & 4H PAS , 4HSED  , 4HTEST/
   DATA sectn/4H.3.3 , 4H.7.3/ , twopi/6.283185307/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         local = Line - 100
         IF ( local<=0 ) THEN
            IF ( Line==1 ) THEN
!
               WRITE (l,99001)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==2 ) THEN
               WRITE (l,99002)
99002          FORMAT (6X,16HPOINT ID.   TYPE,10X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==3 ) THEN
               WRITE (l,99003)
99003          FORMAT (46X,31HR E A L   E I G E N V A L U E S,/)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==4 ) THEN
               WRITE (l,99004)
99004          FORMAT (3X,4HMODE,4X,10HEXTRACTION,7X,10HEIGENVALUE,12X,6HRADIAN,14X,6HCYCLIC,2X,2(9X,11HGENERALIZED))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==5 ) THEN
               WRITE (l,99005)
99005          FORMAT (4X,3HNO.,7X,5HORDER,30X,9HFREQUENCY,11X,9HFREQUENCY,12X,4HMASS,14X,9HSTIFFNESS,/)
               IF ( Id22==-999 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Id22<0 ) THEN
                  IF ( M/=8 .AND. M/=12 ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  WRITE (l,99182)
                  WRITE (l,99006)
99006             FORMAT (1H+,42X,'NASTRAN WARNING MESSAGE 3309, ALL LOWER EIGEN','VALUES',6X,1H*,/37X,1H*,5X,                      &
                        & 'NOT NECESSARY FOUND.',37X,1H*)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Id22==0 ) THEN
                  IF ( .NOT.((M>=3 .AND. M<=7) .OR. M==10 .OR. M==13) ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  WRITE (l,99182)
                  WRITE (l,99007)
99007             FORMAT (1H+,39X,'NASTRAN INFORMATION MESSAGE 3308, LOWEST EIGEN','VALUE FOUND',3X,1H*,/37X,1H*,2X,                &
                         &'AS INDICATED BY THE ','STURM''S SEQUENCE OF THE DYNAMIC MATRIX',2X,1H*)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  WRITE (l,99182)
                  WRITE (l,99008) Id22
99008             FORMAT (1H+,45X,'NASTRAN INFORMATION MESSAGE 3307, POTENTIALLY',9X,1H*,/37X,1H*,I10,                              &
                         &' EIGENVALUE(S) AT LOW FREQ. END NOT',' FOUND',11X,1H*)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Line==6 ) THEN
               WRITE (l,99009) id(5)
99009          FORMAT (41X,39HR E A L   E I G E N V E C T O R   N O .,I11)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==7 ) THEN
               WRITE (l,99010)
99010          FORMAT (7X,7HELEMENT,11X,5HAXIAL,37X,7HELEMENT,11X,5HAXIAL)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==8 ) THEN
               WRITE (l,99011)
99011          FORMAT (9X,3HID.,13X,5HFORCE,10X,6HTORQUE,23X,3HID.,13X,5HFORCE,10X,6HTORQUE)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==9 ) THEN
               WRITE (l,99012)
99012          FORMAT (12H0    ELEMENT,9X,17HBEND-MOMENT END-A,12X,17HBEND-MOMENT END-B,16X,9H- SHEAR -,15X,5HAXIAL)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==10 ) THEN
               WRITE (l,99013)
99013          FORMAT (4X,9H   ID.   ,3(6X,7HPLANE 1,7X,7HPLANE 2,2X),7X,5HFORCE,9X,6HTORQUE)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==11 ) THEN
               WRITE (l,99014)
99014          FORMAT (7X,7HELEMENT,11X,5HFORCE,10X,5HFORCE,22X,7HELEMENT,11X,5HFORCE,10X,5HFORCE)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==12 ) THEN
               WRITE (l,99015)
99015          FORMAT (9X,3HID.,12X,7HPTS 1,3,8X,7HPTS 2,4,23X,3HID.,12X,7HPTS 1,3,8X,7HPTS 2,4)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==13 ) THEN
               WRITE (l,99016)
99016          FORMAT (7X,7HELEMENT,10X,6HMOMENT,9X,6HMOMENT,22X,7HELEMENT,10X,6HMOMENT,9X,6HMOMENT)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==14 ) THEN
               WRITE (l,99017)
99017          FORMAT (1H0,8X,7HELEMENT,2(11X,11HBEND-MOMENT),10X,12HTWIST-MOMENT,2(13X,5HSHEAR,4X))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==15 ) THEN
               WRITE (l,99018)
99018          FORMAT (11X,3HID.,17X,1HX,21X,1HY,43X,1HX,21X,1HY)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==16 ) THEN
               WRITE (l,99019)
99019          FORMAT (6X,3(7HELEMENT,9X,5HFORCE,12X),7HELEMENT,9X,5HFORCE)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==17 ) THEN
               WRITE (l,99020)
99020          FORMAT (8X,3(3HID.,30X),3HID.)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==18 ) THEN
               WRITE (l,99021)
99021          FORMAT (2(7X,7HELEMENT,7X,5HAXIAL,7X,6HSAFETY,6X,9HTORSIONAL,5X,6HSAFETY))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==19 ) THEN
               WRITE (l,99022)
99022          FORMAT (2(9X,3HID.,8X,6HSTRESS,7X,6HMARGIN,8X,6HSTRESS,6X,6HMARGIN))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==20 ) THEN
               WRITE (l,99023)
99023          FORMAT (2X,7HELEMENT,8X,3HSA1,12X,3HSA2,12X,3HSA3,15X,1HS,14X,6HSA-MAX,9X,6HSA-MIN,11X,6HM.S.-T)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==21 ) THEN
               WRITE (l,99024)
99024          FORMAT (4X,3HID.,10X,3HSB1,12X,3HSB2,12X,3HSB3,30X,6HSB-MAX,9X,6HSB-MIN,11X,6HM.S.-C)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==22 ) THEN
               WRITE (l,99025)
99025          FORMAT (2(9X,7HELEMENT,12X,3HMAX,12X,3HAVG,8X,6HSAFETY))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==23 ) THEN
               WRITE (l,99026)
99026          FORMAT (2(11X,3HID.,13X,5HSHEAR,10X,5HSHEAR,7X,6HMARGIN))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==24 ) THEN
               WRITE (l,99027)
99027          FORMAT (2(11X,3HID.,40X,6HMARGIN))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==25 ) THEN
               WRITE (l,99028)
99028          FORMAT (2X,7HELEMENT,11X,32HSTRESSES IN ELEMENT COORD SYSTEM,12X,9HPRINCIPAL,11X,18HPRINCIPAL STRESSES,12X,3HMAX)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==26 ) THEN
               WRITE (l,99029)
99029          FORMAT (4X,3HID.,11X,8HNORMAL-X,7X,8HNORMAL-Y,7X,8HSHEAR-XY,6X,12HSTRESS ANGLE,9X,5HMAJOR,10X,5HMINOR,10X,5HSHEAR)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==27 ) THEN
               WRITE (l,99030)
99030          FORMAT (2X,7HELEMENT,6X,5HFIBRE,15X,'STRESSES IN ELEMENT COORD ','SYSTEM',13X,'PRINCIPAL STRESSES (ZERO SHEAR)',12X, &
                      &3HMAX)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==28 ) THEN
               WRITE (l,99031)
99031          FORMAT (4X,3HID.,7X,8HDISTANCE,11X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X,     &
                      &5HSHEAR)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==29 ) THEN
               WRITE (l,99032)
99032          FORMAT (6X,3(7HELEMENT,9X,6HSTRESS,11X),7HELEMENT,9X,6HSTRESS)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==30 ) THEN
!
!     PROCESS SPC AND MPC SET IDS PROPERLY TO ACCOUNT FOR AXISYMMETRIC
!     PROBLEMS
!
               DO j = 3 , 4
                  IF ( id(j)>=100000000 ) THEN
                     id(j) = id(j) - 100000000
                     IF ( id(j)>=100000000 ) id(j) = id(j) - 100000000
                  ENDIF
               ENDDO
               WRITE (l,99033) id(3) , id(4)
99033          FORMAT (30X,'G R I D   P O I N T   S I N G U L A R I T Y   ','T A B L E',6X,3HSPC,I9,3X,3HMPC,I9)
               linet = linet + 1
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==31 ) THEN
               WRITE (l,99034)
99034          FORMAT (8X,5HPOINT,10X,11HSINGULARITY,18X,'LIST OF COORDINATE ','COMBINATIONS THAT WILL REMOVE SINGULARITY')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==32 ) THEN
               WRITE (l,99035)
99035          FORMAT (9X,3HID.,3X,4HTYPE,7X,5HORDER,7X,21HSTRONGEST COMBINATION,15X,18HWEAKER COMBINATION,17X,                     &
                      &19HWEAKEST COMBINATION)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==33 ) THEN
               WRITE (l,99036)
99036          FORMAT (53X,21HL O A D   V E C T O R)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==34 ) THEN
               WRITE (l,99037)
99037          FORMAT (2X,7HELEMENT,8X,3HSA1,12X,3HSA2,12X,3HSA3,12X,3HSA4,11X,5HAXIAL,10X,6HSA-MAX,9X,17HSA-MIN     M.S.-T)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==35 ) THEN
               WRITE (l,99038)
99038          FORMAT (4X,3HID.,10X,3HSB1,12X,3HSB2,12X,3HSB3,12X,3HSB4,11X,6HSTRESS,9X,6HSB-MAX,9X,17HSB-MIN     M.S.-C)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==36 ) THEN
               WRITE (l,99039)
99039          FORMAT (43X,'F O R C E S   I N   R O D   E L E M E N T S',5X,'( C R O D )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==37 ) THEN
               WRITE (l,99040)
99040          FORMAT (33X,'F O R C E S   I N   B E A M   E L E M E N T S',8X,'( C B E A M )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==38 ) THEN
               WRITE (l,99041)
99041          FORMAT (27X,'F O R C E S   A C T I N G   O N   S H E A R   ','P A N E L   E L E M E N T S   ( C S H E A R )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==39 ) THEN
               WRITE (l,99042)
99042          FORMAT (37X,'F O R C E S   I N   T W I S T   P A N E L S',6X,'( C T W I S T )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==40 ) THEN
               WRITE (l,99043)
99043          FORMAT (21X,'F O R C E S   I N   B A S I C   B E N D I N G   ','T R I A N G L E S',7X,'( C T R B S C )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==41 ) THEN
               WRITE (l,99044)
99044          FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 1 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==42 ) THEN
               WRITE (l,99045)
99045          FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 2 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==43 ) THEN
               WRITE (l,99046)
99046          FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 3 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==44 ) THEN
               WRITE (l,99047)
99047          FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 4 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==45 ) THEN
               WRITE (l,99048)
99048          FORMAT (31X,'F O R C E S   O F   S I N G L E - P O I N T   ','C O N S T R A I N T')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==46 ) THEN
               WRITE (l,99049)
99049          FORMAT (43X,'F O R C E S   I N   R O D   E L E M E N T S',5X,'( C O N R O D )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==47 ) THEN
               WRITE (l,99050)
99050          FORMAT (33X,'F O R C E S   I N   B A R   E L E M E N T S',9X,'( C B A R )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==48 ) THEN
               WRITE (l,99051)
99051          FORMAT (17X,'F O R C E S   I N   B E N D I N G   Q U A D R I L A',' T E R A L S',9X,'( C Q D P L T )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==49 ) THEN
               WRITE (l,99052)
99052          FORMAT (17X,'F O R C E S   I N   G E N E R A L   Q U A D R I L A',' T E R A L   E L E M E N T S     ( C Q U A D 1 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==50 ) THEN
               WRITE (l,99053)
99053          FORMAT (17X,'F O R C E S   I N   G E N E R A L   Q U A D R I L A','T E R A L   E L E M E N T S     ( C Q U A D 2 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==51 ) THEN
               WRITE (l,99054)
99054          FORMAT (21X,'F O R C E S   I N   G E N E R A L   T R I A N G U L',' A R   E L E M E N T S',8X,'( C T R I A 1 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==52 ) THEN
               WRITE (l,99055)
99055          FORMAT (21X,'F O R C E S   I N   G E N E R A L   T R I A N G U L',' A R   E L E M E N T S',8X,'( C T R I A 2 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==53 ) THEN
               WRITE (l,99056)
99056          FORMAT (27X,'F O R C E S   I N   B E N D I N G   T R I A N G L E',' S       ( C T R P L T )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==54 ) THEN
               WRITE (l,99057)
99057          FORMAT (33X,'F O R C E S   I N   R O D   E L E M E N T S     ','( C T U B E )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==55 ) THEN
               WRITE (l,99058)
99058          FORMAT (37X,'S T R E S S E S   I N   R O D   E L E M E N T S',6X,'( C R O D )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==56 ) THEN
               WRITE (l,99059)
99059          FORMAT (34X,'S T R E S S E S   I N   B E A M   E L E M E N T S',8X,'( C B E A M )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==57 ) THEN
               WRITE (l,99060)
99060          FORMAT (40X,'S T R E S S E S   I N   S H E A R   P A N E L S',6X,'( C S H E A R )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==58 ) THEN
               WRITE (l,99061)
99061          FORMAT (40X,'S T R E S S E S   I N   T W I S T   P A N E L S',7X,'( C T W I S T )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==59 ) THEN
               WRITE (l,99062)
99062          FORMAT (22X,'S T R E S S E S   I N   T R I A N G U L A R   ','M E M B R A N E S      ( C T R M E M )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==60 ) THEN
               WRITE (l,99063)
99063          FORMAT (19X,'S T R E S S E S   I N   B A S I C   B E N D I N G  ',' T R I A N G L E S',8X,'( C T R B S C )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==61 ) THEN
               WRITE (l,99064)
99064          FORMAT (30X,'S T R E S S E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 1 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==62 ) THEN
               WRITE (l,99065)
99065          FORMAT (30X,'S T R E S S E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 2 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==63 ) THEN
               WRITE (l,99066)
99066          FORMAT (30X,'S T R E S S E S   I N   S C A L A R   S P R I N G S',8X,'( C E L A S 3 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==64 ) THEN
               WRITE (l,99067)
99067          FORMAT (33X,'S T R E S S E S   I N   B A R   E L E M E N T S',10X,'( C B A R )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==65 ) THEN
               WRITE (l,99068)
99068          FORMAT (37X,'S T R E S S E S   I N   R O D   E L E M E N T S',6X,'( C O N R O D )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==66 ) THEN
               WRITE (l,99069)
99069          FORMAT (21X,'S T R E S S E S   I N   Q U A D R I L A T E R A L  ',' M E M B R A N E S      ( C Q D M E M )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==67 ) THEN
               WRITE (l,99070)
99070          FORMAT (18X,'S T R E S S E S   I N   B E N D I N G   Q U A D R I',' L A T E R A L S',13X,'( C Q D P L T )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==68 ) THEN
               WRITE (l,99071)
99071          FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   Q U A D R I',' L A T E R A L   E L E M E N T S',6X,             &
                      &'( C Q U A D 1 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==69 ) THEN
               WRITE (l,99072)
99072          FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   Q U A D R I',' L A T E R A L   E L E M E N T S',6X,             &
                      &'( C Q U A D 2 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==70 ) THEN
               WRITE (l,99073)
99073          FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   T R I A N G',' U L A R   E L E M E N T S',7X,'( C T R I A 1 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==71 ) THEN
               WRITE (l,99074)
99074          FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   T R I A N G',' U L A R   E L E M E N T S',7X,'( C T R I A 2 )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==72 ) THEN
               WRITE (l,99075)
99075          FORMAT (24X,'S T R E S S E S   I N   B E N D I N G   T R I A N G',' L E S',8X,'( C T R P L T )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==73 ) THEN
               WRITE (l,99076)
99076          FORMAT (36X,'S T R E S S E S   I N   R O D   E L E M E N T S',6X,'( C T U B E )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==74 ) THEN
               WRITE (l,99077)
99077          FORMAT (20X,'S T R E S S E S   F O R   T H E   T R I A N G U L A',' R   R I N G S',5X,'( C T R I A R G )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==75 ) THEN
               WRITE (l,99078)
99078          FORMAT (5X,3HEL ,13X,6HRADIAL,20X,15HCIRCUMFERENTIAL,20X,5HAXIAL,25X,5HSHEAR)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==76 ) THEN
               WRITE (l,99079)
99079          FORMAT (5X,3HID ,15X,3H(X),25X,7H(THETA),25X,3H(Z),27X,4H(ZX))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==77 ) THEN
               WRITE (l,99080)
99080          FORMAT (18X,'S T R E S S E S   F O R   T H E   T R A P E Z O I D',' A L   R I N G S',5X,'( C T R A P R G )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==78 ) THEN
               WRITE (l,99081)
99081          FORMAT (5X,3HEL ,5X,6HSTRESS,15X,6HRADIAL,16X,15HCIRCUMFERENTIAL,16X,5HAXIAL,21X,5HSHEAR)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==79 ) THEN
               WRITE (l,99082)
99082          FORMAT (5X,3HID ,6X,5HPOINT,17X,3H(X),21X,7H(THETA),21X,3H(Z),23X,4H(ZX))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==80 ) THEN
               WRITE (l,99083)
99083          FORMAT (11X,'S T R E S S   R E S U L T A N T S   F O R   T H E  ',                                                   &
                      &' T O R O I D A L   R I N G S     ( C T O R D R G )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==81 ) THEN
               WRITE (l,99084)
99084          FORMAT (5X,3HEL ,8H  STRESS,15X,17HMEMBRANE (FORCES),26X,17HFLEXURE (MOMENTS),23X,5HSHEAR)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==82 ) THEN
               WRITE (l,99085)
99085          FORMAT (5X,2HID,9H    POINT,8X,10HTANGENTIAL,10X,'CIRCUMFERENTIAL',8X,10HTANGENTIAL,11X,15HCIRCUMFERENTIAL,10X,      &
                      &7H(FORCE))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==83 ) THEN
               WRITE (l,99086)
99086          FORMAT (22X,'F O R C E S   F O R   T H E   T R I A N G U L A R  ',' R I N G S     ( C T R I A R G )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==84 ) THEN
               WRITE (l,99087)
99087          FORMAT (5X,12HEL    CORNER,18X,6HRADIAL,26X,15HCIRCUMFERENTIAL,26X,5HAXIAL)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==85 ) THEN
               WRITE (l,99088)
99088          FORMAT (5X,12HID     POINT,20X,3H(X),31X,7H(THETA),31X,3H(Z))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==86 ) THEN
               WRITE (l,99089)
99089          FORMAT (21X,'F O R C E S   F O R   T H E   T R A P E Z O I D A L','   R I N G S     ( C T R A P R G )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==87 ) THEN
               WRITE (l,99090)
99090          FORMAT (23X,'F O R C E S   F O R   T H E   T O R O I D A L   ','R I N G S     ( C T O R D R G )')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==88 ) THEN
               WRITE (l,99091)
99091          FORMAT (5X,12HEL    CORNER,9X,6HRADIAL,8X,15HCIRCUMFERENTIAL,7X,5HAXIAL,13X,6HMOMENT,9X,13HDIRECT STRAIN,7X,         &
                      &9HCURVATURE)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==89 ) THEN
               WRITE (l,99092)
99092          FORMAT (5X,12HID     POINT,11X,3H(X),13X,7H(THETA),12X,3H(Z),15X,4H(ZX),14X,4H(XI),13X,7H(XI,XI))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==90 ) THEN
               IF ( id(16)==1 ) THEN
                  WRITE (l,99093)
99093             FORMAT (30X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R',' Y',9X,'(FEER METHOD)')
               ELSE
                  WRITE (l,99094)
99094             FORMAT (30X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R',' Y     (INVERSE POWER METHOD)')
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==91 ) THEN
               WRITE (l,99095) (id(k),k=11,15) , id(17) , fid(18) , (id(k),k=19,21)
99095          FORMAT (1H0,/1H0,39X,32HNUMBER OF EIGENVALUES EXTRACTED ,6(2H .),I10,/1H0,39X,30HNUMBER OF STARTING POINTS USED,     &
                      &7(2H .),I10,/1H0,39X,30HNUMBER OF STARTING POINT MOVES,7(2H .),I10,/1H0,39X,                                 &
                      &36HNUMBER OF TRIANGULAR DECOMPOSITIONS ,4(2H .),I10,/1H0,39X,34HTOTAL NUMBER OF VECTOR ITERATIONS ,5(2H .),  &
                     & I10,//1H0,39X,22HREASON FOR TERMINATION,11(2H .),I10,1H*,/,/1H0,39X,36HLARGEST OFF-DIAGONAL MODAL MASS TERM, &
                     & 4(2H .),E10.2,/1H0,77X,3(2H .),I10,/50X,9HMODE PAIR,10(2H .),/78X,3(2H .),I10,/1H0,39X,                      &
                      &'NUMBER OF OFF-DIAGONAL MODAL ','MASS',/45X,23HTERMS FAILING CRITERION,8(2H .),I10)
               M = id(17)
               IF ( M>=8 ) nogo = 14
               IF ( id(16)==1 ) THEN
                  IF ( M==2 .OR. M>3 ) nogo = 14
                  IF ( M==0 ) M = 10
                  IF ( M==1 ) M = 13
                  IF ( M==3 ) M = 14
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==92 ) THEN
               WRITE (l,99096)
99096          FORMAT (26X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R',' Y       (DETERMINANT METHOD)')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==93 ) THEN
               WRITE (l,99097) (id(k),k=11,17) , fid(18) , (id(k),k=19,21)
99097          FORMAT (1H0,/1H0,39X,32HNUMBER OF EIGENVALUES EXTRACTED ,6(2H .),I9,/1H0,39X,                                        &
                      &44HNUMBER OF PASSES THROUGH STARTING POINTS . .,I9,/1H0,39X,26HNUMBER OF CRITERIA CHANGES,9(2H .),I9,/1H0,   &
                     & 39X,30HNUMBER OF STARTING POINT MOVES,7(2H .),I9,/1H0,39X,36HNUMBER OF TRIANGULAR DECOMPOSITIONS ,4(2H .),I9,&
                     & /1H0,39X,44HNUMBER OF FAILURES TO ITERATE TO A ROOT  . .,I9,//1H0,39X,22HREASON FOR TERMINATION,11(2H .),I9, &
                      &1H*,//,1H0,39X,36HLARGEST OFF-DIAGONAL MODAL MASS TERM,4(2H .),E9.2,/1H0,77X,3(2H .),I9,/50X,9HMODE PAIR,    &
                      &10(2H .),/78X,3(2H .),I9,/1H0,39X,33HNUMBER OF OFF-DIAGONAL MODAL MASS,/45X,23HTERMS FAILING CRITERION,      &
                     & 8(2H .),I9)
               M = id(17)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==94 ) THEN
               WRITE (l,99098)
99098          FORMAT (10X,14HSTARTING POINT,6X,6HLAMBDA,9X,'RADIAN FREQUENCY  ','  CYCLIC FREQUENCY    DETERMINANT',9X,            &
                      &'SCALE FACTOR',/)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==95 ) THEN
               WRITE (l,99099)
99099          FORMAT (1H0,40X,'S W E P T   D E T E R M I N A N T   F U N C T I',' O N',/)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==96 ) THEN
               WRITE (l,99100)
99100          FORMAT (20X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',' S   S U M M A R Y     (DETERMINANT METHOD)')
               sectn(1) = sectn(2)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==97 ) THEN
               WRITE (l,99101)
99101          FORMAT (42X,5H- P -,35X,10H- DET(P) -,/10X,14HSTARTING POINT,10X,4HREAL,13X,4HIMAG,20X,9HMAGNITUDE,9X,5HPHASE,5X,    &
                      &12HSCALE FACTOR)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==98 ) THEN
               WRITE (l,99102) (id(k),k=11,18)
99102          FORMAT (1H0,/1H0,39X,32HNUMBER OF EIGENVALUES EXTRACTED ,6(2H .),I9,/1H0,39X,                                        &
                      &44HNUMBER OF PASSES THROUGH STARTING POINTS . .,I9,/1H0,39X,26HNUMBER OF CRITERIA CHANGES,9(2H .),I9,/1H0,   &
                     & 39X,30HNUMBER OF STARTING POINT MOVES,7(2H .),I9,/1H0,39X,36HNUMBER OF TRIANGULAR DECOMPOSITIONS ,4(2H .),I9,&
                     & /1H0,39X,44HNUMBER OF FAILURES TO ITERATE TO A ROOT  . .,I9,/1H0,39X,36HNUMBER OF PREDICTIONS OUTSIDE REGION,&
                     & 4(2H .),I9,/1H0,/1H0,39X,22HREASON FOR TERMINATION,11(2H .),I9,1H*)
               M = id(18)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Line==99 ) THEN
               IF ( id(3)==4 ) THEN
                  WRITE (l,99103) id(11) , id(12) , id(18)
99103             FORMAT (1H0,/1H0,/,1H0,35X,32HNUMBER OF EIGENVALUES EXTRACTED ,9(2H .),I9,/,1H0,35X,                              &
                         &30HNUMBER OF EIGENVALUES DESIRED ,10(2H .),I9,/,1H0,35X,22HREASON FOR TERMINATION,14(2H .),I9,1H*)
                  M = id(18)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  WRITE (l,99104) (id(k),k=11,16)
99104             FORMAT (1H0,/1H0,/1H0,35X,32HNUMBER OF EIGENVALUES EXTRACTED ,9(2H .),I9,/1H0,35X,                                &
                         &30HNUMBER OF STARTING POINTS USED,10(2H .),I9,/1H0,35X,                                                   &
                         &50HNUMBER OF STARTING POINT OR SHIFT POINT MOVES  . .,I9,/1H0,35X,                                        &
                         &42HTOTAL NUMBER OF TRIANGULAR DECOMPOSITIONS ,4(2H .),I9,/1H0,35X,34HTOTAL NUMBER OF VECTOR ITERATIONS ,  &
                         &8(2H .),I9,/1H0,/1H0,35X,22HREASON FOR TERMINATION,14(2H .),I9,1H*)
                  M = id(16)
                  IF ( id(17)==1 ) THEN
                     IF ( M>2 ) nogo = 14
                     IF ( M==0 ) M = 10
                     IF ( M==1 ) M = 13
                     IF ( M==2 ) M = 15
                  ENDIF
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Line==100 ) THEN
!
!     ID(3)=2, ID(17)=0, METHOD IS COMPLEX INV
!     ID(3)=2, ID(17)=1, METHOD IS COMPLEX FEER
!     ID(3)=4, ID(17)=0, METHOD IS COMPLEX HESS
!
               sectn(1) = sectn(2)
               IF ( id(17)==1 ) THEN
                  WRITE (l,99105)
99105             FORMAT (23X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',' S   S U M M A R Y     (FEER METHOD)')
               ELSEIF ( id(3)==4 ) THEN
                  WRITE (l,99106)
99106             FORMAT (20X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',' S   S U M M A R Y     (HESSENBERG METHOD)')
               ELSE
                  WRITE (l,99107)
99107             FORMAT (19X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',' S   S U M M A R Y   (INVERSE POWER METHOD)')
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( local==1 ) THEN
            f = sqrt(abs(fid(6)))/twopi
            WRITE (l,99108) fid(6) , f
99108       FORMAT (6X,'EIGENVALUE =',E14.6,4X,'(CYCLIC FREQUENCY =',E14.6,' HZ)'/)
            linet = linet + 1
         ELSEIF ( local==2 ) THEN
            f = sqrt(abs(fid(6)))/twopi
            WRITE (l,99109) fid(6) , f
99109       FORMAT (6X,'EIGENVALUE =',E14.6,4X,'(CYCLIC FREQUENCY =',1P,E14.6,' HZ)'/)
            linet = linet + 1
         ELSEIF ( local==3 ) THEN
            WRITE (l,99110) fid(5)
99110       FORMAT (6X,11HFREQUENCY =,E14.6)
         ELSEIF ( local==4 ) THEN
            WRITE (l,99111) fid(5)
99111       FORMAT (6X,11HFREQUENCY =,1P,E14.6)
         ELSEIF ( local==5 ) THEN
            WRITE (l,99112) fid(5)
99112       FORMAT (6X,6HTIME =,E14.6)
         ELSEIF ( local==6 ) THEN
            WRITE (l,99113) fid(5)
99113       FORMAT (6X,6HTIME =,1P,E14.6)
         ELSEIF ( local==7 ) THEN
            WRITE (l,99114) id(5)
99114       FORMAT (6X,10HPOINT-ID =,I8)
         ELSEIF ( local==8 ) THEN
            WRITE (l,99115) id(5)
99115       FORMAT (6X,12HELEMENT-ID =,I8,/)
         ELSEIF ( local==9 ) THEN
            cycfrq = abs(fid(7))/twopi
            WRITE (l,99116) fid(6) , fid(7) , cycfrq
99116       FORMAT (6X,20HCOMPLEX EIGENVALUE =,E14.6,1H,,E14.6,4X,'(CYCLIC FREQUENCY =',E14.6,'HZ)')
         ELSEIF ( local==10 ) THEN
            cycfrq = abs(fid(7))/twopi
            WRITE (l,99117) fid(6) , fid(7) , cycfrq
99117       FORMAT (6X,20HCOMPLEX EIGENVALUE =,1P,E14.6,1H,,1P,E14.6,4X,'(CYCLIC FREQUENCY =',1P,E14.6,'HZ)')
         ELSEIF ( local==11 ) THEN
            WRITE (l,99118)
99118       FORMAT (6X,16HFREQUENCY   TYPE,10X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
         ELSEIF ( local==12 ) THEN
            WRITE (l,99119)
99119       FORMAT (6X,16H TIME       TYPE,10X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
         ELSEIF ( local==13 ) THEN
            WRITE (l,99120)
99120       FORMAT (48X,30HV E L O C I T Y    V E C T O R)
         ELSEIF ( local==14 ) THEN
            WRITE (l,99121)
99121       FORMAT (44X,38HA C C E L E R A T I O N    V E C T O R)
         ELSEIF ( local==15 ) THEN
            WRITE (l,99122)
99122       FORMAT (41X,45HN O N - L I N E A R - F O R C E   V E C T O R)
         ELSEIF ( local==16 ) THEN
            WRITE (l,99123)
99123       FORMAT (40X,'C O M P L E X   E I G E N V A L U E   S U M M A R Y')
         ELSEIF ( local==17 ) THEN
            WRITE (l,99124)
99124       FORMAT (1H0,16X,19HROOT     EXTRACTION,18X,10HEIGENVALUE,21X,9HFREQUENCY,14X,7HDAMPING)
         ELSEIF ( local==18 ) THEN
            WRITE (l,99125)
99125       FORMAT (18X,3HNO.,8X,5HORDER,13X,6H(REAL),11X,6H(IMAG),16X,8H(CYCLES),12X,11HCOEFFICIENT)
         ELSEIF ( local==19 ) THEN
            WRITE (l,99126)
99126       FORMAT (39X,'C O M P L E X   D I S P L A C E M E N T   V E C T O R')
         ELSEIF ( local==20 ) THEN
            WRITE (l,99127)
99127       FORMAT (43X,'C O M P L E X   V E L O C I T Y   V E C T O R')
         ELSEIF ( local==21 ) THEN
            WRITE (l,99128)
99128       FORMAT (39X,'C O M P L E X   A C C E L E R A T I O N   V E C T O R')
         ELSEIF ( local==22 ) THEN
            WRITE (l,99129)
99129       FORMAT (25X,'C O M P L E X   F O R C E S   O F   S I N G L E   ','P O I N T   C O N S T R A I N T')
         ELSEIF ( local==23 ) THEN
            WRITE (l,99130)
99130       FORMAT (47X,'C O M P L E X   L O A D   V E C T O R')
         ELSEIF ( local==24 ) THEN
            WRITE (l,99131) id(5)
99131       FORMAT (39X,'C O M P L E X   E I G E N V E C T O R   NO.',I11)
         ELSEIF ( local==25 ) THEN
            WRITE (l,99132)
99132       FORMAT (58X,'(REAL/IMAGINARY)')
         ELSEIF ( local==26 ) THEN
            WRITE (l,99133)
99133       FORMAT (57X,'(MAGNITUDE/PHASE)')
         ELSEIF ( local==27 ) THEN
            WRITE (l,99134)
99134       FORMAT (27X,'C O M P L E X   S T R E S S E S   I N   B A R   E L',' E M E N T S   ( C B A R )')
         ELSEIF ( local==28 ) THEN
            WRITE (l,99135)
99135       FORMAT (23X,'C O M P L E X   S T R E S S E S   I N   S C A L A R','   S P R I N G S   ( C E L A S 1 )')
         ELSEIF ( local==29 ) THEN
            WRITE (l,99136)
99136       FORMAT (23X,'C O M P L E X   S T R E S S E S   I N   S C A L A R','   S P R I N G S   ( C E L A S 2 )')
         ELSEIF ( local==30 ) THEN
            WRITE (l,99137)
99137       FORMAT (23X,'C O M P L E X   S T R E S S E S   I N   S C A L A R','   S P R I N G S   ( C E L A S 3 )')
         ELSEIF ( local==31 ) THEN
            WRITE (l,99138)
99138       FORMAT (25X,'C O M P L E X   S T R E S S E S   I N   R O D   E L',' E M E N T S   ( C O N R O D )')
         ELSEIF ( local==32 ) THEN
            WRITE (l,99139)
99139       FORMAT (14X,'C O M P L E X   S T R E S S E S   I N   Q U A D R I',' L A T E R A L   M E M B R A N E S   ( C Q D M E M )'&
                  & )
         ELSEIF ( local==33 ) THEN
            WRITE (l,99140)
99140       FORMAT (16X,'C O M P L E X   S T R E S S E S   I N   B E N D I N',' G   Q U A D R I L A T E R A L S   ( C Q D P L T )')
         ELSEIF ( local==34 ) THEN
            WRITE (l,99141)
99141       FORMAT (6X,'C O M P L E X   S T R E S S E S   I N   G E N E R A L','   Q U A D R I L I A T E R A L   E L E M E N T S   '&
                  & ,'( C Q U A D 1)')
         ELSEIF ( local==35 ) THEN
            WRITE (l,99142)
99142       FORMAT (6X,'C O M P L E X   S T R E S S E S   I N   G E N E R A L','   Q U A D R I L I A T E R A L   E L E M E N T S   '&
                  & ,'( C Q U A D 2 )')
         ELSEIF ( local==36 ) THEN
            WRITE (l,99143)
99143       FORMAT (27X,'C O M P L E X   S T R E S S E S   I N   R O D   E L',' E M E N T S   ( C R O D )')
         ELSEIF ( local==37 ) THEN
            WRITE (l,99144)
99144       FORMAT (25X,'C O M P L E X   S T R E S S E S   I N   S H E A R  ',' P A N E L S   ( C S H E A R )')
         ELSEIF ( local==38 ) THEN
            WRITE (l,99145)
99145       FORMAT (14X,'C O M P L E X   S T R E S S E S   I N   B A S I C  ',' B E N D I N G   T R I A N G L E S   ( C T R B S C )'&
                  & )
         ELSEIF ( local==39 ) THEN
            WRITE (l,99146)
99146       FORMAT (10X,'C O M P L E X   S T R E S S E S   I N   G E N E R A',' L   T R I A N G U L A R   E L E M E N T S   ',      &
                   &'( C T R I A 1 )')
         ELSEIF ( local==40 ) THEN
            WRITE (l,99147)
99147       FORMAT (11X,'C O M P L E X   S T R E S S E S   I N   G E N E R A',' L  T R I A N G U L A R   E L E M E N T S   ',       &
                   &'( C T R I A 2 )')
         ELSEIF ( local==41 ) THEN
         ELSEIF ( local==42 ) THEN
            WRITE (l,99148)
99148       FORMAT (17X,'C O M P L E X   S T R E S S E S   I N   T R I A N G',' U L A R   M E M B R A N E S   ( C T R M E M )')
         ELSEIF ( local==43 ) THEN
            WRITE (l,99149)
99149       FORMAT (20X,'C O M P L E X   S T R E S S E S   I N   B E N D I N',' G   T R I A N G L E S   ( C T R P L T )')
         ELSEIF ( local==44 ) THEN
            WRITE (l,99150)
99150       FORMAT (26X,'C O M P L E X   S T R E S S E S   I N   R O D   ','E L E M E N T S   ( C T U B E )')
         ELSEIF ( local==45 ) THEN
            WRITE (l,99151)
99151       FORMAT (25X,'C O M P L E X   S T R E S S E S   I N   T W I S T  ',' P A N E L S   ( C T W I S T )')
         ELSEIF ( local==46 ) THEN
            WRITE (l,99152)
99152       FORMAT (29X,'C O M P L E X   F O R C E S   I N   B A R   E L E M',' E N T S   ( C B A R )')
         ELSEIF ( local==47 ) THEN
         ELSEIF ( local==48 ) THEN
            WRITE (l,99153)
99153       FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ','S P R I N G S   ( C E L A S 1 )')
         ELSEIF ( local==49 ) THEN
            WRITE (l,99154)
99154       FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ','S P R I N G S   ( C E L A S 2 )')
         ELSEIF ( local==50 ) THEN
            WRITE (l,99155)
99155       FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ','S P R I N G S   ( C E L A S 3 )')
         ELSEIF ( local==51 ) THEN
            WRITE (l,99156)
99156       FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ','S P R I N G S   ( C E L A S 4 )')
         ELSEIF ( local==52 ) THEN
            WRITE (l,99157)
99157       FORMAT (27X,'C O M P L E X   F O R C E S   I N   R O D   E L E M',' E N T S   ( C O N R O D )')
         ELSEIF ( local==53 ) THEN
            WRITE (l,99158)
99158       FORMAT (17X,'C O M P L E X   F O R C E S   I N   B E N D I N G  ',' Q U A D R I L A T E R A L S   ( C Q D P L T )')
         ELSEIF ( local==54 ) THEN
            WRITE (l,99159)
99159       FORMAT (9X,'C O M P L E X   F O R C E S   I N   G E N E R A L   ','Q U A D R I L A T E R A L   E L E M E N T S   ',     &
                   &'( C Q U A D 1 )')
         ELSEIF ( local==55 ) THEN
            WRITE (l,99160)
99160       FORMAT (9X,'C O M P L E X   F O R C E S   I N   G E N E R A L   ','Q U A D R I L A T E R A L   E L E M E N T S   ',     &
                   &'( C Q U A D 2 )')
         ELSEIF ( local==56 ) THEN
            WRITE (l,99161)
99161       FORMAT (29X,'C O M P L E X   F O R C E S   I N   R O D   E L E M',' E N T S   ( C R O D )')
         ELSEIF ( local==57 ) THEN
            WRITE (l,99162)
99162       FORMAT (7X,'C O M P L E X   F O R C E S   A C T I N G   O N   ',                                                        &
                   &'S H E A R   P A N E L   E L E M E N T S   (C S H E A R)')
         ELSEIF ( local==58 ) THEN
            WRITE (l,99163)
99163       FORMAT (16X,'C O M P L E X   F O R C E S   I N   B A S I C   B E',' N D I N G   T R I A N G L E S   ( C T R B S C )')
         ELSEIF ( local==59 ) THEN
            WRITE (l,99164)
99164       FORMAT (12X,'C O M P L E X   F O R C E S   I N   G E N E R A L  ',                                                      &
                   &' T R I A N G U L A R   E L E M E N T S   ( C T R I A 1 )')
         ELSEIF ( local==60 ) THEN
            WRITE (l,99165)
99165       FORMAT (12X,'C O M P L E X   F O R C E S   I N   G E N E R A L  ',                                                      &
                   &' T R I A N G U L A R   E L E M E N T S   ( C T R I A 2 )')
         ELSEIF ( local==61 ) THEN
            WRITE (l,99166)
99166       FORMAT (22X,'C O M P L E X   F O R C E S   I N   B E N D I N G ','  T R I A N G L E S   ( C T R P L T )')
         ELSEIF ( local==62 ) THEN
            WRITE (l,99167)
99167       FORMAT (28X,'C O M P L E X   F O R C E S   I N   R O D   E L E M',' E N T S   ( C T U B E )')
         ELSEIF ( local==63 ) THEN
            WRITE (l,99168)
99168       FORMAT (27X,'C O M P L E X   F O R C E S   I N   T W I S T   P A',' N E L S   ( C T W I S T )')
         ELSEIF ( local==64 ) THEN
            WRITE (l,99169)
99169       FORMAT (12X,7HELEMENT,20X,4(8HLOCATION,7X),6X,7HAVERAGE,/14X,3HID.,26X,1H1,14X,1H2,14X,1H3,14X,1H4,13X,12HAXIAL STRESS)
         ELSEIF ( local==65 ) THEN
            WRITE (l,99170)
99170       FORMAT (17X,7HELEMENT,29X,5HAXIAL,39X,9HTORSIONAL,/19X,3HID.,30X,6HSTRESS,41X,6HSTRESS)
         ELSEIF ( local==66 ) THEN
            WRITE (l,99171)
99171       FORMAT (17X,7HELEMENT,28X,7HMAXIMUM,39X,7HAVERAGE,/19X,3HID.,31X,5HSHEAR,41X,5HSHEAR)
         ELSEIF ( local==67 ) THEN
            WRITE (l,99172)
99172       FORMAT (17X,7HELEMENT,29X,5HAXIAL,41X,6HTORQUE,/19X,3HID.,31X,5HFORCE)
         ELSEIF ( local==68 ) THEN
            WRITE (l,99173)
99173       FORMAT (9H  ELEMENT,7X,5HFIBRE,37X,'- STRESSES IN ELEMENT COORDI','NATE SYSTEM -',/4X,3HID.,8X,8HDISTANCE,18X,          &
                  & 8HNORMAL-X,26X,8HNORMAL-Y,25X,8HSHEAR-XY)
         ELSEIF ( local==69 ) THEN
            WRITE (l,99174)
99174       FORMAT (13X,7HELEMENT,33X,'- STRESSES IN ELEMENT COORDINATE SYST','EM -',/15X,3HID.,18X,8HNORMAL-X,26X,8HNORMAL-Y,26X,  &
                   &8HSHEAR-XY)
         ELSEIF ( local==70 ) THEN
            WRITE (l,99175)
99175       FORMAT (2(16X,7HELEMENT,35X),/2(18X,3HID.,20X,5HFORCE,12X))
         ELSEIF ( local==71 ) THEN
            WRITE (l,99176)
99176       FORMAT (2(16X,7HELEMENT,35X),/2(18X,3HID.,19X,6HSTRESS,12X))
         ELSEIF ( local==72 ) THEN
            WRITE (l,99177)
99177       FORMAT (17X,7HELEMENT,29X,5HFORCE,42X,5HFORCE)
         ELSEIF ( local==73 ) THEN
            WRITE (l,99178)
99178       FORMAT (19X,3HID.,30X,7HPTS 1,3,40X,7HPTS 2,4)
         ELSEIF ( local==74 ) THEN
            WRITE (l,99179)
99179       FORMAT (17X,7HELEMENT,28X,6HMOMENT,41X,6HMOMENT)
         ELSE
            WRITE (l,99001)
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         WRITE (l,99180)
99180    FORMAT (37X,1H*,62X,1H*,/37X,1H*,8X,43H(THIS MESSAGE CAN BE SUPPRESSED BY DIAG 37),11X,1H*,/37X,16(4H****),/)
         linet = linet + 10
         Id22 = -999
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         IF ( M>0 ) WRITE (l,99181) (rt(k,M),k=1,8) , sectn(1)
99181    FORMAT (/1H0,39X,3H(* ,8A4,/41X,'SEE NASTRAN U.M. VOL II, ','SECTION 2',A4,1H))
         Id22 = id(22)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         IF ( M>=3 ) nogo = 14
         IF ( M==1 ) M = 6
         IF ( M==2 ) M = 11
         IF ( M==3 ) M = 8
         IF ( M==4 ) M = 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!
99001 FORMAT (45X,37HD I S P L A C E M E N T   V E C T O R)
99182 FORMAT (/37X,16(4H****),/37X,1H*,62X,1H*,/37X,1H*)
!
END SUBROUTINE ofp1a
