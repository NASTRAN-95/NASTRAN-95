!*==ofp1b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofp1b(Line)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Line
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(50) :: fd
   INTEGER , DIMENSION(50) :: id
   INTEGER :: idd , idx , iharm , jharm , k , local
   INTEGER , SAVE :: idum1 , idum2 , idum3 , idum4 , idum5 , idum6 , idum7 , idum8 , idum9
   REAL , DIMENSION(6) :: of
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE WAS FORMED ONLY TO REDUCE THE SIZE OF OFP1 FOR
!     COMPILATION PURPOSES.  IT IS CALLED ONLY BY OFP1.
!     PREVIOUSLY THIS ROUTINE WAS NAMED OPF1A.
!
   !>>>>EQUIVALENCE (Core(1),Of(1)) , (Id(1),Fd(1),Of(6))
   DATA idum1 , idum2 , idum3 , idum4 , idum5 , idum6/4HDUM1 , 4HDUM2 , 4HDUM3 , 4HDUM4 , 4HDUM5 , 4HDUM6/ , idum7 , idum8 ,        &
       &idum9/4HDUM7 , 4HDUM8 , 4HDUM9/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Line<=294 ) THEN
            local = Line - 174
            IF ( local==1 ) THEN
            ELSEIF ( local==2 ) THEN
               WRITE (L,99001)
!
!     ******************************************************************
!
99001          FORMAT (2(25X,5HAXIAL,30X),/2(7X,4HTIME,14X,5HFORCE,9X,6HTORQUE,15X))
            ELSEIF ( local==3 ) THEN
               WRITE (L,99002)
99002          FORMAT (21X,17HBEND-MOMENT-END-A,12X,17HBEND-MOMENT-END-B,18X,5HSHEAR,17X,/7X,4HTIME,3(8X,7HPLANE 1,7X,7HPLANE 2),9X,&
                      &5HFORCE,10X,6HTORQUE)
            ELSEIF ( local==4 ) THEN
               WRITE (L,99003)
99003          FORMAT (2(25X,5HFORCE,10X,5HFORCE,15X),/2(7X,4HTIME,13X,7HPTS 1,3,8X,7HPTS 2,4,14X))
            ELSEIF ( local==5 ) THEN
               WRITE (L,99004)
99004          FORMAT (2(24X,6HMOMENT,9X,6HMOMENT,15X),/2(7X,4HTIME,13X,7HPTS 1,3,8X,7HPTS 2,4,14X))
            ELSEIF ( local==6 ) THEN
               WRITE (L,99005)
99005          FORMAT (8X,4HTIME,3X,2(11X,11HBEND-MOMENT),11X,12HTWIST-MOMENT,13X,5HSHEAR,17X,5HSHEAR,/31X,1HX,21X,1HY,43X,1HX,21X, &
                      &1HY)
            ELSEIF ( local==7 ) THEN
               WRITE (L,99006)
99006          FORMAT (4(8X,4HTIME,10X,5HFORCE,6X))
            ELSEIF ( local==8 ) THEN
               WRITE (L,99007)
99007          FORMAT (2(21X,5HAXIAL,7X,6HSAFETY,6X,9HTORSIONAL,5X,6HSAFETY),/2(7X,4HTIME,9X,6HSTRESS,7X,6HMARGIN,8X,6HSTRESS,6X,   &
                      &6HMARGIN))
            ELSEIF ( local==9 ) THEN
               WRITE (L,99008)
99008          FORMAT (7X,4HTIME,12X,3HSA1,12X,3HSA2,12X,3HSA3,10X,12HAXIAL-STRESS,8X,6HSA-MAX,9X,6HSA-MIN,11X,6HM.S.-T,/23X,3HSB1, &
                     & 12X,3HSB2,12X,3HSB3,30X,6HSB-MAX,9X,6HSB-MIN,11X,6HM.S.-C)
            ELSEIF ( local==10 ) THEN
               WRITE (L,99009)
99009          FORMAT (2(26X,7HMAXIMUM,8X,7HAVERAGE,6X,6HSAFETY),/2(8X,4HTIME,15X,5HSHEAR,10X,5HSHEAR,7X,6HMARGIN))
            ELSEIF ( local==11 ) THEN
               WRITE (L,99010)
99010          FORMAT (2(54X,6HSAFETY),/2(7X,4HTIME,15X,7HMAXIMUM,8X,7HAVERAGE,6X,6HMARGIN))
            ELSEIF ( local==12 ) THEN
               WRITE (L,99011)
99011          FORMAT (19X,5HFIBRE,11X,32HSTRESSES IN ELEMENT COORD SYSTEM,13X,31HPRINCIPAL STRESSES (ZERO SHEAR),10X,7HMAXIMUM,/7X,&
                      &4HTIME,7X,8HDISTANCE,7X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X,5HSHEAR)
            ELSEIF ( local==13 ) THEN
               WRITE (L,99012)
99012          FORMAT (20X,32HSTRESSES IN ELEMENT COORD SYSTEM,12X,9HPRINCIPAL,11X,18HPRINCIPAL STRESSES,10X,7HMAXIMUM,/7X,4HTIME,  &
                     & 8X,8HNORMAL-X,6X,8HNORMAL-Y,7X,8HSHEAR-XY,6X,12HSTRESS ANGLE,9X,5HMAJOR,10X,5HMINOR,10X,5HSHEAR)
            ELSEIF ( local==14 ) THEN
               WRITE (L,99013)
99013          FORMAT (4(8X,4HTIME,9X,6HSTRESS,6X))
            ELSEIF ( local==15 ) THEN
               WRITE (L,99014)
99014          FORMAT (5X,4HTIME,15X,3HSA1,12X,3HSA2,12X,3HSA3,12X,3HSA4,8X,12HAXIAL-STRESS,6X,6HSA-MAX,9X,6HSA-MIN,5X,6HM.S.-T,    &
                     & /24X,3HSB1,12X,3HSB2,12X,3HSB3,12X,3HSB4,26X,6HSB-MAX,9X,6HSB-MIN,5X,6HM.S.-C)
            ELSEIF ( local==16 ) THEN
               WRITE (L,99015)
99015          FORMAT (53X,5HAXIAL,/13X,9HFREQUENCY,31X,5HFORCE,41X,6HTORQUE)
            ELSEIF ( local==17 ) THEN
               WRITE (L,99016)
99016          FORMAT (11X,2(42X,5HFORCE),/13X,9HFREQUENCY,30X,7HPTS 1,3,40X,7HPTS 2,4)
            ELSEIF ( local==18 ) THEN
               WRITE (L,99017)
99017          FORMAT (11X,2(41X,6HMOMENT),/13X,9HFREQUENCY,30X,7HPTS 1,3,40X,7HPTS 2,4)
            ELSEIF ( local==19 ) THEN
               WRITE (L,99018)
99018          FORMAT (5X,9HFREQUENCY,2X,2(11X,11HBEND-MOMENT),10X,12HTWIST-MOMENT,2(13X,5HSHEAR,4X),/2(31X,1HX,21X,1HY,12X))
            ELSEIF ( local==20 ) THEN
               WRITE (L,99019)
99019          FORMAT (2(12X,9HFREQUENCY,20X,5HFORCE,12X))
            ELSEIF ( local==21 ) THEN
               WRITE (L,99020)
99020          FORMAT (53X,5HAXIAL,39X,9HTORSIONAL,/13X,9HFREQUENCY,2(30X,6HSTRESS,11X))
            ELSEIF ( local==22 ) THEN
               WRITE (L,99021)
99021          FORMAT (52X,7HMAXIMUM,39X,7HAVERAGE,/13X,9HFREQUENCY,2(31X,5HSHEAR,10X))
            ELSEIF ( local==23 ) THEN
               WRITE (L,99022)
99022          FORMAT (20X,5HFIBRE,37X,'- STRESSES IN ELEMENT COORDINATE SYSTEM',2H -,/4X,'FREQUENCY,6X,8HDISTANCE',18X,8HNORMAL-X, &
                     & 26X,8HNORMAL-Y,25X,8HSHEAR-XY)
            ELSEIF ( local==24 ) THEN
               WRITE (L,99023)
99023          FORMAT (53X,41H- STRESSES IN ELEMENT COORDINATE SYSTEM -,/9X,9HFREQUENCY,18X,8HNORMAL-X,26X,8HNORMAL-Y,26X,          &
                     & 8HSHEAR-XY)
            ELSEIF ( local==25 ) THEN
               WRITE (L,99024)
99024          FORMAT (2(12X,9HFREQUENCY,19X,6HSTRESS,12X))
            ELSEIF ( local==26 ) THEN
               WRITE (L,99025)
99025          FORMAT (39X,4(8HLOCATION,7X),6X,7HAVERAGE,/8X,9HFREQUENCY,26X,1H1,14X,1H2,14X,1H3,14X,1H4,13X,12HAXIAL STRESS)
            ELSEIF ( local==27 ) THEN
               WRITE (L,99026)
99026          FORMAT (21X,17HBEND-MOMENT-END-A,12X,17HBEND-MOMENT-END-B,18X,5HSHEAR,17X,/4X,9HFREQUENCY,                           &
                      &3(6X,7HPLANE 1,7X,9HPLANE 2  ),6X,5HFORCE,10X,6HTORQUE)
            ELSEIF ( local==28 ) THEN
               WRITE (L,99027) id(3)
99027          FORMAT (27X,'O U T P U T   F R O M   G R I D   P O I N T   W E I',' G H T   G E N E R A T O R',/1H0,53X,             &
                      &17HREFERENCE POINT =,I9)
            ELSEIF ( local==29 ) THEN
               WRITE (L,99028)
99028          FORMAT (5X,9HSECTOR-ID,/6X,8HPOINT-ID,/7X,7HRING-ID,2X,8HHARMONIC,8X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,   &
                      &2HR3)
            ELSEIF ( local==30 ) THEN
               WRITE (L,99029)
99029          FORMAT (11X,'S T R E S S E S   I N   A X I S - S Y M M E T R I C','   C O N I C A L   S H E L L   E L E M E N T S   '&
                     & ,'(CCONEAX)')
            ELSEIF ( local==31 ) THEN
               WRITE (L,99030)
99030          FORMAT (13X,'F O R C E S   I N   A X I S - S Y M M E T R I C   ',                                                    &
                      &'C O N I C A L   S H E L L   E L E M E N T S   (CCONEAX)')
            ELSEIF ( local==32 ) THEN
               WRITE (L,99031)
99031          FORMAT (8H ELEMENT,10X,5HPOINT,5X,5HFIBRE,11X,'STRESSES IN ELEM','ENT COORD SYSTEM',8X,                              &
                      &'PRINCIPAL STRESSES (ZERO SHEAR)',8X,7HMAXIMUM,/3X,'ID.  HARMONIC  ANGLE    DISTANCE',7X,8HNORMAL-V,6X,      &
                      &8HNORMAL-U,6X,8HSHEAR-UV,6X,5HANGLE,7X,5HMAJOR,9X,5HMINOR,9X,5HSHEAR)
            ELSEIF ( local==33 ) THEN
               WRITE (L,99032)
99032          FORMAT (9H  ELEMENT,5X,8HHARMONIC,4X,5HPOINT,4X,2(7X,5HBEND-,6HMOMENT),6X,12HTWIST-MOMENT,2(11X,5HSHEAR,1X),/3X,     &
                     & 3HID.,9X,6HNUMBER,5X,5HANGLE,15X,1HV,17X,1HU,37X,1HV,16X,1HU)
            ELSEIF ( local==34 ) THEN
               WRITE (L,99033)
99033          FORMAT (31X,'C O M P L E X   D I S P L A C E M E N T   ','V E C T O R  (SOLUTION SET)')
            ELSEIF ( local==35 ) THEN
               WRITE (L,99034)
99034          FORMAT (35X,'C O M P L E X   V E L O C I T Y   V E C T O R  ','(SOLUTION SET)')
            ELSEIF ( local==36 ) THEN
               WRITE (L,99035)
99035          FORMAT (31X,'C O M P L E X   A C C E L E R A T I O N   ','V E C T O R   (SOLUTION SET)')
            ELSEIF ( local==37 ) THEN
               WRITE (L,99036)
99036          FORMAT (43X,46HV E L O C I T Y   V E C T O R   (SOLUTION SET))
            ELSEIF ( local==38 ) THEN
               WRITE (L,99037)
99037          FORMAT (39X,'D I S P L A C E M E N T   V E C T O R   ','(SOLUTION SET)')
            ELSEIF ( local==39 ) THEN
               WRITE (L,99038)
99038          FORMAT (39X,'A C C E L E R A T I O N   V E C T O R   ','(SOLUTION SET)')
            ELSEIF ( local==40 ) THEN
               WRITE (L,99039) id(5)
99039          FORMAT (29X,'C O M P L E X   E I G E N V E C T O R   N O .',I11,3X,14H(SOLUTION SET))
            ELSEIF ( local==41 ) THEN
               WRITE (L,99040)
99040          FORMAT (30X,'E I G E N V A L U E   A N A L Y S I S   ','S U M M A R Y   (GIVENS METHOD)')
            ELSEIF ( local==42 ) THEN
               WRITE (L,99041) (id(k),k=11,14) , id(17) , fd(18) , (id(k),k=19,21)
99041          FORMAT (///,36X,45HNUMBER OF EIGENVALUES EXTRACTED . . . . . . .,I10,//36X,                                          &
                      &45HNUMBER OF EIGENVECTORS COMPUTED . . . . . . .,I10,//36X,45HNUMBER OF EIGENVALUE CONVERGENCE FAILURES . ., &
                     & I10,//36X,45HNUMBER OF EIGENVECTOR CONVERGENCE FAILURES. .,I10,///36X,                                       &
                      &45HREASON FOR TERMINATION. . . . . . . . . . . .,I10,1H*,///36X,                                             &
                      &45HLARGEST OFF-DIAGONAL MODAL MASS TERM. . . . .,1P,E10.2,//76X,5H. . .,I10,/46X,                            &
                      &'MODE PAIR. . . . . . . . . . .',/76X,5H. . .,I10,//36X,33HNUMBER OF OFF-DIAG0NAL MODAL MASS,/41X,           &
                      &40HTERMS FAILING CRITERION. . . . . . . . .,I10)
               IF ( id(17)==1 ) WRITE (L,99042)
99042          FORMAT (//36X,22H(* NORMAL TERMINATION))
               IF ( id(17)/=1 ) WRITE (L,99043)
99043          FORMAT (//36X,31H(* INSUFFICIENT TIME REMAINING))
               IF ( id(17)/=1 ) Nogo = 14
            ELSEIF ( local==43 ) THEN
               WRITE (L,99044)
99044          FORMAT (107X,22HOCTAHEDRAL    PRESSURE,/6X,10HELEMENT-ID,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,   &
                      &6HTAU-XZ,8X,6HTAU-XY,8X,5HTAU-0,10X,1HP)
            ELSEIF ( local==44 ) THEN
               WRITE (L,99045)
99045          FORMAT (107X,22HOCTAHEDRAL    PRESSURE,/6X,10H TIME     ,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,   &
                      &6HTAU-XZ,8X,6HTAU-XY,8X,5HTAU-0,10X,1HP)
            ELSEIF ( local==45 ) THEN
               WRITE (L,99046)
99046          FORMAT (18X,10HELEMENT-ID,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,6HTAU-XZ,8X,6HTAU-XY)
            ELSEIF ( local==46 ) THEN
               WRITE (L,99047)
99047          FORMAT (18X,10HFREQUENCY ,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,6HTAU-XZ,8X,6HTAU-XY)
            ELSEIF ( local==47 ) THEN
               WRITE (L,99048)
99048          FORMAT (19X,'S T R E S S E S   I N   S O L I D   T E T R A H E D',' R O N   E L E M E N T S   ( C T E T R A )')
            ELSEIF ( local==48 ) THEN
               WRITE (L,99049)
99049          FORMAT (11X,'C O M P L E X   S T R E S S E S   I N   S O L I D  ',                                                   &
                      &' T E T R A H E D R O N   E L E M E N T S   ( C T E T R A )')
            ELSEIF ( local==49 ) THEN
               WRITE (L,99050)
99050          FORMAT (25X,'S T R E S S E S   I N   S O L I D   W E D G E   ','E L E M E N T S   ( C W E D G E )')
            ELSEIF ( local==50 ) THEN
               WRITE (L,99051)
99051          FORMAT (17X,'C O M P L E X   S T R E S S E S   I N   S O L I D  ',' W E D G E   E L E M E N T S   ( C W E D G E )')
            ELSEIF ( local==51 ) THEN
               WRITE (L,99052)
99052          FORMAT (20X,'S T R E S S E S   I N   S O L I D   H E X A H E D R',' O N   E L E M E N T S   ( C H E X A 1 )')
            ELSEIF ( local==52 ) THEN
               WRITE (L,99053)
99053          FORMAT (12X,'C O M P L E X   S T R E S S E S   I N   S O L I D  ',                                                   &
                      &' H E X A H E D R O N   E L E M E N T S   ( C H E X A 1 )')
            ELSEIF ( local==53 ) THEN
               WRITE (L,99054)
99054          FORMAT (20X,'S T R E S S E S   I N   S O L I D   H E X A H E D R',' O N   E L E M E N T S   ( C H E X A 2 )')
            ELSEIF ( local==54 ) THEN
               WRITE (L,99055)
99055          FORMAT (12X,'C O M P L E X   S T R E S S E S   I N   S O L I D  ',                                                   &
                      &' H E X A H E D R O N   E L E M E N T S   ( C H E X A 2 )')
            ELSEIF ( local==55 ) THEN
               idd = mod(id(5),500000)
               jharm = (id(5)-idd)/500000
               iharm = (jharm-1)/2
               IF ( mod(jharm,2)==1 ) THEN
                  WRITE (L,99056) idd , iharm
99056             FORMAT (6X,10HPOINT-ID =,I7,4X,10HHARMONIC =,I4,1H*)
               ELSE
                  WRITE (L,99057) idd , iharm
99057             FORMAT (6X,10HPOINT-ID =,I7,4X,10HHARMONIC =,I4)
               ENDIF
            ELSEIF ( local==56 ) THEN
               WRITE (L,99058)
99058          FORMAT (5X,8HHARMONIC,5(3X,8HPOINT-ID,5X,2HT1,5X))
            ELSEIF ( local==57 ) THEN
               WRITE (L,99059)
99059          FORMAT (10X,'V E L O C I T I E S   I N   A X I S Y M M E T R I C','   F L U I D   E L E M E N T S   ( C A X I F 2 - '&
                     & ,'S T R E S S )')
            ELSEIF ( local==58 ) THEN
               WRITE (L,99060)
99060          FORMAT (10X,'V E L O C I T I E S   I N   A X I S Y M M E T R I C','   F L U I D   E L E M E N T S   ( C A X I F 3 - '&
                     & ,'S T R E S S )')
            ELSEIF ( local==59 ) THEN
               WRITE (L,99061)
99061          FORMAT (10X,'V E L O C I T I E S   I N   A X I S Y M M E T R I C','   F L U I D   E L E M E N T S   ( C A X I F 4 - '&
                     & ,'S T R E S S )')
            ELSEIF ( local==60 ) THEN
               WRITE (L,99062)
99062          FORMAT (24X,'V E L O C I T I E S   I N   S L O T   E L E M E N T',' S   ( C S L O T 3 - S T R E S S )')
            ELSEIF ( local==61 ) THEN
               WRITE (L,99063)
99063          FORMAT (24X,'V E L O C I T I E S   I N   S L O T   E L E M E N T',' S   ( C S L O T 4 - S T R E S S )')
            ELSEIF ( local==62 ) THEN
               WRITE (L,99064)
99064          FORMAT (2X,'C O M P L E X   V E L O C I T I E S   I N   A X I S ','Y M M E T R I C   F L U I D   E L E M E N T S   ',&
                      &'( C A X I F 2 - S T R E S S )')
            ELSEIF ( local==63 ) THEN
               WRITE (L,99065)
99065          FORMAT (2X,'C O M P L E X   V E L O C I T I E S   I N   A X I S ','Y M M E T R I C   F L U I D   E L E M E N T S   ',&
                      &'( C A X I F 3 - S T R E S S )')
            ELSEIF ( local==64 ) THEN
               WRITE (L,99066)
99066          FORMAT (2X,'C O M P L E X   V E L O C I T I E S   I N   A X I S ','Y M M E T R I C   F L U I D   E L E M E N T S   ',&
                      &'( C A X I F 4 - S T R E S S )')
            ELSEIF ( local==65 ) THEN
               WRITE (L,99067)
99067          FORMAT (15X,'C O M P L E X   V E L O C I T I E S   I N   S L O T','  E L E M E N T S   ( C S L O T 3 - S T R E S S )'&
                     & )
            ELSEIF ( local==66 ) THEN
               WRITE (L,99068)
99068          FORMAT (15X,'C O M P L E X   V E L O C I T I E S   I N   S L O T','  E L E M E N T S   ( C S L O T 4 - S T R E S S )'&
                     & )
            ELSEIF ( local==67 ) THEN
               WRITE (L,99069)
99069          FORMAT (8X,7HELEMENT,17X,6HCENTER,25X,7HEDGE  1,19X,7HEDGE  2,19X,7HEDGE  3,/10X,3HID.,8X,                           &
                      &27HR --------- PHI --------- Z,12X,15HS --------- PHI,11X,15HS --------- PHI,11X,15HS --------- PHI)
            ELSEIF ( local==68 ) THEN
               WRITE (L,99070)
99070          FORMAT (31X,6HCENTER,25X,7HEDGE  1,19X,7HEDGE  2,19X,7HEDGE  3,/2X,8H   TIME ,10X,27HR --------- PHI --------- Z,12X,&
                      &15HS --------- PHI,11X,15HS --------- PHI,11X,15HS --------- PHI)
            ELSEIF ( local==69 ) THEN
               WRITE (L,99071)
99071          FORMAT (32X,6HCENTER,25X,7HEDGE  1,19X,7HEDGE  2,19X,7HEDGE  3,/4X,9HFREQUENCY,8X,27HR --------- PHI --------- Z,12X,&
                      &15HS --------- PHI,11X,15HS --------- PHI,11X,15HS --------- PHI)
            ELSEIF ( local==70 ) THEN
               WRITE (L,99072)
99072          FORMAT (13X,7HELEMENT,18X,6HCENTER,20X,7HEDGE  1,11X,7HEDGE  2,11X,7HEDGE  3,11X,7HEDGE  4,/15X,3HID.,13X,           &
                      &19HR --------------- Z,17X,1HS,17X,1HS,17X,1HS,17X,1HS)
            ELSEIF ( local==71 ) THEN
               WRITE (L,99073)
99073          FORMAT (38X,6HCENTER,20X,7HEDGE  1,11X,7HEDGE  2,11X,7HEDGE  3,11X,7HEDGE  4,/11X,4HTIME,16X,19HR --------------- Z, &
                     & 17X,1HS,17X,1HS,17X,1HS,17X,1HS)
            ELSEIF ( local==72 ) THEN
               WRITE (L,99074)
99074          FORMAT (38X,6HCENTER,20X,7HEDGE  1,11X,7HEDGE  2,11X,7HEDGE  3,11X,7HEDGE  4,/9X,9HFREQUENCY,13X,                    &
                      &19HR --------------- Z,17X,1HS,17X,1HS,17X,1HS,17X,1HS)
            ELSEIF ( local==73 ) THEN
               WRITE (L,99075)
99075          FORMAT (9X,7HELEMENT,24X,6HCENTER,26X,7HEDGE  1,15X,7HEDGE  2,15X,7HEDGE  3,/11X,3HID.,17X,                          &
                     & 23HR ------------------- Z,21X,1HS,21X,1HS,21X,1HS)
            ELSEIF ( local==74 ) THEN
               WRITE (L,99076)
99076          FORMAT (40X,6HCENTER,26X,7HEDGE  1,15X,7HEDGE  2,15X,7HEDGE  3,/7X,4HTIME,20X,23HR ------------------- Z,21X,1HS,21X,&
                      &1HS,21X,1HS)
            ELSEIF ( local==75 ) THEN
               WRITE (L,99077)
99077          FORMAT (40X,6HCENTER,26X,7HEDGE  1,15X,7HEDGE  2,15X,7HEDGE  3,/5X,9HFREQUENCY,17X,23HR ------------------- Z,21X,   &
                     & 1HS,21X,1HS,21X,1HS)
            ELSEIF ( local==76 ) THEN
               WRITE (L,99078)
99078          FORMAT (14X,7HELEMENT,30X,6HCENTER,47X,4HEDGE,/16X,3HID.,21X,27HR ----------------------- Z,25X,                     &
                      &28HS ---------------------- PHI)
            ELSEIF ( local==77 ) THEN
               WRITE (L,99079)
99079          FORMAT (51X,6HCENTER,47X,4HEDGE,/12X,4HTIME,24X,27HR ----------------------- Z,25X,28HS ---------------------- PHI)
            ELSEIF ( local==78 ) THEN
               WRITE (L,99080)
99080          FORMAT (51X,6HCENTER,47X,4HEDGE,/10X,9HFREQUENCY,21X,27HR ----------------------- Z,25X,                             &
                      &28HS ---------------------- PHI)
            ELSEIF ( local==79 ) THEN
               WRITE (L,99081)
99081          FORMAT (46X,35HT E M P E R A T U R E   V E C T O R)
            ELSEIF ( local==80 ) THEN
               idx = idum1
               WRITE (L,99082) idx
            ELSEIF ( local==81 ) THEN
               idx = idum2
               WRITE (L,99082) idx
            ELSEIF ( local==82 ) THEN
               idx = idum3
               WRITE (L,99082) idx
            ELSEIF ( local==83 ) THEN
               idx = idum4
               WRITE (L,99082) idx
            ELSEIF ( local==84 ) THEN
               idx = idum5
               WRITE (L,99082) idx
            ELSEIF ( local==85 ) THEN
               idx = idum1
               WRITE (L,99083) idx
            ELSEIF ( local==86 ) THEN
               idx = idum2
               WRITE (L,99083) idx
            ELSEIF ( local==87 ) THEN
               idx = idum3
               WRITE (L,99083) idx
            ELSEIF ( local==88 ) THEN
               idx = idum4
               WRITE (L,99083) idx
            ELSEIF ( local==89 ) THEN
               idx = idum5
               WRITE (L,99083) idx
            ELSEIF ( local==90 ) THEN
               WRITE (L,99084)
99084          FORMAT (5X,9H    EL-ID,6X,2HS1,11X,2HS2,11X,2HS3,11X,2HS4,11X,2HS5,11X,2HS6,11X,2HS7,11X,2HS8,11X,2HS9)
            ELSEIF ( local==91 ) THEN
               WRITE (L,99085)
99085          FORMAT (5X,9H    EL-ID,6X,2HF1,11X,2HF2,11X,2HF3,11X,2HF4,11X,2HF5,11X,2HF6,11X,2HF7,11X,2HF8,11X,2HF9)
            ELSEIF ( local==92 ) THEN
               idx = idum1
               WRITE (L,99086) idx
            ELSEIF ( local==93 ) THEN
               idx = idum2
               WRITE (L,99086) idx
            ELSEIF ( local==94 ) THEN
               idx = idum3
               WRITE (L,99086) idx
            ELSEIF ( local==95 ) THEN
               idx = idum4
               WRITE (L,99086) idx
            ELSEIF ( local==96 ) THEN
               idx = idum5
               WRITE (L,99086) idx
            ELSEIF ( local==97 ) THEN
               idx = idum1
               WRITE (L,99087) idx
            ELSEIF ( local==98 ) THEN
               idx = idum2
               WRITE (L,99087) idx
            ELSEIF ( local==99 ) THEN
               idx = idum3
               WRITE (L,99087) idx
            ELSEIF ( local==100 ) THEN
               idx = idum4
               WRITE (L,99087) idx
            ELSEIF ( local==101 ) THEN
               idx = idum5
               WRITE (L,99087) idx
            ELSEIF ( local==102 ) THEN
               WRITE (L,99088)
99088          FORMAT (5X,9H     TIME,6X,2HS1,11X,2HS2,11X,2HS3,11X,2HS4,11X,2HS5,11X,2HS6,11X,2HS7,11X,2HS8,11X,2HS9)
            ELSEIF ( local==103 ) THEN
               WRITE (L,99089)
99089          FORMAT (5X,9H     TIME,6X,2HF1,11X,2HF2,11X,2HF3,11X,2HF4,11X,2HF5,11X,2HF6,11X,2HF7,11X,2HF8,11X,2HF9)
            ELSEIF ( local==104 ) THEN
               WRITE (L,99090)
99090          FORMAT (5X,9HFREQUENCY,6X,2HS1,11X,2HS2,11X,2HS3,11X,2HS4,11X,2HS5,11X,2HS6,11X,2HS7,11X,2HS8,11X,2HS9)
            ELSEIF ( local==105 ) THEN
               WRITE (L,99091)
99091          FORMAT (5X,9HFREQUENCY,6X,2HF1,11X,2HF2,11X,2HF3,11X,2HF4,11X,2HF5,11X,2HF6,11X,2HF7,11X,2HF8,11X,2HF9)
            ELSEIF ( local==106 ) THEN
               idx = idum6
               WRITE (L,99082) idx
            ELSEIF ( local==107 ) THEN
               idx = idum7
               WRITE (L,99082) idx
            ELSEIF ( local==108 ) THEN
               idx = idum8
               WRITE (L,99082) idx
            ELSEIF ( local==109 ) THEN
               idx = idum9
               WRITE (L,99082) idx
            ELSEIF ( local==110 ) THEN
               idx = idum6
               WRITE (L,99083) idx
            ELSEIF ( local==111 ) THEN
               idx = idum7
               WRITE (L,99083) idx
            ELSEIF ( local==112 ) THEN
               idx = idum8
               WRITE (L,99083) idx
            ELSEIF ( local==113 ) THEN
               idx = idum9
               WRITE (L,99083) idx
            ELSEIF ( local==114 ) THEN
               idx = idum6
               WRITE (L,99086) idx
            ELSEIF ( local==115 ) THEN
               idx = idum7
               WRITE (L,99086) idx
            ELSEIF ( local==116 ) THEN
               idx = idum8
               WRITE (L,99086) idx
            ELSEIF ( local==117 ) THEN
               idx = idum9
               WRITE (L,99086) idx
            ELSEIF ( local==118 ) THEN
               idx = idum6
               WRITE (L,99087) idx
            ELSEIF ( local==119 ) THEN
               idx = idum7
               WRITE (L,99087) idx
            ELSEIF ( local==120 ) THEN
               idx = idum8
               WRITE (L,99087) idx
            ELSE
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Line>380 ) RETURN
         local = Line - 294
         IF ( local==1 ) THEN
            idx = idum9
            WRITE (L,99087) idx
         ELSEIF ( local==2 ) THEN
            WRITE (L,99092)
99092       FORMAT (6X,'POINT ID.   TYPE',6X,'ID   VALUE     ID+1 VALUE    ',                                                       &
                   &'ID+2 VALUE     ID+3 VALUE     ID+4 VALUE     ID+5 VALUE')
         ELSEIF ( local==3 ) THEN
            WRITE (L,99093)
99093       FORMAT (19X,'F I N I T E   E L E M E N T   T E M P E R A T U R E','   G R A D I E N T S   A N D   F L U X E S')
         ELSEIF ( local==4 ) THEN
            WRITE (L,99094)
99094       FORMAT (4X,'ELEMENT-ID   EL-TYPE        X-GRADIENT       Y-','GRADIENT       Z-GRADIENT        X-FLUX           Y-FLUX',&
                   &'           Z-FLUX')
         ELSEIF ( local==5 ) THEN
            WRITE (L,99095)
99095       FORMAT (4X,'TIME         EL-TYPE        X-GRADIENT       Y-','GRADIENT       Z-GRADIENT        X-FLUX           Y-FLUX',&
                   &'           Z-FLUX')
         ELSEIF ( local==6 ) THEN
            WRITE (L,99096)
99096       FORMAT (26X,'ELEMENT-ID      APPLIED-LOAD       CONVECTION      ',' RADIATION           TOTAL')
         ELSEIF ( local==7 ) THEN
            WRITE (L,99097)
99097       FORMAT (26X,'TIME            APPLIED-LOAD       CONVECTION      ',' RADIATION           TOTAL')
         ELSEIF ( local==8 ) THEN
            WRITE (L,99098)
99098       FORMAT (33X,'H E A T   F L O W   I N T O   H B D Y   E L E M E N',' T S   (CHBDY)')
         ELSEIF ( local==9 ) THEN
            WRITE (L,99099)
99099       FORMAT (6X,16HTIME        TYPE,6X,7H  VALUE)
         ELSEIF ( local==10 ) THEN
            WRITE (L,99100)
99100       FORMAT (21X,'S T R E S S E S   I N   Q U A D R I L A T E R A L','   M E M B R A N E S      ( C Q D M E M 1 )')
         ELSEIF ( local==11 ) THEN
            WRITE (L,99101)
99101       FORMAT (14X,'C O M P L E X   S T R E S S E S   I N   Q U A D R I',                                                      &
                   &' L A T E R A L   M E M B R A N E S   ( C Q D M E M 1 )')
         ELSEIF ( local==12 ) THEN
            WRITE (L,99102)
99102       FORMAT (26X,'S T R E S S E S   A C T I N G   I N   Q D M E M 2  ',' E L E M E N T S   (CQDMEM2)')
         ELSEIF ( local==13 ) THEN
            WRITE (L,99103)
99103       FORMAT (19X,'C O M P L E X   S T R E S S E S   A C T I N G   I N','   Q D M E M 2   E L E M E N T S   (CQDMEM2)')
         ELSEIF ( local==14 ) THEN
            WRITE (L,99104)
99104       FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   Q U A D R I',' L A T E R A L   E L E M E N T S',6X,                &
                   &15H( C Q U A D 4 ))
         ELSEIF ( local==15 ) THEN
            WRITE (L,99105)
99105       FORMAT ('0*** THIS FORMAT 809/OFP1B NOT USED ***')
         ELSEIF ( local==16 ) THEN
            WRITE (L,99106)
!                   ==============================
99106       FORMAT (28X,'F O R C E S   A C T I N G   O N   Q D M E M 2   E L',' E M E N T S   (CQDMEM2)')
         ELSEIF ( local==17 ) THEN
            WRITE (L,99107)
99107       FORMAT (20X,'C O M P L E X   F O R C E S   A C T I N G   O N   ','Q D M E M 2   E L E M E N T S   (CQDMEM2)')
         ELSEIF ( local==18 ) THEN
            WRITE (L,99108)
99108       FORMAT (18X,                                                                                                            &
                   &106H====== POINT  1 ======      ====== POINT  2 ======      ====== POINT  3 ======      ====== POINT  4 ======, &
                  & /7X,7HELEMENT,4X,8HF-FROM-4,6X,8HF-FROM-2,6X,8HF-FROM-1,6X,8HF-FROM-3,6X,8HF-FROM-2,6X,8HF-FROM-4,6X,8HF-FROM-3,&
                  & 6X,8HF-FROM-1,/9X,2HID,15X,6HKICK-1,7X,8HSHEAR-12,7X,6HKICK-2,7X,8HSHEAR-23,7X,6HKICK-3,7X,8HSHEAR-34,7X,       &
                   &6HKICK-4,7X,8HSHEAR-41)
         ELSEIF ( local==19 ) THEN
            WRITE (L,99109)
99109       FORMAT (18X,                                                                                                            &
                   &106H====== POINT  1 ======      ====== POINT  2 ======      ====== POINT  3 ======      ====== POINT  4 ======, &
                  & /14X,4X,8HF-FROM-4,6X,8HF-FROM-2,6X,8HF-FROM-1,6X,8HF-FROM-3,6X,8HF-FROM-2,6X,8HF-FROM-4,6X,8HF-FROM-3,6X,      &
                   &8HF-FROM-1,/5X,9HFREQUENCY,12X,6HKICK-1,7X,8HSHEAR-12,7X,6HKICK-2,7X,8HSHEAR-23,7X,6HKICK-3,7X,8HSHEAR-34,7X,   &
                   &6HKICK-4,7X,8HSHEAR-41)
         ELSEIF ( local==20 ) THEN
            WRITE (L,99110)
99110       FORMAT (18X,                                                                                                            &
                   &106H====== POINT  1 ======      ====== POINT  2 ======      ====== POINT  3 ======      ====== POINT  4 ======, &
                  & /14X,4X,8HF-FROM-4,6X,8HF-FROM-2,6X,8HF-FROM-1,6X,8HF-FROM-3,6X,8HF-FROM-2,6X,8HF-FROM-4,6X,8HF-FROM-3,6X,      &
                   &8HF-FROM-1,/10X,4HTIME,12X,6HKICK-1,7X,8HSHEAR-12,7X,6HKICK-2,7X,8HSHEAR-23,7X,6HKICK-3,7X,8HSHEAR-34,7X,       &
                   &6HKICK-4,7X,8HSHEAR-41)
         ELSEIF ( local==21 ) THEN
            WRITE (L,99111)
99111       FORMAT (6X,16HSUBCASE     TYPE,10X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
         ELSEIF ( local==22 ) THEN
            WRITE (L,99112)
99112       FORMAT (2(26X,7HMAXIMUM,8X,7HAVERAGE,6X,6HSAFETY),/2(6X,7HSUBCASE,14X,5HSHEAR,10X,5HSHEAR,7X,6HMARGIN))
         ELSEIF ( local==23 ) THEN
            WRITE (L,99113)
99113       FORMAT (20X,32HSTRESSES IN ELEMENT COORD SYSTEM,12X,9HPRINCIPAL,11X,18HPRINCIPAL STRESSES,10X,7HMAXIMUM,/6X,7HSUBCASE,  &
                  & 6X,8HNORMAL-X,6X,8HNORMAL-Y,7X,8HSHEAR-XY,6X,12HSTRESS ANGLE,9X,5HMAJOR,10X,5HMINOR,10X,5HSHEAR)
         ELSEIF ( local==24 ) THEN
            WRITE (L,99114)
99114       FORMAT (6X,7HSUBCASE,11X,3HSA1,12X,3HSA2,12X,3HSA3,12X,3HSA4,8X,12HAXIAL-STRESS,6X,6HSA-MAX,9X,6HSA-MIN,5X,6HM.S.-T,    &
                  & /24X,3HSB1,12X,3HSB2,12X,3HSB3,12X,3HSB4,26X,6HSB-MAX,9X,6HSB-MIN,5X,6HM.S.-C)
         ELSEIF ( local==25 ) THEN
            WRITE (L,99115)
99115       FORMAT (18X,                                                                                                            &
                   &106H====== POINT  1 ======      ====== POINT  2 ======      ====== POINT  3 ======      ====== POINT  4 ======, &
                  & /14X,4X,8HF-FROM-4,6X,8HF-FROM-2,6X,8HF-FROM-1,6X,8HF-FROM-3,6X,8HF-FROM-2,6X,8HF-FROM-4,6X,8HF-FROM-3,6X,      &
                   &8HF-FROM-1,/5X,7HSUBCASE,14X,6HKICK-1,7X,8HSHEAR-12,7X,6HKICK-2,7X,8HSHEAR-23,7X,6HKICK-3,7X,8HSHEAR-34,7X,     &
                   &6HKICK-4,7X,8HSHEAR-41)
         ELSEIF ( local==26 ) THEN
            WRITE (L,99116)
99116       FORMAT (21X,17HBEND-MOMENT-END-A,12X,17HBEND-MOMENT-END-B,18X,5HSHEAR,/6X,7HSUBCASE,6X,3(7HPLANE 1,7X,7HPLANE 2,8X),    &
                   &6H FORCE,10X,6HTORQUE)
         ELSEIF ( local==27 ) THEN
            WRITE (L,99117)
99117       FORMAT (2(21X,5HAXIAL,7X,6HSAFETY,6X,9HTORSIONAL,5X,6HSAFETY),/2(6X,7HSUBCASE,7X,6HSTRESS,7X,6HMARGIN,8X,6HSTRESS,6X,   &
                   &6HMARGIN))
         ELSEIF ( local==28 ) THEN
            WRITE (L,99118)
99118       FORMAT (2(25X,5HAXIAL,30X),/2(6X,7HSUBCASE,12X,5HFORCE,9X,6HTORQUE,15X))
         ELSEIF ( local==29 ) THEN
            WRITE (L,99119)
99119       FORMAT (5X,7HELEMENT,8X,33HSTRESSES IN MATERIAL COORD SYSTEM,12X,9HPRINCIPAL,11X,18HPRINCIPAL STRESSES,12X,3HMAX)
         ELSEIF ( local==30 ) THEN
            WRITE (L,99120)
99120       FORMAT (13X,7HELEMENT,33X,'- STRESSES IN MATERIAL COORDINATE ','SYSTEM -',/15X,3HID.,18X,8HNORMAL-X,26X,8HNORMAL-Y,26X, &
                   &8HSHEAR-XY)
         ELSEIF ( local==31 ) THEN
            WRITE (L,99121)
99121       FORMAT (20X,33HSTRESSES IN MATERIAL COORD SYSTEM,11X,9HPRINCIPAL,11X,18HPRINCIPAL STRESSES,10X,7HMAXIMUM,/7X,4HTIME,8X, &
                   &8HNORMAL-X,6X,8HNORMAL-Y,7X,8HSHEAR-XY,6X,12HSTRESS ANGLE,9X,5HMAJOR,10X,5HMINOR,10X,5HSHEAR)
         ELSEIF ( local==32 ) THEN
            WRITE (L,99122)
99122       FORMAT (53X,42H- STRESSES IN MATERIAL COORDINATE SYSTEM -,/9X,9HFREQUENCY,18X,8HNORMAL-X,26X,8HNORMAL-Y,26X,8HSHEAR-XY)
         ELSEIF ( local==33 ) THEN
            WRITE (L,99123)
99123       FORMAT (20X,33HSTRESSES IN MATERIAL COORD SYSTEM,11X,9HPRINCIPAL,11X,18HPRINCIPAL STRESSES,10X,7HMAXIMUM,/6X,7HSUBCASE, &
                  & 6X,8HNORMAL-X,6X,8HNORMAL-Y,7X,8HSHEAR-XY,6X,12HSTRESS ANGLE,9X,5HMAJOR,10X,5HMINOR,10X,5HSHEAR)
         ELSEIF ( local==34 ) THEN
            idd = id(3) - 64
            WRITE (L,99124) idd
99124       FORMAT (21X,'S T R E S S E S   I N   I S O P A R A M E T R I C  ',' S O L I D   ( C I H E X',I2,2H ))
         ELSEIF ( local==35 ) THEN
            WRITE (L,99125)
99125       FORMAT (2X,7HELEMENT,5X,4HGRID,11X,'STRESSES IN BASIC COORDINATE','SYSTEM',13X,12HDIR. COSINES)
         ELSEIF ( local==36 ) THEN
            WRITE (L,99126)
99126       FORMAT (7X,2HID,4X,5HPOINT,8X,6HNORMAL,12X,5HSHEAR,10X,9HPRINCIPAL,10X,1HA,4X,1HB,4X,1HC,4X,11HMEAN STRESS,5X,          &
                   &9HMAX SHEAR)
         ELSEIF ( local==37 ) THEN
            idd = id(3) - 64
            WRITE (L,99127) idd
99127       FORMAT (13X,'C O M P L E X   S T R E S S E S   I N   I S O P A R',' A M E T R I C   S O L I D   ( C I H E X',I2,2H ))
         ELSEIF ( local==38 ) THEN
            WRITE (L,99128)
99128       FORMAT (7X,2HID,3X,6HPOINTS,5X,8HNORMAL-X,9X,8HNORMAL-Y,9X,8HNORMAL-Z,9X,8HSHEAR-XY,9X,8HSHEAR-YZ,9X,8HSHEAR-ZX)
         ELSEIF ( local==39 ) THEN
            WRITE (L,99129)
99129       FORMAT (25X,'F O R C E   D I S T R I B U T I O N   I N   B A R','   E L E M E N T S,10X,11H( C B A R )')
         ELSEIF ( local==40 ) THEN
            WRITE (L,99130)
99130       FORMAT (21H0    ELEMENT  STATION,9X,11HBEND-MOMENT,22X,11HSHEAR FORCE,21X,5HAXIAL)
         ELSEIF ( local==41 ) THEN
            WRITE (L,99131)
99131       FORMAT (7X,3HID.,5X,5H(PCT),5X,7HPLANE 1,8X,7HPLANE 2,11X,7HPLANE 1,8X,7HPLANE 2,15X,5HFORCE,14X,6HTORQUE)
         ELSEIF ( local==42 ) THEN
            WRITE (L,99132)
99132       FORMAT (25X,'S T R E S S   D I S T R I B U T I O N   I N   B A R','  E L E M E N T S,7X,11H( C B A R )')
         ELSEIF ( local==43 ) THEN
            WRITE (L,99133)
99133       FORMAT (21H0    ELEMENT  STATION,4X,3HSXC,11X,3HSXD,11X,3HSXF,11X,3HSXG,12X,5HAXIAL,10X,5HS-MAX,9X,5HS-MIN,9X,4HM.S.)
         ELSEIF ( local==44 ) THEN
            WRITE (L,99134)
99134       FORMAT (7X,3HID.,5X,5H(PCT))
         ELSEIF ( local==45 ) THEN
            WRITE (L,99135)
99135       FORMAT (21X,'F O R C E S  F O R  T H E  Q U A D R I L A T E R A L','  T H I N  S H E L L     ( C Q U A D T S )')
         ELSEIF ( local==46 ) THEN
            WRITE (L,99136)
99136       FORMAT (6X,2HEL,36X,6HFORCES,51X,7HMOMENTS)
         ELSEIF ( local==47 ) THEN
            WRITE (L,99137)
99137       FORMAT (6X,2HID,5X,5HPOINT,9X,2HFX,17X,2HFY,17X,2HFZ,17X,2HMX,17X,2HMY,17X,2HMZ)
         ELSEIF ( local==48 ) THEN
            WRITE (L,99138)
99138       FORMAT (17X,'F O R C E S   I N   T R I A N G U L A R   T H I N  ','S H E L L   E L E M E N T S   ( C T R S H L )')
         ELSEIF ( local==49 ) THEN
            WRITE (L,99139)
99139       FORMAT (19X,'S T R E S S E S  F O R  T H E  Q U A D R I L A T E ','R A L  T H I N  S H E L L     ( C Q U A D T S )')
         ELSEIF ( local==50 ) THEN
            WRITE (L,99140)
99140       FORMAT (3X,9HEL STRESS,8X,28HMEMBRANE  STRESS  RESULTANTS,24X,17HFLEXURAL  MOMENTS,27X,5HSHEAR)
         ELSEIF ( local==51 ) THEN
            WRITE (L,99141)
99141       FORMAT (3X,'ID  POINT   NORMAL(NX)     NORMAL(NY)     SHEAR(NXY)','     NORMAL(MX)     NORMAL(MY)     TORQUE(MXY)     ',&
                   &'NORMAL(QX)     NORMAL(QY)')
         ELSEIF ( local==52 ) THEN
            WRITE (L,99142)
99142       FORMAT (18X,'S T R E S S E S   I N   T R I A N G U L A R   T H I',' N   S H E L L   E L E M E N T S   ( C T R S H L )')
         ELSEIF ( local==53 ) THEN
            WRITE (L,99143)
99143       FORMAT (5X,'S T R E S S E S  I N  A X I S - S Y M M E T R I C  ',                                                       &
                   &'T R I A N G U L A R  R I N G  E L E M E N T S  (CTRIAAX)')
         ELSEIF ( local==54 ) THEN
            WRITE (L,99144)
99144       FORMAT (' ELEMENT   HARMONIC    POINT    RADIAL      AXIAL',6X,'CIRCUM.     SHEAR      SHEAR      SHEAR      F L U X   '&
                  & ,'D E N S I T I E S',/,' ID.       NUMBER      ANGLE     ',                                                     &
                   &'(R)         (Z)     (THETA-T)    (ZR)       (RT)       ','(ZT)        (R)        (Z)        (T)')
         ELSEIF ( local==55 ) THEN
            WRITE (L,99145)
99145       FORMAT (11X,'F O R C E S  I N  A X I S - S Y M M E T R I C  T R ','I A N G U L A R  R I N G  E L E M E N T S  (CTRIAAX)'&
                  & )
         ELSEIF ( local==56 ) THEN
            WRITE (L,99146)
99146       FORMAT (1X,                                                                                                             &
              &113H  ELEMENT   HARMONIC    POINT            RADIAL            CIRCUMFERENTIAL            AXIAL                CHARGE&
             & ,/1X,'    ID.      NUMBER     ANGLE             (R)',17X,'(THETA-T)                (Z)')
         ELSEIF ( local==57 ) THEN
            WRITE (L,99147)
99147       FORMAT (5X,'S T R E S S E S  I N  A X I S - S Y M M E T R I C  T',                                                      &
                   &' R A P E Z O I D A L  R I N G  E L E M E N T S  (CTRAPAX)')
         ELSEIF ( local==58 ) THEN
            WRITE (L,99148)
99148       FORMAT (11X,'F O R C E S  I N  A X I S - S Y M M E T R I C  T R ',                                                      &
                   &'A P E Z O I D A L  R I N G  E L E M E N T S  (CTRAPAX)')
         ELSEIF ( local==59 ) THEN
            WRITE (L,99149)
99149       FORMAT (43X,45HE L E M E N T   S T R A I N   E N E R G I E S)
         ELSEIF ( local==60 ) THEN
            WRITE (L,99150) id(6) , id(7) , fd(3)
99150       FORMAT (30X,15HELEMENT-TYPE = ,2A4,9X,23H* TOTAL FOR ALL TYPES =,1P,E16.7,/1H0,95X,1H*,/36X,10HELEMENT-ID,10X,          &
                   &13HSTRAIN-ENERGY,11X,16HPERCENT OF TOTAL)
         ELSEIF ( local==61 ) THEN
            WRITE (L,99151)
99151       FORMAT (42X,47HG R I D   P O I N T   F O R C E   B A L A N C E)
         ELSEIF ( local==62 ) THEN
            WRITE (L,99152)
99152       FORMAT (11H   POINT-ID,4X,10HELEMENT-ID,5X,6HSOURCE,13X,2HT1,13X,2HT2,13X,2HT3,13X,2HR1,13X,2HR2,13X,2HR3)
         ELSEIF ( local==63 ) THEN
            WRITE (L,99153)
99153       FORMAT (22X,'F O R C E S   I N   T R I A N G U L A R   P L A T E','  E L E M E N T S   ( C T R P L T 1 )')
         ELSEIF ( local==64 ) THEN
            WRITE (L,99154)
99154       FORMAT (20X,'S T R E S S E S   I N   T R I A N G U L A R   ','P L A T E   E L E M E N T S   ( C T R P L T 1 )')
         ELSEIF ( local==65 ) THEN
            WRITE (L,99155)
99155       FORMAT (1H0,9X,7HELEMENT,4X,5HPOINT,7X,2(11HBEND-MOMENT,9X),12HTWIST-MOMENT,2(11X,5HSHEAR,4X))
         ELSEIF ( local==66 ) THEN
            WRITE (L,99156)
99156       FORMAT (12X,3HID.,7X,3HNO.,13X,1HX,19X,1HY,39X,1HX,19X,1HY)
         ELSEIF ( local==67 ) THEN
            WRITE (L,99157)
99157       FORMAT (1H0,8H ELEMENT,2X,5HPOINT,5X,5HFIBER,11X,32HSTRESSES IN ELEMENT COORD SYSTEM,12X,                               &
                   &31HPRINCIPAL STRESSES (ZERO SHEAR),11X,3HMAX)
         ELSEIF ( local==68 ) THEN
            WRITE (L,99158)
99158       FORMAT (3X,3HID.,6X,3HNO.,5X,8HDISTANCE,7X,8HNORMAL-X,6X,8HNORMAL-Y,6X,8HSHEAR-XY,8X,5HANGLE,9X,5HMAJOR,9X,5HMINOR,10X, &
                   &5HSHEAR)
         ELSEIF ( local==69 ) THEN
            WRITE (L,99159)
99159       FORMAT (18X,'S T R E S S E S   I N   T R I A N G U L A R   ','M E M B R A N E   E L E M E N T S   ( C T R I M 6 )')
         ELSEIF ( local==70 ) THEN
            WRITE (L,99160)
99160       FORMAT (1H0,8H ELEMENT,5X,5HPOINT,7X,32HSTRESSES IN ELEMENT COORD SYSTEM,13X,31HPRINCIPAL STRESSES (ZERO SHEAR),13X,    &
                   &3HMAX)
         ELSEIF ( local==71 ) THEN
            WRITE (L,99161)
99161       FORMAT (4X,3HID.,8X,3HNO.,5X,8HNORMAL-X,7X,8HNORMAL-Y,7X,8HSHEAR-XY,8X,5HANGLE,10X,5HMAJOR,10X,5HMINOR,10X,5HSHEAR)
         ELSEIF ( local==72 ) THEN
            WRITE (L,99162)
99162       FORMAT (2(24X,6HMOMENT,9X,6HMOMENT,15X),/2(6X,7HSUBCASE,11X,7HPTS 1,3,8X,7HPTS 2,4,14X))
         ELSEIF ( local==73 ) THEN
            WRITE (L,99163)
99163       FORMAT (6X,7HSUBCASE,2X,2(11X,11HBEND-MOMENT),11X,12HTWIST-MOMENT,13X,5HSHEAR,17X,5HSHEAR,/31X,1HX,21X,1HY,43X,1HX,21X, &
                   &1HY)
         ELSEIF ( local==74 ) THEN
            WRITE (L,99164)
99164       FORMAT (4(6X,7HSUBCASE,9X,5HFORCE,6X))
         ELSEIF ( local==75 ) THEN
            WRITE (L,99165)
99165       FORMAT (5X,7HSUBCASE,11X,3HSA1,12X,3HSA2,12X,3HSA3,10X,12HAXIAL-STRESS,8X,6HSA-MAX,9X,6HSA-MIN,11X,6HM.S.-T,/23X,3HSB1, &
                  & 12X,3HSB2,12X,3HSB3,30X,6HSB-MAX,9X,6HSB-MIN,11X,6HM.S.-C)
         ELSEIF ( local==76 ) THEN
            WRITE (L,99166)
99166       FORMAT (2(54X,6HSAFETY),/2(5X,7HSUBCASE,14X,7HMAXIMUM,8X,7HAVERAGE,6X,6HMARGIN))
         ELSEIF ( local==77 ) THEN
            WRITE (L,99167)
99167       FORMAT (19X,5HFIBRE,11X,32HSTRESSES IN ELEMENT COORD SYSTEM,13X,31HPRINCIPAL STRESSES (ZERO SHEAR),10X,7HMAXIMUM,/5X,   &
                   &7HSUBCASE,6X,8HDISTANCE,7X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X,5HSHEAR)
         ELSEIF ( local==78 ) THEN
            WRITE (L,99168)
99168       FORMAT (4(6X,7HSUBCASE,8X,6HSTRESS,6X))
         ELSEIF ( local==79 ) THEN
            WRITE (L,99169)
99169       FORMAT (107X,22HOCTAHEDRAL    PRESSURE,/5X,10HSUBCASE   ,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,      &
                   &6HTAU-XZ,8X,6HTAU-XY,8X,5HTAU-0,10X,1HP)
         ELSEIF ( local==80 ) THEN
            WRITE (L,99170)
99170       FORMAT (107X,22HOCTAHEDRAL    PRESSURE,/5X,11HSUBCASE    ,8X,8HSIGMA-XX,6X,8HSIGMA-YY,6X,8HSIGMA-ZZ,7X,6HTAU-YZ,8X,     &
                   &6HTAU-XZ,8X,6HTAU-XY,8X,5HTAU-0,10X,1HP)
         ELSEIF ( local==81 ) THEN
            WRITE (L,99171)
99171       FORMAT (32X,'F O R C E S   O F   M U L T I - P O I N T   C O N S',' T R A I N T')
            WRITE (L,99172)
99172       FORMAT (1H )
         ELSEIF ( local==82 ) THEN
            WRITE (L,99173)
99173       FORMAT (2X,7HELEMENT,4X,16HMAT. COORD. SYS.,6X,33HSTRESSES IN MATERIAL COORD SYSTEM,12X,                                &
                   &31HPRINCIPAL STRESSES (ZERO SHEAR),12X,3HMAX)
         ELSEIF ( local==83 ) THEN
            WRITE (L,99174)
99174       FORMAT (4X,3HID.,6X,15HID./OUTPUT CODE,5X,8HNORMAL-X,7X,8HNORMAL-Y,6X,8HSHEAR-XY,7X,5HANGLE,9X,5HMAJOR,11X,5HMINOR,10X, &
                   &5HSHEAR)
         ELSEIF ( local==84 ) THEN
            WRITE (L,99175)
99175       FORMAT (43X,'S T R E S S E S   A T   G R I D   P O I N T S')
         ELSEIF ( local==85 ) THEN
            WRITE (L,99176)
99176       FORMAT (7X,'S T R A I N S / C U R V A T U R E S   I N   G E N E ','R A L   T R I A N G U L A R   E L E M E N T S',6X,   &
                   &'( C T R I A 1 )')
         ELSEIF ( local==86 ) THEN
            WRITE (L,99177)
99177       FORMAT (7X,'S T R A I N S / C U R V A T U R E S   I N   G E N E ','R A L   T R I A N G U L A R   E L E M E N T S',6X,   &
                   &'( C T R I A 2 )')
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99082 FORMAT (36X,'S T R E S S E S   I N   U S E R   E L E M E N T S','  (C',A4,1H))
99083 FORMAT (38X,'F O R C E S   I N   U S E R   E L E M E N T S   (C',A4,1H))
99086 FORMAT (28X,'C O M P L E X   S T R E S S E S   I N   U S E R   ','E L E M E N T S   (C',A4,1H))
99087 FORMAT (30X,'C O M P L E X   F O R C E S   I N   U S E R   E L E',' M E N T S   (C',A4,1H))
!
END SUBROUTINE ofp1b
