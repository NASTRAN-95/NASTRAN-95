!*==usrmsg.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE usrmsg(I)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_MSGX
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: a , b , c , d , e , f , index , j , ja , jb , jc , jd , jf , k , l , ldset , ldtype , local , p1 , p2 , ul , um , uo ,&
            & ur , us
   REAL , SAVE :: ax , rg
   INTEGER , DIMENSION(5) , SAVE :: bcd
   INTEGER , SAVE :: blank , i2015 , limit
   CHARACTER(8) :: exin
   CHARACTER(8) , SAVE :: exter , inter
   INTEGER , DIMENSION(4) , SAVE :: icrigd
   INTEGER , DIMENSION(6) , SAVE :: itype
   INTEGER , DIMENSION(10) , SAVE :: list
   INTEGER , DIMENSION(2) , SAVE :: name
   CHARACTER(6) , SAVE :: quad4 , tria3
   EXTERNAL errtrc , mesage , page2 , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     USRMSG WILL PRINT THE INDICATED USER LEVEL ERROR MESSAGE
!
   DATA blank/3H   / , limit/234/ , i2015/0/
   DATA bcd/4H  UM , 4H  US , 4H  UO , 4HUAUR , 4HUAUL/
   DATA itype/4HDARE , 4HA    , 4HDELA , 4HY    , 4HDPHA , 4HSE  /
   DATA icrigd/1H1 , 1H2 , 1H3 , 1HR/
   DATA list/15 , 41 , 79 , 103 , 117 , 137 , 199 , 211 , 212 , 215/
   DATA ax , rg/2HAX , 2HRG/ , name/4HUSRM , 4HSG  /
   DATA inter/'INTERNAL'/ , exter/'EXTERNAL'/
   DATA quad4 , tria3/'CQUAD4' , 'CTRIA3'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         l = Msg(2,I)
         p1 = Msg(3,I)
         p2 = Msg(4,I)
         IF ( l<=0 .OR. l>limit ) THEN
!
!     ILLEGAL INPUT TO SUBROUTINE
!
            WRITE (Outtap,99001) l
99001       FORMAT ('0IMPROPER USRMSG NO.',I20)
            CALL mesage(-7,0,name)
            RETURN
         ELSE
            DO j = 1 , 10
               IF ( l==list(j) ) GOTO 10
            ENDDO
            j = 2
            IF ( l==92 ) j = 4
            IF ( l==15 ) THEN
               j = 3
               IF ( i2015>4 ) j = 1
               IF ( i2015>=31 ) j = 0
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
 10         j = 3
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL page2(j)
         local = l - 120
         IF ( local<=0 ) THEN
            IF ( l==1 ) THEN
               WRITE (Outtap,99002) Ufm , p1
            ELSEIF ( l==2 ) THEN
               WRITE (Outtap,99003) Sfm , p1
99003          FORMAT (A25,' 2002, GRID POINT',I9,' NOT IN EQEXIN')
            ELSEIF ( l==3 ) THEN
               WRITE (Outtap,99004) Ufm , p2 , p1
99004          FORMAT (A23,' 2003, COORDINATE SYSTEM',I9,' REFERENCES UNDEFINED GRID POINT',I9)
            ELSEIF ( l==4 ) THEN
               WRITE (Outtap,99005) Ufm , p2 , p1
99005          FORMAT (A23,' 2004, COORDINATE SYSTEM',I9,' REFERENCES UNDEFINED COORDINATE SYSTEM',I9)
            ELSEIF ( l==5 ) THEN
               WRITE (Outtap,99006) Sfm
99006          FORMAT (A25,' 2005, INCONSISTENT COORDINATE SYSTEM DEFINITION')
            ELSEIF ( l==6 ) THEN
               WRITE (Outtap,99007) Ufm , p1 , p2
99007          FORMAT (A23,' 2006, INTERNAL GRID POINT',I9,' REFERENCES UNDEFINED COORDINATE SYSTEM',I9)
            ELSEIF ( l==7 ) THEN
               WRITE (Outtap,99008) Ufm , p1 , p2
99008          FORMAT (A23,' 2007, ELEMENT',I12,' REFERENCES UNDEFINED GRID ','POINT',I12)
            ELSEIF ( l==8 ) THEN
               WRITE (Outtap,99009) Ufm , p1 , p2
99009          FORMAT (A23,' 2008, LOAD SET',I9,' REFERENCES UNDEFINED GRID POINT',I9)
            ELSEIF ( l==9 ) THEN
               WRITE (Outtap,99010) Ufm , p1 , p2
99010          FORMAT (A23,' 2009, TEMP SET',I9,' REFERENCES UNDEFINED GRID ','POINT',I9)
            ELSEIF ( l==10 ) THEN
               WRITE (Outtap,99011) Ufm , p1 , p2
99011          FORMAT (A23,' 2010, ELEMENT',I9,' REFERENCES UNDEFINED PROPERTY',I9)
            ELSEIF ( l==11 ) THEN
               WRITE (Outtap,99012) Ufm , p1 , p2
99012          FORMAT (A23,' 2011, NO PROPERTY CARD FOR ELEMENT TYPE - C',2A4)
            ELSEIF ( l==12 ) THEN
               WRITE (Outtap,99013) Ufm , p1
99013          FORMAT (A23,' 2012, GRID POINT',I9,' SAME AS SCALAR POINT')
            ELSEIF ( l==13 ) THEN
               WRITE (Outtap,99014) Uwm
99014          FORMAT (A25,' 2013, NO STRUCTURAL ELEMENTS EXIST')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==14 ) THEN
               WRITE (Outtap,99015) Ufm
99015          FORMAT (A23,' 2014, LOGIC ERROR IN ECPT CONSTRUCTION')
            ELSEIF ( l==15 ) THEN
               i2015 = i2015 + 1
               IF ( i2015==30 ) WRITE (Outtap,99016)
99016          FORMAT (11X,':',/11X,':',/7X,'AND MORE')
               IF ( i2015<30 ) THEN
                  IF ( i2015==4 ) WRITE (Outtap,99017)
99017             FORMAT (1X)
                  exin = inter
                  IF ( p2/=0 ) exin = exter
                  IF ( p2/=0 ) p1 = p2
                  IF ( i2015<=3 ) WRITE (Outtap,99018) Uwm , exin , p1
99018             FORMAT (A25,' 2015, EITHER NO ELEMENTS CONNECTED TO ',A8,' GRID',' POINT',I9,/5X,                                 &
                         &'OR IT IS CONNECTED TO A RIGID ELEMENT ','OR A GENERAL ELEMENT.')
                  IF ( i2015>3 ) WRITE (Outtap,99019) Uwm , exin , p1
99019             FORMAT (A25,' 2015, ',A8,' GRID PT.',I9,' NOT CONNECTED')
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==16 ) THEN
               WRITE (Outtap,99020) Ufm
99020          FORMAT (A23,' 2016, NO MATERIAL PROPERTIES EXIST')
            ELSEIF ( l==17 ) THEN
               WRITE (Outtap,99021) Ufm , p1
99021          FORMAT (A23,' 2017, MATS1 CARD REFERENCES UNDEFINED MAT1',I9,' CARD')
            ELSEIF ( l==18 ) THEN
               WRITE (Outtap,99022) Ufm , p1
99022          FORMAT (A23,' 2018, MATS2 CARD REFERENCES UNDEFINED MAT2',I9,' CARD')
            ELSEIF ( l==19 ) THEN
               WRITE (Outtap,99023) Ufm , p1
99023          FORMAT (A23,' 2019, MATT1 CARD REFERENCES UNDEFINED MAT1',I9,' CARD')
            ELSEIF ( l==20 ) THEN
               WRITE (Outtap,99024) Ufm , p1
99024          FORMAT (A23,' 2020, MATT2 CARD REFERENCES UNDEFINED MAT2',I9,' CARD')
            ELSEIF ( l==21 ) THEN
               WRITE (Outtap,99025) Ufm
99025          FORMAT (A23,' 2021, BAD GMMAT- CALLING SEQUENCE')
            ELSEIF ( l==22 ) THEN
               WRITE (Outtap,99026) Ufm
99026          FORMAT (A23,' 2022, SMA-B SCALAR POINT INSERTION LOGIC ERROR')
            ELSEIF ( l==23 ) THEN
               WRITE (Outtap,99027) Ufm , p1
99027          FORMAT (A23,' 2023, DETCK UNABLE TO FIND PIVOT POINT',I9,' IN GPCT')
            ELSEIF ( l==24 ) THEN
               WRITE (Outtap,99028)
99028          FORMAT ('0*** UNDEFINED MESSAGE 2024')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==25 ) THEN
               WRITE (Outtap,99029) Ufm , p1
99029          FORMAT (A23,' 2025, UNDEFINED COORDINATE SYSTEM',I9)
            ELSEIF ( l==26 ) THEN
               WRITE (Outtap,99030) Ufm , p1
99030          FORMAT (A23,' 2026,ELEMENT',I9,' GEOMETRY YIELDS UNREASONABLE ','MATRIX')
            ELSEIF ( l==27 ) THEN
               WRITE (Outtap,99031) Ufm , p1 , p2
99031          FORMAT (A23,' 2027,ELEMENT',I9,' HAS INTERIOR ANGLE GREATER THAN',' 180 DEG. AT GRID POINT',I9)
            ELSEIF ( l==28 ) THEN
               WRITE (Outtap,99032) Ufm , p1
99032          FORMAT (A23,' 2028, SMA3A ERROR NO.',I9)
            ELSEIF ( l==29 ) THEN
               WRITE (Outtap,99033) Ufm , p1
99033          FORMAT (A23,' 2029, UNDEFINED TEMPERATURE SET',I9)
            ELSEIF ( l==30 ) THEN
               WRITE (Outtap,99034) Ufm
99034          FORMAT (A23,' 2030, BAD GPTT')
            ELSEIF ( l==31 ) THEN
               WRITE (Outtap,99035) Ufm , p1
99035          FORMAT (A23,' 2031, ELEMENT',I9,' UNACCEPTABLE GEOMETRY')
            ELSEIF ( l==32 ) THEN
               WRITE (Outtap,99036) Ufm , p1
99036          FORMAT (A23,' 2032, ELEMENT',I9,' UNACCEPTABLE GEOMETRY')
            ELSEIF ( l==33 ) THEN
               WRITE (Outtap,99037) Ufm , p1
99037          FORMAT (A23,' 2033, SINGULAR H-MATRIX FOR ELEMENT',I9)
            ELSEIF ( l==34 ) THEN
               WRITE (Outtap,99038) Sfm , p1
99038          FORMAT (A25,' 2034, ELEMENT',I9,' SIL-S DO NOT MATCH PIVOT')
            ELSEIF ( l==35 ) THEN
               WRITE (Outtap,99039) Ufm , p1
99039          FORMAT (A23,' 2035, QUADRILATERAL',I9,' INTERIOR ANGLE GREATER THAN 180 DEG.')
            ELSEIF ( l==36 ) THEN
               WRITE (Outtap,99040) Ufm , p1
99040          FORMAT (A23,' 2036, SINGULAR MATRIX FOR ELEMENT',I9)
            ELSEIF ( l==37 ) THEN
               WRITE (Outtap,99041) Ufm , p1
99041          FORMAT (A23,' 2037, BAD ELEMENT',I9,' GEOMETRY')
            ELSEIF ( l==38 ) THEN
               WRITE (Outtap,99042) Sfm , p1
99042          FORMAT (A25,' 2038, SINGULAR MATRIX FOR ELEMENT',I9)
            ELSEIF ( l==39 ) THEN
               WRITE (Outtap,99043) Ufm , p2 , p1
99043          FORMAT (A23,' 2039, ZERO SLANT LENGTH FOR HARMONIC',I9,' OF CCONEAX',I9)
            ELSEIF ( l==40 ) THEN
               WRITE (Outtap,99044) Ufm , p1
99044          FORMAT (A23,' 2040, SINGULAR MATRIX FOR ELEMENT',I9)
            ELSEIF ( l==41 ) THEN
               WRITE (Outtap,99045) Ufm , p1
99045          FORMAT (A23,' 2041, A MATT1, MATT2, MATT3, OR MATS1 CARD REFER','ENCES TABLE NUMBER',I9,' WHICH IS NOT DEFINED ON',  &
                     & /5X,'A TABLEM1, TABLEM2, TABLEM3, TABLEM4, OR TABLES1 CARD.')
            ELSEIF ( l==42 ) THEN
               WRITE (Outtap,99046) Ufm , p2 , p1
99046          FORMAT (A23,' 2042, MISSING MATERIAL TABLE',I9,' FOR ELEMENT',I9)
            ELSEIF ( l==43 ) THEN
               WRITE (Outtap,99047) Ufm , p1
99047          FORMAT (A23,' 2043, MISSING MATERIAL TABLE',I9)
            ELSEIF ( l==44 ) THEN
               WRITE (Outtap,99048) Ufm , p1
99048          FORMAT (A23,' 2044, UNDEFINED TEMPERATURE SET',I9)
            ELSEIF ( l==45 ) THEN
               WRITE (Outtap,99049) Ufm , p1
99049          FORMAT (A23,' 2045, TEMPERATURE UNDEFINED AT GRID POINT WITH ','INTERNAL INDEX',I9)
            ELSEIF ( l==46 ) THEN
               WRITE (Outtap,99050) Ufm , p1
99050          FORMAT (A23,' 2046, UNDEFINED ELEMENT DEFORMATION SET',I9)
            ELSEIF ( l==47 ) THEN
               WRITE (Outtap,99051) Ufm , p1
99051          FORMAT (A23,' 2047, UNDEFINED MULTI-POINT CONSTRAINT SET',I9)
            ELSEIF ( l==48 ) THEN
               WRITE (Outtap,99052) Ufm , p1 , p2
99052          FORMAT (A23,' 2048, UNDEFINED GRID POINT',I9,' IN MULTI-POINT CONSTRAINT SET',I9)
            ELSEIF ( l==49 ) THEN
               WRITE (Outtap,99053) Ufm , p1
99053          FORMAT (A23,' 2049, UNDEFINED GRID POINT',I9,' REFERENCED ON AN ASET, ASET1, OMIT OR OMIT1 CARD.')
            ELSEIF ( l==50 ) THEN
               WRITE (Outtap,99054) Ufm , p1
99054          FORMAT (A23,' 2050, UNDEFINED GRID POINT',I9,' HAS A SUPPORT COORDINATE')
            ELSEIF ( l==51 ) THEN
               WRITE (Outtap,99055) Ufm , p1 , p2
99055          FORMAT (A23,' 2051, UNDEFINED GRID POINT',I9,' IN SINGLE-POINT CONSTRAINT SET',I9)
            ELSEIF ( l==52 ) THEN
               WRITE (Outtap,99056) Ufm , p1 , p2
99056          FORMAT (A23,' 2052, UNDEFINED GRID POINT',I9,' IN SINGLE-POINT CONSTRAINT SET',I9)
            ELSEIF ( l==53 ) THEN
               WRITE (Outtap,99057) Ufm , p1
99057          FORMAT (A23,' 2053, UNDEFINED SINGLE-POINT CONSTRAINT SET',I9)
            ELSEIF ( l==54 ) THEN
               WRITE (Outtap,99058) Ufm , p1 , p2
99058          FORMAT (A23,' 2054, SUPER ELEMENT',I9,' REFERENCES UNDEFINED SIMPLE ELEMENT',I9)
            ELSEIF ( l==55 ) THEN
               WRITE (Outtap,99059) Sfm
99059          FORMAT (A25,' 2055')
            ELSEIF ( l==56 ) THEN
               WRITE (Outtap,99060) Ufm , p1
99060          FORMAT (A23,' 2056, UNDEFINED SUPER ELEMENT',I9,' PROPERTIES')
            ELSEIF ( l==57 ) THEN
               WRITE (Outtap,99061) Ufm , p1
99061          FORMAT (A23,' 2057, IRRATIONAL SUPER ELEMENT',I9,' TOPOLOGY')
            ELSEIF ( l==58 ) THEN
               WRITE (Outtap,99062) Uwm , p1
99062          FORMAT (A25,' 2058, ELEMENT',I9,' CONTRIBUTES TO THE DAMPING ','MATRIX WHICH IS PURGED.  IT WILL BE IGNORED.')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==59 ) THEN
               WRITE (Outtap,99063) Ufm , p2 , p1
99063          FORMAT (A23,' 2059, UNDEFINED GRID POINT',I9,' ON SE--BFE CARD FOR SUPER ELEMENT',I9)
            ELSEIF ( l==60 ) THEN
               WRITE (Outtap,99064) Ufm , p2 , p1
99064          FORMAT (A23,' 2060, UNDEFINED GRID POINT',I9,' ON QDSEP CARD FOR SUPER ELEMENT',I9)
            ELSEIF ( l==61 ) THEN
               WRITE (Outtap,99065) Ufm , p2 , p1
99065          FORMAT (A23,' 2061, UNDEFINED GRID POINT',I9,' ON GENERAL ','ELEMENT',I9)
            ELSEIF ( l==62 ) THEN
               WRITE (Outtap,99066) Ufm , p2 , p1
99066          FORMAT (A23,' 2062, UNDEFINED SUPER ELEMENT PROPERTY',I9,' FOR SUPER ELEMENT',I9)
            ELSEIF ( l==63 ) THEN
               WRITE (Outtap,99067) Ufm
99067          FORMAT (A23,' 2063, TA1C LOGIC ERROR')
            ELSEIF ( l==64 ) THEN
               WRITE (Outtap,99068) Ufm , p1
99068          FORMAT (A23,' 2064, UNDEFINED EXTRA POINT',I9,' REFERENCED ON SEQEP CARD')
            ELSEIF ( l==65 ) THEN
               WRITE (Outtap,99069) Ufm , p1
99069          FORMAT (A23,' 2065, UNDEFINED GRID POINT',I9,' ON DMIG CARD')
            ELSEIF ( l==66 ) THEN
               WRITE (Outtap,99070) Ufm , p1
99070          FORMAT (A23,' 2066, UNDEFINED GRID POINT',I9,' ON RLOAD- OR TLOAD- CARD')
            ELSEIF ( l==67 ) THEN
!*****
!     DETERMINE NONLINEAR LOAD TYPE AND NONLINEAR LOAD SET ID
!*****
               ldtype = p2/100000000
               ldset = p2 - 100000000*ldtype
               WRITE (Outtap,99071) Ufm , p1 , ldtype , ldset
99071          FORMAT (A23,' 2067, UNDEFINED GRID POINT',I9,' IN NONLINEAR ','(NOLIN',I1,') LOAD SET',I9)
            ELSEIF ( l==68 ) THEN
               WRITE (Outtap,99072) Ufm , p1 , p2
99072          FORMAT (A23,' 2068, UNDEFINED GRID POINT',I9,' IN TRANSFER FUNCTION SET',I9)
            ELSEIF ( l==69 ) THEN
               WRITE (Outtap,99073) Ufm , p1 , p2
99073          FORMAT (A23,' 2069, UNDEFINED GRID POINT',I9,' IN TRANSIENT INITIAL CONDITION SET',I9)
            ELSEIF ( l==70 ) THEN
               WRITE (Outtap,99074) Ufm , p1 , p2
99074          FORMAT (A23,' 2070, REQUESTED DMIG MATRIX ',2A4,' IS UNDEFINED.')
            ELSEIF ( l==71 ) THEN
!*****
!     DETERMINE TYPE OF UNDEFINED SET (DAREA, DELAY OR DPHASE)
!*****
               index = p2/100000000
               p2 = p2 - 100000000*index
               index = 2*index - 1
               WRITE (Outtap,99075) Ufm , p1 , itype(index) , itype(index+1) , p2
99075          FORMAT (A23,' 2071, DYNAMIC LOAD SET',I9,' REFERENCES UNDEFINED ',2A4,' SET',I9)
            ELSEIF ( l==72 ) THEN
               WRITE (Outtap,99076) Swm , p1 , p2
99076          FORMAT (A27,' 2072, CARD TYPE',I9,' NOT FOUND ON DATA BLOCK. ',' BIT POSITION =',I4)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==73 ) THEN
               WRITE (Outtap,99077) Uim , p1 , p2
99077          FORMAT (A29,' 2073, MPYAD METHOD',I9,' NO. PASSES =',I8)
            ELSEIF ( l==74 ) THEN
               WRITE (Outtap,99078) Ufm , p1
99078          FORMAT (A23,' 2074, UNDEFINED TRANSFER FUNCTION SET',I9)
            ELSEIF ( l==75 ) THEN
               WRITE (Outtap,99079) Ufm , p1 , p2
99079          FORMAT (A23,' 2075, IMPROPER KEYWORD ',2A4,' FOR APPROACH PARAMETER IN DMAP INSTRUCTION.')
            ELSEIF ( l==76 ) THEN
               WRITE (Outtap,99080) Uwm
99080          FORMAT (A25,' 2076, SDR2 OUTPUT DATA BLOCK NO. 1 IS PURGED')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==77 ) THEN
               WRITE (Outtap,99081) Uwm
99081          FORMAT (A25,' 2077, SDR2 OUTPUT DATA BLOCK NO. 2 IS PURGED')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==78 ) THEN
               WRITE (Outtap,99082) Uwm
99082          FORMAT (A25,' 2078, SDR2 OUTPUT DATA BLOCK NO. 3 IS PURGED')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==79 ) THEN
               WRITE (Outtap,99083) Uwm
99083          FORMAT (A25,' 2079, SDR2 FINDS THE -EDT-, -EST-, OR -GPTT- ','PURGED OR INADEQUATE AND IS THUS NOT PROCESSING',/5X,  &
                      &'ANY REQUESTS FOR STRESSES OR FORCES.')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==80 ) THEN
               WRITE (Outtap,99084) Uwm
99084          FORMAT (A25,' 2080, SDR2 OUTPUT DATA BLOCK NO. 6 IS PURGED')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==81 ) THEN
               WRITE (Outtap,99085) Ufm
99085          FORMAT (A23,' 2081, DIFFERENTIAL STIFFNESS CAPABILITY NOT ','DEFINED FOR ANY OF THE ELEMENT TYPES IN THE PROBLEM.')
            ELSEIF ( l==82 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==83 ) THEN
               WRITE (Outtap,99086) Ufm
99086          FORMAT (A23,' 2083, NULL DISPLACEMENT VECTOR')
            ELSEIF ( l==84 ) THEN
               WRITE (Outtap,99087) Ufm , p1
99087          FORMAT (A23,' 2084, DSMG2 LOGIC ERROR',I9)
            ELSEIF ( l==85 ) THEN
               WRITE (Outtap,99088) Uim , p1 , p2
99088          FORMAT (A29,' 2085, ',A4,' SPILL, NPVT',I9)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==86 ) THEN
               WRITE (Outtap,99089) Uim , p1
99089          FORMAT (A29,' 2086, SMA2 SPILL, NPVT',I9)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==87 ) THEN
               WRITE (Outtap,99090) Sfm
99090          FORMAT (A25,' 2087, ECPT CONTAINS BAD DATA')
            ELSEIF ( l==88 ) THEN
               WRITE (Outtap,99091) Ufm , p1
99091          FORMAT (A23,' 2088, DUPLICATE TABLE ID',I9)
            ELSEIF ( l==89 ) THEN
               WRITE (Outtap,99092) Ufm , p1
99092          FORMAT (A23,' 2089, TABLE',I9,' UNDEFINED')
            ELSEIF ( l==90 ) THEN
               WRITE (Outtap,99093) Sfm , p1
99093          FORMAT (A25,' 2090, TABLE DICTIONARY ENTRY',I9,' MISSING')
            ELSEIF ( l==91 ) THEN
               WRITE (Outtap,99094) Sfm , p1
99094          FORMAT (A25,' 2091, PLA3, BAD ESTNL ELEMENT ID',I9)
            ELSEIF ( l==92 ) THEN
               WRITE (Outtap,99095) Swm , p1 , p2
99095          FORMAT (A27,' 2092, SDR2 FINDS A SYMMETRY SEQUENCE LENGTH =',I20,/5X,                                                &
                      &'AND AN INSUFFICIENT NUMBER OF VECTORS AVAILABLE=',I21,' WHILE ATTEMPTING TO COMPUTE STRESSES AND FORCES.',  &
                     & /5X,'ALL FURTHER STRESS AND FORCE COMPUTATION TERMINATED.')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==93 ) THEN
               WRITE (Outtap,99096) Ufm , p1 , p2
99096          FORMAT (A23,' 2093, NOLIN CARD FROM NOLIN SET',I9,' REFERENCES GRID POINT',I9,' UD SET.')
            ELSEIF ( l==94 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==95 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==96 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==97 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==98 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==99 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==100 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==101 ) THEN
               j = p2/10
               a = p2 - 10*j
               k = blank
               IF ( a/=0 ) k = a
               ja = j/10
               b = j - 10*ja
               um = blank
               IF ( b/=0 ) um = bcd(1)
               jb = ja/10
               c = ja - 10*jb
               us = blank
               IF ( c/=0 ) us = bcd(2)
               jc = jb/10
               d = jb - 10*jc
               uo = blank
               IF ( d/=0 ) uo = bcd(3)
               jd = jc/10
               e = jc - 10*jd
               ur = blank
               IF ( e/=0 ) ur = bcd(4)
               jf = jd/10
               f = jd - 10*jf
               ul = blank
               IF ( f/=0 ) ul = bcd(5)
               IF ( a==0 ) WRITE (Outtap,99097) Ufm , p1 , um , us , uo , ur , ul
99097          FORMAT (A23,' 2101B, SCALAR POINT',I9,' ILLEGALLY DEFINED IN ','SETS',5(2X,A4))
               IF ( a/=0 ) WRITE (Outtap,99098) Ufm , p1 , k , um , us , uo , ur , ul
99098          FORMAT (A23,' 2101A, GRID POINT',I9,' COMPONENT',I2,' ILLEGALLY DEFINED IN SETS',5(2X,A4))
            ELSEIF ( l==102 ) THEN
               WRITE (Outtap,99099) Uwm , p2 , p1
99099          FORMAT (A25,' 2102, LEFT HAND MATRIX ROW POSITION',I9,' OUT OF RANGE - IGNORED')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==103 ) THEN
               WRITE (Outtap,99100) Sfm
99100          FORMAT (A25,' 2103, SUBROUTINE MAT WAS CALLED WITH INFLAG=2, THE',' SINE OF THE ANGLE X',/5X,                        &
                      &' MATERIAL ORIENTATION ANGLE,',' NON-ZERO, BUT SIN(X)**2+COS(X)**2 DIFFERED FROM 1 IN ',                     &
                      &'ABSOLUTE VALUE BY MORE THAN .0001')
            ELSEIF ( l==104 ) THEN
               WRITE (Outtap,99101) Ufm , p1
99101          FORMAT (A23,' 2104, UNDEFINED COORDINATE SYSTEM',I9)
            ELSEIF ( l==105 ) THEN
               WRITE (Outtap,99102) Ufm , p1 , p2
99102          FORMAT (A23,' 2105, PLOAD2 CARD FROM LOAD SET',I9,' REFERENCES MISSING OR NON-2-D ELEMENT',I9)
            ELSEIF ( l==106 ) THEN
               WRITE (Outtap,99103) Ufm , p1
99103          FORMAT (A23,' 2106, LOAD CARD DEFINES NON-UNIQUE LOAD SET',I9)
            ELSEIF ( l==107 ) THEN
               WRITE (Outtap,99104) Ufm , p1 , p2
99104          FORMAT (A23,' 2107, EIG- CARD FROM SET',I9,' REFERENCES DEPENDENT COORDINATE OF GRID POINT',I9)
            ELSEIF ( l==108 ) THEN
               WRITE (Outtap,99105) Ufm , p1 , p2
99105          FORMAT (A23,' 2108, SPCD ON A POINT NOT IN S SET. GRID',I9,' COMP.',I9)
            ELSEIF ( l==109 ) THEN
               WRITE (Outtap,99106) Ufm
99106          FORMAT (A23,' 2109, NO GRID, SCALAR OR EXTRA POINTS DEFINED')
            ELSEIF ( l==110 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==111 ) THEN
               WRITE (Outtap,99107) Uwm , p1
99107          FORMAT (A25,' 2111, BAR',I9,' COUPLED BENDING INERTIA SET TO 0.0',' IN DIFFERENTIAL STIFFNESS')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==112 ) THEN
               WRITE (Outtap,99108) Ufm , p1
99108          FORMAT (A23,' 2112, UNDEFINED TABLE',I9)
            ELSEIF ( l==113 ) THEN
               WRITE (Outtap,99109) Ufm , p1
99109          FORMAT (A23,' 2113, MATERIAL',I9,', A NON-MAT1 TYPE, IS NOT ','ALLOWED TO BE STRESS-DEPENDENT')
            ELSEIF ( l==114 ) THEN
               WRITE (Outtap,99110) Ufm , p1
99110          FORMAT (A23,' 2114, MATT3 CARD REFERENCES UNDEFINED MAT3',I9,' CARD')
            ELSEIF ( l==115 ) THEN
               WRITE (Outtap,99111) Ufm , p1 , p2
99111          FORMAT (A23,' 2115, TABLE',I9,' (TYPE',I9,') ILLEGAL WITH STRESS','-DEPENDENT MATERIAL')
            ELSEIF ( l==116 ) THEN
               WRITE (Outtap,99112) Sfm , p2 , p1
99112          FORMAT (A25,' 2116, MATID',I9,' TABLEID',I9)
            ELSEIF ( l==117 ) THEN
               WRITE (Outtap,99113) Ufm , p1
99113          FORMAT (A23,' 2117, TEMPERATURE DEPENDENT MATERIAL PROPERTIES ','ARE NOT PERMISSIBLE',/5X,'IN A PIECEWISE LINEAR ',  &
                      &'ANALYSIS PROBLEM.  TEMPERATURE SET =',I9)
            ELSEIF ( l==118 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==119 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( l==120 ) THEN
               WRITE (Outtap,99152) l , p1 , p2
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( local==1 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==2 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==3 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==4 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==5 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==6 ) THEN
            WRITE (Outtap,99114) Ufm , p1
99114       FORMAT (A23,' 2126, UNDEFINED MATERIAL FOR ELEMENT',I9)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==7 ) THEN
            WRITE (Outtap,99115) Sfm , p1
99115       FORMAT (A25,' 2127, PLA2 INPUT DATA BLOCK NO.',I9,' IS PURGED.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==8 ) THEN
            WRITE (Outtap,99116) Sfm , p1
99116       FORMAT (A25,' 2128, PLA2 OUTPUT DATA BLOCK NO.',I9,' IS PURGED.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==9 ) THEN
            WRITE (Outtap,99117) Sfm , p1
99117       FORMAT (A25,' 2129, PLA2, ZERO VECTOR ON APPENDED DATA BLOCK NO.',I9)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==10 ) THEN
            WRITE (Outtap,99118) Ufm
99118       FORMAT (A23,' 2130, ZERO INCREMENTAL DISPLACEMENT VECTOR INPUT ','TO MODULE PLA2.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==11 ) THEN
            WRITE (Outtap,99119) Ufm , p1
99119       FORMAT (A23,' 2131, NON-SCALAR ELEMENT',I9,' REFERENCES A SCALAR POINT.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==12 ) THEN
            WRITE (Outtap,99120) Ufm
99120       FORMAT (A23,' 2132, NON-ZERO SINGLE POINT CONSTRAINT VALUE ','SPECIFIED BUT DATA BLOCK YS IS PURGED.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==13 ) THEN
            WRITE (Outtap,99121) Ufm , p1
99121       FORMAT (A23,' 2133, INITIAL CONDITION IN SET',I9,' SPECIFIED FOR POINT NOT IN ANALYSIS SET.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==14 ) THEN
            WRITE (Outtap,99122) Ufm , p1
99122       FORMAT (A23,' 2134, LOAD SET',I9,' DEFINED FOR BOTH GRAVITY AND ','NON-GRAVITY LOADS.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==15 ) THEN
            WRITE (Outtap,99123) Ufm , p1 , p2
99123       FORMAT (A23,' 2135, DLOAD CARD',I9,' HAS A DUPLICATE SET ID FOR ','SET ID',I9)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==16 ) THEN
            WRITE (Outtap,99124) Ufm , p1
99124       FORMAT (A23,' 2136, SET ID',I9,' HAS BEEN DUPLICATED ON A DLOAD,',' RLOAD1,2 OR TLOAD1,2 CARD.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==17 ) THEN
            WRITE (Outtap,99125) Ufm , p1 , p2
99125       FORMAT (A23,' 2137, PROGRAM RESTRICTION FOR MODULE ',A4,'.  ONLY 360 LOAD SET ID-S.',/5X,'ALLOWED.  DATA CONTAINS',I9,  &
                   &' LOAD SET ID-S.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==18 ) THEN
            WRITE (Outtap,99126) Ufm , p1
99126       FORMAT (A23,' 2138, ELEMENT ID NO.',I9,' IS TOO LARGE')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==19 ) THEN
            WRITE (Outtap,99127) Ufm , p1 , p2
99127       FORMAT (A23,' 2139, ELEMENT',I9,' IN DEFORM SET',I9,' IS UNDEFINED.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==20 ) THEN
            WRITE (Outtap,99128) Ufm , p1
99128       FORMAT (A23,' 2140, GRID OR SCALAR POINT ID',I9,', EXCEEDING MAX',' OF 2140000, COULD BE FATAL')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==21 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==22 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==23 ) THEN
            WRITE (Outtap,99129) Ufm , p1
99129       FORMAT (A23,' 2143, SINGULAR JACOBIAN MATRIX FOR ISOPARAMETRIC ','ELEMENT NO.',I9)
         ELSEIF ( local==24 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==25 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==26 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==27 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==28 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==29 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==30 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==31 ) THEN
            index = p2/100000000
            p2 = p2 - index*100000000
            WRITE (Outtap,99130) Ufm , p1 , icrigd(index) , p2
99130       FORMAT (A23,' 2192, UNDEFINED GRID POINT',I9,' IN RIGD',A1,' ELEMENT',I9)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==32 ) THEN
            WRITE (Outtap,99131) Ufm
99131       FORMAT (A23,' 2193, A REDUNDANT SET OF RIGID BODY MODES WAS ','SPECIFIED FOR THE GENERAL ELEMENT')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==33 ) THEN
            WRITE (Outtap,99132) Ufm
99132       FORMAT (A23,' 2194, A MATRIX D IS SINGULAR IN SUBROUTINE TA1CA')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==34 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==35 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==36 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==37 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==38 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==39 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==40 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==41 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==42 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==43 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==44 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==45 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==46 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==47 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==48 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==49 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==50 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==51 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==52 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==53 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==54 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==55 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==56 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==57 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==58 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==59 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==60 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==61 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==62 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==63 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==64 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==65 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==66 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==67 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==68 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==69 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==70 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==71 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==72 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==73 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==74 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==75 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==76 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==77 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==78 ) THEN
            WRITE (Outtap,99133) Ufm , p1
99133       FORMAT (A23,' 2198, INPUT DATA BLOCK',I9,' HAS BEEN PURGED.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==79 ) THEN
            WRITE (Outtap,99134) Sfm , p1 , p2
99134       FORMAT (A25,' 2199, SUMMARY',/5X,'ONE OR MORE OF THE ABOVE ','FATAL ERRORS WAS ENCOUNTERED IN SUBROUTINE ',2A4)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==80 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==81 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==82 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==83 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==84 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==85 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==86 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==87 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==88 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==89 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==90 ) THEN
            WRITE (Outtap,99152) l , p1 , p2
         ELSEIF ( local==91 ) THEN
            WRITE (Outtap,99135) Ufm , p1
99135       FORMAT (A23,' 2355, GRID POINT COORDINATES OF ELEMENT',I9,' ARE IN ERROR.',/5X,                                         &
                   &'ONE OR MORE OF THE R-COORDINATES ARE ZERO OR NEGATIVE.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==92 ) THEN
            WRITE (Outtap,99136) Ufm , p1
99136       FORMAT (A23,' 2364, GRID POINT COORDINATES OF ELEMENT',I9,' ARE IN ERROR.',/5X,                                         &
                   &'ONE OR MORE OF THE THETA-COORDINATES ARE NONZERO.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==93 ) THEN
            WRITE (Outtap,99137) Ufm , p1
99137       FORMAT (A23,' 2213, MATERIAL ID',I9,' NOT UNIQUELY DEFINED.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==94 ) THEN
            WRITE (Outtap,99138) Ufm , p2 , p2 , p1
99138       FORMAT (A23,' 2214, MATT',I1,' CARD REFERENCES UNDEFINED MAT',I1,I9,' CARD')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==95 ) THEN
            WRITE (Outtap,99139) Ufm , p1 , p2
99139       FORMAT (A23,' 2215, UNDEFINED MATERIAL ID',I9,' WAS REFERENCED BY PROPERTY CARD ID',I9)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==96 ) THEN
            WRITE (Outtap,99140) Ufm , p2 , p2 , p1
99140       FORMAT (A23,' 2216, MATPZT',I1,' CARD REFERENCES UNDEFINED MATPZ',I1,I9,' CARD')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==97 ) THEN
            WRITE (Outtap,99141) Ufm , p1
99141       FORMAT (A23,' 2217, MATPZ1 ID',I9,' HAS SINGULAR SE MATRIX.')
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==98 ) THEN
            WRITE (Outtap,99145) Ufm , p2 , ax , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==99 ) THEN
            WRITE (Outtap,99142) Ufm
99142       FORMAT (A23,' 2219, MAT6 CARDS REQUIRE REPROCESSING. RE-SUBMIT ','JOB WITH THE FOLLOWING DMAP ALTER (AFTER GP1)',//10X, &
                   &'ANISOP  GEOM1,EPT,BGPDT,EQEXIN,MPT/MPTA/S,N,ISOP $',/10X,'EQUIV   MPTA,MPT/ISOP $',/)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==100 ) THEN
            WRITE (Outtap,99143) Uim , p1 , p2
99143       FORMAT (A29,' 2220, NO APPLICABLE ELEMENT OR SUBCASE DURING OUT','PUT SCAN',/5X,                                        &
                   &'EITHER NO VALUES OUTSIDE MAX-MIN RANGE ','OR NOT IN SET SPECIFIED FOR ',2A4)
         ELSEIF ( local==101 ) THEN
            WRITE (Outtap,99145) Ufm , p2 , rg , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==102 ) THEN
            WRITE (Outtap,99144) Uwm , p1 , p2
99144       FORMAT (A25,' 2222, METHOD OF NORMALIZATION ON ',A4,' CARD NOT ','SPECIFIED. DEFAULT OF ''',A4,''' WILL BE USED')
         ELSEIF ( local==103 ) THEN
            WRITE (Outtap,99146) Sfm , tria3 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==104 ) THEN
            WRITE (Outtap,99147) Ufm , tria3 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==105 ) THEN
            WRITE (Outtap,99148) Ufm , p2 , tria3 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==106 ) THEN
            WRITE (Outtap,99149) Ufm , p2 , tria3 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==107 ) THEN
            WRITE (Outtap,99150) Ufm , p2 , tria3 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==108 ) THEN
            WRITE (Outtap,99151) Sfm , tria3
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==109 ) THEN
            WRITE (Outtap,99146) Sfm , quad4 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==110 ) THEN
            WRITE (Outtap,99147) Ufm , quad4 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==111 ) THEN
            WRITE (Outtap,99148) Ufm , p2 , quad4 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==112 ) THEN
            WRITE (Outtap,99149) Ufm , p2 , quad4 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==113 ) THEN
            WRITE (Outtap,99150) Ufm , p2 , quad4 , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( local==114 ) THEN
            WRITE (Outtap,99151) Sfm , quad4
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            WRITE (Outtap,99002) Ufm , p1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     MESSAGE IS FATAL.
!     IF DIAG 1 IS ON, AND MACHINE IS VAX AND UNIX, CALL ERROR TRACEBACK
!
         IF ( Mach>4 ) THEN
            CALL sswtch(1,j)
            IF ( j==1 ) CALL errtrc('USRWRT  ',l)
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!
99002 FORMAT (A23,' 2001, SEQGP CARD REFERENCES UNDEFINED GRID POINT',I9)
99145 FORMAT (A23,' 2218, ',A4,A2,' ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM RADIUS RATIO EXCEEDING 10.',/5X,                         &
             &'ACCURACY OF NUMERICAL INTEGRATION WOULD BE IN DOUBT.')
99146 FORMAT (A25,' 3223 NO PCOMP, PCOMP1 OR PCOMP2 PROPERTY DATA ','FOUND FOR ',A6,' ELEMENT ID =',I9)
99147 FORMAT (A23,' 2224, ',A6,' ELEMENT ID =',I9,' HAS ILLEGAL GEOMETRY OR CONNECTIONS')
99148 FORMAT (A23,' 2225, THE X-AXIS OF THE MATERIAL COORDINATE SYSTEM',' ID =',I9,' HAS NO PROJECTION ON TO THE PLANE OF THE',/5X, &
            & A6,' ELEMENT ID =',I9)
99149 FORMAT (A23,' 2226, ILLEGAL DATA DETECTED ON MATERIAL ID =',I9,' REFERENCED BY ',A6,' ELEMENT ID =',I9,/5X,                   &
             &'FOR MID3 APPLICATION')
99150 FORMAT (A23,' 2228, THE X-AXIS OF THE STRESS COORDINATE SYSTEM ','ID =',I9,' HAS NO PROJECTION ON TO THE PLANE OF THE',/5X,A6,&
             &' ELEMENT ID =',I9)
99151 FORMAT (A25,' 3008, INSUFFICIENT MEMORY IS AVAIL ABLE FOR ',A6,' ELEMENTS GENERATION.  RE-RUN JOB WITH AN ADDITIONAL',/5X,    &
             &'2000 WORDS OF MEMORY')
!
99152 FORMAT ('0*** UNASSIGNED MESSAGE (L=',I3,'), P1=',I20,', P2=',I9)
END SUBROUTINE usrmsg
