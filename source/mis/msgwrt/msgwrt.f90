!*==msgwrt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE msgwrt
   USE c_machin
   USE c_msgx
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , l , lplus
   INTEGER , DIMENSION(2) , SAVE :: ipag , neg , pos
   INTEGER , DIMENSION(2) :: name , png
   INTEGER , SAVE :: nmsgs
   REAL , DIMENSION(4,1) :: xmsg
   EXTERNAL errtrc , fname , msguni , page2 , usrmsg
!
! End of declarations rewritten by SPAG
!
!
!     MSGWRT WILL PRINT THE INDICATED ERROR MESSAGES ON THE OUTPUT TAPE
!
   !>>>>EQUIVALENCE (Xmsg(1,1),Msg(1,1))
   DATA pos , neg/4HWARN , 4HING  , 4HFATA , 4HL   /
   DATA nmsgs/117/ , ipag/4H PAG , 4HE2  /
!
   DO i = 1 , n
      l = iabs(msg(1,i))
      IF ( mach==3 .AND. l>=1125 .AND. l<=1320 ) THEN
         CALL msguni(l)
      ELSE
!
!     *** NOTE ***  CHANGE IF STATEMENT WHEN YOU CHANGE GO TO
!                   MAKE SURE MESSAGE NO. IS WITHIN GO TO RANGE
!
         IF ( l>nmsgs ) THEN
            WRITE (outtap,99001) msg(1,i) , msg(2,i) , msg(3,i) , msg(4,i)
99001       FORMAT ('0NO MESSAGE FOR MESSAGE NO. =',I5,/5X,'PARAMETERS = ',3I20)
         ELSE
            IF ( l/=30 ) THEN
               IF ( msg(3,i)/=ipag(1) .AND. msg(4,i)/=ipag(2) ) CALL page2(4)
               IF ( l<71 .OR. l>nmsgs ) THEN
!
!     --- NOTE --- INCREASE THE UPPER LIMIT TO ADD MORE MESSAGES
!
                  lplus = l + 3000
                  DO j = 1 , 2
                     png(j) = pos(j)
                     IF ( msg(1,i)<0 ) png(j) = neg(j)
                  ENDDO
                  CALL fname(msg(2,i),name)
!
                  IF ( l==1 ) THEN
!
!    *** END OF GINO ERRORS SECTION
!
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99002) msg(2,i) , msg(3,i) , msg(4,i)
!
!
! 100 FORMAT ('0FATAL ERROR')
99002                FORMAT ('0ATTEMPT TO OPEN DATA SET',I4,' IN SUBROUTINE ',A4,A2,', WHICH WAS NOT DEFINED IN THE FIST')
                  ELSEIF ( l==2 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99003) name , msg(2,i) , msg(3,i) , msg(4,i)
99003                FORMAT ('0EOF ENCOUNTERED WHILE READING DATA SET ',2A4,'(FILE',I4,') IN SUBROUTINE ',2A4)
                  ELSEIF ( l==3 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99004) name , msg(2,i) , msg(3,i) , msg(4,i)
99004                FORMAT ('0ATTEMPT TO READ PAST THE END OF A LOGICAL RECORD IN ','DATA SET ',2A4,'(FILE',I4,') IN SUBROUTINE ', &
                           & 2A4)
                  ELSEIF ( l==4 ) THEN
                     WRITE (outtap,99125) sfm , lplus
                     WRITE (outtap,99005) name
99005                FORMAT ('0INCONSISTENT TYPE FLAGS ENCOUNTERED WHILE PACKING DATA','SET ',2A4)
                  ELSEIF ( l==5 ) THEN
                     WRITE (outtap,99124) png , lplus
                     WRITE (outtap,99006) name , msg(3,i) , msg(4,i)
99006                FORMAT ('0ATTEMPT TO OPERATE ON THE SINGULAR MATRIX ',2A4,' IN SUBROUTINE ',2A4)
                  ELSEIF ( l==6 ) THEN
                     WRITE (outtap,99007) sfm , msg(3,i) , msg(4,i) , msg(2,i)
99007                FORMAT (A25,' 3006, BUFFER ASSIGNED WHEN OPENING DATA BLOCK ',2A4,6H,FILE ,I5,1H,,/5X,                         &
                            &'CONFLICTS WITH BUFFERS CURRENTLY ','OPEN.')
                  ELSEIF ( l==7 ) THEN
                     WRITE (outtap,99125) sfm , lplus
                     WRITE (outtap,99008) msg(3,i) , msg(4,i)
99008                FORMAT ('0ILLEGAL INPUT TO SUBROUTINE ',2A4)
                     IF ( msg(1,i)<0 ) CALL errtrc('MSGWRT  ',0)
                  ELSEIF ( l==8 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99009) msg(3,i) , msg(4,i)
99009                FORMAT ('0INSUFFICIENT CORE AVAILABLE FOR SUBROUTINE ',2A4)
                     IF ( msg(2,i)>0 ) WRITE (outtap,99010) msg(2,i)
99010                FORMAT (' ADDITIONAL CORE REQUIRED =',I10,' WORDS.')
                     j = -msg(2,i)
                     IF ( j>0 ) WRITE (outtap,99011) j
99011                FORMAT (' PRESENT OPEN CORE SIZE =',I10,' WORDS.')
                     IF ( mach==3 .OR. mach==5 ) WRITE (outtap,99012)
99012                FORMAT (' USE NASTRAN HICORE CARD TO INCREASE CORE SIZE')
                  ELSEIF ( l==9 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99013) name , msg(2,i)
99013                FORMAT ('0DATA TRANSMISSION ERROR ON DATA SET ',2A4,'(FILE',I4,1H))
                  ELSEIF ( l==10 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99014) name , msg(2,i)
99014                FORMAT ('0ATTEMPT TO MANIPULATE DATA SET ',2A4,'(FILE',I4,' BEFORE OPENING THE FILE')
                  ELSEIF ( l==11 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99015) msg(2,i)
99015                FORMAT ('0ATTEMPT TO WRITE A TRAILER ON FILE',I4,' WHEN IT HAS BEEN PURGED')
                  ELSEIF ( l==12 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99016) name , msg(2,i)
99016                FORMAT ('0ATTEMPT TO OPEN DATA SET ',2A4,'(FILE',I4,') WHICH HAS ALREADY BEEN OPENED')
                  ELSEIF ( l==13 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99017) name , msg(2,i)
99017                FORMAT ('0ATTEMPT TO READ DATA SET ',2A4,'(FILE',I4,') WHEN IT WAS OPENED FOR OUTPUT')
                  ELSEIF ( l==14 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99018) name , msg(2,i)
99018                FORMAT ('0ATTEMPT TO WRITE DATA SET ',2A4,'(FILE',I4,') WHEN IT WAS OPENED FOR INPUT')
                  ELSEIF ( l==15 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99019) name , msg(2,i)
99019                FORMAT ('0ATTEMPT TO FWDREC ON DATA SET ',2A4,'(FILE',I4,') WHEN IT WAS OPENED FOR OUTPUT')
                  ELSEIF ( l==16 ) THEN
                     WRITE (outtap,99125) sfm , lplus
                     WRITE (outtap,99020) name , msg(3,i) , msg(4,i)
99020                FORMAT (1H0,2A4,' MATRIX IS NOT IN PROPER FORM IN SUBROUTINE ',2A4)
                  ELSEIF ( l==17 ) THEN
                     WRITE (outtap,99126) uwm , lplus
                     IF ( msg(2,i)==0 ) WRITE (outtap,99021)
99021                FORMAT ('0    ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT BEEN ',                                             &
                            &'REMOVED BY SINGLE OR MULTI-POINT CONSTRAINTS.',/5X,                                                   &
                            &'(USER COULD REQUEST NASTRAN AUTOMATIC SPC GENERATION ','VIA A ''PARAM AUTOSPC'' BULK DATA CARD)')
                     IF ( msg(2,i)/=0 ) WRITE (outtap,99022)
99022                FORMAT ('0    ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT BEEN ',                                             &
                            &'REMOVED BY SINGLE OR MULTI-POINT CONSTRAINTS.')
                  ELSEIF ( l==18 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99023) msg(3,i) , msg(4,i) , msg(2,i)
99023                FORMAT ('0MODULE ',2A4,', SEQUENCE NO.',I5,', REQUIREMENTS EXCEED AVAILABLE FILES')
                  ELSEIF ( l==19 ) THEN
                     WRITE (outtap,99127) ufm , lplus
                     WRITE (outtap,99024) msg(3,i) , msg(4,i) , msg(2,i)
99024                FORMAT ('0MAXIMUM LINE COUNT EXCEEDED IN SUBROUTINE ',2A4,' LINE COUNT EQUALS',I8)
                  ELSEIF ( l==20 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99025) msg(2,i) , msg(3,i) , msg(4,i)
99025                FORMAT ('0GNFIST OVERFLOWED FIST TABLE AT SEQUENCE NO.',I5,'  DATA SET ',2A4)
                  ELSEIF ( l==21 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99026) msg(2,i)
99026                FORMAT ('0FILE',I4,' NOT DEFINED IN FIST')
                  ELSEIF ( l==22 ) THEN
                     WRITE (outtap,99027) swm , lplus
99027                FORMAT (A27,I5)
                     WRITE (outtap,99028)
99028                FORMAT (1H+,33X,'(SEE PROG. MANUAL SEC. 4.9.7, OR ',7HUSERS' ,'MANUAL P. 6.5-3)')
                     WRITE (outtap,99029) msg(3,i) , msg(4,i)
99029                FORMAT (5X,'DATA BLOCK ',2A4,' MAY BE REQUIRED AS INPUT AND IS ',                                              &
                            &'NOT OUTPUT BY A PREVIOUS MODULE IN THE CURRENT DMAP ','ROUTE.')
                  ELSEIF ( l==23 ) THEN
                     WRITE (outtap,99030) uim , msg(2,i) , msg(3,i) , msg(4,i)
99030                FORMAT (A29,' 3028   B =',I5,' C =',I5,' R =',I5)
                  ELSEIF ( l==24 ) THEN
                     WRITE (outtap,99128) uim , lplus
                     WRITE (outtap,99031) name , msg(3,i)
99031                FORMAT ('0THE BANDWIDTH OF MATRIX ',2A4,' EXCEEDS THE MAXIMUM ','BANDWIDTH. A MAXIMUM BANDWIDTH OF',I5,        &
                            &' WILL BE USED')
                  ELSEIF ( l==25 ) THEN
                     WRITE (outtap,99125) sfm , lplus
                     WRITE (outtap,99032) msg(3,i) , msg(4,i)
99032                FORMAT ('0ILLEGAL INDEX IN ACTIVE ROW OR COLUMN CALCULATION IN ',2A4)
                  ELSEIF ( l==26 ) THEN
                     WRITE (outtap,99125) sfm , lplus
                     WRITE (outtap,99033) name , msg(3,i) , msg(4,i)
99033                FORMAT ('0MATRIX ',2A4,' EXCEEDS MAXIMUM ALLOWABLE SIZE FOR BAND','WIDTH PLUS ACTIVE COLUMNS. BMAX =',I6,      &
                            &' CMAX =',I6)
                  ELSEIF ( l==27 ) THEN
                     WRITE (outtap,99128) uim , lplus
                     WRITE (outtap,99034) msg(2,i)
99034                FORMAT ('0DECOMPOSITION TIME ESTIMATE IS',I6)
                  ELSEIF ( l==28 ) THEN
                     WRITE (outtap,99035) uim , msg(2,i) , msg(3,i) , msg(4,i)
99035                FORMAT (A29,' 3028, BBAR =',I5,' CBAR =',I5,' R =',I5)
                  ELSEIF ( l==29 ) THEN
                     WRITE (outtap,99036) sfm , name , msg(2,i)
99036                FORMAT (A25,' 3029, PHYSICAL EOF ENCOUNTERED ON DATA SET ',2A4,' (FILE',I4,3H ).)
                  ELSEIF ( l==30 ) THEN
                     GOTO 10
                  ELSEIF ( l==31 ) THEN
                     GOTO 20
                  ELSEIF ( l==32 ) THEN
                     GOTO 20
                  ELSEIF ( l==33 ) THEN
                     WRITE (outtap,99037) ufm , msg(2,i)
99037                FORMAT (A23,' 3033, SUBCASE ID',I9,' IS REFERENCED ON ONE OR MORE',' RANDPS CARDS',/5X,                        &
                            &'BUT IS NOT A CURRENT SUBCASE ID.')
                  ELSEIF ( l==34 ) THEN
                     WRITE (outtap,99126) uwm , lplus
                     WRITE (outtap,99038) xmsg(2,i) , xmsg(3,i)
99038                FORMAT ('0ORTHOGONALITY CHECK FAILED, LARGEST TERM = ',1P,E14.7,', EPSILON = ',1P,E14.7)
                  ELSEIF ( l==35 ) THEN
                     WRITE (outtap,99128) uim , lplus
                     WRITE (outtap,99039) msg(2,i) , xmsg(3,i)
99039                FORMAT (5X,'FOR SUBCASE NUMBER',I6,', EPSILON SUB E = ',1P,E15.7)
                  ELSEIF ( l==36 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99040) msg(3,i) , msg(4,i)
99040                FORMAT ('0DATA SET ',2A4,' IS REQUIRED AS INPUT BUT HAS NOT ','BEEN GENERATED OR PURGED')
                  ELSEIF ( l==37 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99041) msg(3,i) , msg(4,i)
99041                FORMAT ('0JOB TERMINATED IN SUBROUTINE ',2A4)
                  ELSEIF ( l==38 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99042) msg(2,i)
99042                FORMAT ('0DATA SET ',A4,' DOES NOT HAVE MULTI-REEL CAPABILITY')
                  ELSEIF ( l==39 ) THEN
                     WRITE (outtap,99043) sfm
99043                FORMAT (A25,' 3039, ENDSYS CANNOT FIND SAVE FILE.')
                  ELSEIF ( l==40 ) THEN
                     WRITE (outtap,99123) png , lplus
                     WRITE (outtap,99044) name , msg(2,i)
99044                FORMAT ('0ATTEMPT TO WRITE DATA SET ',2A4,'(FILE',I4,') WHEN IT IS AN INPUT FILE')
                  ELSEIF ( l==41 ) THEN
                     WRITE (outtap,99126) uwm , lplus
                     WRITE (outtap,99045) msg(2,i)
99045                FORMAT ('0EXTERNAL GRID POINT',I9,' DOES NOT EXIST OR IS NOT A ','GEOMETRIC GRID POINT.',/5X,                  &
                            &'THE BASIC ORIGIN WILL BE USED.')
                  ELSEIF ( l==42 ) THEN
                     WRITE (outtap,99126) uwm , lplus
                     WRITE (outtap,99046) xmsg(2,i)
99046                FORMAT ('0INCONSISTENT SCALAR MASSES HAVE BEEN USED.  EPSILON/','DELTA = ',1P,E15.7)
                  ELSEIF ( l==43 ) THEN
                     WRITE (outtap,99127) ufm , lplus
                     WRITE (outtap,99047) msg(2,i) , msg(3,i) , msg(4,i)
99047                FORMAT ('0UNCONNECTED EXTRA POINT (MODAL COORDINATE =',I9,') HAS BEEN DETECTED BY SUBROUTINE ',2A4)
                  ELSEIF ( l==44 ) THEN
                     WRITE (outtap,99127) ufm , lplus
                     WRITE (outtap,99048) msg(2,i) , msg(3,i)
99048                FORMAT ('0A POINT ON NON-LINEAR LOAD SET',I9,' NOLIN',I1,' IS NOT AN EXTRA POINT.',/5X,                        &
                           & 'ONLY EXTRA POINTS MAY ','HAVE NON-LINEAR LOADS IN A MODAL FORMULATION.')
                  ELSEIF ( l==45 ) THEN
                     WRITE (outtap,99049) uwm , msg(2,i) , msg(3,i) , msg(4,i)
99049                FORMAT (A25,' 3045, INSUFFICIENT TIME TO COMPLETE THE REMAINING',I6,' SOLUTION(S) IN MODULE ',2A4)
                  ELSEIF ( l==46 ) THEN
                     WRITE (outtap,99050) ufm
99050                FORMAT (A23,' 3046, YOUR SELECTED LOADING CONDITION, INITIAL ','CONDITION, AND NON-LINEAR FORCES ARE NULL',/5X,&
                            &'A ZERO SOLUTION WILL RESULT.')
                  ELSEIF ( l==47 ) THEN
                     WRITE (outtap,99051) ufm
99051                FORMAT (A23,' 3047, NO MODES WITHIN RANGE AND LMODES = 0. A MODAL',' FORMULATION CANNOT BE MADE.')
                  ELSEIF ( l==48 ) THEN
                     WRITE (outtap,99052) sfm , msg(2,i) , msg(3,i) , msg(4,i)
99052                FORMAT (A25,' 3048, BUFFER CONTROL WORD INCORRECT FOR GINO ',A4,' OPERATION ON DATA BLOCK ',2A4)
                  ELSEIF ( l==49 ) THEN
                     WRITE (outtap,99053) sfm , msg(3,i) , msg(4,i) , msg(2,i)
99053                FORMAT (A25,' 3049, GINO UNABLE TO POSITION DATA BLOCK ',2A4,' CORRECTLY DURING ',A4,' OPERATION.')
                  ELSEIF ( l==50 ) THEN
                     WRITE (outtap,99054) sfm , msg(3,i) , msg(4,i) , msg(2,i)
99054                FORMAT (A25,' 3050, INSUFFICIENT TIME REMAINING FOR ',2A4,'.  TIME ESTIMATE IS',I9,' SECONDS.')
                  ELSEIF ( l==51 ) THEN
                     WRITE (outtap,99055) ufm , msg(2,i)
99055                FORMAT (A23,' 3051, INITIAL CONDITION SET',I9,' WAS SELECTED FOR',' A MODAL TRANSIENT PROBLEM.',/5X,           &
                            &'INITIAL CONDITIONS ARE NOT ALLOWED IN SUCH A PROBLEM.')
                  ELSEIF ( l==52 ) THEN
                     WRITE (outtap,99056) uwm , msg(2,i) , msg(3,i) , msg(4,i)
99056                FORMAT (A25,' 3052, A RANDOM REQUEST FOR CURVE TYPE - ',A4,' -, POINT -',I9,/5X,'COMPONENT -',I4,              &
                            &' -, SPECIFIES TOO LARGE A COMPONENT ID.  THE LAST ','COMPONENT WILL BE USED.')
                  ELSEIF ( l==53 ) THEN
                     WRITE (outtap,99057) uwm , msg(2,i) , msg(3,i)
99057                FORMAT (A25,' 3053, THE ACCURACY OF EIGENVALUE',I6,' IS IN DOUBT.',' GIVENS-QR FAILED TO CONVERGE IN',I4,      &
                            &' ITERATIONS.')
                  ELSEIF ( l==54 ) THEN
                     WRITE (outtap,99058) uwm , msg(2,i) , xmsg(3,i)
99058                FORMAT (A25,' 3054, THE ACCURACY OF EIGENVECTOR',I6,' CORRESPOND','ING TO THE EIGENVALUE ',1P,E15.7,           &
                            &' IS IN DOUBT.')
                  ELSEIF ( l==55 ) THEN
                     WRITE (outtap,99124) png , lplus
                     WRITE (outtap,99059) msg(3,i) , msg(4,i)
99059                FORMAT ('0AN ATTEMPT TO MULTIPLY OR MULTIPLY AND ADD NON-CONFOR',                                              &
                            &'MABLE MATRICES TOGETHER WAS MADE IN SUBROUTINE ',2A4)
                  ELSEIF ( l==56 ) THEN
                     WRITE (outtap,99124) png , lplus
                     WRITE (outtap,99060)
99060                FORMAT ('0NO MASS MATRIX IS PRESENT BUT MASS DATA IS REQUIRED')
                  ELSEIF ( l==57 ) THEN
                     WRITE (outtap,99124) png , lplus
                     WRITE (outtap,99061) name
99061                FORMAT ('0MATRIX ',2A4,' IS NOT POSITIVE DEFINITE.')
                  ELSEIF ( l==58 ) THEN
                     WRITE (outtap,99124) png , lplus
                     WRITE (outtap,99062) xmsg(2,i) , msg(3,i)
99062                FORMAT ('0EPSILON IS LARGER THAN ',1P,E14.7,' FOR SUBCASE',I5)
                  ELSEIF ( l==59 ) THEN
                     WRITE (outtap,99124) png , lplus
                     WRITE (outtap,99063) msg(2,i) , msg(3,i) , msg(4,i)
99063                FORMAT ('0SET IDENTIFIER ',A4,' DOES NOT EXIST. ERROR DETECTED ','IN SUBROUTINE ',2A4)
                  ELSEIF ( l==60 ) THEN
                     WRITE (outtap,99064) ufm
99064                FORMAT (A23,' 3060, READ MODULE FINDS THAT THE INPUT STIFFNESS ','AND/OR MASS MATRIX IS NULL.')
                  ELSEIF ( l==61 ) THEN
                  ELSEIF ( l==62 ) THEN
                     WRITE (outtap,99065) sfm
99065                FORMAT (A25,' 3062, NO MESSAGE.')
                  ELSEIF ( l==63 ) THEN
                     WRITE (outtap,99066) sfm
99066                FORMAT (A25,' 3063, NO MESSAGE.')
                  ELSEIF ( l==64 ) THEN
                     WRITE (outtap,99067) sfm
99067                FORMAT (A25,' 3064, NO MESSAGE.')
                  ELSEIF ( l==65 ) THEN
                     WRITE (outtap,99068) sfm
99068                FORMAT (A25,' 3065, NO MESSAGE.')
                  ELSEIF ( l==66 ) THEN
                     WRITE (outtap,99069) sfm
99069                FORMAT (A25,' 3066, NO MESSAGE.')
                  ELSEIF ( l==67 ) THEN
                     WRITE (outtap,99070) sfm
99070                FORMAT (A25,' 3067, NO MESSAGE.')
                  ELSEIF ( l==68 ) THEN
                     WRITE (outtap,99071) sfm
99071                FORMAT (A25,' 3068, NO MESSAGE.')
                  ELSEIF ( l==69 ) THEN
                     WRITE (outtap,99072) sfm
99072                FORMAT (A25,' 3069, NO MESSAGE.')
                  ELSEIF ( l==70 ) THEN
                     WRITE (outtap,99073) sfm
99073                FORMAT (A25,' 3070, NO MESSAGE.')
                  ELSE
                     GOTO 5
                  ENDIF
                  CYCLE
               ENDIF
!
!     *** CHANGE L INTO CORRECT GINO.NASTIO.PACKUNPK ERROR NUMBER
!
 5             lplus = l + 1055
               WRITE (outtap,99125) sfm , lplus
!
!     *** BRANCH TO PRINT APPROPRIATE ERROR MESSAGE
!     --- NOTE - EACH NEW MESSAGE REQUIRES A NEW PRINT STATEMENT
!
               lplus = l - 70
               IF ( lplus==2 ) THEN
                  WRITE (outtap,99074)
99074             FORMAT ('0BUFFER ASSIGNED EXTENDS INTO MASTER INDEX AREA.')
               ELSEIF ( lplus==3 ) THEN
                  WRITE (outtap,99075)
99075             FORMAT ('0ON AN OPEN CALL WITHOUT REWIND, THE BLOCK NUMBER READ ','DOES NOT MATCH EXPECTED VALUE.')
               ELSEIF ( lplus==4 ) THEN
                  WRITE (outtap,99076)
99076             FORMAT ('0ON A CALL WRITE THE WORD COUNT IS NEGATIVE.')
               ELSEIF ( lplus==5 ) THEN
                  WRITE (outtap,99077)
99077             FORMAT ('0ON A CALL READ THE CONTROL WORD AT WHICH THE FILE IS ','POSITIONED IS NOT ACCEPTABLE.')
               ELSEIF ( lplus==6 ) THEN
                  WRITE (outtap,99078)
99078             FORMAT ('0LOGICAL RECORD TRAILER NOT RECOGNIZABLE AS SUCH.')
               ELSEIF ( lplus==7 ) THEN
                  WRITE (outtap,99079)
99079             FORMAT ('0UNRECOGNIZABLE CONTROL WORD DURING PROCESSING OF A ','BCKREC CALL.')
               ELSEIF ( lplus==8 ) THEN
                  WRITE (outtap,99080)
99080             FORMAT ('0AFTER A POSITIONING CALL TO IO6600, DURING PROCESSING ',                                                &
                         &'OF A BCKREC CALL THE BLOCK READ WAS NOT THE EXPECTED ','ONE.')
               ELSEIF ( lplus==9 ) THEN
                  WRITE (outtap,99081)
99081             FORMAT ('0CALL SKPFIL IN A FORWARD DIRECTION ON A FILE NOT ','OPENED FOR OUTPUT IS NOT SUPPORTED.')
               ELSEIF ( lplus==10 ) THEN
                  WRITE (outtap,99082)
99082             FORMAT ('0FILPOS WAS CALLED ON A FILE OPENED FOR OUTPUT.')
               ELSEIF ( lplus==11 ) THEN
                  WRITE (outtap,99083)
99083             FORMAT ('0ENDPUT WAS CALLED WITH BLOCK(8) EQUAL TO -1.')
               ELSEIF ( lplus==12 ) THEN
                  WRITE (outtap,99084)
99084             FORMAT ('0MORE TERMS WRITTEN IN STRING THAN WERE AVAILABLE TO ','WRITE.')
               ELSEIF ( lplus==13 ) THEN
                  WRITE (outtap,99085)
99085             FORMAT ('0CURRENT BUFFER POINTER EXCEEDS LAST DATA WORD IN BLOCK')
               ELSEIF ( lplus==14 ) THEN
                  WRITE (outtap,99086)
99086             FORMAT ('0ON AN INITIAL CALL TO GETSTR, THE RECORD IS NOT ','POSITIONED AT THE COLUMN HEADER.')
               ELSEIF ( lplus==15 ) THEN
                  WRITE (outtap,99087)
99087             FORMAT ('0STRING DEFINITION WORD NOT RECOGNIZABLE.')
               ELSEIF ( lplus==16 ) THEN
                  WRITE (outtap,99088)
99088             FORMAT ('0FIRST WORD OF A DOUBLE PRECISION STRING IS NOT ON A ','DOUBLE PRECISION BOUNDARY.')
               ELSEIF ( lplus==17 ) THEN
                  WRITE (outtap,99089)
99089             FORMAT ('0CURRENT BUFFER POINTER IS BEYOND RANGE OF INFORMATION ','IN BUFFER.')
               ELSEIF ( lplus==18 ) THEN
                  WRITE (outtap,99090)
99090             FORMAT ('0ON AN INITIAL CALL TO GETSTB, THE FILE IS NOT ','POSITIONED AT AN ACCEPTABLE POINT.')
               ELSEIF ( lplus==19 ) THEN
                  WRITE (outtap,99091)
99091             FORMAT ('0END-OF-SEGMENT CONTROL WORD SHOULD HAVE IMMEDIATELY ','PRECED CURRENT POSITION AND IT DID NOT.')
               ELSEIF ( lplus==20 ) THEN
                  WRITE (outtap,99092)
99092             FORMAT ('0COLUMN TRAILER NOT FOUND.')
               ELSEIF ( lplus==21 ) THEN
                  WRITE (outtap,99093)
99093             FORMAT ('0PREVIOUS RECORD TO BE READ BACKWARDS WAS NOT WRITTEN ','WITH STRING TRAILERS.')
               ELSEIF ( lplus==22 ) THEN
                  WRITE (outtap,99094)
99094             FORMAT ('0STRING RECOGNITION WORD NOT RECOGNIZED.')
               ELSEIF ( lplus==23 ) THEN
                  WRITE (outtap,99095)
99095             FORMAT ('0RECORD CONTROL WORD NOT IN EXPECTED POSITION.')
               ELSEIF ( lplus==24 ) THEN
                  WRITE (outtap,99096)
99096             FORMAT ('0RECTYP WAS CALLED FOR A FILE OPENED FOR OUTPUT.')
               ELSEIF ( lplus==25 ) THEN
                  WRITE (outtap,99097)
99097             FORMAT ('0RECTYP MUST BE CALLED WHEN THE FILE IS POSITIONED AT ','THE BEGINNING OF A RECORD.')
               ELSEIF ( lplus==26 ) THEN
                  WRITE (outtap,99098)
99098             FORMAT ('ON A CALL TO OPEN THE BUFFER ASSIGNED OVERLAPS A ','PREVIOUSLY ASSIGNED BUFFER.')
               ELSEIF ( lplus==27 ) THEN
                  WRITE (outtap,99099)
99099             FORMAT ('0A CALL TO OPEN FOR AN ALREADY OPEN FILE.')
               ELSEIF ( lplus==28 ) THEN
                  WRITE (outtap,99100)
99100             FORMAT ('0FILE NOT OPEN.')
               ELSEIF ( lplus==29 ) THEN
                  WRITE (outtap,99101)
99101             FORMAT ('0GINO REFERENCE NAME NOT IN FIST OR FILE NOT OPEN.')
               ELSEIF ( lplus==30 ) THEN
                  WRITE (outtap,99102)
99102             FORMAT ('0A CALL TO GETSTR OCCURRED WHEN THE FILE WAS POSITIONED','AT END-OF-FILE.')
               ELSEIF ( lplus==31 ) THEN
                  WRITE (outtap,99103)
99103             FORMAT ('0ATTEMPTED TO WRITE ON AN INPUT FILE.')
               ELSEIF ( lplus==32 ) THEN
                  WRITE (outtap,99104)
99104             FORMAT ('0ATTEMPTED TO READ FROM AN OUTPUT FILE.')
               ELSEIF ( lplus==33 ) THEN
                  WRITE (outtap,99105)
99105             FORMAT ('0A CALL TO BLDPK OR PACK IN WHICH EITHER TYPIN OR ','TYPOUT IS OUT OF RANGE.')
               ELSEIF ( lplus==34 ) THEN
                  WRITE (outtap,99106)
99106             FORMAT ('0ROW POSITIONS OF ELEMENTS FURNISHED TO ZBLPKI OR ','BLDPKI ARE NOT IN A MONOTONIC INCREASING SEQUENCE.',&
                        & /,' (POSSIBLY DUE TO ROW OR COLUMN INDEX ERROR)')
               ELSEIF ( lplus==35 ) THEN
                  WRITE (outtap,99107)
99107             FORMAT ('0ON A CALL TO BLDPKN, FILE NAME DOES NOT MATCH PREVIOUS','CALLS.')
               ELSEIF ( lplus==36 ) THEN
                  WRITE (outtap,99108)
99108             FORMAT ('0A CALL TO INTPK OR UNPACK IN WHICH TYPOUT IS OUT OF ','RANGE.')
               ELSEIF ( lplus==37 ) THEN
                  WRITE (outtap,99109) sfm
99109             FORMAT (A25,' 1162, NO MESSAGE.')
               ELSEIF ( lplus==38 ) THEN
                  WRITE (outtap,99110) sfm
99110             FORMAT (A25,' 1163, NO MESSAGE.')
               ELSEIF ( lplus==39 ) THEN
                  WRITE (outtap,99111)
99111             FORMAT ('0 FOLLOWING A READ ATTEMPT ON AN INDEXED FILE, EITHER ',                                                 &
                         &'AN END-OF-FILE WAS ENCOUNTERED OR THE NUMBER OF WORDS ','READ WAS INCORRECT.')
               ELSEIF ( lplus==40 ) THEN
                  WRITE (outtap,99112)
99112             FORMAT ('0ON AN ATTEMPT TO READ A SEQUENTIAL FILE, AN END-OF-','FILE OR AN END-OF-INFORMATION WAS ENCOUNTERED.')
               ELSEIF ( lplus==41 ) THEN
                  WRITE (outtap,99113) sfm
99113             FORMAT (A25,' 1166, NO MESSAGE.')
               ELSEIF ( lplus==42 ) THEN
                  WRITE (outtap,99114) sfm
99114             FORMAT (A25,' 1167, NO MESSAGE.')
               ELSEIF ( lplus==43 ) THEN
                  WRITE (outtap,99115)
99115             FORMAT ('0A CALL TO IO6600 WITH OPCODE=5 (FORWARD SPACE) IS NOT ','SUPPORTED.')
               ELSEIF ( lplus==44 ) THEN
                  WRITE (outtap,99116) sfm
99116             FORMAT (A25,' 1169, NO MESSAGE.')
               ELSEIF ( lplus==45 ) THEN
                  WRITE (outtap,99117)
99117             FORMAT ('0ILLEGAL CALL TO NASTIO, LOGIC ERROR IN IO6600.')
               ELSEIF ( lplus==46 ) THEN
                  WRITE (outtap,99118)
99118             FORMAT ('0ON A POSITION CALL, THE BLOCK NUMBER REQUESTED IS NOT ','FOUND IN CORE WHEN IT IS EXPECTED THERE.')
               ELSEIF ( lplus==47 ) THEN
                  WRITE (outtap,99119) sfm
99119             FORMAT (A25,' 1172, NO MESSAGE.')
               ELSE
!
!     *** GINO FORMAT NUMBERS MATCH THE MESSAGE NUMBER.
!
                  WRITE (outtap,99120)
99120             FORMAT ('0ADDRESS OF BUFFER LESS THAN ADDRESS OF /XNSTRN/.')
               ENDIF
               CYCLE
            ENDIF
 10         CALL usrmsg(i)
         ENDIF
         CYCLE
 20      WRITE (outtap,99127) ufm , lplus
         WRITE (outtap,99121) msg(2,i) , msg(3,i) , msg(4,i)
99121    FORMAT ('0UNABLE TO FIND SELECTED SET (',I8,') IN TABLE (',A4,') IN SUBROUTINE (',A4,2H).)
      ENDIF
   ENDDO
   IF ( n>=m ) WRITE (outtap,99122) uwm , m
99122 FORMAT (A25,' 3199, NON-FATAL MESSAGES MAY HAVE BEEN LOST BY ','ATTEMPTING TO QUEUE MORE THAN',I5,' MESSAGES')
   i = n
   n = 0
!WKBI WRITE (OUTTAP,100)
   IF ( msg(1,i)<0 ) CALL errtrc('WRTMSG  ',100)
   RETURN
99123 FORMAT (12H0*** SYSTEM ,2A4,8H MESSAGE,I5)
99124 FORMAT (10H0*** USER ,2A4,9H MESSAGE ,I5)
99125 FORMAT (A25,I5)
99126 FORMAT (A25,I5)
99127 FORMAT (A23,I5)
99128 FORMAT (A29,I5)
END SUBROUTINE msgwrt
