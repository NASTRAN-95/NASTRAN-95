
BLOCKDATA ifx7bd
   IMPLICIT NONE
   INTEGER J1(160) , J10(56) , J11(24) , J12(8) , J13(16) , J14(57) , J15(52) , J2(160) , J3(160) , J4(160) , J5(160) , J6(160) ,   &
         & J7(160) , J8(80) , J9(56)
   COMMON /ifpx7 / J1 , J2 , J3 , J4 , J5 , J6 , J7 , J8 , J9 , J10 , J11 , J12 , J13 , J14 , J15
!IFX7BD
!     EACH ENTRY CONTAINS THE ADMISSIBLE SEQUAENCE OF FORMAT CODES FOR
!     THAT CARD TYPE.  THE POINTER TO EACH ENTRY IS PROVIDED FROM THE
!     FIRST WORD OF /IFPX5/
!
!     INPUT BULK DATA CARD FORMAT CODE STRINGS
!           0 = BLANK        3 = BCD
!           1 = INTEGER      4 = DOUBLE PRECISION
!           2 = REAL         5 = ANYTHING
!
!     IF THE DIMENSION OF /IFPX7/ IS INCREASED HERE, MAKE THE SAME
!     LABEL COMMON IN IFP ROUTINE THE SMAE SIZE TOO
!
!******
!
!*****
!    1       1              5              9             13
!    2      17             21             25             29
!    3      33             37             41             45
!    4      49             53             57             61
!    5      65             69             73             77
!    6      81             85             89             93
!    7      97            101            105            109
!    8     113            117            121            125
!    9     129            133            137            141
!    X     145            149            153            157
   DATA J1/1 , 1 , 2 , 2 , 2 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 0 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 , 5 , 2 ,  &
      & 2 , 1 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 1 , &
      & 2 , 1 , 1 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 5 , 2 , 2 , 1 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , &
      & 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 2 , 1 , 1 , 2 , 0 , 1 , 1 , 5 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 2 , &
      & 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 2 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 1 , 1 , 2 , 1/
!
!*****
!    1     161            165            169            173
!    2     177            181            185            189
!    3     193            197            201            205
!    4     209            213            217            221
!    5     225            229            233            237
!    6     241            245            249            253
!    7     257            261            265            269
!    8     273            277            281            285
!    9     289            293            297            301
!    X     305            309            313            317
   DATA J2/2 , 1 , 2 , 0 , 1 , 1 , 2 , 2 , 2 , 2 , 1 , 1 , 1 , 2 , 2 , 2 , 1 , 1 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2 ,  &
      & 2 , 0 , 1 , 2 , 2 , 0 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , &
      & 1 , 2 , 1 , 2 , 1 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 2 , 2 , 1 , 1 , 2 , 2 , 1 , 0 , 2 , 2 , 0 , 0 , 1 , 0 , &
      & 0 , 0 , 0 , 0 , 1 , 1 , 2 , 1 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , 2 , 1 , 2 , 1 , 2 , 1 , 2 , 1 , 1 , 1 , 1 , 2 , 2 , 2 , &
      & 2 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 2 , 2 , 2 , 1 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , 1 , &
      & 1 , 1 , 1 , 2 , 0 , 0/
!
!*****
!    1     321            325            329            333
!    2     337            341            345            349
!    3     353            357            361            365
!    4     369            373            377            381
!    5     385            389            393            397
!    6     401            405            409            413
!    7     417            421            425            429
!    8     433            437            441            445
!    9     449            453            457            461
!    X     465            469            473            477
   DATA J3/0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 2 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 ,  &
      & 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 2 , 2 , &
      & 2 , 2 , 0 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 1 , 1 , &
      & 1 , 2 , 1 , 1 , 1 , 2 , 1 , 1 , 1 , 1 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 1 , 0 , 0 , 0 , &
      & 0 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 3 , 2 , 2 , 1 , 1 , &
      & 1 , 2 , 3 , 1 , 1 , 0/
!
!*****
!    1     481            485            489            493
!    2     497            501            505            509
!    3     513            517            521            525
!    4     529            533            537            541
!    5     545            549            553            557
!    6     561            565            569            573
!    7     577            581            585            589
!    8     593            597            601            605
!    9     609            613            617            621
!    X     625            629            633            637
   DATA J4/0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 2 , 2 , 1 , 2 , 2 , 2 , 1 , 1 , 1 , 0 , 1 , 1 ,  &
      & 1 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 2 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 5 , &
      & 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 2 , 1 , 2 , 2 , 1 , 0 , 0 , 0 , 0 , 0 , &
      & 1 , 1 , 1 , 2 , 1 , 2 , 1 , 2 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 2 , 1 , 1 , 1 , &
      & 0 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , &
      & 2 , 2 , 2 , 2 , 2 , 0/
!
!*****
!    1     641            645            649            653
!    2     657            661            665            669
!    3     673            677            681            685
!    4     689            693            697            701
!    5     705            709            713            717
!    6     721            725            729            733
!    7     737            741            745            749
!    8     753            757            761            765
!    9     769            773            777            781
!    X     785            789            793            797
   DATA J5/0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 1 , 1 , 2 , 1 , 2 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 ,  &
      & 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 0 , 0 , 1 , 1 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 1 , 2 , 2 , 1 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 2 , 1 , 1 , 5 , 5 , &
      & 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 2 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 2 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1 , &
      & 2 , 2 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 1 , 5 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 2 , 2 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , &
      & 5 , 1 , 1 , 1 , 1 , 1/
!
!*****
!    1     801            805            809            813
!    2     817            821            825            829
!    3     833            837            841            845
!    4     849            853            857            861
!    5     865            869            873            877
!    6     881            885            889            893
!    7     897            901            905            909
!    8     913            917            921            925
!    9     929            933            937            941
!    X     945            949            953            957
   DATA J6/1 , 1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 1 , 3 , 3 , 0 , 0 ,  &
      & 0 , 0 , 0 , 1 , 0 , 1 , 2 , 1 , 2 , 1 , 2 , -9 , -9 , -9 , 1 , 1 , 1 , 0 , 0 , 2 , 2 , 0 , 1 , 1 , 1 , 1 , 0 , 2 , 2 , 0 ,  &
      & 1 , 1 , 1 , 1 , 1 , 2 , 2 , 0 , 2 , 2 , 1 , 2 , 1 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 2 , 2 , 1 , 1 , 2 , 2 , 0 , 0 , 0 , 0 , &
      & 0 , 1 , 2 , 2 , 2 , 1 , 0 , 0 , 0 , 1 , 3 , 2 , 2 , 1 , 1 , 1 , 1 , 1 , 1 , 3 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 2 , 2 , &
      & 2 , 0 , 1 , 1 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 5 , 1 , 2 , 1 , 1 , 1 , &
      & 1 , 1 , 1 , 1 , 1 , 1 , 1/
!
!*****
!    1      961            965            969            973
!    2      977            981            985            989
!    3      993            997           1001           1005
!    4     1009           1013           1017           1021
!    5     1025           1029           1033           1037
!    6     1041           1045           1049           1053
!    7     1057           1061           1065           1069
!    8     1073           1077           1081           1085
!    9     1089           1093           1097           1101
!    X     1105           1109           1113           1117
   DATA J7/1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 5 , 5 , 5 , 0 , 0 , 0 ,  &
      & -9 , -9 , 1 , 1 , 3 , 3 , 2 , 2 , 2 , 2 , 0 , 0 , 0 , 0 , 1 , 3 , 1 , 1 , 1 , 3 , 1 , 2 , 0 , 0 , 0 , 0 , 1 , 2 , 2 , 1 ,   &
      & 3 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 2 , 2 , 1 , 2 , 2 , 0 , 0 , 1 , 2 , 2 , 1 , 1 , 1 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , &
      & 1 , 1 , 1 , 1 , 1 , 1 , 1 , 2 , 0 , 0 , 0 , 0 , 0 , 3 , 1 , 1 , 1 , 1 , 1 , 1 , 0 , 1 , 3 , 1 , 1 , 1 , 1 , 1 , 1 , 3 , 1 , &
      & 1 , 2 , 1 , 1 , 2 , -9 , 1 , 1 , 2 , 2 , 2 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 1 , 1 , 2 , 2 , 2 , 1 , 2 , 2 , 2 ,&
      & 1 , 2 , 2 , 2 , 2 , 2 , 2/
!
!*****
!    1     1121           1125           1129           1133
!    2     1137           1141           1145           1149
!    3     1153           1157           1161           1165
!    4     1169-1200
   DATA J8/2 , 2 , 2 , 2 , 5 , 5 , 5 , 5 , 1 , 3 , 2 , 2 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  &
      & 0 , 0 , 1 , 3 , 1 , 1 , 2 , 2 , 0 , 0 , -9 , -9 , -9 , -9 , -9 , -9 , -9 , -9 , 32*0/
!
!*****
!    1     1201           1205-1252      1253
   DATA J9/1 , 2 , 2 , 2 , 48*2 , 0 , 0 , 0 , 0/
!
!*****
!    1     1257           1261-1308      1309
   DATA J10/1 , 1 , 1 , 1 , 48*1 , 0 , 0 , 0 , 0/
!
!*****
!    1     1313           1317           1321           1325
!    2     1329           1333
   DATA J11/1 , 2 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 1 , 0 , 0 , 0 , 1 , 1 , 2 , 2 , 2 , 1 , 5 , 1/
!
!*****
!    1     1337           1341
   DATA J12/1 , 1 , 5 , 1 , 1 , 1 , 1 , 1/
!
!*****
!    1     1345           1349           1353           1357
   DATA J13/1 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 5 , 5 , 5 , 5 , 5 , 5/
!
!*****
!    1     1361-1416      1417
   DATA J14/56*5 , 5/
!
!*****
!    1     1418           1422           1426           1430
!    2     1434           1438           1442           1446
!    3     1450           1454           1458           1462
!    4     1466
   DATA J15/1 , 1 , 1 , 1 , 1 , 1 , 5 , 2 , 0 , 0 , 2 , 2 , 2 , 2 , 0 , 0 , 1 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , &
      & 2 , 2 , 2 , 2 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 5 , 2 , 0 , 0 , 0 , 2 , 2 , 2 , 0 , 0 , 0/
!
END BLOCKDATA ifx7bd