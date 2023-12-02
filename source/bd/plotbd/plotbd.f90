!*==plotbd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
BLOCKDATA plotbd
!PLOTBD
   IMPLICIT NONE
   USE c_char94
   USE c_chrdrw
   USE c_drwaxs
   USE c_pltdat
   USE c_pltscr
   USE c_symbls
   USE c_xxparm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(60,3) , SAVE :: char1
   INTEGER , DIMENSION(60,1) , SAVE :: char2
   INTEGER , DIMENSION(2,52) , SAVE :: chlpqm
   INTEGER , DIMENSION(2,79) , SAVE :: chr19
   INTEGER , DIMENSION(2,88) , SAVE :: chram
   INTEGER , DIMENSION(2,84) , SAVE :: chrnz
   INTEGER , DIMENSION(2,19) , SAVE :: chrsym
   INTEGER , DIMENSION(20,2) , SAVE :: eof , npens , pbfsiz , pltype
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!    1    ... PLOTTING DATA
!    2    ... PEN + PAPER DATA
!    3    ... SCALING DATA
!    4    ... VIEWING DATA
!    5    ... VANTAGE POINT,               PROJECTION,OCULAR SEPARATION
!    6    ... ORIGIN DATA
!    7    ... CONTOUR PLOTTING DATA
!    8    ... DATA FOR USER PLOT TITLE CARD
!    9    ... OFFSET SCALE (WILL BE SET TO 1 BY PLTSET)
!
! ... EQUIV FOR   /CHAR94/...
   !>>>>EQUIVALENCE (Char(1,1),Char1(1,1)) , (Char(1,4),Char2(1,1))
!
! ... EQUIV FOR   /CHRDRW/...
   !>>>>EQUIVALENCE (Chr(1,1),Chr19(1,1)) , (Chr(1,80),Chram(1,1)) , (Chr(1,168),Chrnz(1,1)) , (Chr(1,252),Chlpqm(1,1)) ,                &
!>>>>    & (Chr(1,304),Chrsym(1,1))
!
! ... EQUIV FOR   /PLTDAT/...
   !>>>>EQUIVALENCE (Data(7,1),Npens(1,1)) , (Data(10,1),Pltype(1,1)) , (Data(12,1),Pbfsiz(1,1)) , (Data(13,1),Eof(1,1))
!
!
! ... THE FOLLOWING ARE NUMERIC EQUIVALENTS OF 7094 BINARY CHARACTERS.
!
!    5  ... 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
!    6  ... G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
!    7  ... W, X, Y, Z, (, ),  , -, *, /, =, ., ,, $, -,BLANK
!    8  . EOR,EOF, SPECIAL, FILLER
!
! ... THE FOLLOWING ARE NUMBERIC EQUIVALENTS OF 7094 BCD CHARACTERS.
!
!    9  ... 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
!    O  ... G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
!   11  ... W, X, Y, Z, (, ),  , -, *, /, =, ., ,, $, -,BLANK
!   12  . EOR,EOF, SPECIAL, FILLER
   DATA char1/1H0 , 1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1HA , 1HB , 1HC , 1HD , 1HE , 1HF , 1HG , 1HH , 1HI ,     &
      & 1HJ , 1HK , 1HL , 1HM , 1HN , 1HO , 1HP , 1HQ , 1HR , 1HS , 1HT , 1HU , 1HV , 1HW , 1HX , 1HY , 1HZ , 1H( , 1H) , 1H+ ,     &
      & 1H- , 1H* , 1H/ , 1H= , 1H. , 1H, , 1H$ , 1H- , 1H  , 12*0 , 00 , 01 , 02 , 03 , 04 , 05 , 06 , 07 , 08 , 09 , 17 , 18 ,    &
      & 19 , 20 , 21 , 22 , 23 , 24 , 25 , 33 , 34 , 35 , 36 , 37 , 38 , 39 , 40 , 41 , 50 , 51 , 52 , 53 , 54 , 55 , 56 , 57 , 60 ,&
      & 28 , 16 , 32 , 44 , 49 , 11 , 27 , 59 , 43 , 12 , 48 , 58 , 15 , 63 , 42 , 26 , 7*0 , 10 , 01 , 02 , 03 , 04 , 05 , 06 ,    &
      & 07 , 08 , 09 , 49 , 50 , 51 , 52 , 53 , 54 , 55 , 56 , 57 , 33 , 34 , 35 , 36 , 37 , 38 , 39 , 40 , 41 , 18 , 19 , 20 , 21 ,&
      & 22 , 23 , 24 , 25 , 28 , 60 , 48 , 32 , 44 , 17 , 11 , 59 , 27 , 43 , 12 , 16 , 26 , 15 , 31 , 42 , 58 , 7*0/
!
! ... THE FOLLOWING ARE NUMERIC VALUES ON CDC 6600 TO PRODUCE 7094 BCD
!     CHARACTERS.
!
!    1  ... 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
!    2  ... G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
!    3  ... W, X, Y, Z, (, ),  , -, *, /, =, ., ,, $, -,BLANK
!    4  . EOR,EOF, SPECIAL, FILLER
   DATA char2/27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 35 , 36 , 01 , 02 , 03 , 04 , 05 , 06 , 07 , 08 , 09 , 10 , 11 , 12 , 13 ,    &
      & 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 , 25 , 26 , 41 , 42 , 37 , 38 , 39 , 40 , 44 , 47 , 46 , 43 , 52 , 45 ,&
      & 50 , 49 , 55 , 54 , 58 , 7*0/
!
! ... DATA FOR DRAWING 6X6 CHARACTERS (8 UNITS WIDE - 16 UNITS HIGH).
!
!
!     THE FOLLOWING ARE INDICES USED TO DRAW CHARACTERS.
!
!    1   0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
!    2   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V
!    3   W   X   Y   Z   (   )   +   -   *   /   =   .   ,   $   -  DOT
!    4  CIRCLE SQUARE DIAMOND TRIANGLE END FILLER
   DATA lstchr , chrind/52 , -25 , 001 , 006 , 014 , 027 , 031 , 041 , 052 , 055 , 071 , 080 , 086 , 098 , 106 , 113 , 120 , 126 ,  &
      & 136 , 142 , 148 , 155 , 160 , 163 , 168 , 172 , 181 , 188 , 199 , 208 , 220 , 225 , 231 , 234 , 239 , 243 , 248 , 252 ,     &
      & 256 , 260 , 264 , 266 , 274 , 276 , 280 , 285 , 287 , 302 , 304 , -25 , 309 , 314 , 319 , 323 , 7*0/
!
! ... DATA FOR DRAWING CHARACTERS 1 TO 9.
!
   DATA chr19/2 , 5 , 3 , 6 , 3 , 0 , 2 , 0 , 4 , 0 , 0 , 5 , 1 , 6 , 4 , 6 , 5 , 5 , 5 , 4 , 0 , 1 , 0 , 0 , 5 , 0 , 0 , 5 , 1 ,   &
      & 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 2 , 3 , 4 , 3 , 5 , 2 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 4 , 0 , 4 , 6 , 0 , 2 , 5 , 2 , &
      & 5 , 6 , 0 , 6 , 0 , 3 , 1 , 4 , 4 , 4 , 5 , 3 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 4 , &
      & 0 , 5 , 1 , 5 , 2 , 4 , 3 , 1 , 3 , 0 , 2 , 0 , 6 , 5 , 6 , 2 , 0 , 4 , 3 , 5 , 4 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 4 , &
      & 1 , 3 , 4 , 3 , 5 , 2 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 0 , 2 , 1 , 3 , 5 , 0 , 5 , 5 , 4 , 6 , 2 , 6 , 1 , 5 , 1 , 4 , 2 , &
      & 3 , 4 , 3 , 5 , 4/
!
! ... DATA FOR DRAWING CHARACTERS A TO M.
!
   DATA chram/0 , 0 , 3 , 6 , 5 , 2 , 1 , 2 , 5 , 2 , 6 , 0 , 0 , 0 , 0 , 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 0 , 3 , 4 , 3 , 5 ,   &
      & 2 , 5 , 1 , 4 , 0 , 0 , 0 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 4 , 0 , 5 , 1 , 5 , 4 , 4 , 6 , 0 , 6 , 0 , 0 , &
      & 4 , 0 , 5 , 2 , 5 , 4 , 5 , 6 , 0 , 6 , 0 , 3 , 3 , 3 , 0 , 3 , 0 , 0 , 5 , 0 , 5 , 6 , 0 , 6 , 0 , 3 , 3 , 3 , 0 , 3 , 0 , &
      & 0 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 4 , 0 , 5 , 1 , 5 , 3 , 3 , 3 , 0 , 6 , 0 , 0 , 0 , 3 , 5 , 3 , 5 , 0 , &
      & 5 , 6 , 2 , 6 , 4 , 6 , 3 , 6 , 3 , 0 , 2 , 0 , 4 , 0 , 3 , 6 , 5 , 6 , 4 , 6 , 4 , 1 , 3 , 0 , 1 , 0 , 0 , 1 , 0 , 6 , 0 , &
      & 0 , -5 , 0 , 0 , 3 , 5 , 6 , 0 , 6 , 0 , 0 , 5 , 0 , 0 , 0 , 0 , 6 , 3 , 0 , 6 , 6 , 6 , 0/
!
! ... DATA FOR DRAWING CHARACTERS N TO Z.
!
   DATA chrnz/0 , 0 , 0 , 6 , 5 , 0 , 5 , 6 , 6 , 5 , 5 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 5 , 0 , 6 , 1 , 6 , 5 , 0 , 0 , 0 ,   &
      & 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 0 , 3 , 6 , 5 , 5 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 5 , 0 , 6 , 1 , 6 , 5 , -4 , 2 ,&
      & 6 , 0 , 0 , 0 , 0 , 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 0 , 3 , 3 , 3 , 5 , 0 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 4 , 1 , &
      & 3 , 4 , 3 , 5 , 2 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 0 , 6 , 3 , 6 , 3 , 0 , 3 , 6 , 6 , 6 , 0 , 6 , 0 , 1 , 1 , 0 , 4 , 0 , &
      & 5 , 1 , 5 , 6 , 0 , 6 , 3 , 0 , 6 , 6 , 0 , 6 , 1 , 0 , 3 , 4 , 5 , 0 , 6 , 6 , 0 , 6 , 6 , 0 , -6 , 6 , 0 , 0 , 0 , 6 , 3 ,&
      & 3 , 3 , 0 , 3 , 3 , 6 , 6 , 0 , 6 , 6 , 6 , 0 , 0 , 6 , 0/
!
! ... DATA FOR DRAWING CHARACTERS ( TO -.
!
   DATA chlpqm/5 , 6 , 3 , 4 , 3 , 2 , 5 , 0 , 1 , 6 , 3 , 4 , 3 , 2 , 1 , 0 , 3 , 5 , 3 , 1 , -1 , 3 , 5 , 3 , 1 , 3 , 5 , 3 , 1 , &
      & 5 , 5 , 1 , -3 , 5 , 3 , 1 , -5 , 5 , 1 , 1 , -5 , 3 , 1 , 3 , 0 , 0 , 6 , 6 , 1 , 4 , 4 , 4 , -1 , 2 , 4 , 2 , 2 , 0 , 2 , &
      & 1 , 3 , 1 , 3 , 0 , 2 , 0 , 1 , 0 , 3 , 2 , 6 , 5 , 5 , 6 , 1 , 6 , 0 , 5 , 0 , 4 , 1 , 3 , 5 , 3 , 6 , 2 , 6 , 1 , 5 , 0 , &
      & 3 , 0 , 3 , 6 , 3 , 0 , 1 , 0 , 0 , 1 , 3 , 6 , 3 , 4/
!
! ... DATA FOR DRAWING DOT, SQUARE, DIAMOND, TRIANGLE.
!
   DATA chrsym/3 , 4 , 2 , 3 , 3 , 2 , 4 , 3 , 3 , 4 , 0 , 0 , 0 , 6 , 6 , 6 , 6 , 0 , 0 , 0 , 3 , 6 , 0 , 3 , 3 , 0 , 6 , 3 , 3 ,  &
      & 6 , 0 , 0 , 3 , 6 , 6 , 0 , 0 , 0/
!
!    1 ... CAMERA 2,  1 BLANK FRAME,  PLOTTER MODEL --M,1--
!    2 ... PAPER = DEFAULT,VELLUM...PEN SIZE = 1, COLOR = BLACK
!    3 ... FIND THE SCALES, MAX DEFORMATION = 0
!    4 ... AXES = +X,+Y,+Z, VIEW ANGLES
!    5 ... FIND VANTAGE POINT, ORTHOGRAPIC PROJECTION, PLANE+OCULAR SEP.
!    6 ... LEFT=BOTTOM=0, RIGHT=TOP=1.
!    7 ... NCNTR=10=NO. CONTOURS, CNTR=LIST CONTOUR VALUES, ICNTVL=
!          MAJOR PRIN. STRESS, WHERE = Z1, DIRECT = COMMON
!    8 ... DATA FOR USER PLOT TITLE CARD
!    9 ... OFFSET SCALE (AND ALSO PLOT TAPE MESSAGE CONTROL)
   DATA bufsiz/0/ , camera , bframs , pltmdl , tapden/2 , 1 , 1HM , 1 , 0/ , nopens , papsiz , paptyp , pensiz , penclr/8 , 2*0. ,  &
       &4HVELL , 2HUM , 8*1 , 8*4HBLAC , 8*1HK/ , scale(2) , fscale , maxdef/1. , 1 , 0./ , axis , daxis , vangle/1 , 2 , 3 , 1 ,   &
      & 2 , 3 , 0. , -1.E10 , 34.27 , 23.17 , 0./ , fvp , prject , d02 , d03 , s0s/1 , 1 , 1. , 2. , 2.756/ , norg , org , forg ,   &
      & edge/10 , 0 , 1 , 22*0. , 22*1./ , ncntr , cntr , icntvl , where , direct , flag , lasset/10 , 50*0.0 , 1 , 1 , 2 , 0 , 0/ ,&
      & fpltit , pltitl/0 , 17*4H    / , offscl/0/
!
! ... PLOTTER DATA.
!
   DATA model , ploter , chrscl/ - 1 , 1 , 1.0/
!
! ... 1  NASTRAN GENERAL PURPOSE MICROFILM PLOTTER.
!
   DATA data(1,1)/1023.0/ , data(2,1)/1023.0/ , data(3,1)/146.1429/ , data(4,1)/8.0/ , data(5,1)/16.0/ , data(6,1)/1023.0/ ,        &
      & data(8,1)/0.0/ , data(9,1)/0.0/ , data(11,1)/4HPLT2/ , data(14,1)/1484.761/ , data(15,1)/0.0/ , data(16,1)/0.0/ , data(17,1)&
      & /0.0/ , data(18,1)/0.0/ , data(19,1)/0.0/ , data(20,1)/0.0/
!
! ... 2  NASTRAN GENERAL PURPOSE TABLE OR DRUM PLOTTER
!
   DATA data(1,2)/3000.0/ , data(2,2)/3000.0/ , data(3,2)/100.0/ , data(4,2)/8.0/ , data(5,2)/16.0/ , data(6,2)/3000.0/ , data(8,2) &
      & /0.0/ , data(9,2)/0.0/ , data(11,2)/4HPLT2/ , data(14,2)/100.0/ , data(15,2)/0.0/ , data(16,2)/0.0/ , data(17,2)/0.0/ ,     &
      & data(18,2)/0.0/ , data(19,2)/0.0/ , data(20,2)/0.0/
!
   DATA npens(1,1) , pltype(1,1) , pbfsiz(1,1) , eof(1,1)/64 , -1 , 3000 , 1/ , npens(1,2) , pltype(1,2) , pbfsiz(1,2) , eof(1,2)   &
      & /64 , -2 , 3000 , 1/
!
! ... SYMBOL DATA.
!
!           X, *, +, -, DOT, CIRCLE, SQUARE, DIAMOND, TRIANGLE
   DATA nsym , symbol/9 , 34 , 41 , 39 , 40 , 48 , 49 , 50 , 51 , 52 , 11*0 , 34 , 41 , 39 , 40 , 48 , 49 , 50 , 51 , 52 , 11*0/
!
! ... PLOTTER SCRATCH AREA
!
!          NCOR = ARRAY LENGTH
   DATA ncor , pltsc/50 , 50*0/
!
! ... DATA FOR DRAWING A X-Y-Z COORDINATE TRIAD IN /DRWAXS/
!     G   - X,Y,Z COORD. POINT DATA AND SYMBOLS
!
   DATA g/9*0.0 , 1HX , 1HY , 1HZ/
!
END BLOCKDATA plotbd
