
BLOCKDATA plotbd
!PLOTBD
   IMPLICIT NONE
   INTEGER Axis(3) , Axymax(2) , Bframs , Bufsiz , Camera , Char(60,4) , Char1(60,3) , Char2(60,1) , Chlpqm(2,52) , Chr(2,350) ,    &
         & Chr19(2,79) , Chram(2,88) , Chrind(60) , Chrnz(2,84) , Chrsym(2,19) , Color , D0 , Daxis(3) , Defmax , Direct , Eof(20,2)&
         & , Flag , Forg , Fpltit , Fscale , Fvp , Icntvl , Lasset , Layer , Lstchr , Model , Ncntr , Ncor , Nopens , Norg ,        &
         & Npens(20,2) , Nsym , Offscl , Org , Origin(11) , Paptyp(2) , Pbfsiz(20,2) , Penclr(8,2) , Penpap , Pensiz(8) , Ploter ,  &
         & Pltdat(20) , Pltitl(17) , Pltmdl(2) , Pltsc(50) , Pltype(20,2) , Prject , R0 , S0l , S0r , Subcas , Symbol(20,2) , T0 ,  &
         & Tapden , Value , View(4) , Where , Xy(11,3) , Xyedge(11) , Xymax(2) , Xymin(2)
   REAL Chrscl , Cntr(50) , D02 , D03 , Data(20,2) , Edge(11,4) , G(12) , Maxdef , Papsiz(2) , S0s , Scale(2) , Vangle(5)
   COMMON /char94/ Char
   COMMON /chrdrw/ Lstchr , Chrind , Chr
   COMMON /drwaxs/ G
   COMMON /pltdat/ Model , Ploter , Xymin , Xymax , Axymax , Xyedge , Chrscl , Pltdat , Data
   COMMON /pltscr/ Ncor , Pltsc
   COMMON /symbls/ Nsym , Symbol
   COMMON /xxparm/ Bufsiz , Camera , Bframs , Pltmdl , Tapden , Nopens , Papsiz , Paptyp , Pensiz , Penclr , Penpap , Scale ,       &
                 & Fscale , Maxdef , Defmax , Axis , Daxis , Vangle , View , Fvp , R0 , S0l , S0r , T0 , D0 , D02 , D03 , Prject ,  &
                 & S0s , Forg , Org , Norg , Origin , Edge , Xy , Ncntr , Cntr , Icntvl , Where , Direct , Subcas , Flag , Value ,  &
                 & Lasset , Fpltit , Pltitl , Color , Layer , Offscl
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
   EQUIVALENCE (Char(1,1),Char1(1,1)) , (Char(1,4),Char2(1,1))
!
! ... EQUIV FOR   /CHRDRW/...
   EQUIVALENCE (Chr(1,1),Chr19(1,1)) , (Chr(1,80),Chram(1,1)) , (Chr(1,168),Chrnz(1,1)) , (Chr(1,252),Chlpqm(1,1)) ,                &
    & (Chr(1,304),Chrsym(1,1))
!
! ... EQUIV FOR   /PLTDAT/...
   EQUIVALENCE (Data(7,1),Npens(1,1)) , (Data(10,1),Pltype(1,1)) , (Data(12,1),Pbfsiz(1,1)) , (Data(13,1),Eof(1,1))
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
   DATA Char1/1H0 , 1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1HA , 1HB , 1HC , 1HD , 1HE , 1HF , 1HG , 1HH , 1HI ,     &
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
   DATA Char2/27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 35 , 36 , 01 , 02 , 03 , 04 , 05 , 06 , 07 , 08 , 09 , 10 , 11 , 12 , 13 ,    &
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
   DATA Lstchr , Chrind/52 , -25 , 001 , 006 , 014 , 027 , 031 , 041 , 052 , 055 , 071 , 080 , 086 , 098 , 106 , 113 , 120 , 126 ,  &
      & 136 , 142 , 148 , 155 , 160 , 163 , 168 , 172 , 181 , 188 , 199 , 208 , 220 , 225 , 231 , 234 , 239 , 243 , 248 , 252 ,     &
      & 256 , 260 , 264 , 266 , 274 , 276 , 280 , 285 , 287 , 302 , 304 , -25 , 309 , 314 , 319 , 323 , 7*0/
!
! ... DATA FOR DRAWING CHARACTERS 1 TO 9.
!
   DATA Chr19/2 , 5 , 3 , 6 , 3 , 0 , 2 , 0 , 4 , 0 , 0 , 5 , 1 , 6 , 4 , 6 , 5 , 5 , 5 , 4 , 0 , 1 , 0 , 0 , 5 , 0 , 0 , 5 , 1 ,   &
      & 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 2 , 3 , 4 , 3 , 5 , 2 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 4 , 0 , 4 , 6 , 0 , 2 , 5 , 2 , &
      & 5 , 6 , 0 , 6 , 0 , 3 , 1 , 4 , 4 , 4 , 5 , 3 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 4 , &
      & 0 , 5 , 1 , 5 , 2 , 4 , 3 , 1 , 3 , 0 , 2 , 0 , 6 , 5 , 6 , 2 , 0 , 4 , 3 , 5 , 4 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 4 , &
      & 1 , 3 , 4 , 3 , 5 , 2 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 0 , 2 , 1 , 3 , 5 , 0 , 5 , 5 , 4 , 6 , 2 , 6 , 1 , 5 , 1 , 4 , 2 , &
      & 3 , 4 , 3 , 5 , 4/
!
! ... DATA FOR DRAWING CHARACTERS A TO M.
!
   DATA Chram/0 , 0 , 3 , 6 , 5 , 2 , 1 , 2 , 5 , 2 , 6 , 0 , 0 , 0 , 0 , 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 0 , 3 , 4 , 3 , 5 ,   &
      & 2 , 5 , 1 , 4 , 0 , 0 , 0 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 4 , 0 , 5 , 1 , 5 , 4 , 4 , 6 , 0 , 6 , 0 , 0 , &
      & 4 , 0 , 5 , 2 , 5 , 4 , 5 , 6 , 0 , 6 , 0 , 3 , 3 , 3 , 0 , 3 , 0 , 0 , 5 , 0 , 5 , 6 , 0 , 6 , 0 , 3 , 3 , 3 , 0 , 3 , 0 , &
      & 0 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 4 , 0 , 5 , 1 , 5 , 3 , 3 , 3 , 0 , 6 , 0 , 0 , 0 , 3 , 5 , 3 , 5 , 0 , &
      & 5 , 6 , 2 , 6 , 4 , 6 , 3 , 6 , 3 , 0 , 2 , 0 , 4 , 0 , 3 , 6 , 5 , 6 , 4 , 6 , 4 , 1 , 3 , 0 , 1 , 0 , 0 , 1 , 0 , 6 , 0 , &
      & 0 , -5 , 0 , 0 , 3 , 5 , 6 , 0 , 6 , 0 , 0 , 5 , 0 , 0 , 0 , 0 , 6 , 3 , 0 , 6 , 6 , 6 , 0/
!
! ... DATA FOR DRAWING CHARACTERS N TO Z.
!
   DATA Chrnz/0 , 0 , 0 , 6 , 5 , 0 , 5 , 6 , 6 , 5 , 5 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 5 , 0 , 6 , 1 , 6 , 5 , 0 , 0 , 0 ,   &
      & 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 0 , 3 , 6 , 5 , 5 , 6 , 1 , 6 , 0 , 5 , 0 , 1 , 1 , 0 , 5 , 0 , 6 , 1 , 6 , 5 , -4 , 2 ,&
      & 6 , 0 , 0 , 0 , 0 , 6 , 4 , 6 , 5 , 5 , 5 , 4 , 4 , 3 , 0 , 3 , 3 , 3 , 5 , 0 , 5 , 5 , 4 , 6 , 1 , 6 , 0 , 5 , 0 , 4 , 1 , &
      & 3 , 4 , 3 , 5 , 2 , 5 , 1 , 4 , 0 , 1 , 0 , 0 , 1 , 0 , 6 , 3 , 6 , 3 , 0 , 3 , 6 , 6 , 6 , 0 , 6 , 0 , 1 , 1 , 0 , 4 , 0 , &
      & 5 , 1 , 5 , 6 , 0 , 6 , 3 , 0 , 6 , 6 , 0 , 6 , 1 , 0 , 3 , 4 , 5 , 0 , 6 , 6 , 0 , 6 , 6 , 0 , -6 , 6 , 0 , 0 , 0 , 6 , 3 ,&
      & 3 , 3 , 0 , 3 , 3 , 6 , 6 , 0 , 6 , 6 , 6 , 0 , 0 , 6 , 0/
!
! ... DATA FOR DRAWING CHARACTERS ( TO -.
!
   DATA Chlpqm/5 , 6 , 3 , 4 , 3 , 2 , 5 , 0 , 1 , 6 , 3 , 4 , 3 , 2 , 1 , 0 , 3 , 5 , 3 , 1 , -1 , 3 , 5 , 3 , 1 , 3 , 5 , 3 , 1 , &
      & 5 , 5 , 1 , -3 , 5 , 3 , 1 , -5 , 5 , 1 , 1 , -5 , 3 , 1 , 3 , 0 , 0 , 6 , 6 , 1 , 4 , 4 , 4 , -1 , 2 , 4 , 2 , 2 , 0 , 2 , &
      & 1 , 3 , 1 , 3 , 0 , 2 , 0 , 1 , 0 , 3 , 2 , 6 , 5 , 5 , 6 , 1 , 6 , 0 , 5 , 0 , 4 , 1 , 3 , 5 , 3 , 6 , 2 , 6 , 1 , 5 , 0 , &
      & 3 , 0 , 3 , 6 , 3 , 0 , 1 , 0 , 0 , 1 , 3 , 6 , 3 , 4/
!
! ... DATA FOR DRAWING DOT, SQUARE, DIAMOND, TRIANGLE.
!
   DATA Chrsym/3 , 4 , 2 , 3 , 3 , 2 , 4 , 3 , 3 , 4 , 0 , 0 , 0 , 6 , 6 , 6 , 6 , 0 , 0 , 0 , 3 , 6 , 0 , 3 , 3 , 0 , 6 , 3 , 3 ,  &
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
   DATA Bufsiz/0/ , Camera , Bframs , Pltmdl , Tapden/2 , 1 , 1HM , 1 , 0/ , Nopens , Papsiz , Paptyp , Pensiz , Penclr/8 , 2*0. ,  &
       &4HVELL , 2HUM , 8*1 , 8*4HBLAC , 8*1HK/ , Scale(2) , Fscale , Maxdef/1. , 1 , 0./ , Axis , Daxis , Vangle/1 , 2 , 3 , 1 ,   &
      & 2 , 3 , 0. , -1.E10 , 34.27 , 23.17 , 0./ , Fvp , Prject , D02 , D03 , S0s/1 , 1 , 1. , 2. , 2.756/ , Norg , Org , Forg ,   &
      & Edge/10 , 0 , 1 , 22*0. , 22*1./ , Ncntr , Cntr , Icntvl , Where , Direct , Flag , Lasset/10 , 50*0.0 , 1 , 1 , 2 , 0 , 0/ ,&
      & Fpltit , Pltitl/0 , 17*4H    / , Offscl/0/
!
! ... PLOTTER DATA.
!
   DATA Model , Ploter , Chrscl/ - 1 , 1 , 1.0/
!
! ... 1  NASTRAN GENERAL PURPOSE MICROFILM PLOTTER.
!
   DATA Data(1,1)/1023.0/ , Data(2,1)/1023.0/ , Data(3,1)/146.1429/ , Data(4,1)/8.0/ , Data(5,1)/16.0/ , Data(6,1)/1023.0/ ,        &
      & Data(8,1)/0.0/ , Data(9,1)/0.0/ , Data(11,1)/4HPLT2/ , Data(14,1)/1484.761/ , Data(15,1)/0.0/ , Data(16,1)/0.0/ , Data(17,1)&
      & /0.0/ , Data(18,1)/0.0/ , Data(19,1)/0.0/ , Data(20,1)/0.0/
!
! ... 2  NASTRAN GENERAL PURPOSE TABLE OR DRUM PLOTTER
!
   DATA Data(1,2)/3000.0/ , Data(2,2)/3000.0/ , Data(3,2)/100.0/ , Data(4,2)/8.0/ , Data(5,2)/16.0/ , Data(6,2)/3000.0/ , Data(8,2) &
      & /0.0/ , Data(9,2)/0.0/ , Data(11,2)/4HPLT2/ , Data(14,2)/100.0/ , Data(15,2)/0.0/ , Data(16,2)/0.0/ , Data(17,2)/0.0/ ,     &
      & Data(18,2)/0.0/ , Data(19,2)/0.0/ , Data(20,2)/0.0/
!
   DATA Npens(1,1) , Pltype(1,1) , Pbfsiz(1,1) , Eof(1,1)/64 , -1 , 3000 , 1/ , Npens(1,2) , Pltype(1,2) , Pbfsiz(1,2) , Eof(1,2)   &
      & /64 , -2 , 3000 , 1/
!
! ... SYMBOL DATA.
!
!           X, *, +, -, DOT, CIRCLE, SQUARE, DIAMOND, TRIANGLE
   DATA Nsym , Symbol/9 , 34 , 41 , 39 , 40 , 48 , 49 , 50 , 51 , 52 , 11*0 , 34 , 41 , 39 , 40 , 48 , 49 , 50 , 51 , 52 , 11*0/
!
! ... PLOTTER SCRATCH AREA
!
!          NCOR = ARRAY LENGTH
   DATA Ncor , Pltsc/50 , 50*0/
!
! ... DATA FOR DRAWING A X-Y-Z COORDINATE TRIAD IN /DRWAXS/
!     G   - X,Y,Z COORD. POINT DATA AND SYMBOLS
!
   DATA G/9*0.0 , 1HX , 1HY , 1HZ/
!
END BLOCKDATA plotbd
