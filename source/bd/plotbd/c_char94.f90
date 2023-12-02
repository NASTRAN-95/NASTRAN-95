!*==/home/marcusmae/nasa/nastran/SPAGged/C_CHAR94.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_CHAR94
   INTEGER, DIMENSION(60, 4) :: Char
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
   INTEGER , DIMENSION(60,3) :: char1
   INTEGER , DIMENSION(60,1) :: char2
! ... EQUIV FOR   /CHAR94/...
   EQUIVALENCE (Char(1,1),Char1(1,1)) , (Char(1,4),Char2(1,1))
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

END MODULE C_CHAR94
