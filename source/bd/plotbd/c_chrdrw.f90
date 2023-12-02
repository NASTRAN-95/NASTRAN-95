!*==/home/marcusmae/nasa/nastran/SPAGged/C_CHRDRW.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_CHRDRW
   INTEGER, DIMENSION(2, 350) :: Chr
   INTEGER, DIMENSION(60) :: Chrind
   INTEGER :: Lstchr
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
   INTEGER , DIMENSION(2,52) , SAVE :: chlpqm
   INTEGER , DIMENSION(2,79) , SAVE :: chr19
   INTEGER , DIMENSION(2,88) , SAVE :: chram
   INTEGER , DIMENSION(2,84) , SAVE :: chrnz
   INTEGER , DIMENSION(2,19) , SAVE :: chrsym
! ... EQUIV FOR   /CHRDRW/...
   EQUIVALENCE (Chr(1,1),Chr19(1,1)) , (Chr(1,80),Chram(1,1)) , (Chr(1,168),Chrnz(1,1)) , (Chr(1,252),Chlpqm(1,1)) ,                &
    & (Chr(1,304),Chrsym(1,1))
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

END MODULE C_CHRDRW
