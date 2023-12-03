!*==/home/marcusmae/nasa/nastran/SPAGged/C_XXPARM.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_XXPARM
   INTEGER, DIMENSION(3) :: Axis, Daxis
   INTEGER :: Bframs, Bufsiz, Camera, Color, D0, Defmax, Direct, Flag, Forg, Fpltit, Fscale, Fvp, Icntvl, Lasset, Layer,&
            & Ncntr, Nopens, Norg, Offscl, Org, Penpap, Prject, R0, S0l, S0r, Subcas, T0, Tapden, Value, Where
   REAL, DIMENSION(50) :: Cntr
   REAL :: D02, D03, Maxdef, S0s
   REAL, DIMENSION(11, 4) :: Edge
   INTEGER, DIMENSION(11) :: Origin
   REAL, DIMENSION(2) :: Papsiz, Scale
   INTEGER, DIMENSION(2) :: Paptyp, Pltmdl
   INTEGER, DIMENSION(8, 2) :: Penclr
   INTEGER, DIMENSION(8) :: Pensiz
   INTEGER, DIMENSION(17) :: Pltitl
   REAL, DIMENSION(5) :: Vangle
   INTEGER, DIMENSION(4) :: View
   INTEGER, DIMENSION(11, 3) :: Xy
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

END MODULE C_XXPARM
