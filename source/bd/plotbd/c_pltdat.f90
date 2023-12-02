!*==/home/marcusmae/nasa/nastran/SPAGged/C_PLTDAT.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_PLTDAT
   INTEGER, DIMENSION(2) :: Axymax, Xymax, Xymin
   REAL :: Chrscl
   REAL, DIMENSION(20, 2) :: Data
   INTEGER :: Model, Ploter
   INTEGER, DIMENSION(20) :: Pltdat
   INTEGER, DIMENSION(11) :: Xyedge
!    1    ... PLOTTING DATA
!    2    ... PEN + PAPER DATA
!    3    ... SCALING DATA
!    4    ... VIEWING DATA
!    5    ... VANTAGE POINT,               PROJECTION,OCULAR SEPARATION
!    6    ... ORIGIN DATA
!    7    ... CONTOUR PLOTTING DATA
!    8    ... DATA FOR USER PLOT TITLE CARD
!    9    ... OFFSET SCALE (WILL BE SET TO 1 BY PLTSET)
   INTEGER , DIMENSION(20,2) :: eof , npens , pbfsiz , pltype
!
! ... EQUIV FOR   /PLTDAT/...
   EQUIVALENCE (Data(7,1),Npens(1,1)) , (Data(10,1),Pltype(1,1)) , (Data(12,1),Pbfsiz(1,1)) , (Data(13,1),Eof(1,1))
!
   DATA npens(1,1) , pltype(1,1) , pbfsiz(1,1) , eof(1,1)/64 , -1 , 3000 , 1/ , npens(1,2) , pltype(1,2) , pbfsiz(1,2) , eof(1,2)   &
      & /64 , -2 , 3000 , 1/
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

END MODULE C_PLTDAT
