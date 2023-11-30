
SUBROUTINE xyprpl
   IMPLICIT NONE
   LOGICAL Exceed
   REAL Fid(300) , Xinc , Xmin
   INTEGER I123 , Id(300) , Iframe , Ihead(96) , L , Maxplt , Maxrow , Sysbuf , Titlec(32) , Titlel(14) , Titler(14) , Xtitle(32) , &
         & Z(1)
   COMMON /output/ Ihead
   COMMON /system/ Sysbuf , L
   COMMON /xypppp/ Iframe , Titlec , Titlel , Titler , Xtitle , Id , Maxplt , Xmin , Xinc , Exceed , I123 , Maxrow
   COMMON /zzzzzz/ Z
   LOGICAL any
   INTEGER blank , buff , clorwd , curvch , eor , eye , i , ibuf(2) , icol , icore , icurve , igraph(3,8) , inprwd , irow , j ,     &
         & minxd , n , noeor , nwords , symbol(10) , xypltt
   REAL buf(2) , center , delta , graph(3,8) , temp , xmax , xratio , ymax , ymin , yratio
   INTEGER korsz
!
   !>>>>EQUIVALENCE (Fid(1),Id(1)) , (graph(1,1),igraph(1,1)) , (buf(1),ibuf(1))
   DATA symbol/1H* , 1H0 , 1HA , 1HB , 1HC , 1HD , 1HE , 1HF , 1HG , 1HH/
!
!     GRAPH ARRAY CONTENTS
!
!     COL 1 = LEFT COLUMN USED
!     COL 2 = CENTER COLUMN USED
!     COL 3 = RIGHT COLUMN USED
!     COL 4 = WIDTH OF GRAPH
!     COL 5 = YRATIO
!     COL 6 = YMIN
!     COL 7 = CENTER
!     COL 8 = YMAX
!
   DATA igraph(1,1) , igraph(1,2) , igraph(1,3) , igraph(1,4)/1 , 60 , 119 , 118/
   DATA igraph(2,1) , igraph(2,2) , igraph(2,3) , igraph(2,4)/1 , 30 , 59 , 58/
   DATA igraph(3,1) , igraph(3,2) , igraph(3,3) , igraph(3,4)/61 , 90 , 119 , 58/
   DATA blank/4H    / , eye/4HI   /
   DATA xypltt , minxd , noeor , eor , inprwd , clorwd/201 , 10 , 0 , 1 , 0 , 1/
!
!
   icore = korsz(Z)
   buff = icore - Sysbuf
!
   icore = buff - 1
   Maxrow = icore/30
   any = .FALSE.
   Exceed = .FALSE.
   CALL open(*99999,xypltt,Z(buff),inprwd)
 100  CALL fwdrec(*300,xypltt)
!
!     READ ID RECORD
!
 200  CALL read(*300,*300,xypltt,Id(1),300,eor,nwords)
!
!     SKIP RECORD IF PLOT ONLY
!
   IF ( Id(289)==0 .OR. Id(289)==1 ) GOTO 100
!
!     SKIP INITIALIZATION IF AXIS AND SCALES ARE COMPLETE
!
   icurve = mod(Id(3),10)
   IF ( icurve==0 ) icurve = 10
   curvch = symbol(icurve)
   IF ( Id(8)/=0 ) THEN
!
!     1 = UPPER,  0 = WHOLE,  -1 = LOWER
!
      IF ( Id(7)>=0 ) THEN
!
!     OUTPUT OUR GRAPH IF THERE IS ONE TO OUTPUT
!
         IF ( any ) CALL xygraf(igraph)
         any = .TRUE.
!
!     INITIALIZE MATRIX TO ALL BLANKS
!
!
!     COMPUTE XRATIO = LINES/UNIT VALUE    FID(MINXD) = MIN-X  INCREMENT
!
!
!     MAX OF 400 LINES PER PLOT
!
         Xmin = Fid(15)
         xmax = Fid(17)
         temp = amin1(400.,float(Maxrow))
         temp = amin1(temp,3.0*float(Id(246)))
         Xinc = Fid(minxd)
         Xinc = amax1(Xinc,(xmax-Xmin)/temp)
         xratio = 1.0/Xinc
         Maxplt = abs((xmax-Xmin)/Xinc+1.5)
         Maxplt = min0(Maxplt,Maxrow)
         n = 30*Maxplt
         DO i = 1 , n
            Z(i) = blank
         ENDDO
      ENDIF
!
!     FILL CURVE TITLE AND HEADING
!     DEMO D10023A INDICATES HEADING WORDS (1-32, AND 36) ARE NUMERIC
!     0 OR 1. REPLACE THEM BY BLANKS.
!     (DON'T KNOW WHO PUTS THOSE 0 & 1 HERE)
!
      DO i = 1 , 32
         Xtitle(i) = Id(i+178)
         Titlec(i) = Id(i+145)
      ENDDO
      DO i = 1 , 96
         Ihead(i) = Id(i+50)
         IF ( Ihead(i)==0 ) Ihead(i) = blank
      ENDDO
      IF ( Ihead(36)==1 ) Ihead(36) = blank
      Iframe = Id(281)
      IF ( Id(7)<0 ) THEN
         I123 = 2
         DO i = 1 , 14
            Titlel(i) = Id(i+210)
         ENDDO
      ELSEIF ( Id(7)==0 ) THEN
         I123 = 1
         DO i = 1 , 14
            Titlel(i) = Id(i+210)
         ENDDO
      ELSE
         I123 = 3
         DO i = 1 , 14
            Titler(i) = Id(i+210)
         ENDDO
      ENDIF
!
!     PLOT GRID  (WHOLE LOWER OR UPPER)
!
      DO j = 1 , 3
         DO i = 1 , Maxplt
            CALL xychar(i,igraph(I123,j),eye)
         ENDDO
      ENDDO
!
!     UNITS AND VALUES
!
      ymin = Fid(23)
      ymax = Fid(25)
      delta = ymax - ymin
      IF ( delta==0.0 ) delta = ymin
      IF ( delta==0.0 ) delta = 1.0
      yratio = float(igraph(I123,4))/delta
      center = ymin + delta/2.0
      graph(I123,5) = yratio
      graph(I123,6) = ymin
      graph(I123,7) = center
      graph(I123,8) = ymax
   ENDIF
   DO
!
!     READ DATA AND PLOT POINTS
!
      CALL read(*300,*200,xypltt,buf(1),2,noeor,nwords)
      IF ( ibuf(1)/=1 ) THEN
         irow = (buf(1)-Xmin)*xratio + 1.5
         icol = (buf(2)-ymin)*yratio + 1.5
         icol = icol + igraph(I123,1) - 1
         CALL xychar(irow,icol,curvch)
      ENDIF
   ENDDO
!
!     TERMINIATE  (DUMP GRAPH IF ANY)
!
 300  IF ( any ) CALL xygraf(igraph)
   CALL close(xypltt,clorwd)
99999 RETURN
END SUBROUTINE xyprpl