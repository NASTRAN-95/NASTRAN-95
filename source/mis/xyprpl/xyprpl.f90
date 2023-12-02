!*==xyprpl.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xyprpl
   USE c_output
   USE c_system
   USE c_xypppp
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: any
   INTEGER , SAVE :: blank , clorwd , eor , eye , inprwd , minxd , noeor , xypltt
   REAL , DIMENSION(2) :: buf
   INTEGER :: buff , curvch , i , icol , icore , icurve , irow , j , n , nwords
   REAL :: center , delta , temp , xmax , xratio , ymax , ymin , yratio
   REAL , DIMENSION(300) :: fid
   REAL , DIMENSION(3,8) :: graph
   INTEGER , DIMENSION(2) :: ibuf
   INTEGER , DIMENSION(3,8) , SAVE :: igraph
   INTEGER , DIMENSION(10) , SAVE :: symbol
   EXTERNAL close , fwdrec , korsz , open , read , xychar , xygraf
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         icore = korsz(z)
         buff = icore - sysbuf
!
         icore = buff - 1
         maxrow = icore/30
         any = .FALSE.
         exceed = .FALSE.
         CALL open(*99999,xypltt,z(buff),inprwd)
         spag_nextblock_1 = 2
      CASE (2)
         CALL fwdrec(*40,xypltt)
!
!     READ ID RECORD
!
 20      CALL read(*40,*40,xypltt,id(1),300,eor,nwords)
!
!     SKIP RECORD IF PLOT ONLY
!
         IF ( id(289)==0 .OR. id(289)==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SKIP INITIALIZATION IF AXIS AND SCALES ARE COMPLETE
!
         icurve = mod(id(3),10)
         IF ( icurve==0 ) icurve = 10
         curvch = symbol(icurve)
         IF ( id(8)/=0 ) THEN
!
!     1 = UPPER,  0 = WHOLE,  -1 = LOWER
!
            IF ( id(7)>=0 ) THEN
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
               xmin = fid(15)
               xmax = fid(17)
               temp = amin1(400.,float(maxrow))
               temp = amin1(temp,3.0*float(id(246)))
               xinc = fid(minxd)
               xinc = amax1(xinc,(xmax-xmin)/temp)
               xratio = 1.0/xinc
               maxplt = abs((xmax-xmin)/xinc+1.5)
               maxplt = min0(maxplt,maxrow)
               n = 30*maxplt
               DO i = 1 , n
                  z(i) = blank
               ENDDO
            ENDIF
!
!     FILL CURVE TITLE AND HEADING
!     DEMO D10023A INDICATES HEADING WORDS (1-32, AND 36) ARE NUMERIC
!     0 OR 1. REPLACE THEM BY BLANKS.
!     (DON'T KNOW WHO PUTS THOSE 0 & 1 HERE)
!
            DO i = 1 , 32
               xtitle(i) = id(i+178)
               titlec(i) = id(i+145)
            ENDDO
            DO i = 1 , 96
               ihead(i) = id(i+50)
               IF ( ihead(i)==0 ) ihead(i) = blank
            ENDDO
            IF ( ihead(36)==1 ) ihead(36) = blank
            iframe = id(281)
            IF ( id(7)<0 ) THEN
               i123 = 2
               DO i = 1 , 14
                  titlel(i) = id(i+210)
               ENDDO
            ELSEIF ( id(7)==0 ) THEN
               i123 = 1
               DO i = 1 , 14
                  titlel(i) = id(i+210)
               ENDDO
            ELSE
               i123 = 3
               DO i = 1 , 14
                  titler(i) = id(i+210)
               ENDDO
            ENDIF
!
!     PLOT GRID  (WHOLE LOWER OR UPPER)
!
            DO j = 1 , 3
               DO i = 1 , maxplt
                  CALL xychar(i,igraph(i123,j),eye)
               ENDDO
            ENDDO
!
!     UNITS AND VALUES
!
            ymin = fid(23)
            ymax = fid(25)
            delta = ymax - ymin
            IF ( delta==0.0 ) delta = ymin
            IF ( delta==0.0 ) delta = 1.0
            yratio = float(igraph(i123,4))/delta
            center = ymin + delta/2.0
            graph(i123,5) = yratio
            graph(i123,6) = ymin
            graph(i123,7) = center
            graph(i123,8) = ymax
         ENDIF
         DO
!
!     READ DATA AND PLOT POINTS
!
            CALL read(*40,*20,xypltt,buf(1),2,noeor,nwords)
            IF ( ibuf(1)/=1 ) THEN
               irow = (buf(1)-xmin)*xratio + 1.5
               icol = (buf(2)-ymin)*yratio + 1.5
               icol = icol + igraph(i123,1) - 1
               CALL xychar(irow,icol,curvch)
            ENDIF
         ENDDO
!
!     TERMINIATE  (DUMP GRAPH IF ANY)
!
 40      IF ( any ) CALL xygraf(igraph)
         CALL close(xypltt,clorwd)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE xyprpl
