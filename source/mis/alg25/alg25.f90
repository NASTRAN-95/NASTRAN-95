!*==alg25.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg25(Ix,Lx,Log1,X,Y1)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ix
   INTEGER :: Lx
   INTEGER :: Log1
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Y1
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: blank , cross , dash , xi
   INTEGER :: i , kline , l , mx , my , spag_nextblock_1
   REAL , DIMENSION(121) :: line
   REAL , DIMENSION(1) , SAVE :: symbol
   REAL :: xh , xinc , xl , xmax , xmin , xrange , yh , yinc , yinc2 , yl , ymax , ymin , ynum
   REAL , DIMENSION(13) :: xnum
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   DATA symbol/1H*/ , dash/1H-/ , cross/1H+/ , blank/1H / , xi/1HI/
!
   ymin = Y1(1)
   ymax = ymin
   DO i = 1 , Ix
      IF ( Y1(i)<ymin ) ymin = Y1(i)
      IF ( Y1(i)>ymax ) ymax = Y1(i)
   ENDDO
   IF ( ymin==ymax ) THEN
!
      WRITE (Log1,99001)
99001 FORMAT (//35X,54HNO PLOT HAS BEEN MADE BECAUSE 'X' OR 'Y' RANGE IS ZERO)
      RETURN
   ENDIF
   yh = ymax + (ymax-ymin)/18.0
   yl = ymin - (ymax-ymin)/18.0
   xh = 60.0
   IF ( Lx>59 ) xh = float(Lx) + 1.0
   xl = xh - 60.0
   xmax = abs(xh)
   xmin = abs(xl)
   ymin = abs(yl)
   ymax = abs(yh)
   IF ( xmin>xmax ) xmax = xmin
   IF ( ymin>ymax ) ymax = ymin
   xmax = alog10(xmax)
   ymax = alog10(ymax)
   IF ( xmax<0.0 ) xmax = xmax - 1.0
   IF ( ymax<0.0 ) ymax = ymax - 1.0
   mx = -xmax
   my = -ymax
   WRITE (Log1,99002) mx , my
99002 FORMAT (20X,46HSCALES - 'X' IS SHOWN TIMES 10 TO THE POWER OF,I3,40H   'Y' IS SHOWN TIMES 10 TO THE POWER OF,I3,/)
   yinc = (yh-yl)/54.0
   yinc2 = yinc/2.0
   xrange = xh - xl
   DO kline = 1 , 55
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            IF ( kline==1 .OR. kline==55 ) THEN
               DO l = 2 , 120
                  line(l) = dash
               ENDDO
               line(1) = cross
               line(121) = cross
               DO l = 11 , 111 , 10
                  line(l) = xi
               ENDDO
            ELSE
               DO l = 2 , 120
                  line(l) = blank
               ENDDO
               IF ( kline==7 .OR. kline==13 .OR. kline==19 .OR. kline==25 .OR. kline==31 .OR. kline==37 .OR. kline==43 .OR.         &
                  & kline==49 ) THEN
                  line(1) = dash
                  line(121) = dash
               ELSE
                  line(1) = xi
                  line(121) = xi
               ENDIF
               DO i = 1 , Ix
                  IF ( Y1(i)<=yh+yinc2 .AND. Y1(i)>yh-yinc2 ) THEN
                     l = (X(i)-xl)/xrange*120.0 + 1.5
                     line(l) = symbol(1)
                  ENDIF
               ENDDO
               IF ( kline/=1 .AND. kline/=7 .AND. kline/=13 .AND. kline/=19 .AND. kline/=25 .AND. kline/=31 .AND. kline/=37 .AND.   &
                  & kline/=43 .AND. kline/=49 .AND. kline/=55 ) THEN
                  WRITE (Log1,99003) line
99003             FORMAT (8X,121A1)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            ynum = yh*10.0**my
            WRITE (Log1,99004) ynum , line
99004       FORMAT (1X,F6.3,1X,121A1)
            spag_nextblock_1 = 2
         CASE (2)
            yh = yh - yinc
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   xnum(1) = xl*10.0**mx
   xinc = ((xh-xl)/12.0)*10.0**mx
   DO i = 2 , 13
      xnum(i) = xnum(i-1) + xinc
   ENDDO
   WRITE (Log1,99005) xnum
99005 FORMAT (6X,12(F6.3,4X),F6.3)
END SUBROUTINE alg25
