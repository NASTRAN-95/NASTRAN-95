
SUBROUTINE xygraf(Graph)
   IMPLICIT NONE
   LOGICAL Exceed
   INTEGER I123 , Id(300) , Idum(6) , Idum2(2) , Iframe , Itlns , L , Lines , Maxplt , Maxrow , Nlpp , Sysbuf , Titlec(32) ,        &
         & Titlel(14) , Titler(14) , Xtitle(32) , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   REAL Xinc , Xmin
   COMMON /system/ Sysbuf , L , Idum , Nlpp , Idum2 , Lines , Itlns
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xypppp/ Iframe , Titlec , Titlel , Titler , Xtitle , Id , Maxplt , Xmin , Xinc , Exceed , I123 , Maxrow
   COMMON /zzzzzz/ Z
   REAL Graph(3,8)
   REAL f , temp
   INTEGER i , i1 , i2 , j , m(5) , n , n1 , n2 , n3 , n4 , n5 , nu(5,10)
!
!
   DATA nu(1,1) , nu(1,2) , nu(1,3)/4H**** , 4H **  , 4H****/ , nu(2,1) , nu(2,2) , nu(2,3)/4H*  * , 4H  *  , 4H   */ , nu(3,1) ,   &
      & nu(3,2) , nu(3,3)/4H*  * , 4H  *  , 4H  * / , nu(4,1) , nu(4,2) , nu(4,3)/4H*  * , 4H  *  , 4H *  / , nu(5,1) , nu(5,2) ,   &
      & nu(5,3)/4H**** , 4H**** , 4H****/
!
!
   DATA nu(1,4) , nu(1,5) , nu(1,6)/4H**** , 4H* *  , 4H****/ , nu(2,4) , nu(2,5) , nu(2,6)/4H   * , 4H* *  , 4H*   / , nu(3,4) ,   &
      & nu(3,5) , nu(3,6)/4H *** , 4H**** , 4H****/ , nu(4,4) , nu(4,5) , nu(4,6)/4H   * , 4H  *  , 4H   */ , nu(5,4) , nu(5,5) ,   &
      & nu(5,6)/4H**** , 4H  *  , 4H****/nu(1,7) , nu(1,8) , nu(1,9)/4H**** , 4H**** , 4H****/ , nu(2,7) , nu(2,8) , nu(2,9)        &
      & /4H*    , 4H   * , 4H*  */ , nu(3,7) , nu(3,8) , nu(3,9)/4H**** , 4H  *  , 4H****/ , nu(4,7) , nu(4,8) , nu(4,9)/4H*  * ,   &
       &4H *   , 4H*  */ , nu(5,7) , nu(5,8) , nu(5,9)/4H**** , 4H*    , 4H****/
!
   DATA nu(1,10)/4H****/ , nu(2,10)/4H*  */ , nu(3,10)/4H****/ , nu(4,10)/4H   */ , nu(5,10)/4H****/
!
   CALL page1
!
!     GRAPH HEADING DATA
!
   IF ( Iframe<0 .OR. Iframe>99999 ) Iframe = 0
   n = 100000
   DO i = 1 , 5
      n = n/10
      m(i) = Iframe/n
      Iframe = Iframe - m(i)*n
      m(i) = m(i) + 1
   ENDDO
   n1 = m(1)
   n2 = m(2)
   n3 = m(3)
   n4 = m(4)
   n5 = m(5)
   Lines = Lines + 21
   Itlns = Itlns + 21
   WRITE (L,99001) (nu(i,n1),nu(i,n2),nu(i,n3),nu(i,n4),nu(i,n5),i=1,5)
99001 FORMAT (1H0,60X,25HF     R     A     M     E,//,5(59X,A4,2X,A4,2X,A4,2X,A4,2X,A4,/))
   WRITE (L,99002) Titlec , (Xtitle(i),i=1,28)
99002 FORMAT (1H0,4X,31A4,A3,/1H0,4X,15HX-AXIS TITLE = ,28A4,/1H0)
!
   IF ( I123==1 ) THEN
!
!     WHOLE FRAME TITLE FRAME
!
      WRITE (L,99010)
      WRITE (L,99003) Titlel
99003 FORMAT (13X,1HI,117X,1HI/13X,2HI ,14A4,60X,1HI,/13X,1HI,117X,1HI)
      WRITE (L,99004) Graph(1,6) , Graph(1,7) , Graph(1,8)
99004 FORMAT (13X,1HI,1P,E14.6,37X,1P,E14.6,37X,1P,E14.6,2H I)
      WRITE (L,99010)
   ELSE
!
!     DUAL FRAME TITLE FRAME
!
      WRITE (L,99011)
      WRITE (L,99005) Titlel , Titler
99005 FORMAT (13X,1HI,57X,3HI I,57X,1HI,/13X,2HI ,14A4,4HI I ,14A4,1HI,/13X,1HI,57X,3HI I,57X,1HI)
      WRITE (L,99006) (Graph(i,6),Graph(i,7),Graph(i,8),i=2,3)
99006 FORMAT (12X,2(2H I,1P,E14.6,1P,E21.6,1P,E21.6,2H I))
      WRITE (L,99011)
   ENDIF
!
!     DUMP GRAPH
!
   f = Xmin - Xinc
   DO i = 1 , Maxplt
      temp = f + float(i)*Xinc
      i1 = (i-1)*30 + 1
      i2 = i1 + 29
      Lines = Lines + 1
      Itlns = Itlns + 1
      IF ( Lines<=Nlpp ) THEN
         WRITE (L,99007) temp , (Z(j),j=i1,i2)
99007    FORMAT (1X,1P,E11.4,1X,29A4,A3)
      ELSE
         Lines = 1
         WRITE (L,99008) temp , (Z(j),j=i1,i2)
99008    FORMAT (1H1,1P,E11.4,1X,29A4,A3)
      ENDIF
   ENDDO
!
   IF ( I123==1 ) THEN
      WRITE (L,99010)
   ELSE
      WRITE (L,99011)
   ENDIF
!
   IF ( Exceed ) WRITE (L,99009) Uim
99009 FORMAT (A29,'. THERE WERE MORE POINTS BELOW THIS POINT WHICH WE','ARE NOT PLOTTED HERE',/5X,'DUE TO CORE RESTRICTION')
   Exceed = .FALSE.
99010 FORMAT (13X,1H+,117(1H-),1H+)
99011 FORMAT (13X,1H+,57(1H-),3H+ +,57(1H-),1H+)
END SUBROUTINE xygraf
