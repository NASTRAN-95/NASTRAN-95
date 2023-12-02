!*==xygraf.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xygraf(Graph)
   USE c_system
   USE c_xmssg
   USE c_xypppp
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3,8) :: Graph
!
! Local variable declarations rewritten by SPAG
!
   REAL :: f , temp
   INTEGER :: i , i1 , i2 , j , n , n1 , n2 , n3 , n4 , n5
   INTEGER , DIMENSION(5) :: m
   INTEGER , DIMENSION(5,10) , SAVE :: nu
   EXTERNAL page1
!
! End of declarations rewritten by SPAG
!
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
   IF ( iframe<0 .OR. iframe>99999 ) iframe = 0
   n = 100000
   DO i = 1 , 5
      n = n/10
      m(i) = iframe/n
      iframe = iframe - m(i)*n
      m(i) = m(i) + 1
   ENDDO
   n1 = m(1)
   n2 = m(2)
   n3 = m(3)
   n4 = m(4)
   n5 = m(5)
   lines = lines + 21
   itlns = itlns + 21
   WRITE (l,99001) (nu(i,n1),nu(i,n2),nu(i,n3),nu(i,n4),nu(i,n5),i=1,5)
99001 FORMAT (1H0,60X,25HF     R     A     M     E,//,5(59X,A4,2X,A4,2X,A4,2X,A4,2X,A4,/))
   WRITE (l,99002) titlec , (xtitle(i),i=1,28)
99002 FORMAT (1H0,4X,31A4,A3,/1H0,4X,15HX-AXIS TITLE = ,28A4,/1H0)
!
   IF ( i123==1 ) THEN
!
!     WHOLE FRAME TITLE FRAME
!
      WRITE (l,99010)
      WRITE (l,99003) titlel
99003 FORMAT (13X,1HI,117X,1HI/13X,2HI ,14A4,60X,1HI,/13X,1HI,117X,1HI)
      WRITE (l,99004) Graph(1,6) , Graph(1,7) , Graph(1,8)
99004 FORMAT (13X,1HI,1P,E14.6,37X,1P,E14.6,37X,1P,E14.6,2H I)
      WRITE (l,99010)
   ELSE
!
!     DUAL FRAME TITLE FRAME
!
      WRITE (l,99011)
      WRITE (l,99005) titlel , titler
99005 FORMAT (13X,1HI,57X,3HI I,57X,1HI,/13X,2HI ,14A4,4HI I ,14A4,1HI,/13X,1HI,57X,3HI I,57X,1HI)
      WRITE (l,99006) (Graph(i,6),Graph(i,7),Graph(i,8),i=2,3)
99006 FORMAT (12X,2(2H I,1P,E14.6,1P,E21.6,1P,E21.6,2H I))
      WRITE (l,99011)
   ENDIF
!
!     DUMP GRAPH
!
   f = xmin - xinc
   DO i = 1 , maxplt
      temp = f + float(i)*xinc
      i1 = (i-1)*30 + 1
      i2 = i1 + 29
      lines = lines + 1
      itlns = itlns + 1
      IF ( lines<=nlpp ) THEN
         WRITE (l,99007) temp , (z(j),j=i1,i2)
99007    FORMAT (1X,1P,E11.4,1X,29A4,A3)
      ELSE
         lines = 1
         WRITE (l,99008) temp , (z(j),j=i1,i2)
99008    FORMAT (1H1,1P,E11.4,1X,29A4,A3)
      ENDIF
   ENDDO
!
   IF ( i123==1 ) THEN
      WRITE (l,99010)
   ELSE
      WRITE (l,99011)
   ENDIF
!
   IF ( exceed ) WRITE (l,99009) uim
99009 FORMAT (A29,'. THERE WERE MORE POINTS BELOW THIS POINT WHICH WE','ARE NOT PLOTTED HERE',/5X,'DUE TO CORE RESTRICTION')
   exceed = .FALSE.
99010 FORMAT (13X,1H+,117(1H-),1H+)
99011 FORMAT (13X,1H+,57(1H-),3H+ +,57(1H-),1H+)
END SUBROUTINE xygraf
