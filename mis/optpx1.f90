
SUBROUTINE optpx1(*,Stor,Nogo,Nen,Loc1)
   IMPLICIT NONE
   REAL Core(1) , Skp1(5)
   INTEGER Iy(1) , Outtap , Sysbuf , X(7) , Ycor
   CHARACTER*23 Ufm
   COMMON /blank / Skp1 , Ycor
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Core
   INTEGER Loc1 , Nen , Nogo
   INTEGER Stor(15)
   INTEGER i1 , i2 , i3 , l , nam(2) , thru
!
!     PROCESS PID DATA ON PLIMIT CARD
!
   !>>>>EQUIVALENCE (Core(1),X(1)) , (X(7),Iy(1))
   DATA thru/4HTHRU/
!
   nam(1) = Stor(1)
   nam(2) = Stor(2)
   IF ( Stor(6)==thru ) THEN
!
!     USER SPECIFIED BY USING THRU
!
      l = 8
      Stor(9) = Stor(8)
      Stor(8) = Stor(5)
   ELSE
!
!     USER SPECIFIED BY EXPLICIT ID-S
!
      CALL sort(0,0,1,1,Stor(5),5)
!
!     CREATE PSEUDO THRU RANGE
!     LOCATE FIRST NONZERO
!
      DO l = 5 , 9
         IF ( Stor(l)/=0 ) GOTO 100
      ENDDO
      CALL page2(-2)
      WRITE (Outtap,99001) Ufm , nam
99001 FORMAT (A23,' 2293, NO PID ENTRIES ON PLIMIT CARD (',2A4,2H).)
      Nogo = Nogo + 1
      GOTO 400
   ENDIF
!
!     LOOP ON ENTRIES
!
 100  i1 = Stor(l)
   i3 = 1
   DO
      i2 = Stor(l+1)
      IF ( l<9 ) THEN
         IF ( i2-i1<i3 ) GOTO 300
         IF ( i2-i1==i3 ) THEN
!
!     THRU CAN BE EXPANDED
!
            l = l + 1
            i3 = i3 + 1
         ELSE
            EXIT
         ENDIF
      ELSEIF ( l==9 ) THEN
         EXIT
      ELSE
!
         CALL mesage(-7,0,nam)
         GOTO 500
      ENDIF
   ENDDO
!
!     PUT OUT I1,I2
!
   Stor(1) = i1
   Stor(2) = Stor(l)
   IF ( Loc1+3+Nen>Ycor ) GOTO 500
   CALL bishel(*300,Stor,Nen,4,Iy(Loc1))
 200  l = l + 1
   IF ( l>9 ) GOTO 400
   GOTO 100
!
!     DUPLICATE ENTRIES FOUND
!
 300  CALL page2(-2)
   WRITE (Outtap,99002) Ufm , i1 , i2 , nam
99002 FORMAT (A23,' 2294, DUPLICATE',I8,' THRU',I8,' RANGE FOR ELEMENT',1X,2A4,' REJECTED PLIMIT. SCAN CONTINUED.')
   Nogo = Nogo + 1
   GOTO 200
!
!     THIS PLIMIT FINISHED
!
 400  RETURN
!
!     INSUFFICIENT CORE
!
 500  Stor(1) = nam(1)
   Stor(2) = nam(2)
   RETURN 1
END SUBROUTINE optpx1