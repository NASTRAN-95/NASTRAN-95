
SUBROUTINE ploadx
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Lc , Slt
   REAL Pi , Z(1)
   COMMON /condsa/ Pi
   COMMON /loadx / Lc , Slt
   COMMON /ssg1ax/ Z
!
! Local variable declarations
!
   REAL flag , gd(12) , p(3) , p1 , p3 , pn(3) , rl , slc(5) , zl
   INTEGER i , igd(1) , islc(1) , j , k , nam(2)
!
! End of declarations
!
!
!     PLOADX BUILDS THE PRESSURE LOADS FROM A PLOADX CARD FOR THE
!     TRIAX6 ELEMENT
!
   EQUIVALENCE (slc(1),islc(1),p1) , (slc(2),p3) , (gd(1),igd(1))
   DATA nam/4HPLOA , 4HDX  /
!
   CALL read(*100,*200,Slt,slc,5,0,flag)
   j = 1
   DO i = 1 , 3
      CALL fndpnt(gd(j),islc(i+2))
      j = j + 4
   ENDDO
   rl = gd(10) - gd(2)
   zl = gd(12) - gd(4)
!
!     LOADS IN NORMAL DIRECTION
!
   pn(1) = Pi/30.*(9.0*gd(2)*p1+gd(2)*p3+gd(10)*p1-gd(10)*p3)
   pn(2) = Pi/7.5*(3.*(gd(2)*p1+gd(10)*p3)+2.*(gd(2)*p3+gd(10)*p1))
   pn(3) = Pi/30.*(9.0*gd(10)*p3+gd(2)*p3+gd(10)*p1-gd(2)*p1)
!
   j = 1
   DO i = 1 , 3
      p(1) = -zl*pn(i)
      p(2) = 0.0
      p(3) = rl*pn(i)
!
!     CONVERT TO GLOBAL IF NEEDED, AND INSERT INTO THE LOAD VECTOR
!
      IF ( igd(j)/=0 ) CALL basglb(p,p,gd(j+1),igd(j))
      CALL fndsil(islc(i+2))
      k = islc(i+2)
      Z(k) = Z(k) + p(1)
      Z(k+1) = Z(k+1) + p(2)
      Z(k+2) = Z(k+2) + p(3)
      j = j + 4
   ENDDO
   GOTO 99999
!
!     ERROR MESSAGE
!
 100  j = -1
   GOTO 300
 200  j = -2
 300  CALL mesage(j,Slt,nam)
!
99999 RETURN
END SUBROUTINE ploadx
