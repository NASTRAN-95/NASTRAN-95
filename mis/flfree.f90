
SUBROUTINE flfree(Frrec,Afe,Nafe,Kge,Nkge)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(3) , Rho , Sysbuf , Tilt(2) , Z(1)
   LOGICAL Error
   INTEGER Ibgpdt , Icore , Igrav , Inflag , Isil , Iz(1) , Lcore , Matid , Ngbpdt , Ngrav , Nofree , Nograv , Nout , Nsil
   CHARACTER*23 Ufm
   COMMON /blank / Nograv , Nofree , Tilt
   COMMON /flbptr/ Error , Icore , Lcore , Ibgpdt , Ngbpdt , Isil , Nsil , Igrav , Ngrav
   COMMON /matin / Matid , Inflag
   COMMON /matout/ Dum , Rho
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Nafe , Nkge
   DOUBLE PRECISION Afe(16) , Kge(16)
   INTEGER Frrec(7)
!
! Local variable declarations
!
   DOUBLE PRECISION a , afact , r12(3) , r13(3) , rhoxg , rt(3)
   REAL g
   INTEGER gf1 , gf2 , gf3 , grid(3,4) , i , icol , iloc , irow , it , itria , j , lgrav , ngridf
   LOGICAL grav , ltilt
!
! End of declarations
!
!
!     CALCULATES THE AREA FACTOR MATRIX AND GRAVITATIONAL STIFFNESS
!     MATRIX FOR A SINGLE FLUID ELEMENT ON THE FREE SURFACE
!
   EQUIVALENCE (Z(1),Iz(1))
!
!     GRID POINTS DEFINING FOUR OVERLAPING TRIANGLES IN A QUAD
!
   DATA grid/1 , 2 , 3 , 2 , 3 , 4 , 3 , 4 , 1 , 4 , 1 , 2/
   DATA ltilt/.FALSE./
!
!
!     CALCULATE SIZE OF ELEMENT MATRICES
!
   ngridf = 4
   IF ( Frrec(6)<0 ) ngridf = 3
   Nafe = ngridf*ngridf*2
   Nkge = 0
!
!     OBTAIN MATERIAL PROPERTY AND GRAVITY DATA IF A GRAV ID IS GIVEN
!
   grav = .FALSE.
   IF ( Frrec(7)/=0 ) THEN
      Inflag = 11
      Matid = Frrec(2)
      CALL mat(Frrec(1))
!
      IF ( Ngrav/=0 ) THEN
         lgrav = Igrav + Ngrav - 1
         DO i = Igrav , lgrav , 6
            IF ( Iz(i)==Frrec(7) ) GOTO 50
!
         ENDDO
      ENDIF
!
!     ERROR CONDITIONS
!
      WRITE (Nout,99001) Ufm , Frrec(1) , Frrec(7)
99001 FORMAT (A23,' 8012, FLUID ELEMENT',I9,' ON A CFFREE CARD REFERENCES UNDEFINED GRAVITY ID',I9)
      Error = .TRUE.
      GOTO 99999
!
 50   g = sqrt(Z(i+3)**2+Z(i+4)**2+Z(i+5)**2)
!
!     USING THE FIRST GRAV VECTOR DETERMING THE FREE SURFACE PLOTTING
!     ANGLE
!
      IF ( .NOT.(ltilt) ) THEN
         Tilt(1) = Z(i+5)/g
         Tilt(2) = Z(i+3)/g
         ltilt = .FALSE.
      ENDIF
!
      g = g*Z(i+2)
      rhoxg = dble(Rho)*dble(g)
      Nkge = Nafe
      Nograv = 1
      grav = .TRUE.
   ENDIF
!
!     DETERMINE NUMBER OF OVERLAPING TRIANGLES TO BE UESED
!
!     1 IF TRIANGLAR FLUID FACE
!     4 IF QUADRATIC FLUID FACE
!
   itria = 4
   IF ( ngridf/=4 ) itria = 1
!
!     ZERO OUT GRAVITATIONAL STIFFNESS AND AREA FACTOR MATRIX
!
   DO i = 1 , 16
      Kge(i) = 0.0D0
      Afe(i) = 0.0D0
   ENDDO
!
!     LOOP OVER TRIANGLES
!
!     FIRST LOCATE GRID POINT COORDINATES FOR CORNERS FO THIS TRIANGLE
!
   DO it = 1 , itria
!
      i = grid(1,it)
      gf1 = Ibgpdt + (Frrec(i+2)-1)*4
      i = grid(2,it)
      gf2 = Ibgpdt + (Frrec(i+2)-1)*4
      i = grid(3,it)
      gf3 = Ibgpdt + (Frrec(i+2)-1)*4
!
!     CALCUATE AREA OF TRIAGLE
!     DIVIDE AREA BY TWO IF OVERLAPPING TRIAGLES USED
!
      DO i = 1 , 3
         r12(i) = Z(gf2+i) - Z(gf1+i)
         r13(i) = Z(gf3+i) - Z(gf1+i)
      ENDDO
!
      CALL dcross(r12,r13,rt)
!
      a = dsqrt(rt(1)*rt(1)+rt(2)*rt(2)+rt(3)*rt(3))/2.0D0
      IF ( itria==4 ) a = a/2.0D0
!
!     INSERT AREA AND STIFFNESS CONTRIBUTIONS INTO FULL SIZE
!     ELEMTENT MATRICES
!
      DO i = 1 , 3
         icol = grid(i,it)
         iloc = ngridf*(icol-1)
         DO j = 1 , 3
            irow = grid(j,it)
            IF ( irow==icol ) afact = a/6.0D0
            IF ( irow/=icol ) afact = a/12.0D0
            Afe(iloc+irow) = Afe(iloc+irow) + afact
            IF ( grav ) Kge(iloc+irow) = Kge(iloc+irow) + rhoxg*afact
         ENDDO
      ENDDO
!
   ENDDO
!
   RETURN
99999 RETURN
END SUBROUTINE flfree
