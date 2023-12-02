!*==flfree.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flfree(Frrec,Afe,Nafe,Kge,Nkge)
USE C_BLANK
USE C_FLBPTR
USE C_MATIN
USE C_MATOUT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Frrec
   REAL(REAL64) , DIMENSION(16) :: Afe
   INTEGER :: Nafe
   REAL(REAL64) , DIMENSION(16) :: Kge
   INTEGER :: Nkge
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a , afact , rhoxg
   REAL :: g
   INTEGER :: gf1 , gf2 , gf3 , i , icol , iloc , irow , it , itria , j , lgrav , ngridf
   LOGICAL :: grav
   INTEGER , DIMENSION(3,4) , SAVE :: grid
   INTEGER , DIMENSION(1) :: iz
   LOGICAL , SAVE :: ltilt
   REAL(REAL64) , DIMENSION(3) :: r12 , r13 , rt
   EXTERNAL dcross , mat
!
! End of declarations rewritten by SPAG
!
!
!     CALCULATES THE AREA FACTOR MATRIX AND GRAVITATIONAL STIFFNESS
!     MATRIX FOR A SINGLE FLUID ELEMENT ON THE FREE SURFACE
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
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
            IF ( iz(i)==Frrec(7) ) GOTO 50
!
         ENDDO
      ENDIF
!
!     ERROR CONDITIONS
!
      WRITE (Nout,99001) Ufm , Frrec(1) , Frrec(7)
99001 FORMAT (A23,' 8012, FLUID ELEMENT',I9,' ON A CFFREE CARD REFERENCES UNDEFINED GRAVITY ID',I9)
      Error = .TRUE.
      RETURN
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
END SUBROUTINE flfree
