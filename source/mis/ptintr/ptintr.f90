!*==ptintr.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ptintr(A,Aa,B,Bb,S,K,Eps)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: A
   REAL(REAL64) , DIMENSION(2) :: Aa
   REAL(REAL64) , DIMENSION(2) :: B
   REAL(REAL64) , DIMENSION(2) :: Bb
   REAL(REAL64) , DIMENSION(2) :: S
   INTEGER :: K
   REAL(REAL64) , DIMENSION(2) :: Eps
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: aaa , ax , ay , bbb , bx , by , d , pa , paa , pb , pbb , u , v , x , y
   REAL(REAL64) :: dist
   REAL(REAL64) , DIMENSION(2) :: p
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
!     RETURNS DOUBLE PRECISION VALUES OF X,Y COORDINATES (S) OF
!         POINT OF INTERSECTION (IF ANY) OF LINE SEGMENTS
!         FROM A TO AA AND B TO BB
!           A .NE. AA  AND  B .NE. BB
!     K IS CONDITION FLAG RETURNED --
!         K = 1   LINES INTERSECT AT S
!         K = 0   LINES INTERSECT AT S, AN ENDPOINT OF ONE LINE SEGMENT
!         K =-1   LINES DO NOT INTERSECT
!
!
!     EPS ARRAY FOR SIGNIFICANCE TESTING
!         EPS(1) IS AREA, ANGLE LIMIT
!         EPS(2) IS LENGTH LIMIT
!
!
!     DOUBLE PRECISION FUNCTION FOR DISTANCE BETWEEN 2 POINTS
!
   dist(x,y,u,v) = (x-u)**2 + (y-v)**2
!
   x = 0.D0
   y = x
   u = x
   v = x
   p(1) = 0.D0
   p(2) = 0.D0
   S(1) = 0.D0
   S(2) = 0.D0
!
   K = -1
!
   ax = Aa(1) - A(1)
   ay = Aa(2) - A(2)
   bx = Bb(1) - B(1)
   by = Bb(2) - B(2)
!
   aaa = ax**2 + ay**2
   bbb = bx**2 + by**2
   d = bx*ay - ax*by
!
!     IS EITHER LINE TOO SHORT?
!
   IF ( aaa<=Eps(1) .OR. bbb<=Eps(1) ) RETURN
!
!     ARE A AND B PARALLEL?
!
   IF ( dabs(d)<=Eps(1) ) THEN
!
!     A AND B ARE PARALLEL -- ARE THEY SAME LINE?
!
      p(1) = B(1)
      p(2) = B(2)
      IF ( dist(B(1),B(2),A(1),A(2))>Eps(1) .AND. dist(B(1),B(2),Aa(1),Aa(2))>Eps(1) ) THEN
         p(1) = Bb(1)
         p(2) = Bb(2)
!
!     A PARALLEL TO B AND NOT SAME LINE
!
         IF ( dist(Bb(1),Bb(2),A(1),A(2))>Eps(1) .AND. dist(Bb(1),Bb(2),Aa(1),Aa(2))>Eps(1) ) RETURN
      ENDIF
!
!     IS A PARALLEL TO Y AXIS?
!
   ELSEIF ( dabs(ax)>Eps(2) ) THEN
      p(1) = ((B(2)-A(2))*ax*bx+A(1)*ay*bx-B(1)*ax*by)/d
      p(2) = A(2) + (p(1)-A(1))*ay/ax
   ELSE
      p(1) = A(1)
      p(2) = B(2) + (p(1)-B(1))*by/bx
   ENDIF
!
   aaa = aaa + Eps(1)
   bbb = bbb + Eps(1)
   pa = dist(p(1),p(2),A(1),A(2))
   pb = dist(p(1),p(2),B(1),B(2))
   paa = dist(p(1),p(2),Aa(1),Aa(2))
   pbb = dist(p(1),p(2),Bb(1),Bb(2))
!
!     POINT OF INTERSECTION NOT ON EITHER SEGMENT
!
   IF ( pa>aaa .OR. paa>aaa .OR. pb>bbb .OR. pbb>bbb ) RETURN
!
!     LINES INTERSECT AT P
!
   K = 1
   S(1) = p(1)
   S(2) = p(2)
!
!     LINES INTERSECT AT P, AN ENDPOINT OF ONE SEGMENT
!
   IF ( (pa<Eps(2) .OR. paa<Eps(2)) .OR. (pb<Eps(2) .OR. pbb<Eps(2)) ) K = 0
END SUBROUTINE ptintr
