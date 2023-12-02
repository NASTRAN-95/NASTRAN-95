!*==formg2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE formg2(Ig,Jr,Jd,Ir,Id)
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ig
   INTEGER :: Jr
   INTEGER :: Jd
   INTEGER :: Ir
   INTEGER :: Id
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   REAL(REAL64) :: xd , yd , zd
   REAL , DIMENSION(1) :: zr
   REAL(REAL64) , DIMENSION(1) :: zz
!
! End of declarations rewritten by SPAG
!
!
!     FORMGG FORMS THE GG MATRIX FOR EACH RIGID ELEMENT DEGREE OF
!     FREEDOM.  IG IS THE START OF THE ROW STORED GG MATRIX - 1
!     JR IS THE START OF THE TA MATRIX - 1.
!     JD IS THE START OF THE TB MATRIX - 1.
!     IR IS THE START OF THE BGPDT INFORMATION FOR REFERENCE POINT
!     ID IS THE START OF THE BGPDT INFORMATION FOR DEPENDENT POINT
!
   !>>>>EQUIVALENCE (Zz(1),zr(1))
   !>>>>EQUIVALENCE (Zz(1),Z(1))
!
!     CALCULATE THE X,Y,AND Z DIRECTED DISTANCES WITH RESPECT TO THE
!     REFERENCE GRID POINT
!
   xd = zr(Id+1) - zr(Ir+1)
   yd = zr(Id+2) - zr(Ir+2)
   zd = zr(Id+3) - zr(Ir+3)
!
!     IF NO TRANSFORMATION IS NECESSARY, GO TO 30
!
   IF ( Z(Ir)==0 .AND. Z(Id)==0 ) THEN
!
!     NO TRANSFORMATIONS
!
      DO i = 1 , 36
         zz(Ig+i) = 0.0
      ENDDO
      zz(Ig+1) = 1.0
      zz(Ig+8) = 1.0
      zz(Ig+15) = 1.0
      zz(Ig+22) = 1.0
      zz(Ig+29) = 1.0
      zz(Ig+36) = 1.0
      zz(Ig+5) = zd
      zz(Ig+6) = -yd
      zz(Ig+10) = -zd
      zz(Ig+12) = xd
      zz(Ig+16) = yd
      zz(Ig+17) = -xd
      RETURN
!
!     IF ONLY DEPENDENT GRID POINT HAS A TRANSFORMATION, GO TO 20
!
   ELSEIF ( Z(Ir)/=0 ) THEN
!
!     IF BOTH HAVE TRANSFORMATIONS, GO TO 10
!
!
      IF ( Z(Id)/=0 ) THEN
!
!     BOTH HAVE TRANSFORMATIONS
!
         zz(Ig+1) = zz(Jd+1)*zz(Jr+1) + zz(Jd+4)*zz(Jr+4) + zz(Jd+7)*zz(Jr+7)
         zz(Ig+2) = zz(Jd+1)*zz(Jr+2) + zz(Jd+4)*zz(Jr+5) + zz(Jd+7)*zz(Jr+8)
         zz(Ig+3) = zz(Jd+1)*zz(Jr+3) + zz(Jd+4)*zz(Jr+6) + zz(Jd+7)*zz(Jr+9)
         zz(Ig+4) = zz(Jd+1)*zd*zz(Jr+4) - zz(Jd+1)*yd*zz(Jr+7) + zz(Jd+4)*xd*zz(Jr+7) - zz(Jd+4)*zd*zz(Jr+1) + zz(Jd+7)*yd*zz(Jr+1)&
                  & - zz(Jd+7)*xd*zz(Jr+4)
         zz(Ig+5) = zz(Jd+1)*zd*zz(Jr+5) - zz(Jd+1)*yd*zz(Jr+8) + zz(Jd+4)*xd*zz(Jr+8) - zz(Jd+4)*zd*zz(Jr+2) + zz(Jd+7)*yd*zz(Jr+2)&
                  & - zz(Jd+7)*xd*zz(Jr+5)
         zz(Ig+6) = zz(Jd+1)*zd*zz(Jr+6) - zz(Jd+1)*yd*zz(Jr+9) + zz(Jd+4)*xd*zz(Jr+9) - zz(Jd+4)*zd*zz(Jr+3) + zz(Jd+7)*yd*zz(Jr+3)&
                  & - zz(Jd+7)*xd*zz(Jr+6)
         zz(Ig+7) = zz(Jd+2)*zz(Jr+1) + zz(Jd+5)*zz(Jr+4) + zz(Jd+8)*zz(Jr+7)
         zz(Ig+8) = zz(Jd+2)*zz(Jr+2) + zz(Jd+5)*zz(Jr+5) + zz(Jd+8)*zz(Jr+8)
         zz(Ig+9) = zz(Jd+2)*zz(Jr+3) + zz(Jd+5)*zz(Jr+6) + zz(Jd+8)*zz(Jr+9)
         zz(Ig+10) = zz(Jd+2)*zd*zz(Jr+4) - zz(Jd+2)*yd*zz(Jr+7) + zz(Jd+5)*xd*zz(Jr+7) - zz(Jd+5)*zd*zz(Jr+1) + zz(Jd+8)           &
                   & *yd*zz(Jr+1) - zz(Jd+8)*xd*zz(Jr+4)
         zz(Ig+11) = zz(Jd+2)*zd*zz(Jr+5) - zz(Jd+2)*yd*zz(Jr+8) + zz(Jd+5)*xd*zz(Jr+8) - zz(Jd+5)*zd*zz(Jr+2) + zz(Jd+8)           &
                   & *yd*zz(Jr+2) - zz(Jd+8)*xd*zz(Jr+5)
         zz(Ig+12) = zz(Jd+2)*zd*zz(Jr+6) - zz(Jd+2)*yd*zz(Jr+9) + zz(Jd+5)*xd*zz(Jr+9) - zz(Jd+5)*zd*zz(Jr+3) + zz(Jd+8)           &
                   & *yd*zz(Jr+3) - zz(Jd+8)*xd*zz(Jr+6)
         zz(Ig+13) = zz(Jd+3)*zz(Jr+1) + zz(Jd+6)*zz(Jr+4) + zz(Jd+9)*zz(Jr+7)
         zz(Ig+14) = zz(Jd+3)*zz(Jr+2) + zz(Jd+6)*zz(Jr+5) + zz(Jd+9)*zz(Jr+8)
         zz(Ig+15) = zz(Jd+3)*zz(Jr+3) + zz(Jd+6)*zz(Jr+6) + zz(Jd+9)*zz(Jr+9)
         zz(Ig+16) = zz(Jd+3)*zd*zz(Jr+4) - zz(Jd+3)*yd*zz(Jr+7) + zz(Jd+6)*xd*zz(Jr+7) - zz(Jd+6)*zd*zz(Jr+1) + zz(Jd+9)           &
                   & *yd*zz(Jr+1) - zz(Jd+9)*xd*zz(Jr+4)
         zz(Ig+17) = zz(Jd+3)*zd*zz(Jr+5) - zz(Jd+3)*yd*zz(Jr+8) + zz(Jd+6)*xd*zz(Jr+8) - zz(Jd+6)*zd*zz(Jr+2) + zz(Jd+9)           &
                   & *yd*zz(Jr+2) - zz(Jd+9)*xd*zz(Jr+5)
         zz(Ig+18) = zz(Jd+3)*zd*zz(Jr+6) - zz(Jd+3)*yd*zz(Jr+9) + zz(Jd+6)*xd*zz(Jr+9) - zz(Jd+6)*zd*zz(Jr+3) + zz(Jd+9)           &
                   & *yd*zz(Jr+3) - zz(Jd+9)*xd*zz(Jr+6)
         zz(Ig+19) = 0.0
         zz(Ig+20) = 0.0
         zz(Ig+21) = 0.0
         zz(Ig+22) = zz(Ig+1)
         zz(Ig+23) = zz(Ig+2)
         zz(Ig+24) = zz(Ig+3)
         zz(Ig+25) = 0.0
         zz(Ig+26) = 0.0
         zz(Ig+27) = 0.0
         zz(Ig+28) = zz(Ig+7)
         zz(Ig+29) = zz(Ig+8)
         zz(Ig+30) = zz(Ig+9)
         zz(Ig+31) = 0.0
         zz(Ig+32) = 0.0
         zz(Ig+33) = 0.0
         zz(Ig+34) = zz(Ig+13)
         zz(Ig+35) = zz(Ig+14)
         zz(Ig+36) = zz(Ig+15)
         RETURN
      ELSE
!
!     ONLY REFERENCE GRID POINT HAS A TRANSFORMATION
!
         zz(Ig+1) = zz(Jr+1)
         zz(Ig+2) = zz(Jr+2)
         zz(Ig+3) = zz(Jr+3)
         zz(Ig+4) = zd*zz(Jr+4) - yd*zz(Jr+7)
         zz(Ig+5) = zd*zz(Jr+5) - yd*zz(Jr+8)
         zz(Ig+6) = zd*zz(Jr+6) - yd*zz(Jr+9)
         zz(Ig+7) = zz(Jr+4)
         zz(Ig+8) = zz(Jr+5)
         zz(Ig+9) = zz(Jr+6)
         zz(Ig+10) = xd*zz(Jr+7) - zd*zz(Jr+1)
         zz(Ig+11) = xd*zz(Jr+8) - zd*zz(Jr+2)
         zz(Ig+12) = xd*zz(Jr+9) - zd*zz(Jr+3)
         zz(Ig+13) = zz(Jr+7)
         zz(Ig+14) = zz(Jr+8)
         zz(Ig+15) = zz(Jr+9)
         zz(Ig+16) = yd*zz(Jr+1) - xd*zz(Jr+4)
         zz(Ig+17) = yd*zz(Jr+2) - xd*zz(Jr+5)
         zz(Ig+18) = yd*zz(Jr+3) - xd*zz(Jr+6)
         zz(Ig+19) = 0.0
         zz(Ig+20) = 0.0
         zz(Ig+21) = 0.0
         zz(Ig+22) = zz(Ig+1)
         zz(Ig+23) = zz(Ig+2)
         zz(Ig+24) = zz(Ig+3)
         zz(Ig+25) = 0.0
         zz(Ig+26) = 0.0
         zz(Ig+27) = 0.0
         zz(Ig+28) = zz(Ig+7)
         zz(Ig+29) = zz(Ig+8)
         zz(Ig+30) = zz(Ig+9)
         zz(Ig+31) = 0.0
         zz(Ig+32) = 0.0
         zz(Ig+33) = 0.0
         zz(Ig+34) = zz(Ig+13)
         zz(Ig+35) = zz(Ig+14)
         zz(Ig+36) = zz(Ig+15)
         RETURN
      ENDIF
   ENDIF
!
!     DEPENDENT GRID POINT HAS TRANSFORMATION
!
   zz(Ig+1) = zz(Jd+1)
   zz(Ig+2) = zz(Jd+4)
   zz(Ig+3) = zz(Jd+7)
   zz(Ig+4) = zz(Jd+7)*yd - zz(Jd+4)*zd
   zz(Ig+5) = zz(Jd+1)*zd - zz(Jd+7)*xd
   zz(Ig+6) = zz(Jd+4)*xd - zz(Jd+1)*yd
   zz(Ig+7) = zz(Jd+2)
   zz(Ig+8) = zz(Jd+5)
   zz(Ig+9) = zz(Jd+8)
   zz(Ig+10) = zz(Jd+8)*yd - zz(Jd+5)*zd
   zz(Ig+11) = zz(Jd+2)*zd - zz(Jd+8)*xd
   zz(Ig+12) = zz(Jd+5)*xd - zz(Jd+2)*yd
   zz(Ig+13) = zz(Jd+3)
   zz(Ig+14) = zz(Jd+6)
   zz(Ig+15) = zz(Jd+9)
   zz(Ig+16) = zz(Jd+9)*yd - zz(Jd+6)*zd
   zz(Ig+17) = zz(Jd+3)*zd - zz(Jd+9)*xd
   zz(Ig+18) = zz(Jd+6)*xd - zz(Jd+3)*yd
   zz(Ig+19) = 0.0
   zz(Ig+20) = 0.0
   zz(Ig+21) = 0.0
   zz(Ig+22) = zz(Ig+1)
   zz(Ig+23) = zz(Ig+2)
   zz(Ig+24) = zz(Ig+3)
   zz(Ig+25) = 0.0
   zz(Ig+26) = 0.0
   zz(Ig+27) = 0.0
   zz(Ig+28) = zz(Ig+7)
   zz(Ig+29) = zz(Ig+8)
   zz(Ig+30) = zz(Ig+9)
   zz(Ig+31) = 0.0
   zz(Ig+32) = 0.0
   zz(Ig+33) = 0.0
   zz(Ig+34) = zz(Ig+13)
   zz(Ig+35) = zz(Ig+14)
   zz(Ig+36) = zz(Ig+15)
   RETURN
END SUBROUTINE formg2
