
SUBROUTINE formgg(Ig,Jr,Jd,Ir,Id)
   IMPLICIT NONE
   INTEGER Z(1)
   REAL Zz(1)
   COMMON /zzzzzz/ Z
   INTEGER Id , Ig , Ir , Jd , Jr
   INTEGER i
   REAL xd , yd , zd
!
!     FORMGG FORMS THE GG MATRIX FOR EACH RIGID ELEMENT DEGREE OF
!     FREEDOM.  IG IS THE START OF THE ROW STORED GG MATRIX - 1
!     JR IS THE START OF THE TA MATRIX - 1.
!     JD IS THE START OF THE TB MATRIX - 1.
!     IR IS THE START OF THE BGPDT INFORMATION FOR REFERENCE POINT
!     ID IS THE START OF THE BGPDT INFORMATION FOR DEPENDENT POINT
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
!
!     CALCULATE THE X,Y,AND Z DIRECTED DISTANCES WITH RESPECT TO THE
!     REFERENCE GRID POINT
!
   xd = Zz(Id+1) - Zz(Ir+1)
   yd = Zz(Id+2) - Zz(Ir+2)
   zd = Zz(Id+3) - Zz(Ir+3)
!
!     IF NO TRANSFORMATION IS NECESSARY, GO TO 30
!
   IF ( Z(Ir)==0 .AND. Z(Id)==0 ) THEN
!
!     NO TRANSFORMATIONS
!
      DO i = 1 , 36
         Zz(Ig+i) = 0.0
      ENDDO
      Zz(Ig+1) = 1.0
      Zz(Ig+8) = 1.0
      Zz(Ig+15) = 1.0
      Zz(Ig+22) = 1.0
      Zz(Ig+29) = 1.0
      Zz(Ig+36) = 1.0
      Zz(Ig+5) = zd
      Zz(Ig+6) = -yd
      Zz(Ig+10) = -zd
      Zz(Ig+12) = xd
      Zz(Ig+16) = yd
      Zz(Ig+17) = -xd
      GOTO 99999
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
         Zz(Ig+1) = Zz(Jd+1)*Zz(Jr+1) + Zz(Jd+4)*Zz(Jr+4) + Zz(Jd+7)*Zz(Jr+7)
         Zz(Ig+2) = Zz(Jd+1)*Zz(Jr+2) + Zz(Jd+4)*Zz(Jr+5) + Zz(Jd+7)*Zz(Jr+8)
         Zz(Ig+3) = Zz(Jd+1)*Zz(Jr+3) + Zz(Jd+4)*Zz(Jr+6) + Zz(Jd+7)*Zz(Jr+9)
         Zz(Ig+4) = Zz(Jd+1)*zd*Zz(Jr+4) - Zz(Jd+1)*yd*Zz(Jr+7) + Zz(Jd+4)*xd*Zz(Jr+7) - Zz(Jd+4)*zd*Zz(Jr+1) + Zz(Jd+7)*yd*Zz(Jr+1)&
                  & - Zz(Jd+7)*xd*Zz(Jr+4)
         Zz(Ig+5) = Zz(Jd+1)*zd*Zz(Jr+5) - Zz(Jd+1)*yd*Zz(Jr+8) + Zz(Jd+4)*xd*Zz(Jr+8) - Zz(Jd+4)*zd*Zz(Jr+2) + Zz(Jd+7)*yd*Zz(Jr+2)&
                  & - Zz(Jd+7)*xd*Zz(Jr+5)
         Zz(Ig+6) = Zz(Jd+1)*zd*Zz(Jr+6) - Zz(Jd+1)*yd*Zz(Jr+9) + Zz(Jd+4)*xd*Zz(Jr+9) - Zz(Jd+4)*zd*Zz(Jr+3) + Zz(Jd+7)*yd*Zz(Jr+3)&
                  & - Zz(Jd+7)*xd*Zz(Jr+6)
         Zz(Ig+7) = Zz(Jd+2)*Zz(Jr+1) + Zz(Jd+5)*Zz(Jr+4) + Zz(Jd+8)*Zz(Jr+7)
         Zz(Ig+8) = Zz(Jd+2)*Zz(Jr+2) + Zz(Jd+5)*Zz(Jr+5) + Zz(Jd+8)*Zz(Jr+8)
         Zz(Ig+9) = Zz(Jd+2)*Zz(Jr+3) + Zz(Jd+5)*Zz(Jr+6) + Zz(Jd+8)*Zz(Jr+9)
         Zz(Ig+10) = Zz(Jd+2)*zd*Zz(Jr+4) - Zz(Jd+2)*yd*Zz(Jr+7) + Zz(Jd+5)*xd*Zz(Jr+7) - Zz(Jd+5)*zd*Zz(Jr+1) + Zz(Jd+8)           &
                   & *yd*Zz(Jr+1) - Zz(Jd+8)*xd*Zz(Jr+4)
         Zz(Ig+11) = Zz(Jd+2)*zd*Zz(Jr+5) - Zz(Jd+2)*yd*Zz(Jr+8) + Zz(Jd+5)*xd*Zz(Jr+8) - Zz(Jd+5)*zd*Zz(Jr+2) + Zz(Jd+8)           &
                   & *yd*Zz(Jr+2) - Zz(Jd+8)*xd*Zz(Jr+5)
         Zz(Ig+12) = Zz(Jd+2)*zd*Zz(Jr+6) - Zz(Jd+2)*yd*Zz(Jr+9) + Zz(Jd+5)*xd*Zz(Jr+9) - Zz(Jd+5)*zd*Zz(Jr+3) + Zz(Jd+8)           &
                   & *yd*Zz(Jr+3) - Zz(Jd+8)*xd*Zz(Jr+6)
         Zz(Ig+13) = Zz(Jd+3)*Zz(Jr+1) + Zz(Jd+6)*Zz(Jr+4) + Zz(Jd+9)*Zz(Jr+7)
         Zz(Ig+14) = Zz(Jd+3)*Zz(Jr+2) + Zz(Jd+6)*Zz(Jr+5) + Zz(Jd+9)*Zz(Jr+8)
         Zz(Ig+15) = Zz(Jd+3)*Zz(Jr+3) + Zz(Jd+6)*Zz(Jr+6) + Zz(Jd+9)*Zz(Jr+9)
         Zz(Ig+16) = Zz(Jd+3)*zd*Zz(Jr+4) - Zz(Jd+3)*yd*Zz(Jr+7) + Zz(Jd+6)*xd*Zz(Jr+7) - Zz(Jd+6)*zd*Zz(Jr+1) + Zz(Jd+9)           &
                   & *yd*Zz(Jr+1) - Zz(Jd+9)*xd*Zz(Jr+4)
         Zz(Ig+17) = Zz(Jd+3)*zd*Zz(Jr+5) - Zz(Jd+3)*yd*Zz(Jr+8) + Zz(Jd+6)*xd*Zz(Jr+8) - Zz(Jd+6)*zd*Zz(Jr+2) + Zz(Jd+9)           &
                   & *yd*Zz(Jr+2) - Zz(Jd+9)*xd*Zz(Jr+5)
         Zz(Ig+18) = Zz(Jd+3)*zd*Zz(Jr+6) - Zz(Jd+3)*yd*Zz(Jr+9) + Zz(Jd+6)*xd*Zz(Jr+9) - Zz(Jd+6)*zd*Zz(Jr+3) + Zz(Jd+9)           &
                   & *yd*Zz(Jr+3) - Zz(Jd+9)*xd*Zz(Jr+6)
         Zz(Ig+19) = 0.0
         Zz(Ig+20) = 0.0
         Zz(Ig+21) = 0.0
         Zz(Ig+22) = Zz(Ig+1)
         Zz(Ig+23) = Zz(Ig+2)
         Zz(Ig+24) = Zz(Ig+3)
         Zz(Ig+25) = 0.0
         Zz(Ig+26) = 0.0
         Zz(Ig+27) = 0.0
         Zz(Ig+28) = Zz(Ig+7)
         Zz(Ig+29) = Zz(Ig+8)
         Zz(Ig+30) = Zz(Ig+9)
         Zz(Ig+31) = 0.0
         Zz(Ig+32) = 0.0
         Zz(Ig+33) = 0.0
         Zz(Ig+34) = Zz(Ig+13)
         Zz(Ig+35) = Zz(Ig+14)
         Zz(Ig+36) = Zz(Ig+15)
         RETURN
      ELSE
!
!     ONLY REFERENCE GRID POINT HAS A TRANSFORMATION
!
         Zz(Ig+1) = Zz(Jr+1)
         Zz(Ig+2) = Zz(Jr+2)
         Zz(Ig+3) = Zz(Jr+3)
         Zz(Ig+4) = zd*Zz(Jr+4) - yd*Zz(Jr+7)
         Zz(Ig+5) = zd*Zz(Jr+5) - yd*Zz(Jr+8)
         Zz(Ig+6) = zd*Zz(Jr+6) - yd*Zz(Jr+9)
         Zz(Ig+7) = Zz(Jr+4)
         Zz(Ig+8) = Zz(Jr+5)
         Zz(Ig+9) = Zz(Jr+6)
         Zz(Ig+10) = xd*Zz(Jr+7) - zd*Zz(Jr+1)
         Zz(Ig+11) = xd*Zz(Jr+8) - zd*Zz(Jr+2)
         Zz(Ig+12) = xd*Zz(Jr+9) - zd*Zz(Jr+3)
         Zz(Ig+13) = Zz(Jr+7)
         Zz(Ig+14) = Zz(Jr+8)
         Zz(Ig+15) = Zz(Jr+9)
         Zz(Ig+16) = yd*Zz(Jr+1) - xd*Zz(Jr+4)
         Zz(Ig+17) = yd*Zz(Jr+2) - xd*Zz(Jr+5)
         Zz(Ig+18) = yd*Zz(Jr+3) - xd*Zz(Jr+6)
         Zz(Ig+19) = 0.0
         Zz(Ig+20) = 0.0
         Zz(Ig+21) = 0.0
         Zz(Ig+22) = Zz(Ig+1)
         Zz(Ig+23) = Zz(Ig+2)
         Zz(Ig+24) = Zz(Ig+3)
         Zz(Ig+25) = 0.0
         Zz(Ig+26) = 0.0
         Zz(Ig+27) = 0.0
         Zz(Ig+28) = Zz(Ig+7)
         Zz(Ig+29) = Zz(Ig+8)
         Zz(Ig+30) = Zz(Ig+9)
         Zz(Ig+31) = 0.0
         Zz(Ig+32) = 0.0
         Zz(Ig+33) = 0.0
         Zz(Ig+34) = Zz(Ig+13)
         Zz(Ig+35) = Zz(Ig+14)
         Zz(Ig+36) = Zz(Ig+15)
         RETURN
      ENDIF
   ENDIF
!
!     DEPENDENT GRID POINT HAS TRANSFORMATION
!
   Zz(Ig+1) = Zz(Jd+1)
   Zz(Ig+2) = Zz(Jd+4)
   Zz(Ig+3) = Zz(Jd+7)
   Zz(Ig+4) = Zz(Jd+7)*yd - Zz(Jd+4)*zd
   Zz(Ig+5) = Zz(Jd+1)*zd - Zz(Jd+7)*xd
   Zz(Ig+6) = Zz(Jd+4)*xd - Zz(Jd+1)*yd
   Zz(Ig+7) = Zz(Jd+2)
   Zz(Ig+8) = Zz(Jd+5)
   Zz(Ig+9) = Zz(Jd+8)
   Zz(Ig+10) = Zz(Jd+8)*yd - Zz(Jd+5)*zd
   Zz(Ig+11) = Zz(Jd+2)*zd - Zz(Jd+8)*xd
   Zz(Ig+12) = Zz(Jd+5)*xd - Zz(Jd+2)*yd
   Zz(Ig+13) = Zz(Jd+3)
   Zz(Ig+14) = Zz(Jd+6)
   Zz(Ig+15) = Zz(Jd+9)
   Zz(Ig+16) = Zz(Jd+9)*yd - Zz(Jd+6)*zd
   Zz(Ig+17) = Zz(Jd+3)*zd - Zz(Jd+9)*xd
   Zz(Ig+18) = Zz(Jd+6)*xd - Zz(Jd+3)*yd
   Zz(Ig+19) = 0.0
   Zz(Ig+20) = 0.0
   Zz(Ig+21) = 0.0
   Zz(Ig+22) = Zz(Ig+1)
   Zz(Ig+23) = Zz(Ig+2)
   Zz(Ig+24) = Zz(Ig+3)
   Zz(Ig+25) = 0.0
   Zz(Ig+26) = 0.0
   Zz(Ig+27) = 0.0
   Zz(Ig+28) = Zz(Ig+7)
   Zz(Ig+29) = Zz(Ig+8)
   Zz(Ig+30) = Zz(Ig+9)
   Zz(Ig+31) = 0.0
   Zz(Ig+32) = 0.0
   Zz(Ig+33) = 0.0
   Zz(Ig+34) = Zz(Ig+13)
   Zz(Ig+35) = Zz(Ig+14)
   Zz(Ig+36) = Zz(Ig+15)
   RETURN
99999 RETURN
END SUBROUTINE formgg