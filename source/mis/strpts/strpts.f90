!*==strpts.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strpts(Ts6,Nots)
   IMPLICIT NONE
   USE C_MATOUT
   USE C_SDR2X5
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(40) :: Ts6
   LOGICAL :: Nots
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a11 , a12 , a13 , a14 , a15 , a16 , a21 , a22 , a23 , a24 , a25 , a26 , a31 , a32 , a33 , a34 , a35 , a36 , a37 , a38 ,  &
         & a39 , a40 , a41 , d11 , d12 , d13 , d21 , d22 , d23 , d31 , d32 , d33 , j11 , j12 , j22 , thk , thk1 , x2 , xy , y2
   INTEGER :: i
!
! End of declarations rewritten by SPAG
!
!
!    STRESS ROUTINE ,CALLED FROM STRP11, FOR HIGHER ORDER PLATE ELEMENT
!
   DO i = 1 , 40
      Ts6(i) = 0.0
   ENDDO
   thk = A1 + A2*X + A3*Y
   thk1 = thk**3/12.0
   d11 = Em(1)*thk1
   d12 = Em(2)*thk1
   d13 = Em(3)*thk1
   d22 = Em(4)*thk1
   d23 = Em(5)*thk1
   d33 = Em(6)*thk1
   d21 = d12
   d31 = d13
   d32 = d23
   IF ( Nots ) THEN
      j11 = 1.0
      j12 = 0.0
      j22 = 1.0
   ELSE
      thk = B1 + B2*X + B3*Y
      j11 = 1.0/(Em(6)*thk)
      j12 = 0.0
      j22 = j11
   ENDIF
!
   a11 = -(j11*d11+j12*d13)
   a12 = -(j11*d12+j12*d23)
   a13 = -(j11*d13+j12*d33)
   a14 = -(j11*d31+j12*d21)
   a15 = -(j11*d32+j12*d22)
   a16 = -(j11*d33+j12*d23)
   a21 = -(j12*d11+j22*d13)
   a22 = -(j12*d12+j22*d23)
   a23 = -(j12*d13+j22*d33)
   a24 = -(j12*d13+j22*d12)
   a25 = -(j12*d23+j22*d22)
   a26 = -(j12*d33+j22*d32)
   a31 = a14 + 2.0*a13
   a32 = a12 + 2.0*a16
   a33 = a24 + 2.0*a23
   a34 = a22 + 2.0*a26
   a35 = a33 + a11
   a36 = a34 + a31
   a37 = a25 + a32
!
   x2 = X*X
   xy = X*Y
   y2 = Y*Y
   a38 = a13 + a14
   a39 = a12 + a16
   a40 = a23 + a24
   a41 = a22 + a26
   Ts6(7) = 6.0*a11
   Ts6(8) = 2.0*a31
   Ts6(9) = 2.0*a32
   Ts6(10) = 6.0*a15
   Ts6(11) = 24.0*a11*X
   Ts6(12) = 6.0*(a31*X+a11*Y)
   Ts6(13) = 4.0*(a32*X+a31*Y)
   Ts6(14) = 6.0*(a15*X+a32*Y)
   Ts6(15) = 24.0*a15*Y
   IF ( Nots ) THEN
      Ts6(16) = 60.0*a11*x2
      Ts6(17) = 6.0*(a32*x2+2.0*a31*xy+a11*y2)
      Ts6(18) = 6.0*(a15*x2+2.0*a32*xy+a31*y2)
      Ts6(19) = 12.0*(2.0*a15*xy+a32*y2)
      Ts6(20) = 60.0*a15*y2
   ELSE
      Ts6(16) = 120.0*(-a11*a11-a13*a21+0.5*a11*x2)
      Ts6(17) = 12.0*(-a11*a32-a13*a34-a38*a31-a39*a33-a16*a11-a15*a21) + 6.0*(a32*x2+2.0*a31*xy+a11*y2)
      Ts6(18) = 12.0*(-a11*a15-a13*a25-a38*a32-a39*a34-a16*a31-a15*a33) + 6.0*(a15*x2+2.0*a32*xy+a31*y2)
      Ts6(19) = 24.0*(-a39*a25-a16*a32-a15*a34+a15*xy+0.5*a32*y2-a38*a15)
      Ts6(20) = -120.0*(a16*a15+a15*a25-0.5*a15*y2)
   ENDIF
   Ts6(27) = 6.0*a21
   Ts6(28) = 2.0*a33
   Ts6(29) = 2.0*a34
   Ts6(30) = 6.0*a25
   Ts6(31) = 24.0*a21*X
   Ts6(32) = 6.0*(a33*X+a21*Y)
   Ts6(33) = 4.0*(a34*X+a33*Y)
   Ts6(34) = 6.0*(a25*X+a34*Y)
   Ts6(35) = 24.0*a25*Y
   IF ( Nots ) THEN
      Ts6(36) = 60.0*a21*x2
      Ts6(37) = 6.0*(a34*x2+2.0*a33*xy+a21*y2)
      Ts6(38) = 6.0*(a25*x2+2.0*a34*xy+a33*y2)
      Ts6(39) = 12.0*(2.0*a25*xy+a34*y2)
      Ts6(40) = 60.0*a25*y2
   ELSE
      Ts6(36) = 120.0*(-a21*a11-a23*a21+0.5*a21*x2)
      Ts6(37) = 12.0*(-a21*a32-a23*a34-a40*a31-a41*a33-a26*a11-a25*a21) + 6.0*(a34*x2+2.0*a33*xy+a21*y2)
      Ts6(38) = 12.0*(-a21*a15-a23*a25-a40*a32-a41*a34-a26*a31-a25*a33) + 6.0*(a25*x2+2.0*a34*xy+a33*y2)
      Ts6(39) = 24.0*(-a41*a25-a26*a32-a25*a34+a25*xy+0.5*a34*y2-a40*a15)
      Ts6(40) = -120.0*(a26*a15+a25*a25-0.5*a25*y2)
   ENDIF
END SUBROUTINE strpts
