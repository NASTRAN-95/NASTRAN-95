!*==alg18.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg18(Beta1,Beta2,I1,I2,Fact,X0,Y0,S0,Xr,Y1,X1,Y2,Rdius,S,C1)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Beta1
   REAL :: Beta2
   INTEGER :: I1
   INTEGER :: I2
   REAL :: Fact
   REAL :: X0
   REAL :: Y0
   REAL :: S0
   REAL :: Xr
   REAL :: Y1
   REAL :: X1
   REAL :: Y2
   REAL :: Rdius
   REAL , DIMENSION(80) :: S
   REAL :: C1
!
! Local variable declarations rewritten by SPAG
!
   REAL :: am , delx , phi1 , phi2 , xx
   INTEGER :: i3 , j
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
!
   delx = Xr/float(I2-I1)
   xx = X0
   i3 = I1 + 1
   IF ( Beta1==Beta2 ) THEN
      am = tan(Beta1/C1)
      DO j = i3 , I2
         xx = xx + delx
         S(j) = (xx-X0)*sqrt(am*am+1.0) + S0
      ENDDO
      Y2 = am*(xx-X0) + Y0
      RETURN
   ENDIF
   Y1 = -cos(Beta1/C1)/(sin(Beta1/C1)-sin(Beta2/C1))
   X1 = sin(Beta1/C1)/(sin(Beta1/C1)-sin(Beta2/C1))
   Y2 = tan((Beta1+Beta2)/(2.0*C1))
   Rdius = abs(1.0/(sin(Beta1/C1)-sin(Beta2/C1)))
   Y2 = Y2*Fact + Y0
   Y1 = Y1*Fact + Y0
   X1 = X1*Fact + X0
   Rdius = Rdius*Fact
   DO j = i3 , I2
      xx = xx + delx
      phi1 = atan(-1./sqrt(Rdius**2-(xx-X1)**2)*(xx-X1))
      IF ( (Beta1-Beta2)<0.0 ) phi1 = -phi1
      phi2 = abs(Beta1/C1-phi1)
      S(j) = Rdius*phi2 + S0
   ENDDO
END SUBROUTINE alg18
