
SUBROUTINE alg18(Beta1,Beta2,I1,I2,Fact,X0,Y0,S0,Xr,Y1,X1,Y2,Rdius,S,C1)
   IMPLICIT NONE
   REAL Beta1 , Beta2 , C1 , Fact , Rdius , S0 , X0 , X1 , Xr , Y0 , Y1 , Y2
   INTEGER I1 , I2
   REAL S(80)
   REAL am , delx , phi1 , phi2 , xx
   INTEGER i3 , j
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
      GOTO 99999
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
   RETURN
99999 RETURN
END SUBROUTINE alg18