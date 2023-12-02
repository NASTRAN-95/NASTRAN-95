!*==alg10.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg10
   IMPLICIT NONE
   USE C_UD300C
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c1h , c1t , v5 , vinh , vint , x1
!
! End of declarations rewritten by SPAG
!
!
!
!
   IF ( I>1 ) THEN
      vint = vint + sqrt((X(Nstrms,I)-X(Nstrms,I-1))**2+(R(Nstrms,I)-R(Nstrms,I-1))**2)*((Vm(Nstrms,I)+Vm(Nstrms,I-1))/2.0)         &
           & **4/Sclfac
      Delt(I) = v5*(c1t+0.016*vint)**0.8/Vm(Nstrms,I)**3.4*Sclfac*Shape
      Delh(I) = 0.0
      IF ( I>Istag ) THEN
         vinh = vinh + sqrt((X(1,I)-X(1,I-1))**2+(R(1,I)-R(1,I-1))**2)*((Vm(1,I)+Vm(1,I-1))/2.0)**4/Sclfac
         Delh(I) = v5*(c1h+0.016*vinh)**0.8/Vm(1,I)**3.4*Sclfac*Shape
      ENDIF
      Wwbl(I) = 0.5*Wwbl(I) + 0.5*(((2.0*R(Nstrms,I)-Delt(I)*cos(Phi(Nstrms)))*Delt(I)/Cppg(Nstrms)+(2.0*R(1,I)+Delh(I)*cos(Phi(1)))&
              & *Delh(I)/Cppg(1))/((R(Nstrms,I)+R(1,I))*Xl(Nstrms,I)))
      IF ( Wwbl(I)>0.3 ) Wwbl(I) = 0.3
      IF ( Wwbl(I)<0.0 ) Wwbl(I) = 0.3
   ELSE
      v5 = Visk**0.2
      vinh = 0.0
      vint = 0.0
      IF ( Wwbl(1)<=0.0 ) THEN
         c1h = 0.0
         c1t = 0.0
         Delh(1) = 0.0
         Delt(1) = 0.0
      ELSEIF ( Istag>0 ) THEN
         Delh(1) = 0.0
         c1h = 0.0
         IF ( abs(Phi(Nstrms))>Pi/2.0-0.00015 .AND. abs(Phi(Nstrms))<Pi/2.0+0.00015 ) THEN
            Delt(1) = Wwbl(1)*Xl(Nstrms,1)/Cppg(Nstrms)
            c1t = (Delt(1)*Vm(Nstrms,1)**3.4/(v5*Sclfac*Shape))**1.25
         ELSE
            x1 = (R(Nstrms,1)-sqrt(R(Nstrms,1)**2-cos(Phi(Nstrms))*Cppg(Nstrms)*Wwbl(1)*(R(Nstrms,1)+R(1,1))*Xl(Nstrms,I)))         &
               & /cos(Phi(Nstrms))
            Delt(1) = x1
            c1t = (x1/(Shape*Sclfac*v5)*Vm(Nstrms,1)**3.4)**1.25
         ENDIF
      ELSE
         x1 = Wwbl(1)*Xl(Nstrms,1)*(Cppg(1)+Cppg(Nstrms))/4.0
         Delh(1) = x1
         Delt(1) = x1
         x1 = x1/(Sclfac*Shape)
         c1h = (x1*Vm(1,1)**3.4/v5)**1.25
         c1t = (x1*Vm(Nstrms,1)**3.4/v5)**1.25
      ENDIF
   ENDIF
END SUBROUTINE alg10
