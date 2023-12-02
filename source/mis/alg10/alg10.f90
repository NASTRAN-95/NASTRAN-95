!*==alg10.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg10
   USE c_ud300c
   IMPLICIT NONE
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
   IF ( i>1 ) THEN
      vint = vint + sqrt((x(nstrms,i)-x(nstrms,i-1))**2+(r(nstrms,i)-r(nstrms,i-1))**2)*((vm(nstrms,i)+vm(nstrms,i-1))/2.0)         &
           & **4/sclfac
      delt(i) = v5*(c1t+0.016*vint)**0.8/vm(nstrms,i)**3.4*sclfac*shape
      delh(i) = 0.0
      IF ( i>istag ) THEN
         vinh = vinh + sqrt((x(1,i)-x(1,i-1))**2+(r(1,i)-r(1,i-1))**2)*((vm(1,i)+vm(1,i-1))/2.0)**4/sclfac
         delh(i) = v5*(c1h+0.016*vinh)**0.8/vm(1,i)**3.4*sclfac*shape
      ENDIF
      wwbl(i) = 0.5*wwbl(i) + 0.5*(((2.0*r(nstrms,i)-delt(i)*cos(phi(nstrms)))*delt(i)/cppg(nstrms)+(2.0*r(1,i)+delh(i)*cos(phi(1)))&
              & *delh(i)/cppg(1))/((r(nstrms,i)+r(1,i))*xl(nstrms,i)))
      IF ( wwbl(i)>0.3 ) wwbl(i) = 0.3
      IF ( wwbl(i)<0.0 ) wwbl(i) = 0.3
   ELSE
      v5 = visk**0.2
      vinh = 0.0
      vint = 0.0
      IF ( wwbl(1)<=0.0 ) THEN
         c1h = 0.0
         c1t = 0.0
         delh(1) = 0.0
         delt(1) = 0.0
      ELSEIF ( istag>0 ) THEN
         delh(1) = 0.0
         c1h = 0.0
         IF ( abs(phi(nstrms))>pi/2.0-0.00015 .AND. abs(phi(nstrms))<pi/2.0+0.00015 ) THEN
            delt(1) = wwbl(1)*xl(nstrms,1)/cppg(nstrms)
            c1t = (delt(1)*vm(nstrms,1)**3.4/(v5*sclfac*shape))**1.25
         ELSE
            x1 = (r(nstrms,1)-sqrt(r(nstrms,1)**2-cos(phi(nstrms))*cppg(nstrms)*wwbl(1)*(r(nstrms,1)+r(1,1))*xl(nstrms,i)))         &
               & /cos(phi(nstrms))
            delt(1) = x1
            c1t = (x1/(shape*sclfac*v5)*vm(nstrms,1)**3.4)**1.25
         ENDIF
      ELSE
         x1 = wwbl(1)*xl(nstrms,1)*(cppg(1)+cppg(nstrms))/4.0
         delh(1) = x1
         delt(1) = x1
         x1 = x1/(sclfac*shape)
         c1h = (x1*vm(1,1)**3.4/v5)**1.25
         c1t = (x1*vm(nstrms,1)**3.4/v5)**1.25
      ENDIF
   ENDIF
END SUBROUTINE alg10
