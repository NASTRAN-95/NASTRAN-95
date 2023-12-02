!*==fzy2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fzy2(Xij,X1,X2,Eta,Zeta,Yb,Zb,A,Beta2,Cbar,K,Fzzr,Fzzi,Fzyr,Fzyi,Fyzr,Fyzi,Fyyr,Fyyi)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Xij
   REAL :: X1
   REAL :: X2
   REAL :: Eta
   REAL :: Zeta
   REAL :: Yb
   REAL :: Zb
   REAL :: A
   REAL :: Beta2
   REAL :: Cbar
   REAL :: K
   REAL :: Fzzr
   REAL :: Fzzi
   REAL :: Fzyr
   REAL :: Fzyi
   REAL :: Fyzr
   REAL :: Fyzi
   REAL :: Fyyr
   REAL :: Fyyi
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a2 , arg1 , arg2 , capa , ct2 , delta , delta2 , delta3 , deno4 , denom , dx , earg , eps , fri , frr , fthi , fthr ,    &
         & kbar , kbar2 , kbar3 , m , part1 , qi , qr , ra1 , ra12 , ra13 , ra2 , ra22 , ra23 , raa , raa2 , raa3 , raa4 , rwig ,   &
         & rwig2 , st2 , tau , tau2 , trm1 , trm2 , xa
   REAL , SAVE :: capdi , capdr , cth , i1 , i10 , i11 , i2 , i3 , i4 , i5 , i6 , i7 , i8 , i9 , raij , raij2 , sth , test1 , test2
   INTEGER , SAVE :: lastbr
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
!   ***   THIS SUBROUTINE IS AN ALTERNATIVE TO SUBROUTINE  FMZY   ---
!         IT IS USED WHENEVER THE OPTION FLAG   IBFS  =  1
!   ***
   DATA lastbr/0/
   DATA test1 , test2 , cth , sth , raij , raij2/0.142857 , 0.5 , 1.0 , 3*0.0/
   DATA capdr , capdi , i1 , i2 , i3 , i4 , i5 , i6 , i7 , i8 , i9 , i10 , i11/13*0.0/
   m = sqrt(1.0-Beta2)
   IF ( K<=0.0001 .AND. m<=0.0001 ) THEN
      kbar = 0.0
      kbar2 = 0.0
   ELSE
      kbar = 2.0*K*m*A/Cbar
      kbar2 = kbar*kbar
   ENDIF
   xa = 0.5*(X1+X2)
   dx = X2 - X1
   a2 = A*A
   eps = 0.001*a2
   IF ( Eta==Yb .AND. Zeta==Zb ) THEN
      raij = 0.0
      raij2 = 0.0
      cth = 1.0
      sth = 0.0
      rwig2 = a2
   ELSE
      raij2 = (Eta-Yb)**2 + (Zeta-Zb)**2
      raij = sqrt(raij2)
      cth = (Eta-Yb)/raij
      sth = (Zeta-Zb)/raij
      IF ( raij2>a2 ) THEN
         rwig2 = raij2
      ELSE
         rwig2 = a2
      ENDIF
   ENDIF
   raa = sqrt((xa-Xij)**2+Beta2*rwig2)
   ct2 = cth*cth
   st2 = 0.0
   IF ( abs(sth)>0.0001 ) st2 = sth*sth
   rwig = sqrt(rwig2)
   raa2 = raa*raa
   raa3 = raa*raa2
   raa4 = raa*raa3
   capa = m - (xa-Xij)/raa
   delta = dx/raa
   delta2 = delta*delta
   earg = 0.0
   IF ( kbar<=0.0001 ) THEN
      qr = 1.0/(4.0*dx)
      qi = 0.0
   ELSE
      earg = kbar*(m*(xa-Xij)-raa)/(Beta2*A)
      qr = cos(earg)/(4.0*dx)
      qi = sin(earg)/(4.0*dx)
   ENDIF
   IF ( delta>test1 ) THEN
      IF ( delta>test2 ) THEN
         lastbr = 1
         rwig = sqrt(rwig2)
         ra12 = (X1-Xij)**2 + Beta2*rwig2
         ra22 = (X2-Xij)**2 + Beta2*rwig2
         ra1 = sqrt(ra12)
         ra2 = sqrt(ra22)
         i1 = ((X2-Xij)/ra2-(X1-Xij)/ra1)/(Beta2*rwig2)
      ELSE
         lastbr = 0
         tau = (xa-Xij)/raa
         tau2 = tau*tau
         i1 = delta*(1.0-(-1.0+5.0*tau2)*delta2/8.0)/raa2
      ENDIF
      trm1 = A*Beta2*i1
      fthr = A*qr*trm1
      fthi = A*qi*trm1
      IF ( kbar>0.0001 ) THEN
         IF ( lastbr/=0 ) THEN
            part1 = 0.5*dx*(xa-Xij)
            i2 = -((part1+raa2)/ra2+(part1-raa2)/ra1)/(Beta2*rwig2)
            denom = X1 - Xij + ra1
            i11 = alog(abs((X2-Xij+ra2)/denom))
            i3 = i11 - 2.0*(xa-Xij)*i2 - raa2*i1
            deno4 = sqrt(Beta2)*rwig
            arg1 = (X2-Xij)/deno4
            arg2 = (X1-Xij)/deno4
            i4 = (atan(arg1)-atan(arg2))/deno4
            i5 = 0.5*alog(ra22/ra12) - (xa-Xij)*i4
         ELSE
            delta3 = delta*delta2
            i2 = -(tau*delta3)/(4.0*raa)
            i3 = delta3/12.0
            i4 = delta*(1.0+(-1.0+3.0*tau2)*delta2/12.0)/raa
            i5 = -(tau*delta3)/6.0
         ENDIF
         trm1 = trm1 - (kbar2*capa*i5)/(A*Beta2)
         trm2 = kbar*(capa*i2+i4-i3*Beta2*rwig2/(2.0*raa3))
         fthr = A*(qr*trm1-qi*trm2)
         fthi = A*(qr*trm2+qi*trm1)
      ENDIF
      IF ( raij2>(a2+eps) ) THEN
         kbar3 = kbar*kbar2
         IF ( lastbr/=0 ) THEN
            ra13 = ra1*ra12
            ra23 = ra2*ra22
            i6 = ((X2-Xij)/ra23-(X1-Xij)/ra13+2.0*i1)/(3.0*Beta2*rwig2)
         ELSE
            i6 = delta*(1.0+5.0*(-1.0+7.0*tau2)*delta2/24.0)/raa4
         ENDIF
         trm1 = -3.0*a2*Beta2*Beta2*i6
         capdr = raij2*qr*trm1
         capdi = raij2*qi*trm1
         IF ( kbar>0.0001 ) THEN
            IF ( lastbr/=0 ) THEN
               i7 = -(1.0/ra23-1.0/ra13)/3.0 - (xa-Xij)*i6
               i8 = i1 - 2.0*(xa-Xij)*i7 - raa2*i6
               i9 = ((X2-Xij)/ra22-(X1-Xij)/ra12+i4)/(2.0*Beta2*rwig2)
               i10 = -((part1+raa2)/ra22+(part1-raa2)/ra12+(xa-Xij)*i4)/(2.0*Beta2*rwig2)
            ELSE
               i7 = -5.0*tau*delta3/(12.0*raa3)
               i8 = delta3/(12.0*raa2)
               i9 = delta*(1.0+(-1.0+6.0*tau2)*delta2/6.0)/raa3
               i10 = -delta3*tau/(3.0*raa2)
            ENDIF
            trm1 = trm1 + kbar2*(i1+3.0*capa*i10)
            trm2 = 3.0*A*Beta2*kbar*(-capa*i7+i8*Beta2*rwig2/(2.0*raa3)-i9) + kbar3*capa*i2/(A*Beta2)
            capdr = raij2*(qr*trm1-qi*trm2)
            capdi = raij2*(qr*trm2+qi*trm1)
         ENDIF
         frr = fthr + capdr
         fri = fthi + capdi
      ELSE
         frr = fthr
         fri = fthi
      ENDIF
   ELSE
      i1 = delta/raa2
      trm1 = Beta2*A*i1
      fthr = A*qr*trm1
      fthi = A*qi*trm1
      IF ( kbar>0.0001 ) THEN
         i4 = delta/raa
         trm2 = kbar*i4
         fthr = fthr - A*qi*trm2
         fthi = fthi + A*qr*trm2
      ENDIF
      IF ( raij2>(a2+eps) ) THEN
         i6 = delta/raa4
         trm1 = -3.0*a2*Beta2*Beta2*i6
         capdr = raij2*qr*trm1
         capdi = raij2*qi*trm1
         IF ( kbar>0.0001 ) THEN
            i9 = delta/raa3
            trm1 = trm1 + kbar2*i1
            trm2 = -3.0*A*Beta2*kbar*i9
            capdr = raij2*(qr*trm1-qi*trm2)
            capdi = raij2*(qr*trm2+qi*trm1)
         ENDIF
         frr = fthr + capdr
         fri = fthi + capdi
      ELSE
         frr = fthr
         fri = fthi
      ENDIF
   ENDIF
   Fzzr = ct2*fthr + st2*frr
   Fzzi = ct2*fthi + st2*fri
   Fyyr = st2*fthr + ct2*frr
   Fyyi = st2*fthi + ct2*fri
   IF ( cth==0.0 .OR. sth==0.0 ) THEN
      Fzyr = 0.0
      Fzyi = 0.0
   ELSE
      Fzyr = cth*sth*(frr-fthr)
      Fzyi = cth*sth*(fri-fthi)
   ENDIF
   Fyzr = Fzyr
   Fyzi = Fzyi
END SUBROUTINE fzy2
