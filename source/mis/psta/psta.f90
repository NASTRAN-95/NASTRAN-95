!*==psta.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE psta(Deltay,Bi,Ca,Alph,Thi,Ajjl)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_CONDAS
   USE C_PACKX
   USE C_PSTONC
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Deltay
   REAL , DIMENSION(1) :: Bi
   REAL , DIMENSION(1) :: Ca
   REAL , DIMENSION(1) :: Alph
   REAL , DIMENSION(13) :: Thi
   REAL :: Ajjl
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,3) , SAVE :: a , h
   REAL , DIMENSION(6) :: ai , aj , ek
   REAL :: alpha , alpha2 , b , bref , cbar1 , cbar2 , cbar3 , const , e1k , e1ks , ems , rfc , secs , t , tau , tauh , taut , ts , &
         & zetah , zetam
   REAL , DIMENSION(3,3) :: g , gi , q , qi
   INTEGER :: i , it , j , k , l , m1 , n , n2
   REAL , DIMENSION(3,6) :: p
   COMPLEX , DIMENSION(3) :: pc
   EXTERNAL pack
!
! End of declarations rewritten by SPAG
!
   DATA a/9*0.0/ , h/9*0.0/
   bref = Refc*.5
   rfc = Rfk/bref
   Ii = Nrow + 1
   Nn = Nrow
!
!     BUILD AJJL FOR EACH STRIP
!
   DO i = 1 , Nstrip
      b = Bi(i)
      const = 8.0*Deltay(i)*(rfc*b)**2
      a(1,1) = -1.0
      a(2,1) = -.5*b
      a(2,2) = b
      a(3,3) = b
      h(1,1) = -1.0
      h(1,2) = a(2,1)
      h(2,2) = b
      h(3,3) = b
      alpha = Alph(1)
      IF ( Nalpha/=1 ) alpha = Alph(i)
      alpha = alpha*Degra
      alpha2 = alpha*alpha
      n = 2
      IF ( Ca(i)/=0.0 ) n = 3
      IF ( Nthick==0 ) THEN
         IF ( Ntaus/=1 ) THEN
            k = (i-1)*3 + 1
            tau = Thi(k)
            tauh = Thi(k+1)
            taut = Thi(k+2)
            IF ( n==2 ) taut = 0.
            t = tauh - taut
            k = (i-1)*2 + 1 + 3*Nstrip
            zetam = Thi(k)
            zetah = Thi(k+1)
            DO j = 1 , 6
               aj(j) = 0.
            ENDDO
         ELSE
            tau = Thi(1)
            tauh = Thi(2)
            taut = Thi(3)
            IF ( n==2 ) taut = 0.
            t = tauh - taut
            zetam = Thi(4)
            zetah = Thi(5)
         ENDIF
         IF ( n==2 ) zetah = 1.0
         IF ( n/=2 ) THEN
            aj(1) = -.5*t
            aj(2) = -.25*t*(1.0+zetah)
            aj(3) = -(1./6.)*t*(1.+zetah+zetah*zetah)
            aj(4) = .25*t*t/(1.-zetah)
            aj(5) = .125*t*t*(1.0+zetah)/(1.-zetah)
            aj(6) = (1./12.)*t*t*(1.+zetah+zetah*zetah)/(1.-zetah)
         ENDIF
         ts = tau - tauh*(tau-tauh)
         ai(1) = tauh*.5 + aj(1)
         ai(2) = -(tau/3.)*zetah + (tauh/6.)*(2.*zetah+zetam) + aj(2)
         ai(3) = -(tau/12.)*zetah*(3.*zetah+2.*zetam) + (tauh/12.)*(3.*zetah*zetah+2.*zetah*zetam+zetam*zetam) + aj(3)
         ai(4) = (tau*tau/(3.*zetam)) + (1./3.)*ts*(zetah-zetam) + aj(4)
         ai(5) = (tau*tau/12.) + (1./12.)*ts*(3.*zetah+zetam)/(zetah-zetam) + aj(5)
         ai(6) = (tau*tau/30.)*zetam + (1./30.)*ts*(6.*zetah*zetah+3.*zetah*zetam+zetam*zetam)/(zetah-zetam) + aj(6)
      ELSE
         DO j = 1 , 6
            ai(j) = Thi(j)
            aj(j) = 0.0
            IF ( n==3 ) aj(j) = Thi(j+6)
         ENDDO
         zetah = 1.0
         IF ( Nxis==1 ) zetah = Thi(13)
         IF ( Nxis>1 ) zetah = Thi(i+12)
      ENDIF
      ems = Emach*Emach
      secs = Seclam*Seclam
      IF ( Nthry/=0 ) THEN
         cbar1 = Emach/sqrt(ems-secs)
         cbar2 = (ems*ems*(1.4+1.)-4.*secs*(ems-secs))/(4.*(ems-secs)**2)
      ELSE
         cbar1 = 1.
         cbar2 = (1.4+1.)/4.
      ENDIF
      cbar3 = (1.4+1.)/12.
      ek(1) = (1./Emach)*(cbar1+2.*cbar2*Emach*ai(1)+3.*cbar3*ems*(ai(4)+alpha2))
      ek(2) = (1./Emach)*(cbar1+4.*cbar2*Emach*ai(2)+3.*cbar3*ems*(2.*ai(5)+alpha2))
      ek(3) = (4./(3.*Emach))*(cbar1+6.*cbar2*Emach*ai(3)+3.*cbar3*ems*(3.*ai(6)+alpha2))
      IF ( n/=3 ) THEN
         ek(4) = (1./Emach)*(cbar1*(1.-zetah)+2.*cbar2*Emach*aj(1)+3.*cbar3*ems*aj(4)+alpha2*(1.-zetah))
         ek(5) = (1./Emach)*(cbar1*(1.-zetah*zetah)+4.*cbar2*Emach*aj(2)+3.*cbar3*ems*(2.*aj(5)+alpha2*(1.-zetah*zetah)))
         ek(6) = (4./(3.*Emach))*(cbar1*(1.-zetah**3)+6.*cbar2*Emach*aj(3)+3.*cbar3*ems*(3.*aj(6)+alpha2*(1.-zetah**3)))
         e1k = 1.0/(rfc*b)
         e1ks = e1k*e1k
         g(1,1) = 0.
         g(1,2) = -ek(1)*e1ks
         g(2,1) = 0.
         g(2,2) = -ek(2)*e1ks
         gi(1,1) = -ek(1)*e1k
         gi(1,2) = -ek(2)*e1k
         gi(2,1) = gi(1,2)
         gi(2,2) = -ek(3)*e1k
         IF ( n/=3 ) THEN
            g(1,3) = -ek(4)*e1ks
            g(2,3) = -ek(5)*e1ks
            g(3,1) = 0.
            g(3,2) = -(ek(5)-2.*ek(4)*zetah)*e1ks
            g(3,3) = g(3,2)
            gi(1,3) = -(ek(5)-2.*ek(4)*zetah)*e1k
            gi(2,3) = -(ek(6)-2.*ek(5)*zetah)*e1k
            gi(3,1) = gi(1,3)
            gi(3,2) = -(ek(6)-2.*ek(5)*zetah)*e1k
            gi(3,3) = -(ek(6)-4.*ek(5)*zetah+4.*ek(4)*zetah*zetah)*e1k
         ENDIF
      ENDIF
!
!     MATRICES BUILT TIME TO MULTIPLY
!
      DO k = 1 , n
         DO l = 1 , n
            q(k,l) = 0.
            qi(k,l) = 0.
            DO m1 = 1 , n
               q(k,l) = q(k,l) + a(k,m1)*g(m1,l)
               qi(k,l) = qi(k,l) + a(k,m1)*gi(m1,l)
            ENDDO
         ENDDO
      ENDDO
      n2 = 2*n
      DO k = 1 , n
         DO l = 1 , n2 , 2
            it = l/2 + 1
            p(k,l) = 0.
            p(k,l+1) = 0.
            DO m1 = 1 , n
               p(k,l) = p(k,l) + q(k,m1)*h(m1,it)
               p(k,l+1) = p(k,l+1) + qi(k,m1)*h(m1,it)
            ENDDO
            p(k,l) = p(k,l)*const
            p(k,l+1) = p(k,l+1)*const
         ENDDO
      ENDDO
!
!     PACK OUT
!
      Nn = Nn + n
      DO j = 1 , n2 , 2
         DO k = 1 , n
            pc(k) = cmplx(p(k,j),p(k,j+1))
         ENDDO
         CALL pack(pc,Ajjl,Mcb)
      ENDDO
      Ii = Ii + n
   ENDDO
END SUBROUTINE psta
