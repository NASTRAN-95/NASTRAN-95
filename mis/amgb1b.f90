
SUBROUTINE amgb1b(Q)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Amach , Amachr , Blspc , Bspace , Chord , Den , Dum(2) , Flowa , Radius , Redf , Refc , Refcrd , Refden , Refflo , Refmac , &
      & Refstg , Refvel , Rfreq , Sigma , Stag , Tsonic , Vel
   INTEGER Ibuf , Iout , Iref , Mach , Maxmac , Mcb(7) , Minmac , Nlines , Nrow , Nstns , Nstnsx , Sln
   CHARACTER*23 Ufm
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /bamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stag , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amach , Redf , Blspc , Amachr , Tsonic
   COMMON /system/ Ibuf , Iout
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   COMPLEX Q(Nstns,Nstns)
!
! Local variable declarations
!
   COMPLEX a(20,30) , ab(401) , an(401) , cb(401) , cn(401) , ff , fg , fk(401) , fo , fs , loads(21) , p(50) , pd(401) , s1(100) , &
         & slope , so(100) , st , stp , stt(20) , sum
   REAL b , b2 , beta , cc , cj , ck , cl , con , con2 , ct , d , delta , deltm , disp(20,10) , h , hh , kappa , lamda , lamdm , m ,&
      & mu , mus , nu , omega , pi , pi2 , s , sl , ss , w(8) , x(20) , xl , xlam , zer
   INTEGER i , j , jj , jk , k , kkk , l , n , n1 , n11 , n1m , n1n , n2 , n22 , nf , nk , nk1 , nk2 , nm , nnn
!
! End of declarations
!
!
!     SUBSONIC RAO (CASCADES)
!
   DATA w/1.48283 , .89414 , .83521 , .66721 , .64172 , .55519 , .54026 , .48547/
!
!     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING
!     STATIONS PER STREAMLINE IS REFLECTED IN CODING.
!
   IF ( Nstns>10 ) THEN
!
      WRITE (Iout,99001) Ufm , Sln , Nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUBSONIC CASCADE ',        &
             &'ROUTINE AMGB1B ALLOWS ONLY A MAXIMUM OF 10.')
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
      m = Amach
      omega = Redf
      ss = 2*Blspc
      deltm = -Sigma
      xlam = Stag
      nm = Nstns
      n = 20
      pi = 3.141593
      pi2 = pi*2
      con = 1.0E-5
      nnn = 100
      kkk = 2*nnn + 1
      deltm = deltm/360
      xl = xlam*pi/180
      b = 1.0/n
      b2 = 2*b
      d = ss*sin(xl)
      hh = ss*cos(xl)
      beta = sqrt(1.-m**2)
      h = hh*beta
      zer = 0.0
      s = sqrt(h**2+d**2)
      lamdm = atan(d/h)
      cl = cos(lamdm)
      sl = sin(lamdm)
      nu = omega/beta**2
      kappa = m*nu
      lamda = m*kappa
      delta = deltm + lamda*d/pi2
      mu = kappa*s/pi2
      mus = mu**2
      ff = (0.0,1.0)
      fg = cmplx(zer,nu*s)
      l = 1
      cc = delta**2 - mus
      IF ( cc/=0.0 ) THEN
         IF ( cc<0.0 ) fk(l) = sqrt(-cc)*ff
         IF ( cc>0.0 ) fk(l) = sqrt(cc)
         an(l) = fk(l)*cl + ff*delta*sl
         ab(l) = fk(l)*cl - ff*delta*sl
         pd(l) = fk(l)*(pi2*ab(l)+fg)
         ck = pi2*b/s
         cn(l) = cexp(-an(l)*ck)
         cb(l) = cexp(-ab(l)*ck)
         DO i = 1 , nnn
            l = l + 1
            cc = (delta+i)**2 - mus
            IF ( cc==0.0 ) GOTO 100
            IF ( cc<0.0 ) fk(l) = sqrt(-cc)*ff
            IF ( cc>0.0 ) fk(l) = sqrt(cc)
            an(l) = fk(l)*cl + (delta+i)*ff*sl
            ab(l) = fk(l)*cl - (delta+i)*ff*sl
            pd(l) = fk(l)*(pi2*ab(l)+fg)
            cn(l) = cexp(-an(l)*ck)
            cb(l) = cexp(-ab(l)*ck)
            l = l + 1
            cc = (delta-i)**2 - mus
            IF ( cc==0.0 ) GOTO 100
            IF ( cc>0.0 ) fk(l) = sqrt(cc)
            IF ( cc<0.0 ) fk(l) = sqrt(-cc)*ff
            an(l) = fk(l)*cl + (delta-i)*ff*sl
            ab(l) = fk(l)*cl - (delta-i)*ff*sl
            pd(l) = fk(l)*(pi2*ab(l)+fg)
            cn(l) = cexp(-an(l)*ck)
            cb(l) = cexp(-ab(l)*ck)
         ENDDO
         stp = 0.0
         l = 1
         st = ((1-cn(l))/an(l)+(1-cb(l))/ab(l))/fk(l)
         DO i = 2 , kkk , 2
            l = i
            st = ((1-cn(l))/an(l)+(1-cb(l))/ab(l))/fk(l) + st
            l = l + 1
            st = ((1-cn(l))/an(l)+(1-cb(l))/ab(l))/fk(l) + st
            IF ( cabs(st-stp)<con ) EXIT
            stp = st
         ENDDO
         so(1) = -st*s/(2*pi2*b2)
         DO j = 2 , n
            jk = 2*(j-1)
            l = 1
            stp = 0.0
            st = cn(l)**jk/fk(l)
            DO i = 2 , kkk , 2
               l = l + 1
               st = cn(l)**jk/fk(l) + st
               l = l + 1
               st = cn(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT
               stp = st
            ENDDO
            so(j) = -0.5*st
         ENDDO
         n1 = n + 1
         n2 = 3*n - 1
         DO j = n1 , n2
            jk = j - n
            stp = 0.0
            l = 1
            st = cb(l)**jk/fk(l)
            DO i = 2 , kkk , 2
               l = l + 1
               st = cb(l)**jk/fk(l) + st
               l = l + 1
               st = cb(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT
               stp = st
            ENDDO
            so(j) = -0.5*st
         ENDDO
         DO j = 1 , n
            jk = (j-1)*2 + 1
            l = 1
            stp = 0.0
            st = an(l)*cn(l)**jk/fk(l)
            DO i = 2 , kkk , 2
               l = l + 1
               st = an(l)*cn(l)**jk/fk(l) + st
               l = l + 1
               st = an(l)*cn(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT
               stp = st
            ENDDO
            s1(j) = -pi/s*st
         ENDDO
         n1 = n + 1
         n2 = 2*n
         DO j = n1 , n2
            jk = (j-n1)*2 + 1
            l = 1
            stp = 0.0
            st = ab(l)*cb(l)**jk/fk(l)
            DO i = 2 , kkk , 2
               l = l + 1
               st = ab(l)*cb(l)**jk/fk(l) + st
               l = l + 1
               st = ab(l)*cb(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT
               stp = st
            ENDDO
            s1(j) = pi/s*st
         ENDDO
         DO j = 1 , n
            jk = (j-1)*2 + 1
            l = 1
            stp = 0.0
            st = cb(l)**jk/pd(l)
            DO i = 2 , kkk , 2
               l = l + 1
               st = cb(l)**jk/pd(l) + st
               l = l + 1
               st = cb(l)**jk/pd(l) + st
               IF ( cabs(st-stp)<con ) EXIT
               stp = st
            ENDDO
            p(j) = -s/2*st
         ENDDO
         fg = cmplx(zer,-nu*b)
         fg = 1/(cexp(fg)+cmplx(zer,nu*b2))
         fs = cmplx(zer,nu)
         cj = (nu*beta)**2
         l = 0
         ct = 2*kappa**2*b
         DO j = 1 , n
            DO i = 1 , n
               l = l + 1
               nk = i - j + 1
               nk1 = i - j
               nk2 = nk1 + 1
               IF ( i==j ) nk1 = n + 1
               IF ( i==j ) nk2 = 1
               IF ( j>i ) THEN
                  nk1 = n + j - i + 1
                  nk2 = nk1 - 1
                  nk = n + 2*(j-i)
               ENDIF
               a(i,j) = s1(nk1) - s1(nk2) + ct*so(nk)
               IF ( j==n ) THEN
                  nk = n + 2*(j-i) + 1
                  nk2 = j - i + 1
                  a(i,j) = a(i,j) - fg*(s1(nk1)+so(nk)*fs+cj*p(nk2))
               ENDIF
            ENDDO
         ENDDO
         x(1) = -1.0 + b
         DO i = 2 , n
            x(i) = x(i-1) + b2
         ENDDO
         n1 = n + nm
         n1n = n - 1
         n1m = nm - 1
         n11 = n + 1
         n22 = n + 2
         fo = ff*omega
         DO i = 1 , n
            disp(i,1) = -1.0
            disp(i,2) = -1.0 - x(i)
            stt(i) = cexp(-ff*lamda*x(i))*pi2/beta
            a(i,n11) = stt(i)*fo*disp(i,1)
            a(i,n22) = stt(i)*(fo*disp(i,2)-1.)
         ENDDO
         DO jj = 3 , nm
            nf = n + jj
            con2 = pi*(jj-2)/2
            DO i = 1 , n
               con = con2*disp(i,2)
               disp(i,jj) = sin(con)
               a(i,nf) = stt(i)*(fo*disp(i,jj)-con2*cos(con))
            ENDDO
         ENDDO
!WKBR SPR93019 10/93      CALL GAUSS (A,N,N1)
         CALL gauss2(a,n,n1)
         DO j = 1 , nm
            nf = n + j
            DO i = 1 , n
               loads(i) = a(i,nf)
            ENDDO
!
            slope = loads(2)/3./b
            a(1,nf) = 2.*cexp(lamda*ff*x(1))*(ff*nu*loads(1)+slope)
!
            slope = (loads(n)-loads(n1n))/b2
            a(n,nf) = 2.*cexp(lamda*ff*x(n))*(ff*nu*loads(n)+slope)
!
            DO i = 2 , n1n
               slope = (loads(i+1)-loads(i-1))/4./b
               a(i,nf) = 2.*cexp(lamda*ff*x(i))*(ff*nu*loads(i)+slope)
            ENDDO
         ENDDO
         DO i = 1 , n
            a(i,1) = sqrt((1-x(i))/(1+x(i)))
            DO j = 2 , n1m
               a(i,j) = -disp(i,j+1)
            ENDDO
            DO j = nm , n
               con2 = -pi*(j-1)*disp(i,2)/2
               a(i,j) = sin(con2)
            ENDDO
         ENDDO
!WKBR SPR93019 10/93      CALL GAUSS (A,N,N1)
         CALL gauss2(a,n,n1)
         a(1,1) = pi
         con = 1.
         DO j = 1 , n1n
            a(1,j+1) = con*4/j/pi
            con = 1. - con
         ENDDO
         a(2,1) = pi/2
         con = 0.
         DO j = 1 , n1n
            a(2,j+1) = a(1,j+1) - con*4/j/pi
            con = 1. - con
         ENDDO
         DO i = 3 , nm
            DO j = 2 , n
               con = 0.
               IF ( (i-1)==j ) con = 1.
               a(i,j) = con
            ENDDO
         ENDDO
         DO j = 3 , nm
            a(j,1) = w(j-2)
         ENDDO
         DO j = 1 , nm
            DO k = 1 , nm
               nf = n + k
               sum = (0.,0.)
               DO i = 1 , n
                  sum = sum + a(j,i)*a(i,nf)
               ENDDO
               Q(j,k) = sum
            ENDDO
         ENDDO
      ENDIF
   ENDIF
 100  RETURN
99999 RETURN
END SUBROUTINE amgb1b
