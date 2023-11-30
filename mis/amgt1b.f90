
SUBROUTINE amgt1b(Q,Nstns2,C1sbar,C2sbar)
   IMPLICIT NONE
   REAL Amach , Amachr , Blspc , Bspace , Chord , Dcbdzb , Den , Dum(2) , Redf , Refc , Refcrd , Refden , Refmac , Refstg , Refswp ,&
      & Refvel , Rfreq , Sigma , Stag , Sweep , Tsonic , Vel
   INTEGER Ibuf , Iout , Iref , Mach , Maxmac , Mcb(7) , Minmac , Nlines , Nrow , Nstns , Nstnsx , Sln
   CHARACTER*23 Ufm
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /system/ Ibuf , Iout
   COMMON /tamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refswp , Sln , Nstnsx ,   &
                 & Stag , Chord , Dcbdzb , Bspace , Mach , Den , Vel , Sweep , Amach , Redf , Blspc , Amachr , Tsonic
   COMMON /xmssg / Ufm
   REAL C1sbar , C2sbar
   INTEGER Nstns2
   COMPLEX Q(Nstns2,Nstns2)
   COMPLEX a(20,40) , ab(401) , an(401) , cb(401) , cn(401) , ff , fg , fk(401) , fo , fs , loads(21) , p(50) , pd(401) , s1(100) , &
         & slope , so(100) , st , stp , stt(20) , stti , sum1 , sum2
   REAL b , b2 , beta , c2ssch , cc , cj , ck , cl , con , con2 , conn , csbar , csbar1 , csblsb , csbm2s , ct , d , delta , deltm ,&
      & disp(20,10) , dlsdzb , h , hh , kappa , lamda , lamdm , m , m2sbar , mu , mus , nu , omega , pi , pi2 , s , sl , ss ,       &
      & tanlam , td , w(8) , ww(8) , x(20) , xl , xlam , zer
   INTEGER i , ir , is , j , jj , jk , k , kkk , l , n , n1 , n11 , n1m , n1n , n2 , n22 , nf , nk , nk1 , nk2 , nm , nn , nn1 ,    &
         & nn11 , nn22 , nnf , nnm , nnn
!
!      SUBSONIC RAO (CASCADES) CODE FOR SWEPT TURBOPROPS.
!
   DATA w/1.48283 , .89414 , .83521 , .66721 , .64172 , .55519 , .54026 , .48547/
   DATA ww/1.13333 , -0.00036 , 0.18796 , -0.00027 , 0.08469 , -0.00022 , 0.05049 , -0.00019/
!
!     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING
!     STATIONS PER STREAMLINE IS REFLECTED IN CODING.
!
   IF ( Nstns>10 ) THEN
!
      WRITE (Iout,99001) Ufm , Sln , Nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUBSONIC CASCADE ',        &
             &'ROUTINE AMGT1B ALLOWS ONLY A MAXIMUM OF 10.')
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
!
      m = Amach
      omega = Redf
      ss = 2*Blspc
      deltm = -Sigma
      xlam = Stag
      nm = Nstns
      nnm = Nstns2
      csbar = .25*(Den*Vel**2*Chord**2)/(Refden*Refvel**2)
      csbar1 = 2.0/Chord
      m2sbar = -Dcbdzb/Chord
      c2ssch = csbar1*C2sbar
      csblsb = csbar*csbar1
      csbm2s = csbar*m2sbar
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
         nn = n1
         nn1 = nn + nm
         n1n = n - 1
         n1m = nm - 1
         n11 = n + 1
         nn11 = nn + 1
         n22 = n + 2
         nn22 = nn + 2
         fo = ff*omega
         tanlam = tan(Sweep*pi/180.0)
         dlsdzb = Dcbdzb/2.0
         td = tanlam*dlsdzb
         DO i = 1 , n
            disp(i,1) = -1.0
            disp(i,2) = -1.0 - x(i)
            stt(i) = cexp(-ff*lamda*x(i))*pi2/beta
            stti = stt(i)
            a(i,n11) = stti*disp(i,1)*(fo+td)
            a(i,nn11) = stti*disp(i,1)*tanlam
            a(i,n22) = stti*(disp(i,2)*(fo+td)-1.)
            a(i,nn22) = stti*disp(i,2)*tanlam
         ENDDO
         DO jj = 3 , nm
            nf = n + jj
            nnf = nn + jj
            con2 = pi*(jj-2)/2
            DO i = 1 , n
               con = con2*disp(i,2)
               disp(i,jj) = sin(con)
               a(i,nf) = stt(i)*(disp(i,jj)*(fo+td)-con2*cos(con))
               a(i,nnf) = stt(i)*disp(i,jj)*tanlam
            ENDDO
         ENDDO
!WKBR SPR93019 10/93 CALL GAUSS (A,N,NN1)
         CALL gauss2(a,n,nn1)
         DO j = 1 , nnm
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
!WKBR SPR93019 10/93      CALL GAUSS (A,N,NN1)
         CALL gauss2(a,n,nn1)
         a(1,1) = c2ssch*pi + C1sbar*pi/2.
         a(2,1) = (c2ssch+C1sbar)*pi/2.
         con = 1.
         conn = 1.
         DO j = 1 , n1n
            a(1,j+1) = (c2ssch*con+C1sbar*conn)*4./j/pi
            a(2,j+1) = (c2ssch+2.*C1sbar)*conn*4./j/pi - con*C1sbar*32./(j*pi)**3
            con = 1. - con
            conn = -conn
         ENDDO
         DO i = 3 , nm
            ir = i - 2
            DO j = 2 , n
               is = j - 1
               IF ( ir==is ) THEN
                  a(i,j) = c2ssch + C1sbar
               ELSEIF ( (ir+is)/2*2==(ir+is) ) THEN
                  a(i,j) = (0.,0.)
               ELSE
                  a(i,j) = -C1sbar*16.*ir*is/(pi*(ir+is)*(ir-is))**2
               ENDIF
            ENDDO
         ENDDO
         DO j = 3 , nm
            a(j,1) = c2ssch*w(j-2) + C1sbar*ww(j-2)
         ENDDO
         DO j = 1 , nm
            DO k = 1 , nm
               nf = n + k
               nnf = nn + k
               sum1 = (0.,0.)
               sum2 = (0.,0.)
               DO i = 1 , n
                  sum1 = sum1 + a(j,i)*a(i,nf)
                  sum2 = sum2 + a(j,i)*a(i,nnf)
               ENDDO
               Q(j,k) = csblsb*sum1 + csbm2s*sum2
               Q(j,k+nm) = csbar*sum2
               Q(j+nm,k) = (0.,0.)
               Q(j+nm,k+nm) = (0.,0.)
            ENDDO
         ENDDO
      ENDIF
   ENDIF
 100  RETURN
99999 RETURN
END SUBROUTINE amgt1b
