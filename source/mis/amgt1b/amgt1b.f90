!*==amgt1b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgt1b(Q,Nstns2,C1sbar,C2sbar)
   USE c_amgmn
   USE c_system
   USE c_tamg1l
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nstns2
   COMPLEX , DIMENSION(Nstns2,Nstns2) :: Q
   REAL :: C1sbar
   REAL :: C2sbar
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(20,40) :: a
   COMPLEX , DIMENSION(401) :: ab , an , cb , cn , fk , pd
   REAL :: b , b2 , beta , c2ssch , cc , cj , ck , cl , con , con2 , conn , csbar , csbar1 , csblsb , csbm2s , ct , d , delta ,     &
         & deltm , dlsdzb , h , hh , kappa , lamda , lamdm , m , m2sbar , mu , mus , nu , omega , pi , pi2 , s , sl , ss , tanlam , &
         & td , xl , xlam , zer
   REAL , DIMENSION(20,10) :: disp
   COMPLEX :: ff , fg , fo , fs , slope , st , stp , stti , sum1 , sum2
   INTEGER :: i , ir , is , j , jj , jk , k , kkk , l , n , n1 , n11 , n1m , n1n , n2 , n22 , nf , nk , nk1 , nk2 , nm , nn , nn1 , &
            & nn11 , nn22 , nnf , nnm , nnn
   COMPLEX , DIMENSION(21) :: loads
   COMPLEX , DIMENSION(50) :: p
   COMPLEX , DIMENSION(100) :: s1 , so
   COMPLEX , DIMENSION(20) :: stt
   REAL , DIMENSION(8) , SAVE :: w , ww
   REAL , DIMENSION(20) :: x
   EXTERNAL gauss2 , mesage
!
! End of declarations rewritten by SPAG
!
!
!      SUBSONIC RAO (CASCADES) CODE FOR SWEPT TURBOPROPS.
!
   DATA w/1.48283 , .89414 , .83521 , .66721 , .64172 , .55519 , .54026 , .48547/
   DATA ww/1.13333 , -0.00036 , 0.18796 , -0.00027 , 0.08469 , -0.00022 , 0.05049 , -0.00019/
!
!     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING
!     STATIONS PER STREAMLINE IS REFLECTED IN CODING.
!
   IF ( nstns>10 ) THEN
!
      WRITE (iout,99001) ufm , sln , nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUBSONIC CASCADE ',        &
             &'ROUTINE AMGT1B ALLOWS ONLY A MAXIMUM OF 10.')
      CALL mesage(-61,0,0)
      RETURN
   ELSE
!
      m = amach
      omega = redf
      ss = 2*blspc
      deltm = -sigma
      xlam = stag
      nm = nstns
      nnm = Nstns2
      csbar = .25*(den*vel**2*chord**2)/(refden*refvel**2)
      csbar1 = 2.0/chord
      m2sbar = -dcbdzb/chord
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
            IF ( cc==0.0 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            IF ( cc<0.0 ) fk(l) = sqrt(-cc)*ff
            IF ( cc>0.0 ) fk(l) = sqrt(cc)
            an(l) = fk(l)*cl + (delta+i)*ff*sl
            ab(l) = fk(l)*cl - (delta+i)*ff*sl
            pd(l) = fk(l)*(pi2*ab(l)+fg)
            cn(l) = cexp(-an(l)*ck)
            cb(l) = cexp(-ab(l)*ck)
            l = l + 1
            cc = (delta-i)**2 - mus
            IF ( cc==0.0 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
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
         SPAG_Loop_1_1: DO i = 2 , kkk , 2
            l = i
            st = ((1-cn(l))/an(l)+(1-cb(l))/ab(l))/fk(l) + st
            l = l + 1
            st = ((1-cn(l))/an(l)+(1-cb(l))/ab(l))/fk(l) + st
            IF ( cabs(st-stp)<con ) EXIT SPAG_Loop_1_1
            stp = st
         ENDDO SPAG_Loop_1_1
         so(1) = -st*s/(2*pi2*b2)
         DO j = 2 , n
            jk = 2*(j-1)
            l = 1
            stp = 0.0
            st = cn(l)**jk/fk(l)
            SPAG_Loop_2_2: DO i = 2 , kkk , 2
               l = l + 1
               st = cn(l)**jk/fk(l) + st
               l = l + 1
               st = cn(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT SPAG_Loop_2_2
               stp = st
            ENDDO SPAG_Loop_2_2
            so(j) = -0.5*st
         ENDDO
         n1 = n + 1
         n2 = 3*n - 1
         DO j = n1 , n2
            jk = j - n
            stp = 0.0
            l = 1
            st = cb(l)**jk/fk(l)
            SPAG_Loop_2_3: DO i = 2 , kkk , 2
               l = l + 1
               st = cb(l)**jk/fk(l) + st
               l = l + 1
               st = cb(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT SPAG_Loop_2_3
               stp = st
            ENDDO SPAG_Loop_2_3
            so(j) = -0.5*st
         ENDDO
         DO j = 1 , n
            jk = (j-1)*2 + 1
            l = 1
            stp = 0.0
            st = an(l)*cn(l)**jk/fk(l)
            SPAG_Loop_2_4: DO i = 2 , kkk , 2
               l = l + 1
               st = an(l)*cn(l)**jk/fk(l) + st
               l = l + 1
               st = an(l)*cn(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT SPAG_Loop_2_4
               stp = st
            ENDDO SPAG_Loop_2_4
            s1(j) = -pi/s*st
         ENDDO
         n1 = n + 1
         n2 = 2*n
         DO j = n1 , n2
            jk = (j-n1)*2 + 1
            l = 1
            stp = 0.0
            st = ab(l)*cb(l)**jk/fk(l)
            SPAG_Loop_2_5: DO i = 2 , kkk , 2
               l = l + 1
               st = ab(l)*cb(l)**jk/fk(l) + st
               l = l + 1
               st = ab(l)*cb(l)**jk/fk(l) + st
               IF ( cabs(st-stp)<con ) EXIT SPAG_Loop_2_5
               stp = st
            ENDDO SPAG_Loop_2_5
            s1(j) = pi/s*st
         ENDDO
         DO j = 1 , n
            jk = (j-1)*2 + 1
            l = 1
            stp = 0.0
            st = cb(l)**jk/pd(l)
            SPAG_Loop_2_6: DO i = 2 , kkk , 2
               l = l + 1
               st = cb(l)**jk/pd(l) + st
               l = l + 1
               st = cb(l)**jk/pd(l) + st
               IF ( cabs(st-stp)<con ) EXIT SPAG_Loop_2_6
               stp = st
            ENDDO SPAG_Loop_2_6
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
         tanlam = tan(sweep*pi/180.0)
         dlsdzb = dcbdzb/2.0
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
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
   END SUBROUTINE spag_block_1
END SUBROUTINE amgt1b
