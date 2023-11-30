
FUNCTION dkls(Np,I,L,R,Z)
   IMPLICIT NONE
   INTEGER I , L , Np
   REAL dkls
   REAL R(3) , Z(3)
   REAL a , aj , ar , beta , dfact , dl1 , dr , dz , dzj , eps , fact , factl , gkl , one , pr , ra , rak , rb , rbk , two , za ,   &
      & zb , zero
   INTEGER j , jfact , k , l1 , l2 , lfact , lk , lmjf , m , nam(2)
!-----
!    THIS ROUTINE CALCULATES THE SINGLE PRECISION INTEGRALS FOR
!    AXISYMMETRIC SOLIDS IN EMG
!
!   INPUT
!     NP = NUMBER OF POINTS (3 OR 4)
!     I,L= THE INTEGRAL DESIRED (I SERIES STARTS WITH -1)
!     R  = RADIUS ARRAY (NP LONG)
!     Z  = Z-CORD ARRAY (NP LONG)
!
!   OUTPUT
!     DKL = DESIRED INTEGRAL
!
!-----
   DATA eps/.01/ , nam/4HDKLS , 1H /
   DATA zero , one , two/0. , 1. , 2./
!
   dkls = zero
   l1 = L + 1
   l2 = L + 2
   dl1 = l1
   k = I + 1
!
!  . LOOP ON NUMBER OF POINTS...
   IF ( R(1)<=zero ) GOTO 200
   DO m = 1 , Np
      j = m + 1
      IF ( m==Np ) j = 1
      ra = R(m)
      rb = R(j)
      za = Z(m)
      zb = Z(j)
      dr = rb - ra
      dz = zb - za
!
!  . TEST IF RADIUS IS .LE. 0 (DRIVER SHOULD FIND THIS)...
      IF ( rb<=zero ) GOTO 200
      gkl = zero
      pr = ra + rb
      ar = pr/two
!
!  . CHECK FOR APPROXIMATION, DR/AVE(R)...
      IF ( abs(dr/ar)>=eps ) THEN
!
         a = za*dr - ra*dz
         beta = a/dr
!
!  . CHECK FOR BETA .EQ. 0 CASE...
         IF ( abs(beta/ar)>eps ) THEN
!
!  . GENERAL CASE...
            rak = ra**k
            rbk = rb**k
            IF ( k<0 ) GOTO 200
            IF ( k==0 ) THEN
!
!  . GENERAL CASE, K.EQ.0, CONSTANT TERM...
               gkl = alog(ra/rb)/dl1
            ELSE
!
!  . GENERAL CASE, CONSTANT TERM...
               ar = k*l1
               gkl = (rak-rbk)/ar
            ENDIF
!
!  . GENERAL CASE, SUMMATION...
            IF ( dz/=zero ) THEN
               lfact = 1
!  . CALCULATE FACTORIAL (L+1)...
               DO j = 2 , L
                  lfact = lfact*j
               ENDDO
               factl = lfact
               jfact = 1
               aj = one
               dzj = one
               lmjf = lfact*l1
               DO j = 1 , l1
                  jfact = jfact*j
!  . CALCULATE (L+1-J) FACTORIAL IN LMJF...
                  lmjf = lmjf/(l2-j)
                  fact = factl/float(jfact*lmjf)
                  dfact = k + j
                  dfact = fact/dfact
                  aj = aj*a
                  rak = rak*ra
                  rbk = rbk*rb
                  dzj = dzj*dz
                  gkl = gkl + (dfact*dzj*(rak-rbk))/aj
               ENDDO
            ENDIF
!-----
            gkl = gkl*beta**l1
!
         ELSEIF ( dz/=zero ) THEN
            lk = L + k + 1
            ar = lk
            gkl = (dz/dr)**l1*(ra**lk-rb**lk)/(dl1*ar)
         ENDIF
!
!  . APPROXIMATE CODE...
      ELSEIF ( dr/=zero ) THEN
         dzj = l1*l2
         rbk = zb**l1
         j = k - 1
         gkl = -dr*ar**j*rbk/dl1
!
         IF ( dz/=zero ) gkl = gkl + (((2.*ra+rb)/3.)**j*dr*abs(za**l2-rbk*zb))/(dzj*dz)
      ENDIF
!
      dkls = dkls + gkl
   ENDDO
!-----
!
!  . ALL DONE
!
 100  RETURN
!
!  . ERROR...
!
 200  CALL mesage(-7,k,nam)
   GOTO 100
END FUNCTION dkls
