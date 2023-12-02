!*==ais.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION ais(Np,I,L,Rr,Zz)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: ais
   INTEGER :: Np
   INTEGER :: I
   INTEGER :: L
   REAL , DIMENSION(4) :: Rr
   REAL , DIMENSION(4) :: Zz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a , aj , ar , beta , dfact , dkl , dl1 , dr , dz , dzj , fact , gkl , pr , ra , rak , rb , rbk , za , zb
   REAL(REAL64) , SAVE :: eps , one , three , two , zero
   REAL :: factl
   INTEGER :: j , jfact , k , l1 , l2 , lfact , lk , lmjf , m
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL(REAL64) , DIMENSION(4) :: r , z
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES THE SINGLE PRECISION  DELTA(IJ) INTEGRALS
!     FOR AXISYMMETRIC SOLIDS IN SDR2. CALCULATIONS DONE IN DOUBLE
!     PRECISION
!
!     INPUT
!     NP  = NUMBER OF POINTS (3 OR 4.  MORE THAN 4 WILL FAIL FSN-5)
!     I,L = THE INTEGRAL DESIRED (I SERIES STARTS WITH -1)
!     R   = RADIUS ARRAY (NP LONG)
!     Z   = Z-CORD ARRAY (NP LONG)
!
!     OUTPUT
!     DKL = DESIRED INTEGRAL
!
   DATA eps/.01D0/
   DATA zero , one , two , three/0.0D0 , 1.0D0 , 2.0D0 , 3.0D0/
   DATA nam/4HAIS  , 1H /
!
   DO m = 1 , Np
      r(m) = Rr(m)
      z(m) = Zz(m)
   ENDDO
   dkl = zero
   l1 = L + 1
   l2 = L + 2
   dl1 = l1
   k = I + 1
!
!     LOOP ON NUMBER OF POINTS
!
   IF ( r(1)<=zero ) THEN
      CALL spag_block_2
      RETURN
   ENDIF
   DO m = 1 , Np
      j = m + 1
      IF ( m==Np ) j = 1
      ra = r(m)
      rb = r(j)
      za = z(m)
      zb = z(j)
      dr = rb - ra
      dz = zb - za
!
!     TEST IF RADIUS IS .LE. 0 (DRIVER SHOULD FIND THIS)
!
      IF ( rb<=zero ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      gkl = zero
      pr = ra + rb
      ar = pr/two
!
!     CHECK FOR APPROXIMATION, DR/AVE(R)
!
      IF ( dabs(dr/ar)>=eps ) THEN
!
         a = za*dr - ra*dz
         beta = a/dr
!
!     CHECK FOR BETA .EQ. 0 CASE
!
         IF ( dabs(beta/ar)>eps ) THEN
!
!     GENERAL CASE
!
            rak = ra**k
            rbk = rb**k
            IF ( k<0 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( k==0 ) THEN
!
!     GENERAL CASE, K.EQ.0, CONSTANT TERM
!
               gkl = dlog(ra/rb)/dl1
            ELSE
!
!     GENERAL CASE, CONSTANT TERM
!
               ar = k*l1
               gkl = (rak-rbk)/ar
            ENDIF
!
!     GENERAL CASE, SUMMATION
!
            IF ( dz/=zero ) THEN
               lfact = 1
!
!     CALCULATE FACTORIAL (L+1)
!
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
!
!     CALCULATE (L+1-J) FACTORIAL IN LMJF
!
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
!
            gkl = gkl*beta**l1
!
         ELSEIF ( dz/=zero ) THEN
            lk = L + k + 1
            ar = lk
            gkl = (dz/dr)**l1*(ra**lk-rb**lk)/(dl1*ar)
         ENDIF
!
!     APPROXIMATE CODE
!
      ELSEIF ( dr/=zero ) THEN
         dzj = l1*l2
         rbk = zb**l1
         j = k - 1
         gkl = -dr*ar**j*rbk/dl1
!
         IF ( dz/=zero ) gkl = gkl + (((two*ra+rb)/three)**j*dr*dabs(za**l2-rbk*zb))/(dzj*dz)
      ENDIF
!
      dkl = dkl + gkl
   ENDDO
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     ALL DONE
!
      ais = dkl
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     ERROR
!
      CALL mesage(-7,k,nam)
      CALL spag_block_1
      RETURN
   END SUBROUTINE spag_block_2
END FUNCTION ais
