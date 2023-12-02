!*==fqrwv.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fqrwv(M,E,Er,A,B,W,P,Q,Xm,Int,Zb,Srfle,Mcbc)
USE C_FEERXX
USE C_LHPWX
USE C_MACHIN
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: M
   REAL(REAL64) , DIMENSION(1) :: E
   REAL(REAL64) , DIMENSION(1) :: Er
   REAL(REAL64) , DIMENSION(1) :: A
   REAL(REAL64) , DIMENSION(2) :: B
   REAL(REAL64) , DIMENSION(1) :: W
   REAL(REAL64) , DIMENSION(1) :: P
   REAL(REAL64) , DIMENSION(1) :: Q
   REAL(REAL64) , DIMENSION(1) :: Xm
   LOGICAL , DIMENSION(1) :: Int
   REAL , DIMENSION(1) :: Zb
   INTEGER :: Srfle
   INTEGER , DIMENSION(7) :: Mcbc
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , SAVE :: base
   REAL(REAL64) :: bmax , c , delta , dim , dimf , e1 , e2 , emax , eps , epx , epx2 , erf , ev , f , gg , hov , pprc , prc ,       &
                 & ratio , s , scale , shift , sqrt2 , ss , sum , sumx , t , tmax , tol , x , x1 , y , z , zerr
   INTEGER :: i , icf , ij , io , iprec , irp , it , j , jerr , jrp , k , k1 , l , l1 , m1 , mvec , niter , nrp , nv
   INTEGER , SAVE :: iexp , ilim
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL close , gopen , makmcb , pack , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!                                                  SR5FLE SR4FLE
!
   !>>>>EQUIVALENCE (Ksystm(2),Io) , (Ksystm(55),Iprec)
   DATA ilim , iexp , base/120 , 60 , 2.D0/
!
!     IACC =  MACHINE ACCURACY CONTROL (EPSILON)
!     IACC IS USED TO CONTROL NUMBER UNDERFLOW
!     IEXP AND BASE ARE USED TO CONTROL NUMBER OVERFLOW
!
   IF ( M==1 ) RETURN
   Iprc = 2
   CALL makmcb(mcb(1),Srfle,M,2,Iprc)
   icf = Mcbc(1)
   Incr = 1
   Incrp = 1
   Itp1 = Iprc
   Itp2 = Iprc
   it = Iacc*iprec
   prc = 10.D0**(-it)
   pprc = 10.D-4
   jerr = 0
   epx = 10.D0**(2-it)
   epx2 = epx**2
   hov = base**iexp
   IF ( (Machx>=5 .AND. Machx<=11) .OR. Machx==13 .OR. Machx==21 ) hov = base**(iexp-10)
   sqrt2 = dsqrt(base)
   m1 = M - 1
   DO i = 1 , M
      E(i) = A(i)
   ENDDO
   tol = prc/(10.D0*dble(float(M)))
   bmax = 0.D0
   tmax = 0.D0
   W(M+1) = 0.D0
   DO i = 1 , M
      IF ( bmax<dabs(B(i)) ) bmax = dabs(B(i))
      IF ( tmax<dabs(A(i)) ) tmax = dabs(A(i))
   ENDDO
   IF ( tmax<bmax ) tmax = bmax
   scale = 1.D0
   SPAG_Loop_1_1: DO i = 1 , ilim
      IF ( scale*tmax>hov ) EXIT SPAG_Loop_1_1
      scale = scale*2.D0
   ENDDO SPAG_Loop_1_1
   IF ( bmax/=0.D0 ) THEN
      DO i = 1 , M
         E(i) = A(i)*scale
         W(i) = (B(i)*scale)**2
      ENDDO
      delta = tmax*scale*tol
      eps = delta*delta
      k = M
      SPAG_Loop_1_2: DO
         l = k
         IF ( l<=0 ) THEN
            DO i = 1 , M
               E(i) = E(i)/scale
            ENDDO
            DO l = 1 , m1
               k = M - l
               DO i = 1 , k
                  IF ( E(i)<=E(i+1) ) THEN
                     x = E(i)
                     E(i) = E(i+1)
                     E(i+1) = x
                  ENDIF
               ENDDO
            ENDDO
            DO l = 1 , m1
               k = M - l
               DO i = 1 , k
                  IF ( dabs(E(i))<=dabs(E(i+1)) ) THEN
                     x = E(i)
                     E(i) = E(i+1)
                     E(i+1) = x
                  ENDIF
               ENDDO
            ENDDO
            EXIT SPAG_Loop_1_2
         ELSE
            l1 = l - 1
            SPAG_Loop_2_3: DO i = 1 , l
               k1 = k
               k = k - 1
               IF ( W(k1)<=eps ) EXIT SPAG_Loop_2_3
            ENDDO SPAG_Loop_2_3
            IF ( k1/=l ) THEN
               t = E(l) - E(l1)
               x = W(l)
               y = .5D0*t
               s = dsqrt(x)
               IF ( dabs(t)>delta ) s = (x/y)/(1.D0+dsqrt(1.D0+x/y**2))
               e1 = E(l) + s
               e2 = E(l1) - s
               IF ( k1/=l1 ) THEN
                  shift = e1
                  IF ( dabs(t)<delta .AND. dabs(e2)<dabs(e1) ) shift = e2
                  s = 0.D0
                  c = 1.D0
                  gg = E(k1) - shift
                  SPAG_Loop_2_4: DO
                     IF ( dabs(gg)<delta ) gg = gg + c*delta*dsign(1.D0,gg)
                     f = gg**2/c
                     k = k1
                     k1 = k + 1
                     x = W(k1)
                     t = x + f
                     W(k) = s*t
                     IF ( k<l ) THEN
                        c = f/t
                        s = x/t
                        x = gg
                        gg = c*(E(k1)-shift) - s*x
                        E(k) = (x-gg) + E(k1)
                     ELSE
                        E(k) = gg + shift
                        EXIT SPAG_Loop_2_4
                     ENDIF
                  ENDDO SPAG_Loop_2_4
               ELSE
                  E(l) = e1
                  E(l1) = e2
                  W(l1) = 0.D0
               ENDIF
            ELSE
               W(l) = 0.D0
            ENDIF
         ENDIF
      ENDDO SPAG_Loop_1_2
   ENDIF
   IF ( M==0 ) RETURN
!
!     COMPUTE EIGENVECTORS BY INVERSE ITERATION
!
   erf = B(M+1)
   mvec = M
   f = scale/hov
   DO i = 1 , M
      A(i) = A(i)*f
      B(i) = B(i)*f
   ENDDO
   x1 = 0.D0
   dimf = 10.D0**(-it/3)
   DO nv = 1 , mvec
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            ij = nv
            sumx = 0.D0
            irp = 0
            IF ( nv==1 ) THEN
               nrp = 0
               W(i) = 1.D0
               Iip = 1
               Nnp = M
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ratio = dabs(E(nv)/E(nv-1)-1.D0)
               dim = .02D0*dabs(1.D0-Lambda*E(nv))
               IF ( ratio<dim .OR. ratio<dimf ) THEN
!
!     MULTIPLE EIGENVALUES
!
                  nrp = nrp + 1
               ELSE
                  nrp = 0
               ENDIF
               IF ( nv/=2 ) THEN
                  CALL gopen(Srfle,Zb(1),Wrt)
               ELSE
                  CALL gopen(Srfle,Zb(1),Wrtrew)
                  mcb(2) = 0
                  mcb(6) = 0
               ENDIF
               Iip = 1
               Nnp = M
               CALL pack(W(1),Srfle,mcb(1))
               CALL close(Srfle,Norew)
               sum = 0.D0
               ss = 1.0D0
               DO i = 1 , M
                  ss = -ss
                  ij = ij + 1
                  P(i) = float(mod(ij,3)+1)/(3.0*float((mod(ij,13)+1)*(1+5*i/M)))
                  P(i) = P(i)*ss
                  sum = sum + P(i)**2
               ENDDO
               sum = 1.D0/dsqrt(sum)
               DO i = 1 , M
                  P(i) = P(i)*sum
                  Q(i) = P(i)
               ENDDO
               CALL gopen(Srfle,Zb(1),Rdrew)
               j = 0
               SPAG_Loop_2_5: DO
                  sum = 0.D0
                  j = j + 1
                  DO i = 1 , M
                     sum = sum + W(i)*P(i)
                  ENDDO
                  DO i = 1 , M
                     Q(i) = Q(i) - sum*W(i)
                  ENDDO
                  IF ( j==(nv-1) ) EXIT SPAG_Loop_2_5
                  Ii = 1
                  Nn = M
                  CALL unpack(*10,Srfle,W(1))
               ENDDO SPAG_Loop_2_5
            ENDIF
 10         CALL close(Srfle,Norew)
            sum = 0.D0
            DO i = 1 , M
               sum = sum + Q(i)**2
            ENDDO
            sum = 1.D0/dsqrt(sum)
            DO i = 1 , M
               Q(i) = Q(i)*sum
               W(i) = Q(i)
            ENDDO
            spag_nextblock_1 = 2
         CASE (2)
            ev = E(nv)*f
            x = A(1) - ev
            y = B(2)
            DO i = 1 , m1
               c = A(i+1) - ev
               s = B(i+1)
               IF ( dabs(x)>=dabs(s) ) THEN
                  IF ( dabs(x)<tol ) x = tol
                  P(i) = x
                  Q(i) = y
                  Int(i) = .FALSE.
                  z = -s/x
                  x = c + z*y
                  y = B(i+2)
               ELSE
                  P(i) = s
                  Q(i) = c
                  Int(i) = .TRUE.
                  z = -x/s
                  x = y + z*c
                  IF ( i<m1 ) y = z*B(i+2)
               ENDIF
               Xm(i) = z
            ENDDO
            IF ( dabs(x)<tol ) x = tol
            niter = 0
            SPAG_Loop_2_6: DO
               niter = niter + 1
               W(M) = W(M)/x
               emax = dabs(W(M))
               DO l = 1 , m1
                  i = M - l
                  y = W(i) - Q(i)*W(i+1)
                  IF ( Int(i) ) y = y - B(i+2)*W(i+2)
                  W(i) = y/P(i)
                  IF ( dabs(W(i))>emax ) emax = dabs(W(i))
               ENDDO
               sum = 0.D0
               DO i = 1 , M
!WKBR W(I) = (W(I)/EMAX)/EPX
                  IF ( emax/=0.0 ) W(i) = (W(i)/emax)/epx
                  IF ( dabs(W(i))<epx2 ) W(i) = epx2
                  sum = sum + W(i)**2
               ENDDO
               s = dsqrt(sum)
               DO i = 1 , M
                  W(i) = W(i)/s
               ENDDO
               IF ( niter>=4 ) THEN
                  IF ( nv==1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     MULTIPLE EIGENVALUES AND ORTHOGONALIZATION
!
                  irp = irp + 1
                  CALL gopen(Srfle,Zb(1),Rdrew)
                  DO i = 1 , M
                     Q(i) = W(i)
                  ENDDO
                  sumx = 0.D0
                  jrp = nv - 1
                  DO i = 1 , jrp
                     Ii = 1
                     Nn = M
                     CALL unpack(*20,Srfle,P(1))
                     sum = 0.D0
                     DO j = 1 , M
                        sum = sum + P(j)*Q(j)
                     ENDDO
                     IF ( dabs(sum)>sumx ) sumx = dabs(sum)
                     DO j = 1 , M
                        W(j) = W(j) - sum*P(j)
                     ENDDO
                  ENDDO
                  EXIT SPAG_Loop_2_6
               ELSE
                  DO i = 1 , m1
                     IF ( Int(i) ) THEN
                        y = W(i)
                        W(i) = W(i+1)
                        W(i+1) = y + Xm(i)*W(i)
                     ELSE
                        W(i+1) = W(i+1) + Xm(i)*W(i)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO SPAG_Loop_2_6
 20         CALL close(Srfle,Norew)
            spag_nextblock_1 = 3
         CASE (3)
!
!     LOGIC SETTING SUM (BY G.CHAN/UNISYS  7/92)
!
!     SUM = PRC*PREC COULD PRODUCE UNDERFLOW (IT=16, PRC=10.**-32)
!     SUM = ZERO, COULD CAUSE DIVIDED BY ZERO AFTER 420 FOR NULL VECTOR
!     SO, WE CHOOSE SUM A LITTLE SMALLER THAN PRC
!
!     SUM = PRC*PRC
!     SUM = 0.0D+0
            sum = prc*1.0D-2
!
            DO i = 1 , M
               IF ( dabs(W(i))>=prc ) sum = sum + W(i)*W(i)
            ENDDO
            sum = 1.D0/dsqrt(sum)
            DO i = 1 , M
               W(i) = W(i)*sum
            ENDDO
            IF ( sumx>0.9D0 .AND. irp<3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( L16/=0 ) WRITE (io,99001) nv , niter , irp , sumx
99001       FORMAT (10X,18H FEER QRW ELEMENT ,I5,6H ITER ,2I3,6H PROJ ,D16.8)
            IF ( jerr<=0 ) THEN
               zerr = dabs(W(1))
               DO i = 2 , M
                  IF ( dabs(W(i))>zerr ) zerr = dabs(W(i))
               ENDDO
               zerr = (dabs(W(M)))/zerr
               IF ( zerr>pprc ) jerr = nv - 1
               IF ( jerr/=0 ) WRITE (io,99002) Uwm , jerr
99002          FORMAT (A25,' 2399',/5X,'ONLY THE FIRST',I5,' EIGENSOLUTIONS ',                                                      &
                      &'CLOSEST TO THE SHIFT POINT (F1 OR ZERO) PASS THE FEER ','ACCURACY TEST FOR EIGENVECTORS.')
            ENDIF
            CALL pack(W(1),icf,Mcbc(1))
            Er(nv) = dabs(W(M)*erf/E(nv))
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE fqrwv
