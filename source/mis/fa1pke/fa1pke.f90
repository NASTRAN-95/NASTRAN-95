!*==fa1pke.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa1pke(Khh,Bhh,Mhh,Bxhh,Fsave,Nloop,Bref,Rref,Neiw,Eps)
USE C_BLANK
USE C_CONDAS
USE C_FA1PKC
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Khh
   INTEGER :: Bhh
   INTEGER :: Mhh
   INTEGER :: Bxhh
   INTEGER :: Fsave
   INTEGER :: Nloop
   REAL :: Bref
   REAL :: Rref
   INTEGER :: Neiw
   REAL :: Eps
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a10 , a11 , bov , d1 , det , ei , er , kint , rbv , rf , rg , rho , rk , rktst , rvs , vel , vels , x10 , x11 , x12 ,    &
         & xav , xav1 , y10 , y11 , yav1
   INTEGER :: buf1 , i , iad , ib , ibh , ibuf2 , ic0 , ic0d , ieigns , ifl , ikh , ima , imh , imhere , iop , ip0 , ip0d , iq0 ,   &
            & iqi , iqr , ising , it , iv , j , ji , k , kn1 , l , l39 , neign , nit , nl , nlft , nn , nn2 , nr , nra , nrem ,     &
            & nroot , nrow , nrs , nwr
   REAL(REAL64) :: dsum , dx1 , dx2
   REAL(REAL64) , DIMENSION(1) :: dz
   LOGICAL :: eigv
   INTEGER , SAVE :: istart
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , fa1pka , fa1pkv , gmmats , gopen , invers , mesage , rdtrl , read , rsort , skprec , sswtch , unpack , write ,  &
          & wrttrl , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     FA1PKE COMPUTES THE EIGENVALUES FOR THE PK METHOD
!
!     LAST REVISED  2/91, BY J.PETKAS/LOCKHEED
!     ELEMENTS OF INTERPOLATION MATRIX IN D.P. AND LEAST SQUARE FIT
!
   !>>>>EQUIVALENCE (Z(1),Dz(1))
   DATA name/4HFA1P , 4HKE  /
   DATA istart/0/
!
!     REINITIALIZE EVERY TIME MACH CHANGES
!
   IF ( Iflag/=0 ) THEN
      CALL sswtch(39,l39)
      trl(1) = Khh
      CALL rdtrl(trl)
      nrow = trl(2)
      Neiw = min0(Neiw,nrow)
      neign = nrow*2
      Iout = 1
      Inn = 1
      Incr1 = 1
      Nnn = nrow
      ieigns = Ncore - nrow*5 - 1
      buf1 = ieigns - Sysbuf
      nn = nrow*nrow
      nn2 = nn*2
      imh = Icp
      ibh = imh + nn
      ikh = ibh + nn
      iv = ikh + nn
      ib = iv + nn
      ima = ib + nn
      IF ( mod(ima,2)==0 ) ima = ima + 1
      iop = ima + nn2*4
!
!     CORE CHECK
!
      IF ( iop+Sysbuf>ieigns ) CALL mesage(-8,0,name)
!
!     PUT K B M IN CORE
!
      ifl = Khh
      ji = ikh
      SPAG_Loop_1_1: DO
         CALL gopen(ifl,Z(buf1),0)
         DO i = 1 , nrow
            spag_nextblock_1 = 1
            SPAG_DispatchLoop_1: DO
               SELECT CASE (spag_nextblock_1)
               CASE (1)
                  CALL unpack(*2,ifl,Z(ji))
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 2                CALL zeroc(Z(ji),nrow)
                  spag_nextblock_1 = 2
               CASE (2)
                  ji = ji + nrow
                  EXIT SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_1
         ENDDO
         CALL close(ifl,1)
         IF ( ifl==Mhh ) THEN
!
!     MODIFICATION FOR LEVEL 17.7 UPDATE
!     REPLACE CALLS TO INVAER WITH CALLS TO INVERS.
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
            ising = -1
            CALL invers(nrow,Z(imh),nrow,0,0,det,ising,Z(iop))
            IF ( ising==2 ) CALL mesage(-7,0,name)
            EXIT SPAG_Loop_1_1
         ELSE
            IF ( ifl/=Bhh ) THEN
               ifl = Bhh
               ji = ibh
               trl(1) = Bhh
               CALL rdtrl(trl)
               IF ( trl(1)>0 ) CYCLE
               CALL zeroc(Z(ji),nn)
            ENDIF
            ifl = Mhh
            ji = imh
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
!
!     START OF LARGE LOOP WITH K = 0.0
!
   kint = 0.0
   IF ( Eps<=0.0 ) Eps = .001
   kn1 = Nk + 1
   iq0 = ieigns - (nn2+1)
   ic0 = iq0 - kn1*2 - 2
   IF ( mod(ic0,2)==0 ) ic0 = ic0 - 1
   ip0 = ic0 - kn1*2 - 2
   IF ( mod(ip0,2)==0 ) ip0 = ip0 - 1
!
   i = (Floop-1)*3
   eigv = .FALSE.
   IF ( Z(Imvr+i+1)<0.0 ) eigv = .TRUE.
   vel = abs(Z(Imvr+i+1))
   vels = vel*vel
   rho = (Rref*Z(Imvr+i+2))/2.0
   IF ( l39/=0 ) WRITE (Nout,99001) Floop , Z(Imvr+i) , Z(Imvr+i+1) , Z(Imvr+i+2)
99001 FORMAT ('0 TRACE FOR PK METHOD LOOP',I5,6X,4HMACH,8X,8HVELOCITY,8X,7HDENSITY,/,30X,1P,E15.5,1P,E15.5,1P,E15.5)
   nit = 0
   nroot = 0
!
!     INITIALIZE LEAST SQUARE COEFFCIENTS
!
   xav = 0.
   yav1 = 0.
   x10 = 0.
   x11 = 0.
   x12 = 0.
   y10 = 0.
   y11 = 0.
   SPAG_Loop_1_3: DO
!
!     BUILD P
!
      nit = nit + 1
!
!     SUM LEAST SQUARE COEFFICIENTS ASSOCIATED WITH INDEPENDENT
!     VARIABLE STARTING WITH SECOND TRIAL
!
      IF ( nit/=1 ) THEN
         xav = xav + kint
         x10 = x10 + 1.
         x11 = x11 + kint
         x12 = x12 + kint**2
      ENDIF
!
      ip0d = ip0/2 + 1
      dx1 = kint
      DO i = 1 , Nk
         dx2 = Z(Ik+i-1)
         dz(ip0d+i) = dabs((dx1-dx2)**3) + (dx1+dx2)**3
      ENDDO
      dz(ip0d+kn1) = 1.D0
!
!     FIND C = A-1  P
!
      iad = Ia/2 + 1
      ic0d = ic0/2 + 1
      l = iad
      DO i = 1 , kn1
         dsum = 0.D+0
         DO j = 1 , kn1
            dsum = dsum + dz(l)*dz(ip0d+j)
            l = l + 1
         ENDDO
         dz(ic0d+i) = dsum
      ENDDO
!
!     FIND QR AND QI = Q  C  Q IS COLUMN STORED
!
      l = Iq
      DO i = 1 , nn2
         dsum = 0.D+0
         DO j = 1 , Nk
            k = l + (j-1)*nn2
            dsum = dsum + Z(k)*dz(ic0d+j)
         ENDDO
         l = l + 1
         Z(iq0+i) = dsum
      ENDDO
!
!     COLUMN STORED M-1  BHH  KNH  QR (Z(IQ0+1)   QI (Z(IQ3+NN+1)
!
!     B  =  -BHH  + RHO*BREF*VEL  QHHI
!
!     K  =  -KHH  + RHO*VELS      QHHR
!
!     BUILD  A
!                  0         I
!
!                   -1       -1
!                 -M K     -M B
!
      nrem = iq0 - iop
      IF ( nrem<=nn ) CALL mesage(-8,0,name)
      it = iop
      IF ( mod(it,2)==0 ) it = it + 1
      IF ( eigv .AND. it+nn>buf1 ) CALL mesage(-8,0,name)
      bov = Bref/vel
      rbv = rho*Bref*vel
      iqr = iq0
      iqi = iq0 + nn
      rvs = rho*vels
!
!     BUILD M-1K IN IB AND M-1B IN IT  THEN GMMATS INTO IV AND IB
!
      DO i = 1 , nn
         Z(it+i-1) = -Z(ibh+i-1) + rbv*Z(iqi+i)
         Z(ib+i-1) = -Z(ikh+i-1) + rvs*Z(iqr+i)
      ENDDO
      CALL gmmats(Z(ib),nrow,nrow,0,Z(imh),nrow,nrow,0,Z(iv))
      CALL gmmats(Z(it),nrow,nrow,0,Z(imh),nrow,nrow,0,Z(ib))
!
!     CALL FA1PKA TO MAKE A MATRIX AND GET EIGENVALUES
!
      CALL fa1pka(Z(ima),Z(iv),Z(ib),Z(it),ieigns-it,nrow)
!
!     SORT EIGENVALUES
!
      j = neign*2
      CALL rsort(2,1,Z(it),j)
      CALL rsort(2,2,Z(it),j)
      IF ( kint==0.0 ) THEN
         nlft = neign
         SPAG_Loop_2_2: DO i = 1 , j , 2
            IF ( Z(it+i)>=0.0 ) EXIT SPAG_Loop_2_2
            nlft = nlft - 1
         ENDDO SPAG_Loop_2_2
         nl = it + (neign-nlft)*2
         nr = 0
         DO i = 1 , j , 2
            IF ( Z(it+i)==0.0 ) THEN
               nr = nr + 1
               IF ( eigv ) CALL fa1pkv(Z(ima),Z(iv),Z(ib),nrow,Z(it+i-1),Z(ima),Bref,Pi,vel,Z(buf1))
            ENDIF
         ENDDO
         nrs = nr + 1
         nr = nr/2
         nra = 0
      ENDIF
      IF ( l39/=0 ) THEN
         WRITE (Nout,99002) kint
99002    FORMAT (1H0,29H ESTIMATED REDUCED FREQUENCY ,1P,E15.5,/10X,11HEIGENVALUES,10X,18H REDUCED FREQUENCY,4X,9HFREQUENCY,6X,     &
                &8H DAMPING,/,7X,4HREAL,10X,4HIMAG)
         DO i = 1 , j , 2
            er = Z(it+i-1)
            ei = Z(it+i)
            IF ( ei==0.0 ) THEN
               rk = 0.0
               rf = 0.0
               rg = (Bref/(Pi*vel))*er
            ELSE
               rk = bov*ei
               rf = (1.0/Twopi)*ei
               rg = (2.0*er)/ei
            ENDIF
            WRITE (Nout,99003) er , ei , rk , rf , rg
99003       FORMAT (1H ,1P,E15.5,1P,E15.5,3X,1P,E15.5,1P,E15.5,1P,E15.5)
         ENDDO
      ENDIF
!
!     ROOT ACCEPTANCE AND SAVING
!
      j = nlft*2
      l = nroot*2 + 1 + nra*2
      imhere = 200
      IF ( l<=j ) THEN
!
         DO i = l , j , 2
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  k = (nroot*5) + 1 + ieigns
                  IF ( Z(nl+i)/=0.0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( kint/=0.0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( nrs/=nr ) nrs = nrs - 1
                  IF ( nrs/=nr ) CYCLE
                  nra = nra + 1
                  Z(k) = Z(nl+i-1)
                  Z(k+1) = Z(nl+i)
                  Z(k+2) = 0.0
                  Z(k+3) = 0.0
                  Z(k+4) = (Bref/(.34657*vel))*Z(nl+i-1)
                  spag_nextblock_2 = 2
               CASE (2)
                  nroot = nroot + 1
!
!     PRINT EIGENVECTORS IF ASKED FOR
!
                  nit = 0
!
!     NO. OF ITERATIONS RESET TO ZERO. RE-INITIALIZE LEASE SQUARE COEFF.
!
                  xav = 0.
                  yav1 = 0.
                  x10 = 0.
                  x11 = 0.
                  x12 = 0.
                  y10 = 0.
                  y11 = 0.
                  IF ( nroot<Neiw ) CYCLE
                  EXIT SPAG_Loop_1_3
               CASE (3)
                  rktst = bov*Z(nl+i)
                  IF ( abs(rktst-kint)>=Eps ) THEN
                     IF ( rktst/=0.0 ) THEN
!
!     SUM LEAST SQUARE COEFFICIENTS ASSOCIATED WITH DEPENDENT VARIABLE
!     STARTING WITH RESULT OF SECOND TIRAL
!
                        IF ( nit/=1 ) THEN
                           yav1 = yav1 + rktst
                           y10 = y10 + rktst
                           y11 = y11 + rktst*kint
                        ENDIF
                        kint = rktst
                        IF ( nit/=10 ) CYCLE SPAG_Loop_1_3
!
!     FAILURE TO CONVERGE. REPLACE LOOP END WITH LEAST SQUARES FIT
!
                        nit = nit + 1
                        xav1 = xav/(nit-2)
                        xav = (xav+rktst)/(nit-1)
                        yav1 = yav1/(nit-2)
                        d1 = x12*x10 - x11*x11
                        a11 = (x10*y11-x11*y10)/d1
                        a10 = (x12*y10-x11*y11)/d1
                        rktst = -a10/(a11-1.)
                        WRITE (Nout,99004) Uwm , nit , Floop , nroot , Neiw
99004                   FORMAT (A25,', PK METHOD FIALED TO CONVERGE',/1X,I4,' ITERATIONS ON LOOP',I5,',  FOUND',I5,                 &
                              & ',  ROOTS WANTED',I5,/5X,'LEAST SQUARES FIT APPROXIMATION IMPLEMENTED.')
                        IF ( l39==1 ) WRITE (Nout,99005) xav1 , yav1 , xav , a11 , a10 , rktst
99005                   FORMAT (/5X,'AVG. TRIAL = ',1P,E12.5,',  AGV. RESLT. = ',1P,E12.5,',  NET AVG. = ',1P,E12.5,//9X,'SLOPE = ',&
                              & 1P,E12.5,',    INTERCEPT = ',1P,E12.5,',  VALUE    = ',1P,E12.5)
                     ENDIF
                  ENDIF
!
!     START LOOP OVER
!
                  Z(k) = Z(nl+i-1)
                  Z(k+1) = Z(nl+i)
                  Z(k+2) = rktst
                  Z(k+3) = (1.0/Twopi)*Z(nl+i)
                  IF ( Z(nl+i)/=0.0 ) Z(k+4) = (2.0*Z(nl+i-1))/Z(nl+i)
                  IF ( Z(nl+i)==0.0 ) Z(k+4) = (Bref/(.34657*vel))*Z(nl+i-1)
                  IF ( eigv ) CALL fa1pkv(Z(ima),Z(iv),Z(ib),nrow,Z(k),Z(ima),Bref,Pi,vel,Z(buf1))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     LOGIC ERROR
!
         imhere = 270
      ENDIF
!
      WRITE (Nout,99006) Sfm , imhere , l , j
99006 FORMAT (A25,'. ERROR IN FA1PKE/@',I3,'  L,J=',2I7)
      CALL mesage(-61,0,0)
      RETURN
   ENDDO SPAG_Loop_1_3
!
!     SAVE EIGENVALUES ON BXHH
!
   IF ( istart==0 ) THEN
      istart = 1
      CALL gopen(Bxhh,Z(buf1),1)
      CALL close(Bxhh,2)
   ENDIF
   CALL gopen(Bxhh,Z(buf1),3)
   CALL write(Bxhh,Z(ieigns+1),nroot*5,1)
   IF ( Floop>=Nloop ) THEN
!
!     LAST LOOP BUILD FSAVE
!
      CALL close(Bxhh,1)
      ibuf2 = buf1 - Sysbuf
      CALL gopen(Bxhh,Z(buf1),0)
      CALL gopen(Fsave,Z(ibuf2),0)
      CALL skprec(Fsave,3)
      CALL close(Fsave,2)
      CALL gopen(Fsave,Z(ibuf2),3)
      CALL read(*200,*100,Bxhh,Z(1),ibuf2,1,nwr)
   ELSE
      CALL close(Bxhh,3)
      RETURN
   ENDIF
 100  DO
      CALL write(Fsave,Z(1),nwr,1)
      CALL read(*200,*100,Bxhh,Z(1),ibuf2,1,nwr)
   ENDDO
 200  CALL close(Bxhh,1)
   CALL close(Fsave,1)
   trl(1) = Fsave
   trl(2) = Nloop
   trl(7) = Neiw
   CALL wrttrl(trl)
!
END SUBROUTINE fa1pke
