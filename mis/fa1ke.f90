
SUBROUTINE fa1ke(Scr1,Kfreq,Bref,Rho,Rref,Floop,Nloop)
   IMPLICIT NONE
   COMPLEX Cz(1)
   INTEGER Incr1 , Inn , Iout , Nnn , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /zzzzzz/ Z
   REAL Bref , Kfreq , Rho , Rref
   INTEGER Floop , Nloop , Scr1
   INTEGER buf1 , fsave , i , iam1k , ibuf2 , ick , icp , ifl , ikc , ikp , im , imp , ims , in , ipk , ipm , ism , j , ji , khh ,  &
         & l , mhh , mout , n , n2 , name(2) , ncore , nl , nn , nwr , trl(7)
   REAL k2b2 , rr2
   INTEGER korsz
!
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Cz(1))
!
   DATA name/4HFA1K , 4HE   /
   DATA khh/101/ , mhh/103/ , mout/203/ , fsave/201/
!
!     INITILIZE ON FIRST LOOP
!
   IF ( Floop<=1 ) THEN
      ncore = korsz(Z)
      trl(1) = khh
      CALL rdtrl(trl)
      n = trl(3)
      nn = n*n
      n2 = n*2
      im = nn*2
!
!       LOC     SIZE     USE
!
!     IAM1K     N*N*2    A-1 K
!     IKC       N*N*2    K   SCRATCH FOR ALLMAT
!     IMS       N*N*2    M + Q   LAMBDA FOR ALLMAT
!     IPM       N*N*2    M   HELD IN CORE
!     IPK       N*N*2    K   BETWEEN LOOPS
!
      iam1k = 1
      ikc = iam1k + im
      ims = ikc + im
      icp = ims + im
      IF ( im*5+Sysbuf>ncore ) CALL mesage(-8,0,name)
      ipm = ncore - im
      ipk = ipm - im
      ncore = ipk - 1
      buf1 = ncore - Sysbuf
      Iout = 3
      Inn = 1
      Nnn = n
      Incr1 = 1
!
!     PUT MHH AND KHH IN CORE
!
      ifl = khh
      ji = ipk
      DO
         CALL gopen(ifl,Z(buf1),0)
         DO i = 1 , n
            CALL unpack(*10,ifl,Z(ji))
            GOTO 20
 10         CALL zeroc(Z(ji),n2)
 20         ji = ji + n2
         ENDDO
         CALL close(ifl,1)
         IF ( ifl==mhh ) THEN
!
!     WRITE A HEADER ON MOUT
!
            CALL gopen(mout,Z(buf1),1)
            CALL close(mout,2)
            EXIT
         ELSE
            ifl = mhh
            ji = ipm
         ENDIF
      ENDDO
   ENDIF
!
!              2  2
!     SOLVE   K /B  MHH + (RHO*RREF)/2.0 QHH     KHH
!
   k2b2 = (Kfreq*Kfreq)/(Bref*Bref)
   rr2 = (Rho*Rref)/2.0
   Iout = 3
   Inn = 1
   Nnn = n
   Incr1 = 1
   DO i = 1 , ikc
      Z(i) = 0.0
   ENDDO
   ji = ims
   CALL gopen(Scr1,Z(buf1),0)
   DO i = 1 , n
      CALL unpack(*50,Scr1,Z(ji))
 50   ji = ji + n2
   ENDDO
   CALL close(Scr1,1)
   ick = ikc - 1
   ikp = ipk - 1
   imp = ipm - 1
   ism = ims - 1
   j = nn*2
   DO i = 1 , j
      Z(i+ism) = Z(i+ism)*rr2 + Z(i+imp)*k2b2
      Z(i+ick) = -Z(i+ikp)
   ENDDO
   CALL incore(Z(ims),n,Z(ikc),Z(iam1k),n)
!
!     GET EIGENVALUES FROM ALLMAT
!
   im = ims + n2
   in = im + n2
   l = 0
   CALL allmat(Z(iam1k),Z(ims),Z(ikc),0,0,Z(im),0,Z(in),n,l,0)
!
!     WRITE OUT EIGENVALUES ON MOUT
!
   im = ims/2
   nl = 2*l
   DO i = 1 , l
      IF ( Cz(i+im)/=(0.0,0.0) ) Cz(i+im) = csqrt(Cz(i+im))
      IF ( aimag(Cz(i+im))<0.0 ) Cz(i+im) = -Cz(i+im)
   ENDDO
   CALL gopen(mout,Z(buf1),3)
   CALL write(mout,Z(ims),nl,1)
   IF ( Floop>=Nloop ) THEN
!
!     LAST LOOP BUILD FSAVE
!
      CALL close(mout,1)
      ibuf2 = buf1 - Sysbuf
      CALL gopen(mout,Z(buf1),0)
      CALL gopen(fsave,Z(ibuf2),0)
      CALL skprec(fsave,3)
      CALL close(fsave,2)
      CALL gopen(fsave,Z(ibuf2),3)
      CALL read(*200,*100,mout,Z(1),ibuf2,1,nwr)
   ELSE
      CALL close(mout,3)
      RETURN
   ENDIF
 100  DO
      CALL write(fsave,Z(1),nwr,1)
      CALL read(*200,*100,mout,Z(1),ibuf2,1,nwr)
   ENDDO
 200  CALL close(mout,1)
   CALL close(fsave,1)
   trl(1) = fsave
   trl(2) = Nloop
   trl(7) = l
   CALL wrttrl(trl)
END SUBROUTINE fa1ke