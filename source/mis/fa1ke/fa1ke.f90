!*==fa1ke.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa1ke(Scr1,Kfreq,Bref,Rho,Rref,Floop,Nloop)
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Scr1
   REAL :: Kfreq
   REAL :: Bref
   REAL :: Rho
   REAL :: Rref
   INTEGER :: Floop
   INTEGER :: Nloop
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , i , iam1k , ibuf2 , ick , icp , ifl , ikc , ikp , im , imp , ims , in , ipk , ipm , ism , j , ji , l , n , n2 ,&
            & ncore , nl , nn , nwr
   COMPLEX , DIMENSION(1) :: cz
   INTEGER , SAVE :: fsave , khh , mhh , mout
   REAL :: k2b2 , rr2
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL allmat , close , gopen , incore , korsz , mesage , rdtrl , read , skprec , unpack , write , wrttrl , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
      ncore = korsz(z)
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
      IF ( im*5+sysbuf>ncore ) CALL mesage(-8,0,name)
      ipm = ncore - im
      ipk = ipm - im
      ncore = ipk - 1
      buf1 = ncore - sysbuf
      iout = 3
      inn = 1
      nnn = n
      incr1 = 1
!
!     PUT MHH AND KHH IN CORE
!
      ifl = khh
      ji = ipk
      SPAG_Loop_1_1: DO
         CALL gopen(ifl,z(buf1),0)
         DO i = 1 , n
            spag_nextblock_1 = 1
            SPAG_DispatchLoop_1: DO
               SELECT CASE (spag_nextblock_1)
               CASE (1)
                  CALL unpack(*2,ifl,z(ji))
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 2                CALL zeroc(z(ji),n2)
                  spag_nextblock_1 = 2
               CASE (2)
                  ji = ji + n2
                  EXIT SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_1
         ENDDO
         CALL close(ifl,1)
         IF ( ifl==mhh ) THEN
!
!     WRITE A HEADER ON MOUT
!
            CALL gopen(mout,z(buf1),1)
            CALL close(mout,2)
            EXIT SPAG_Loop_1_1
         ELSE
            ifl = mhh
            ji = ipm
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
!
!              2  2
!     SOLVE   K /B  MHH + (RHO*RREF)/2.0 QHH     KHH
!
   k2b2 = (Kfreq*Kfreq)/(Bref*Bref)
   rr2 = (Rho*Rref)/2.0
   iout = 3
   inn = 1
   nnn = n
   incr1 = 1
   DO i = 1 , ikc
      z(i) = 0.0
   ENDDO
   ji = ims
   CALL gopen(Scr1,z(buf1),0)
   DO i = 1 , n
      CALL unpack(*50,Scr1,z(ji))
 50   ji = ji + n2
   ENDDO
   CALL close(Scr1,1)
   ick = ikc - 1
   ikp = ipk - 1
   imp = ipm - 1
   ism = ims - 1
   j = nn*2
   DO i = 1 , j
      z(i+ism) = z(i+ism)*rr2 + z(i+imp)*k2b2
      z(i+ick) = -z(i+ikp)
   ENDDO
   CALL incore(z(ims),n,z(ikc),z(iam1k),n)
!
!     GET EIGENVALUES FROM ALLMAT
!
   im = ims + n2
   in = im + n2
   l = 0
   CALL allmat(z(iam1k),z(ims),z(ikc),0,0,z(im),0,z(in),n,l,0)
!
!     WRITE OUT EIGENVALUES ON MOUT
!
   im = ims/2
   nl = 2*l
   DO i = 1 , l
      IF ( cz(i+im)/=(0.0,0.0) ) cz(i+im) = csqrt(cz(i+im))
      IF ( aimag(cz(i+im))<0.0 ) cz(i+im) = -cz(i+im)
   ENDDO
   CALL gopen(mout,z(buf1),3)
   CALL write(mout,z(ims),nl,1)
   IF ( Floop>=Nloop ) THEN
!
!     LAST LOOP BUILD FSAVE
!
      CALL close(mout,1)
      ibuf2 = buf1 - sysbuf
      CALL gopen(mout,z(buf1),0)
      CALL gopen(fsave,z(ibuf2),0)
      CALL skprec(fsave,3)
      CALL close(fsave,2)
      CALL gopen(fsave,z(ibuf2),3)
      CALL read(*200,*100,mout,z(1),ibuf2,1,nwr)
   ELSE
      CALL close(mout,3)
      RETURN
   ENDIF
 100  DO
      CALL write(fsave,z(1),nwr,1)
      CALL read(*200,*100,mout,z(1),ibuf2,1,nwr)
   ENDDO
 200  CALL close(mout,1)
   CALL close(fsave,1)
   trl(1) = fsave
   trl(2) = Nloop
   trl(7) = l
   CALL wrttrl(trl)
END SUBROUTINE fa1ke
