
SUBROUTINE invp3(norm1,sub,mtimsu,xtrnsy)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Comflg , Dmpfil , Eofnrw , Filek(7) , Filel(7) , Filelm , Filelt(7) , Filem(7) , Filevc , Ibuck , Idum40(40) , Idummy(27)&
         & , Iip , Iiu , Incrp , Incru , Ind , Iofff , Iopen , Ioutpt , Iprec , Ireg , Istart , Iterto , Itp1 , Itp2 , Itu , Ivect ,&
         & Jjp , Jju , Ksystm(65) , Lfile(7) , Ndmnus , Ndplus , Neg , Nlns , Nlpp , Nochng , Noest , Noneg , Nopos , Norew ,       &
         & Northo , Nzero , Option , Rew , Sr2fil(7) , Sr3fil , Sr7fil , Sr8fil , Switch , Sysbuf , Timed
   REAL Dumx(20) , Eps , Lammax , Lammin , Rd , Rdrew , Rzero , Sr1fil(7) , Sr4fil , Sr5fil , Sr6fil , Wrt , Wrtrew , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda , Lmbda
   COMMON /dcompx/ Dumx , Iofff
   COMMON /fbsx  / Lfile
   COMMON /infbsx/ Filel , Filelt
   COMMON /invpwx/ Filek , Filem , Sr1fil , Sr2fil , Filelm , Filevc , Sr3fil , Sr4fil , Sr5fil , Sr6fil , Sr7fil , Sr8fil ,        &
                 & Dmpfil , Lammin , Lammax , Noest , Ndplus , Ndmnus , Eps , Northo
   COMMON /invpxx/ Lambda , Comflg , Iterto , Timed , Nopos , Rzero , Neg , Nochng , Ind , Lmbda , Switch , Nzero , Noneg , Ivect , &
                 & Ireg , Istart
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /packx / Itp1 , Itp2 , Iip , Jjp , Incrp
   COMMON /regean/ Idum40 , Ibuck
   COMMON /reigkr/ Option
   COMMON /system/ Ksystm
   COMMON /trdxx / Idummy , Iopen
   COMMON /unpakx/ Itu , Iiu , Jju , Incru
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   REAL a , deltm1 , ep1 , ep2 , ep3 , gamma , xxx
   DOUBLE PRECISION aln , alnm1 , cn , delta , dtemp , eta , etanm1 , freq , h2n , h2nm1 , lam1 , lam1d , lam2 , lm1nm1 , lm2nm1
   INTEGER end , i , ibuf1 , ibuf2 , ibuf3 , icurnt , idum , iend , iep2 , iepcnt , ifile , ii , ii1 , ii2 , iijjkk , ijkk , in1 ,  &
         & intsub , iobuf , irapid , iter , ix , ixx , ixz , j , jj1 , jj2 , jj3 , jj4 , jj5 , k , kep2 , khr , klocal , kold ,     &
         & kount , ksave , l16 , mcbvc(7) , name(2) , navg , ncol , ncol2 , no , nz , opt2 , t1 , t2 , timeit
   INTEGER korsz
   EXTERNAL mtimsu , norm1 , sub , xtrnsy
!
! End of declarations
!
!
!     SUBROUTINE INVP3, THE MAIN LINK OF INVPWR, SOLVES FOR THE
!     EIGENVALUES AND EIGENVECTORS OF (K-LAMBDA*M)
!     THIS ROUTINE HANDLES BOTH SINGLE AND DOUBLE PRECISION VERSIONS
!
   EQUIVALENCE (Dz(1),Z(1)) , (Ksystm(1),Sysbuf) , (Ksystm(2),Ioutpt) , (Ksystm(9),Nlpp) , (Ksystm(12),Nlns) , (Ksystm(55),Iprec)
   DATA name/4HINVP , 4H3   / , opt2/4HUINV/
!
!     DEFINITION OF LOCAL PARAMETERS
!
!     ITER     =  NUMBER OF ITERATIONS FROM THE CURRENT SHIFT POINT
!     IRAPID   =  1 = RAPID CONVERGENCE DO ONE MORE ITERATION
!     IEP2     =  1 = EPSILON 2 TEST FAILED
!     A        =  CONVERGENCE SAFETY FACTOR
!     EP1      =  EPSILON FOR DETERMINING IF IT IS POSSIBLE TO SHIFT
!     EP2      =  EPSILON TO DETERMINE IF LAMBDA 2 IS VALID
!     EP3      =  EPSILON TO DETERMINE IF EIGENVALUE IS TOO CLOSE TO SHI
!     GAMMA    =  CLOSE ROOT CRITERION
!     II1      =  POINTER TO U(N)
!     II2      =  POINTER TO U(N-1) OR DELTA U(N)
!     JJ1      =  POINTER TO F(N)
!     JJ2      =  POINTER TO DELTA F(N-1)
!     JJ3      =  POINTER TO F(N-1) OR DELTA F(N)
!     ALN      =  ALPHA(N)
!     ALNM1    =  ALPHA(N-1)
!     CN       =  NORMALIZATION FACTOR FOR LAST EIGENVECTOR
!
   Iopen = -10
   CALL sswtch(16,l16)
   khr = 0
   nz = korsz(Z)
   ncol = Filek(2)
   ncol2 = ncol*Iprec
   CALL makmcb(mcbvc,Filevc,ncol,2,Iprec)
   Itu = Iprec
   Iiu = 1
   Jju = ncol
   Incru = 1
   Itp1 = Iprec
   Itp2 = Iprec
   Iip = 1
   Jjp = ncol
   Incrp = 1
!
!     INITIALIZE
!
   iter = 0
   irapid = 0
   iep2 = 0
   kep2 = 0
   kold = -1
   kount = 0
!
 100  iepcnt = 0
   IF ( Switch==1 ) THEN
      Filel(1) = Sr7fil
      Filelt(1) = Sr8fil
   ELSE
      Filel(1) = Sr2fil(1)
      Filelt(1) = Sr3fil
   ENDIF
!
   DO i = 2 , 7
      Lfile(i) = Filek(i)
      Filel(i) = Filek(i)
   ENDDO
   Lfile(5) = Iprec
   Filel(5) = Iprec
   Lfile(1) = Filel(1)
   Filelt(7) = Iofff
!
!     SET CONVERGENCE CRITERIA
!
   a = .1
   ep1 = .003
   ep2 = .00001
   ep2 = .02
   ep3 = .05
   gamma = .01
   IF ( l16/=0 .AND. khr==0 ) THEN
      CALL page1
      Nlns = Nlns + 10
      WRITE (Ioutpt,99001)
99001 FORMAT (85H0D I A G   1 6   O U T P U T   F R O M    R O U T I N E   I N V P 3   F O L L O W S .,//)
      WRITE (Ioutpt,99002) Rzero , Eps , gamma , a , ep1 , ep2 , ep3
99002 FORMAT (8H0RZERO =,1P,E13.5,4X,5HEPS =,1P,E13.5,4X,7HGAMMA =,1P,E13.5,4X,3HA =,1P,E13.5,/,8H EP1   =,1P,E13.5,4X,5HEP2 =,1P,  &
            & E13.5,4X,7HEP3   =,1P,E13.5)
      WRITE (Ioutpt,99003)
99003 FORMAT (5H0ITER,5H CFLG,11X,3HSTP,11X,3HSHP,10X,4HLAM1,10X,4HLAM2,11X,3HETA,9X,5HDELTA,4X,1HK,11X,3HH2N,9X,5HLAM1D,/1X,       &
            & 126(1H=))
   ENDIF
!
!     INITIALIZE POINTERS TO VECTORS
!
   ii1 = 1
   ii2 = ii1 + ncol2
   jj1 = ii2 + ncol2
   jj2 = jj1 + ncol2
   jj3 = jj2 + ncol2
   jj4 = jj3 + ncol2
   jj5 = jj4 + ncol2
   end = jj5 + ncol2
   iend = end
   end = iend + ncol
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   iobuf = ibuf3 - Sysbuf
   IF ( end>=iobuf ) THEN
!
!     ERROR EXITS
!
      no = -8
      ifile = end - iobuf
      GOTO 1400
   ELSE
!
!     GET ORTHOGONALITY FLAGS FOR PREVIOUS EIGENVECTORS
!
      IF ( Iterto/=0 ) THEN
         IF ( Northo/=0 ) THEN
            ifile = Dmpfil
            CALL gopen(Dmpfil,Z(iobuf),Rdrew)
            CALL read(*1200,*1300,Dmpfil,Z(iend),Northo,1,idum)
            CALL close(Dmpfil,1)
         ENDIF
      ELSEIF ( Northo/=0 ) THEN
         CALL gopen(Filevc,Z(iobuf),Rdrew)
         CALL gopen(Filem,Z(ibuf1),Rdrew)
         DO i = 1 , Northo
            ix = iend + i - 1
            Z(ix) = 1.0
            CALL unpack(*110,Filevc,Z(ii1))
            GOTO 120
 110        j = ncol2
            IF ( Iprec==2 ) THEN
               DO
                  Dz(j) = 0.0D0
                  j = j - 1
                  IF ( j<=0 ) EXIT
               ENDDO
            ELSE
               DO
                  Z(j) = 0.0
                  j = j - 1
                  IF ( j<=0 ) EXIT
               ENDDO
            ENDIF
 120        CALL mtimsu(Z(ii1),Z(jj1),Z(ibuf1))
            CALL xtrnsy(Z(ii1),Z(jj1),dtemp)
            IF ( dtemp<0.0D0 ) Z(ix) = -1.0
         ENDDO
         CALL close(Filem,Rew)
         CALL close(Filevc,Rew)
      ENDIF
      ifile = Filem(1)
      CALL gopen(ifile,Z(ibuf3),Rdrew)
      ifile = Filel(1)
      CALL gopen(ifile,Z(ibuf1),Rdrew)
!WKBNB 1/95    FILELT NOT NEEDED FOR SMCOMP OR SDCOMP - ONLY DECOMP
      IF ( Option==opt2 ) THEN
         ifile = Filelt(1)
         CALL gopen(ifile,Z(ibuf2),Rdrew)
      ENDIF
!WKBNE 1/95
!
!     GENERATE A STARTING VECTOR
!
      IF ( Ivect==1 ) THEN
!
!      USE PREVIOUSLY STORED VECTOR AS A STARTING VECTOR
!
         ifile = Filevc
         CALL gopen(Filevc,Z(iobuf),Rd)
         CALL bckrec(Filevc)
         in1 = 1
         IF ( Comflg==1 ) THEN
            in1 = jj5
            CALL bckrec(Filevc)
         ENDIF
         CALL unpack(*200,Filevc,Z(in1))
         GOTO 300
      ELSE
         ksave = k
         k = iabs(Ind)
         IF ( Iprec==2 ) THEN
            DO i = 1 , ncol
               Dz(i) = 1.0D0/float((mod(k,13)+1)*(1+5*i/ncol))
               k = k + 1
            ENDDO
         ELSE
            DO i = 1 , ncol
               Z(i) = 1.0/float((mod(k,13)+1)*(1+5*i/ncol))
               k = k + 1
            ENDDO
         ENDIF
         k = ksave
         intsub = 1
         GOTO 700
      ENDIF
   ENDIF
 200  j = in1 + ncol2
   IF ( Iprec==2 ) THEN
      DO
         j = j - 1
         Dz(j) = 0.0D0
         IF ( j<=in1 ) EXIT
      ENDDO
   ELSE
      DO
         j = j - 1
         Z(j) = 0.0
         IF ( j<=in1 ) EXIT
      ENDDO
   ENDIF
 300  IF ( Comflg==1 ) THEN
!
!     PICK UP THE LAST ITERATED VECTOR FOR A STARTING VECTOR
!
      CALL unpack(*400,Filevc,Z)
      GOTO 500
   ELSE
      CALL bckrec(Filevc)
      CALL close(Filevc,Norew)
      Ivect = 0
      intsub = 1
      GOTO 700
   ENDIF
 400  j = ncol2
   IF ( Iprec==2 ) THEN
      DO
         Dz(j) = 0.0D0
         j = j - 1
         IF ( j<=0 ) EXIT
      ENDDO
   ELSE
      DO
         Z(j) = 0.0
         j = j - 1
         IF ( j<=0 ) EXIT
      ENDDO
   ENDIF
 500  CALL bckrec(Filevc)
   CALL bckrec(Filevc)
   CALL close(Filevc,Norew)
   intsub = 1
   GOTO 700
!
!     SHIFT POINTERS TO VECTORS
!
 600  ii = ii1
   ii1 = ii2
   ii2 = ii
   ii = jj1
   jj1 = jj2
   jj2 = jj3
   jj3 = ii
   IF ( l16/=0 .AND. khr/=0 ) THEN
      IF ( Nlns>=Nlpp ) CALL page1
      Nlns = Nlns + 1
      WRITE (Ioutpt,99006) Iterto , Comflg , Lmbda , Lambda , lam1 , lam2 , eta , delta , k , h2n , lam1d
   ENDIF
   khr = 1
!
!     SAVE N-1 VECTOR
!
   IF ( Switch==0 ) THEN
      ixx = jj5 + ncol2 - 1
      ixz = ii2
      IF ( Iprec/=2 ) THEN
         DO i = jj5 , ixx
            Z(i) = Z(ixz)
            ixz = ixz + 1
         ENDDO
      ELSE
         DO i = jj5 , ixx , 2
            Z(i) = Z(ixz)
            Z(i+1) = Z(ixz+1)
            ixz = ixz + 2
         ENDDO
      ENDIF
   ENDIF
!
!     SHIFT PARAMETERS
!
   alnm1 = aln
   etanm1 = eta
   h2nm1 = h2n
   lm1nm1 = lam1
   lm2nm1 = lam2
!
!     CALL INVFBS TO MAKE ONE ITERATION
!
   CALL klock(t1)
   IF ( Option/=opt2 ) THEN
      CALL fbsinv(Z(jj3),Z(ii1),Z(iobuf))
   ELSE
      IF ( Filel(5)==2 ) CALL invfbs(Z(jj3),Z(ii1),Z(iobuf))
      IF ( Filel(5)==1 ) CALL intfbs(Z(jj3),Z(ii1),Z(iobuf))
   ENDIF
   Iterto = Iterto + 1
   iter = iter + 1
   iepcnt = iepcnt + 1
   CALL tmtogo(ijkk)
   IF ( ijkk<=0 ) THEN
      Comflg = 8
      GOTO 900
   ELSE
      intsub = 2
   ENDIF
 700  IF ( Northo/=0 ) THEN
!
!     NORMALIZE CURRENT ITERANT WITH RESPECT TO VECTORS FOUND IN THE
!     CURRENT AND PREVIOUS SEARCH REGIONS
!
      CALL mtimsu(Z(ii1),Z(jj1),Z(iobuf))
      ifile = Filevc
      CALL gopen(Filevc,Z(iobuf),Rdrew)
      DO i = 1 , Northo
         CALL unpack(*720,Filevc,Z(jj4))
         GOTO 740
 720     j = jj4 + ncol2
         IF ( Iprec==2 ) THEN
            DO
               j = j - 1
               Dz(j) = 0.0D0
               IF ( j<=jj4 ) EXIT
            ENDDO
         ELSE
            DO
               j = j - 1
               Z(j) = 0.0
               IF ( j<=jj4 ) EXIT
            ENDDO
         ENDIF
 740     CALL xtrnsy(Z(jj4),Z(jj1),dtemp)
         ix = iend + i - 1
         dtemp = -dtemp*Z(ix)
         CALL sub(Z(jj4),Z(ii1),dtemp,-1.0D0)
      ENDDO
      CALL close(Filevc,Norew)
   ENDIF
   CALL norm1(Z(ii1),cn)
!
!     BEGIN TESTING CONVERGENCE CRITERIA
!
!     COMPUTE F(N)
!
   CALL mtimsu(Z(ii1),Z(jj1),Z(iobuf))
!
!     COMPUTE ALPHA(N)
!
   CALL xtrnsy(Z(ii1),Z(jj1),aln)
   aln = dsqrt(dabs(aln))
!
!     COMPUTE DELTA U(N)
!
   IF ( intsub==1 ) GOTO 600
   CALL sub(Z(ii1),Z(ii2),1.0D0/aln,1.0D0/alnm1)
!
!     COMPUTE DELTA F(N)
!
   CALL sub(Z(jj1),Z(jj3),1.0D0/aln,1.0D0/alnm1)
   lam1 = alnm1/(cn*aln)
   IF ( irapid/=1 ) THEN
      CALL xtrnsy(Z(ii2),Z(jj3),eta)
      eta = dsqrt(dabs(eta))
!
!     RAPID CONVERGENCE TEST
!
      IF ( eta>=a*Eps*gamma*dabs(1.0D0+Lambda/lam1) ) THEN
         IF ( iter==1 ) GOTO 600
         IF ( etanm1<1.E-6 ) THEN
            IF ( eta>1.01*etanm1 ) THEN
               irapid = 1
               GOTO 600
            ENDIF
         ENDIF
!
!     EPSILON 2 TEST
!
         IF ( iep2/=1 ) THEN
            IF ( eta==0.D0 ) GOTO 800
            CALL xtrnsy(Z(ii2),Z(jj2),dtemp)
            lam2 = lam1*dtemp/eta**2
            h2n = (lam2-lm2nm1)/Lambda
!WKBI 3/94 THE FOLLOWING LINE ADDED TO GET AROUND AN APPARENT COMPILER BUG ON
!          ULTRIX
            IF ( eta==0.D0 ) PRINT * , ' invp3,lam1,dtemp,eta=' , lam1 , dtemp , eta
            IF ( iter>=4 ) THEN
               IF ( ep2>dabs(h2n) .AND. dabs(h2n)>dabs(h2nm1) ) THEN
                  iep2 = 1
                  lam2 = lm2nm1
               ENDIF
            ENDIF
         ENDIF
         deltm1 = delta
         delta = eta**2/dmin1((1.0D0-lam2/lam1)**2,10.0D0)
!
!     VECTOR CONVERGENCE TEST
!
         IF ( dsqrt(delta)>a*Eps ) THEN
            IF ( iter<=3 ) GOTO 600
!
!     EPSILON 1 TEST
!
            IF ( iepcnt>=100 ) GOTO 1100
            IF ( iepcnt<10 ) THEN
               lam1d = dabs(lam1-lm1nm1)/Rzero
               IF ( lam1d>=dble(ep1) ) GOTO 600
            ENDIF
!
!     SHIFT DECISION
!
            IF ( iepcnt<=5 .OR. delta<=deltm1 ) THEN
               IF ( dabs(lam2/lam1)>1. ) THEN
                  kep2 = 0
                  CALL klock(t2)
                  timeit = t2 - t1
                  k = dlog(dsqrt(dabs(delta))/(a*Eps))/dabs(dlog(dabs(lam2/lam1))) + 1.
                  k = min0(k,9999)
                  IF ( k/=kold ) THEN
                     kold = k
                     kount = 0
                  ELSE
                     kount = kount + 1
                     IF ( kount>=6 ) THEN
                     ENDIF
                  ENDIF
               ELSEIF ( kep2>=0 ) THEN
                  kep2 = -1
                  GOTO 600
               ENDIF
            ENDIF
            Lambda = Lambda + lam1
            k = 0
            kold = -1
            kount = 0
            iepcnt = 0
            IF ( l16/=0 ) THEN
               IF ( Nlns>=Nlpp ) CALL page1
               Nlns = Nlns + 3
               WRITE (Ioutpt,99004) Lambda
99004          FORMAT (18H0NEW SHIFT POINT =,1P,D14.5,/)
            ENDIF
!
!     STORE THE LAST VECTOR BEFORE A SHIFT FOR USE AS A STARTING VECTOR
!
            IF ( Switch==1 ) THEN
               in1 = jj5
            ELSE
               in1 = ii1
            ENDIF
            ifile = Filevc
            CALL gopen(Filevc,Z(iobuf),Wrt)
            CALL pack(Z(in1),Filevc,mcbvc)
            Ivect = 1
            Comflg = 1
!
!     STORE THE CURRENT VECTOR ON THE EIGENVECTOR FILE SO IT CAN BE
!     USED AS A STARTING VECTOR
!
            CALL pack(Z(ii1),Filevc,mcbvc)
            CALL close(Filevc,Eofnrw)
            GOTO 900
         ENDIF
      ELSE
         irapid = 1
         GOTO 600
      ENDIF
!
!     MAKE EPSILON 1 TEST
!
   ELSEIF ( dabs(lam1-lm1nm1)/Rzero>=dble(ep1) ) THEN
      GOTO 600
   ENDIF
!
!     CONVERGENCE ACHIEVED, NORMALIZE THE EIGENVECTOR
!
 800  CALL mtimsu(Z(ii1),Z(jj1),Z(iobuf))
   CALL xtrnsy(Z(ii1),Z(jj1),dtemp)
   ix = iend + Northo
   Z(ix) = 1.0
   IF ( dtemp<0.0D0 ) Z(ix) = -1.0
   dtemp = 1.0D0/dsqrt(dabs(dtemp))
   j = ii1
   klocal = ii1 + ncol2 - 1
   IF ( Iprec/=2 ) THEN
      DO i = j , klocal
         Z(i) = Z(i)*dtemp
      ENDDO
   ELSE
      j = (j+1)/2
      klocal = klocal/2
      DO i = j , klocal
         Dz(i) = Dz(i)*dtemp
      ENDDO
   ENDIF
!
!     STORE THE EIGENVECTOR AND EIGENVALUE ON THE OUTPUT FILES
!
   lam1 = lam1 + Lambda
   IF ( l16/=0 ) THEN
      IF ( Nlns>=Nlpp ) CALL page1
      Nlns = Nlns + 3
      freq = (1.0D0/(8.0D0*datan(1.0D0)))*dsqrt(dabs(lam1))
      WRITE (Ioutpt,99005) lam1 , freq
99005 FORMAT (32H0CONVERGENCE ACHIEVED AND LAM1 =,1P,D14.5,7X,'FREQ =',1P,D14.5,'HZ',/)
   ENDIF
   ifile = Filevc
   CALL gopen(Filevc,Z(iobuf),Wrt)
   CALL pack(Z(ii1),Filevc,mcbvc)
   CALL close(Filevc,Eofnrw)
   CALL gopen(Filelm,Z(iobuf),Wrt)
   CALL write(Filelm,lam1,2,1)
   CALL close(Filelm,Eofnrw)
   CALL close(Sr7fil,Eofnrw)
   CALL close(Filel,Rew)
   CALL close(Filelt,Rew)
   CALL close(Filem,Rew)
   Northo = Northo + 1
   iep2 = 0
   irapid = 0
   Nochng = 0
   IF ( lam1>=0 ) THEN
      IF ( lam1<=Lammax ) Nopos = Nopos + 1
   ELSEIF ( Ibuck/=3 ) THEN
      IF ( lam1<=Lammax ) Nopos = Nopos + 1
   ELSE
      IF ( lam1>=Lammin ) Noneg = Noneg + 1
   ENDIF
   IF ( Nopos>=Ndplus .AND. Noneg>=Ndmnus ) THEN
      Comflg = 6
   ELSEIF ( Northo>=ncol-Nzero ) THEN
      Comflg = 5
   ELSE
      IF ( Northo>=3*Noest ) THEN
         Comflg = 4
         GOTO 900
      ELSE
         Comflg = 0
         IF ( Switch==0 ) THEN
            Ivect = 0
            IF ( iter<=5 ) GOTO 850
         ELSE
            Switch = 0
            Lambda = Lmbda
         ENDIF
         in1 = jj5
         CALL gopen(Filevc,Z(iobuf),Wrt)
         CALL pack(Z(in1),Filevc,mcbvc)
         CALL close(Filevc,Eofnrw)
         Ivect = 1
      ENDIF
 850  iter = 0
!
!     TEST IF REGION IS EXHAUSTED
!
      IF ( Neg<0 ) THEN
!
!     ON NEGATIVE SIDE
!
         IF ( Noneg>=Ndmnus .OR. lam1<Lammin ) THEN
            Comflg = 7
            GOTO 900
         ENDIF
      ELSEIF ( Neg==0 ) THEN
!
!     NO NEGATIVE REGION
!
         IF ( lam1>Lammax ) THEN
            Comflg = 7
            GOTO 900
         ENDIF
!
!     ON POSITIVE SIDE
!
      ELSEIF ( Nopos>=Ndplus .OR. lam1>Lammax ) THEN
!
!     SWITCH TO NEGATIVE SIDE
!
         Comflg = 3
         GOTO 900
      ENDIF
!
!     CONTINUE ON SAME SIDE
!
      IF ( lam1<=Lambda+Rzero .AND. lam1>=Lambda-Rzero ) THEN
         Ind = iabs(Ind)
         Ireg = 1
         xxx = lam1 - Lambda
         IF ( Eps*abs(Rzero)<ep3*abs(xxx) ) GOTO 1000
         GOTO 1100
      ELSEIF ( Ireg/=0 .AND. Ind>0 ) THEN
!
         Ind = -(Ind+1)
         Ivect = 0
         IF ( Ind==-13 ) Ind = -1
         GOTO 1000
      ELSE
         Comflg = 0
         Ind = -Ind
      ENDIF
   ENDIF
 900  CALL close(Filel,Rew)
   CALL close(Filelt,Rew)
   CALL close(Filem,Rew)
   CALL wrttrl(mcbvc)
   IF ( l16/=0 ) THEN
      IF ( Nlns>=Nlpp ) CALL page1
      Nlns = Nlns + 1
      WRITE (Ioutpt,99006) Iterto , Comflg , Lmbda , Lambda , lam1 , lam2 , eta , delta , k , h2n , lam1d
   ENDIF
   IF ( Northo==0 ) RETURN
!
   CALL gopen(Dmpfil,Z(iobuf),Wrtrew)
   CALL write(Dmpfil,Z(iend),Northo,1)
   CALL close(Dmpfil,1)
   RETURN
 1000 IF ( Northo/=0 ) THEN
      CALL gopen(Dmpfil,Z(iobuf),Wrtrew)
      CALL write(Dmpfil,Z(iend),Northo,1)
      CALL close(Dmpfil,1)
   ENDIF
!
   IF ( Northo==0 ) GOTO 100
   CALL klock(icurnt)
   CALL tmtogo(iijjkk)
   navg = (icurnt-Istart)/Northo
   IF ( iijjkk>=2*navg ) GOTO 100
   Comflg = 8
   GOTO 900
!
!     CURRENT SHIFT POINT TOO CLOSE TO THE EIGENVALUE
!
 1100 IF ( Comflg/=2 ) THEN
      xxx = lam1 - Lambda
      Lambda = Lambda + sign(.02,xxx)*Rzero
      Comflg = 2
   ELSE
      Comflg = 9
   ENDIF
   GOTO 900
 1200 no = -2
   GOTO 1400
 1300 no = -3
 1400 CALL mesage(no,ifile,name(1))
99006 FORMAT (2I5,6(1P,D14.5),I5,2(1P,D14.5))
END SUBROUTINE invp3
