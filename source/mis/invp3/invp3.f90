!*==invp3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invp3(norm1,sub,mtimsu,xtrnsy)
   USE c_dcompx
   USE c_fbsx
   USE c_infbsx
   USE c_invpwx
   USE c_invpxx
   USE c_names
   USE c_packx
   USE c_regean
   USE c_reigkr
   USE c_system
   USE c_trdxx
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   EXTERNAL norm1
   EXTERNAL sub
   EXTERNAL mtimsu
   EXTERNAL xtrnsy
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , deltm1 , ep1 , ep2 , ep3 , gamma , xxx
   REAL(REAL64) :: aln , alnm1 , cn , delta , dtemp , eta , etanm1 , freq , h2n , h2nm1 , lam1 , lam1d , lam2 , lm1nm1 , lm2nm1
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: end , i , ibuf1 , ibuf2 , ibuf3 , icurnt , idum , iend , iep2 , iepcnt , ifile , ii , ii1 , ii2 , iijjkk , ijkk ,     &
            & in1 , intsub , iobuf , ioutpt , iprec , irapid , iter , ix , ixx , ixz , j , jj1 , jj2 , jj3 , jj4 , jj5 , k , kep2 , &
            & khr , klocal , kold , kount , ksave , l16 , navg , ncol , ncol2 , nlns , nlpp , no , nz , sysbuf , t1 , t2 , timeit
   INTEGER , DIMENSION(7) :: mcbvc
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: opt2
   EXTERNAL bckrec , close , fbsinv , gopen , intfbs , invfbs , klock , korsz , makmcb , mesage , pack , page1 , read , sswtch ,    &
          & tmtogo , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     SUBROUTINE INVP3, THE MAIN LINK OF INVPWR, SOLVES FOR THE
!     EIGENVALUES AND EIGENVECTORS OF (K-LAMBDA*M)
!     THIS ROUTINE HANDLES BOTH SINGLE AND DOUBLE PRECISION VERSIONS
!
   !>>>>EQUIVALENCE (Dz(1),Z(1)) , (Ksystm(1),Sysbuf) , (Ksystm(2),Ioutpt) , (Ksystm(9),Nlpp) , (Ksystm(12),Nlns) , (Ksystm(55),Iprec)
   DATA name/4HINVP , 4H3   / , opt2/4HUINV/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         iopen = -10
         CALL sswtch(16,l16)
         khr = 0
         nz = korsz(z)
         ncol = filek(2)
         ncol2 = ncol*iprec
         CALL makmcb(mcbvc,filevc,ncol,2,iprec)
         itu = iprec
         iiu = 1
         jju = ncol
         incru = 1
         itp1 = iprec
         itp2 = iprec
         iip = 1
         jjp = ncol
         incrp = 1
!
!     INITIALIZE
!
         iter = 0
         irapid = 0
         iep2 = 0
         kep2 = 0
         kold = -1
         kount = 0
         spag_nextblock_1 = 2
      CASE (2)
!
         iepcnt = 0
         IF ( switch==1 ) THEN
            filel(1) = sr7fil
            filelt(1) = sr8fil
         ELSE
            filel(1) = sr2fil(1)
            filelt(1) = sr3fil
         ENDIF
!
         DO i = 2 , 7
            lfile(i) = filek(i)
            filel(i) = filek(i)
         ENDDO
         lfile(5) = iprec
         filel(5) = iprec
         lfile(1) = filel(1)
         filelt(7) = iofff
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
            nlns = nlns + 10
            WRITE (ioutpt,99001)
99001       FORMAT (85H0D I A G   1 6   O U T P U T   F R O M    R O U T I N E   I N V P 3   F O L L O W S .,//)
            WRITE (ioutpt,99002) rzero , eps , gamma , a , ep1 , ep2 , ep3
99002       FORMAT (8H0RZERO =,1P,E13.5,4X,5HEPS =,1P,E13.5,4X,7HGAMMA =,1P,E13.5,4X,3HA =,1P,E13.5,/,8H EP1   =,1P,E13.5,4X,       &
                  & 5HEP2 =,1P,E13.5,4X,7HEP3   =,1P,E13.5)
            WRITE (ioutpt,99003)
99003       FORMAT (5H0ITER,5H CFLG,11X,3HSTP,11X,3HSHP,10X,4HLAM1,10X,4HLAM2,11X,3HETA,9X,5HDELTA,4X,1HK,11X,3HH2N,9X,5HLAM1D,/1X, &
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
         ibuf1 = nz - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         iobuf = ibuf3 - sysbuf
         IF ( end>=iobuf ) THEN
!
!     ERROR EXITS
!
            no = -8
            ifile = end - iobuf
            spag_nextblock_1 = 11
         ELSE
!
!     GET ORTHOGONALITY FLAGS FOR PREVIOUS EIGENVECTORS
!
            IF ( iterto/=0 ) THEN
               IF ( northo/=0 ) THEN
                  ifile = dmpfil
                  CALL gopen(dmpfil,z(iobuf),rdrew)
                  CALL read(*60,*80,dmpfil,z(iend),northo,1,idum)
                  CALL close(dmpfil,1)
               ENDIF
            ELSEIF ( northo/=0 ) THEN
               CALL gopen(filevc,z(iobuf),rdrew)
               CALL gopen(filem,z(ibuf1),rdrew)
               DO i = 1 , northo
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        ix = iend + i - 1
                        z(ix) = 1.0
                        CALL unpack(*2,filevc,z(ii1))
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
 2                      j = ncol2
                        IF ( iprec==2 ) THEN
                           SPAG_Loop_2_1: DO
                              dz(j) = 0.0D0
                              j = j - 1
                              IF ( j<=0 ) EXIT SPAG_Loop_2_1
                           ENDDO SPAG_Loop_2_1
                        ELSE
                           SPAG_Loop_2_2: DO
                              z(j) = 0.0
                              j = j - 1
                              IF ( j<=0 ) EXIT SPAG_Loop_2_2
                           ENDDO SPAG_Loop_2_2
                        ENDIF
                        spag_nextblock_2 = 2
                     CASE (2)
                        CALL mtimsu(z(ii1),z(jj1),z(ibuf1))
                        CALL xtrnsy(z(ii1),z(jj1),dtemp)
                        IF ( dtemp<0.0D0 ) z(ix) = -1.0
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               CALL close(filem,rew)
               CALL close(filevc,rew)
            ENDIF
            ifile = filem(1)
            CALL gopen(ifile,z(ibuf3),rdrew)
            ifile = filel(1)
            CALL gopen(ifile,z(ibuf1),rdrew)
!WKBNB 1/95    FILELT NOT NEEDED FOR SMCOMP OR SDCOMP - ONLY DECOMP
            IF ( option==opt2 ) THEN
               ifile = filelt(1)
               CALL gopen(ifile,z(ibuf2),rdrew)
            ENDIF
!WKBNE 1/95
!
!     GENERATE A STARTING VECTOR
!
            IF ( ivect==1 ) THEN
!
!      USE PREVIOUSLY STORED VECTOR AS A STARTING VECTOR
!
               ifile = filevc
               CALL gopen(filevc,z(iobuf),rd)
               CALL bckrec(filevc)
               in1 = 1
               IF ( comflg==1 ) THEN
                  in1 = jj5
                  CALL bckrec(filevc)
               ENDIF
               CALL unpack(*20,filevc,z(in1))
               spag_nextblock_1 = 3
            ELSE
               ksave = k
               k = iabs(ind)
               IF ( iprec==2 ) THEN
                  DO i = 1 , ncol
                     dz(i) = 1.0D0/float((mod(k,13)+1)*(1+5*i/ncol))
                     k = k + 1
                  ENDDO
               ELSE
                  DO i = 1 , ncol
                     z(i) = 1.0/float((mod(k,13)+1)*(1+5*i/ncol))
                     k = k + 1
                  ENDDO
               ENDIF
               k = ksave
               intsub = 1
               spag_nextblock_1 = 6
            ENDIF
         ENDIF
         CYCLE
 20      j = in1 + ncol2
         IF ( iprec==2 ) THEN
            SPAG_Loop_1_3: DO
               j = j - 1
               dz(j) = 0.0D0
               IF ( j<=in1 ) EXIT SPAG_Loop_1_3
            ENDDO SPAG_Loop_1_3
         ELSE
            SPAG_Loop_1_4: DO
               j = j - 1
               z(j) = 0.0
               IF ( j<=in1 ) EXIT SPAG_Loop_1_4
            ENDDO SPAG_Loop_1_4
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( comflg==1 ) THEN
!
!     PICK UP THE LAST ITERATED VECTOR FOR A STARTING VECTOR
!
            CALL unpack(*40,filevc,z)
            spag_nextblock_1 = 4
         ELSE
            CALL bckrec(filevc)
            CALL close(filevc,norew)
            ivect = 0
            intsub = 1
            spag_nextblock_1 = 6
         ENDIF
         CYCLE
 40      j = ncol2
         IF ( iprec==2 ) THEN
            SPAG_Loop_1_5: DO
               dz(j) = 0.0D0
               j = j - 1
               IF ( j<=0 ) EXIT SPAG_Loop_1_5
            ENDDO SPAG_Loop_1_5
         ELSE
            SPAG_Loop_1_6: DO
               z(j) = 0.0
               j = j - 1
               IF ( j<=0 ) EXIT SPAG_Loop_1_6
            ENDDO SPAG_Loop_1_6
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         CALL bckrec(filevc)
         CALL bckrec(filevc)
         CALL close(filevc,norew)
         intsub = 1
         spag_nextblock_1 = 6
      CASE (5)
!
!     SHIFT POINTERS TO VECTORS
!
         ii = ii1
         ii1 = ii2
         ii2 = ii
         ii = jj1
         jj1 = jj2
         jj2 = jj3
         jj3 = ii
         IF ( l16/=0 .AND. khr/=0 ) THEN
            IF ( nlns>=nlpp ) CALL page1
            nlns = nlns + 1
            WRITE (ioutpt,99006) iterto , comflg , lmbda , lambda , lam1 , lam2 , eta , delta , k , h2n , lam1d
         ENDIF
         khr = 1
!
!     SAVE N-1 VECTOR
!
         IF ( switch==0 ) THEN
            ixx = jj5 + ncol2 - 1
            ixz = ii2
            IF ( iprec/=2 ) THEN
               DO i = jj5 , ixx
                  z(i) = z(ixz)
                  ixz = ixz + 1
               ENDDO
            ELSE
               DO i = jj5 , ixx , 2
                  z(i) = z(ixz)
                  z(i+1) = z(ixz+1)
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
         IF ( option/=opt2 ) THEN
            CALL fbsinv(z(jj3),z(ii1),z(iobuf))
         ELSE
            IF ( filel(5)==2 ) CALL invfbs(z(jj3),z(ii1),z(iobuf))
            IF ( filel(5)==1 ) CALL intfbs(z(jj3),z(ii1),z(iobuf))
         ENDIF
         iterto = iterto + 1
         iter = iter + 1
         iepcnt = iepcnt + 1
         CALL tmtogo(ijkk)
         IF ( ijkk<=0 ) THEN
            comflg = 8
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
            intsub = 2
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( northo/=0 ) THEN
!
!     NORMALIZE CURRENT ITERANT WITH RESPECT TO VECTORS FOUND IN THE
!     CURRENT AND PREVIOUS SEARCH REGIONS
!
            CALL mtimsu(z(ii1),z(jj1),z(iobuf))
            ifile = filevc
            CALL gopen(filevc,z(iobuf),rdrew)
            DO i = 1 , northo
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     CALL unpack(*42,filevc,z(jj4))
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 42                  j = jj4 + ncol2
                     IF ( iprec==2 ) THEN
                        SPAG_Loop_2_7: DO
                           j = j - 1
                           dz(j) = 0.0D0
                           IF ( j<=jj4 ) EXIT SPAG_Loop_2_7
                        ENDDO SPAG_Loop_2_7
                     ELSE
                        SPAG_Loop_2_8: DO
                           j = j - 1
                           z(j) = 0.0
                           IF ( j<=jj4 ) EXIT SPAG_Loop_2_8
                        ENDDO SPAG_Loop_2_8
                     ENDIF
                     spag_nextblock_3 = 2
                  CASE (2)
                     CALL xtrnsy(z(jj4),z(jj1),dtemp)
                     ix = iend + i - 1
                     dtemp = -dtemp*z(ix)
                     CALL sub(z(jj4),z(ii1),dtemp,-1.0D0)
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            CALL close(filevc,norew)
         ENDIF
         CALL norm1(z(ii1),cn)
!
!     BEGIN TESTING CONVERGENCE CRITERIA
!
!     COMPUTE F(N)
!
         CALL mtimsu(z(ii1),z(jj1),z(iobuf))
!
!     COMPUTE ALPHA(N)
!
         CALL xtrnsy(z(ii1),z(jj1),aln)
         aln = dsqrt(dabs(aln))
!
!     COMPUTE DELTA U(N)
!
         IF ( intsub==1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sub(z(ii1),z(ii2),1.0D0/aln,1.0D0/alnm1)
!
!     COMPUTE DELTA F(N)
!
         CALL sub(z(jj1),z(jj3),1.0D0/aln,1.0D0/alnm1)
         lam1 = alnm1/(cn*aln)
         IF ( irapid/=1 ) THEN
            CALL xtrnsy(z(ii2),z(jj3),eta)
            eta = dsqrt(dabs(eta))
!
!     RAPID CONVERGENCE TEST
!
            IF ( eta>=a*eps*gamma*dabs(1.0D0+lambda/lam1) ) THEN
               IF ( iter==1 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( etanm1<1.E-6 ) THEN
                  IF ( eta>1.01*etanm1 ) THEN
                     irapid = 1
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!     EPSILON 2 TEST
!
               IF ( iep2/=1 ) THEN
                  IF ( eta==0.D0 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL xtrnsy(z(ii2),z(jj2),dtemp)
                  lam2 = lam1*dtemp/eta**2
                  h2n = (lam2-lm2nm1)/lambda
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
               IF ( dsqrt(delta)>a*eps ) THEN
                  IF ( iter<=3 ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     EPSILON 1 TEST
!
                  IF ( iepcnt>=100 ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( iepcnt<10 ) THEN
                     lam1d = dabs(lam1-lm1nm1)/rzero
                     IF ( lam1d>=dble(ep1) ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
!
!     SHIFT DECISION
!
                  IF ( iepcnt<=5 .OR. delta<=deltm1 ) THEN
                     IF ( dabs(lam2/lam1)>1. ) THEN
                        kep2 = 0
                        CALL klock(t2)
                        timeit = t2 - t1
                        k = dlog(dsqrt(dabs(delta))/(a*eps))/dabs(dlog(dabs(lam2/lam1))) + 1.
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
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  lambda = lambda + lam1
                  k = 0
                  kold = -1
                  kount = 0
                  iepcnt = 0
                  IF ( l16/=0 ) THEN
                     IF ( nlns>=nlpp ) CALL page1
                     nlns = nlns + 3
                     WRITE (ioutpt,99004) lambda
99004                FORMAT (18H0NEW SHIFT POINT =,1P,D14.5,/)
                  ENDIF
!
!     STORE THE LAST VECTOR BEFORE A SHIFT FOR USE AS A STARTING VECTOR
!
                  IF ( switch==1 ) THEN
                     in1 = jj5
                  ELSE
                     in1 = ii1
                  ENDIF
                  ifile = filevc
                  CALL gopen(filevc,z(iobuf),wrt)
                  CALL pack(z(in1),filevc,mcbvc)
                  ivect = 1
                  comflg = 1
!
!     STORE THE CURRENT VECTOR ON THE EIGENVECTOR FILE SO IT CAN BE
!     USED AS A STARTING VECTOR
!
                  CALL pack(z(ii1),filevc,mcbvc)
                  CALL close(filevc,eofnrw)
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               irapid = 1
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     MAKE EPSILON 1 TEST
!
         ELSEIF ( dabs(lam1-lm1nm1)/rzero>=dble(ep1) ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     CONVERGENCE ACHIEVED, NORMALIZE THE EIGENVECTOR
!
         CALL mtimsu(z(ii1),z(jj1),z(iobuf))
         CALL xtrnsy(z(ii1),z(jj1),dtemp)
         ix = iend + northo
         z(ix) = 1.0
         IF ( dtemp<0.0D0 ) z(ix) = -1.0
         dtemp = 1.0D0/dsqrt(dabs(dtemp))
         j = ii1
         klocal = ii1 + ncol2 - 1
         IF ( iprec/=2 ) THEN
            DO i = j , klocal
               z(i) = z(i)*dtemp
            ENDDO
         ELSE
            j = (j+1)/2
            klocal = klocal/2
            DO i = j , klocal
               dz(i) = dz(i)*dtemp
            ENDDO
         ENDIF
!
!     STORE THE EIGENVECTOR AND EIGENVALUE ON THE OUTPUT FILES
!
         lam1 = lam1 + lambda
         IF ( l16/=0 ) THEN
            IF ( nlns>=nlpp ) CALL page1
            nlns = nlns + 3
            freq = (1.0D0/(8.0D0*datan(1.0D0)))*dsqrt(dabs(lam1))
            WRITE (ioutpt,99005) lam1 , freq
99005       FORMAT (32H0CONVERGENCE ACHIEVED AND LAM1 =,1P,D14.5,7X,'FREQ =',1P,D14.5,'HZ',/)
         ENDIF
         ifile = filevc
         CALL gopen(filevc,z(iobuf),wrt)
         CALL pack(z(ii1),filevc,mcbvc)
         CALL close(filevc,eofnrw)
         CALL gopen(filelm,z(iobuf),wrt)
         CALL write(filelm,lam1,2,1)
         CALL close(filelm,eofnrw)
         CALL close(sr7fil,eofnrw)
         CALL close(filel,rew)
         CALL close(filelt,rew)
         CALL close(filem,rew)
         northo = northo + 1
         iep2 = 0
         irapid = 0
         nochng = 0
         IF ( lam1>=0 ) THEN
            IF ( lam1<=lammax ) nopos = nopos + 1
         ELSEIF ( ibuck/=3 ) THEN
            IF ( lam1<=lammax ) nopos = nopos + 1
         ELSE
            IF ( lam1>=lammin ) noneg = noneg + 1
         ENDIF
         IF ( nopos>=ndplus .AND. noneg>=ndmnus ) THEN
            comflg = 6
         ELSEIF ( northo>=ncol-nzero ) THEN
            comflg = 5
         ELSE
            IF ( northo>=3*noest ) THEN
               comflg = 4
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSE
               comflg = 0
               IF ( switch==0 ) THEN
                  ivect = 0
                  IF ( iter<=5 ) GOTO 50
               ELSE
                  switch = 0
                  lambda = lmbda
               ENDIF
               in1 = jj5
               CALL gopen(filevc,z(iobuf),wrt)
               CALL pack(z(in1),filevc,mcbvc)
               CALL close(filevc,eofnrw)
               ivect = 1
            ENDIF
 50         iter = 0
!
!     TEST IF REGION IS EXHAUSTED
!
            IF ( neg<0 ) THEN
!
!     ON NEGATIVE SIDE
!
               IF ( noneg>=ndmnus .OR. lam1<lammin ) THEN
                  comflg = 7
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( neg==0 ) THEN
!
!     NO NEGATIVE REGION
!
               IF ( lam1>lammax ) THEN
                  comflg = 7
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     ON POSITIVE SIDE
!
            ELSEIF ( nopos>=ndplus .OR. lam1>lammax ) THEN
!
!     SWITCH TO NEGATIVE SIDE
!
               comflg = 3
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CONTINUE ON SAME SIDE
!
            IF ( lam1<=lambda+rzero .AND. lam1>=lambda-rzero ) THEN
               ind = iabs(ind)
               ireg = 1
               xxx = lam1 - lambda
               IF ( eps*abs(rzero)>=ep3*abs(xxx) ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ireg/=0 .AND. ind>0 ) THEN
!
               ind = -(ind+1)
               ivect = 0
               IF ( ind==-13 ) ind = -1
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSE
               comflg = 0
               ind = -ind
            ENDIF
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         CALL close(filel,rew)
         CALL close(filelt,rew)
         CALL close(filem,rew)
         CALL wrttrl(mcbvc)
         IF ( l16/=0 ) THEN
            IF ( nlns>=nlpp ) CALL page1
            nlns = nlns + 1
            WRITE (ioutpt,99006) iterto , comflg , lmbda , lambda , lam1 , lam2 , eta , delta , k , h2n , lam1d
         ENDIF
         IF ( northo==0 ) RETURN
!
         CALL gopen(dmpfil,z(iobuf),wrtrew)
         CALL write(dmpfil,z(iend),northo,1)
         CALL close(dmpfil,1)
         RETURN
      CASE (9)
         IF ( northo/=0 ) THEN
            CALL gopen(dmpfil,z(iobuf),wrtrew)
            CALL write(dmpfil,z(iend),northo,1)
            CALL close(dmpfil,1)
         ENDIF
!
         IF ( northo==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL klock(icurnt)
         CALL tmtogo(iijjkk)
         navg = (icurnt-istart)/northo
         IF ( iijjkk>=2*navg ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         comflg = 8
         spag_nextblock_1 = 8
      CASE (10)
!
!     CURRENT SHIFT POINT TOO CLOSE TO THE EIGENVALUE
!
         IF ( comflg/=2 ) THEN
            xxx = lam1 - lambda
            lambda = lambda + sign(.02,xxx)*rzero
            comflg = 2
         ELSE
            comflg = 9
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 60      no = -2
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 80      no = -3
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(no,ifile,name(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99006 FORMAT (2I5,6(1P,D14.5),I5,2(1P,D14.5))
END SUBROUTINE invp3
