!*==cinvp3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvp3
   USE c_cdcmpx
   USE c_cinfbx
   USE c_cinvpx
   USE c_cinvxx
   USE c_names
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , aaa , ep1 , ep2 , ep3 , gamma
   REAL(REAL64) , DIMENSION(2) :: aln , alnm1 , cn , con1 , con2 , delta , eta , etanm1 , h2n , h2nm1 , lam2 , lm1nm1 , lm2nm1 , xyz
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: end , i , ibuf1 , ibuf2 , ibuf3 , icurnt , idiag , iep2 , iepcnt , ifile , ii , ii1 , ii2 , iijjkk , in1 , inu ,      &
            & iobuf , ir1 , irapid , iu , ixx , ixz , j , jj1 , jj2 , jj3 , jj5 , k , kk1 , kk2 , kk3 , kk4 , kk5 , kk6 , kkkk ,    &
            & kx , ll1 , ll2 , navg , ncol , ncol2 , ncol4 , ncount , nz , nzz , s11fil , sr1fil , sr2fil , sr3fil , sr4fil ,       &
            & sr8fil , sr9fil , t1 , t2 , timeit
   INTEGER , DIMENSION(7) :: file
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL(REAL64) , DIMENSION(2) , SAVE :: plus1
   EXTERNAL bckrec , cdivid , cinfbs , close , cmtimu , cnorm , cnorm1 , csqrtx , csub , cxtrny , fread , gopen , klock , korsz ,   &
          & mesage , ortho , skprec , sswtch , tmtogo , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SUBROUTINE CINVP3, THE MAIN LINK OF CINVPR, SOLVES FOR THE
!     EIGENVALUES AND EIGENVECTORS OF (LAMBDA**2*M + LAMBDA*B*K)
!
!     TYPE DECLARATIONS
!
   !>>>>EQUIVALENCE (Filek(2),Ncol) , (Scrfil(1),Sr1fil) , (Scrfil(2),Sr2fil) , (Scrfil(3),Sr3fil) , (Scrfil(4),Sr4fil) ,                &
!>>>>    & (Scrfil(8),Sr8fil) , (Scrfil(9),Sr9fil) , (Dz(1),Z(1)) , (Scrfil(11),S11fil)
   DATA name/4HCINV , 4HP3  /
   DATA plus1/ + 1.D0 , 0.D0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINITION OF LOCAL PARAMETERS
!
!     ITER     =
!     IRAPID   =
!     IEP2     =
!     NCOUNT   =
!     IEPCNT   =
!     SWITCH   =
!     A        =
!     EP1      =
!     EP2      =
!     EP3      =
!     GAMMA    =
!     II1      =    POINTER TO U(N)
!     II2      =    POINTER TO U(N-1) OR DELTA U(N)
!     JJ1      =    POINTER TO F(N)
!     JJ2      =    POINTER TO DELTA F(N-1)
!     JJ3      =    POINTER TO F(N-1) OR DELTA F(N)
!     JJ4      =
!     JJ5      =
!     KK1      =    POINTER TO V(N)
!     KK2      =    POINTER TO V(N-1)
!
         timeit = 0
         nz = korsz(z)
         ncol2 = ncol + ncol
         ncol4 = ncol2 + ncol2
!
!     INITIALIZE
!
         cn(1) = 0.0D0
         cn(2) = 0.0D0
         xyz(1) = 0.0D0
         xyz(2) = 0.0D0
         h2n(1) = 0.0D0
         h2n(2) = 0.0D0
         lam2(1) = 0.0D0
         lam2(2) = 0.0D0
         lam1(1) = 0.0D0
         lam1(2) = 0.0D0
         iter = 0
         CALL klock(t1)
         irapid = 0
         iep2 = 0
         SPAG_Loop_1_1: DO
            ncount = 0
            iepcnt = 0
            IF ( switch==1 ) THEN
               filel(1) = sr8fil
               fileu(1) = sr9fil
            ELSE
               filel(1) = sr3fil
               fileu(1) = sr4fil
            ENDIF
            filel(5) = cdp
            filel(3) = filek(3)
            fileu(7) = iofff
            file(4) = sqr
            file(5) = cdp
!
!     SET CONVERGENCE CRITERIA
!
            a = .1
            CALL sswtch(12,idiag)
            ep1 = .001
            ep2 = .02
            ep3 = .05
            gamma = .01
!
!     INITILIZE POINTERS TO VECTORS
!
            ii1 = 1
            ii2 = ii1 + ncol2
            jj1 = ii2 + ncol2
            jj2 = jj1 + ncol2
            jj3 = jj2 + ncol2
            jj5 = jj3 + ncol2
            kk1 = jj5 + ncol2
            kk2 = kk1 + ncol2
            kk3 = kk2 + ncol2
            kk4 = kk3 + ncol2
            kk5 = kk4 + ncol2
            kk6 = kk5 + ncol2
            ll1 = kk6 + ncol2
            ll2 = ll1 + ncol2
            end = (ll2+ncol2)*2
            iobuf = nz - sysbuf + 1
            ibuf1 = iobuf - sysbuf
            ibuf2 = ibuf1 - sysbuf
            ibuf3 = ibuf2 - sysbuf
!     IBUF4 = IBUF3 - SYSBUF
!     IBUF5 = IBUF4 - SYSBUF
!     IBUF6 = IBUF5 - SYSBUF
!     IF (END .GE. IBUF6) GO TO 240
!     NZZ = IBUF6 - END
            IF ( end>=ibuf3 ) THEN
!
!     ERROR EXITS
!
               j = -8
! 610 J = -1
               CALL mesage(j,ifile,name)
               RETURN
            ELSE
               nzz = ibuf3 - end
!     IFILE = FILEL(1)
!     CALL OPEN (*610,FILEL,Z(IBUF4),0)
!     IFILE = FILEU(1)
!     CALL OPEN (*610,FILEU,Z(IBUF5),0)
!     IFILE = FILEM(1)
!     CALL OPEN (*610,FILEM,Z(IBUF6),0)
!
!     GENERATE A STARTING VECTOR
!
!     FORM U0
!
               IF ( left==1 ) THEN
!
!     RETURN TO MAIN DRIVER TO COMPUTE THE LEFT EIGENVECTOR
!
!
!     ENTRY POINT UPON RETURNING FROM OBTAINING THE LEFT VECTOR
!
                  left = 0
                  IF ( nodes<=noroot ) THEN
!
!     ALL ROOTS IN PROBLEM FOUND
!
!     COMFLG = 5
!     GO TO 176
!
!     NO. DES. ROOTS FOUND IN REGION OF CONVERGENCE OUTSIDE REGION
!
                     comflg = 6
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( noroot>=3*noest ) THEN
!
!     3*NOEST FOUND
!
                     comflg = 4
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     aaa = dsqrt((lambda(1)-lam1(1))**2+(lambda(2)-lam1(2))**2)
                     IF ( aaa<=rzero ) THEN
!
!     FOUND ROOT OUTSIDE REGION OF CURRENT START POINT
!
                        ind = iabs(ind)
                        ireg = 1
                        IF ( eps*rzero/dsqrt((lam1(1)-lambda(1))**2+(lam1(2)-lambda(2))**2)<ep3 ) CYCLE
!
!     CURRENT SHIFT POINT IS TOO CLOSE TO AN EIGENVALUE
!
                        IF ( comflg/=2 ) THEN
                           lambda(1) = lambda(1) + .02*rzero
                           lambda(2) = lambda(2) + .02*rzero
                           comflg = 2
                        ELSE
                           comflg = 9
                        ENDIF
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        IF ( ireg==0 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( ind<=0 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     GENERATE NEW ARBITRARY STARTING VECTOR
!
                        ind = -(ind+1)
                        ivect = 0
                        IF ( ind==-13 ) ind = -1
                        lam1(1) = 0.0D0
                        lam1(2) = 0.0D0
                        IF ( northo/=0 ) THEN
!
!     TEST FOR INSUFFICIENT TIME
!
                           CALL klock(icurnt)
                           CALL tmtogo(iijjkk)
                           navg = (icurnt-istart)/northo
                           IF ( iijjkk<2*navg ) THEN
                              comflg = 8
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  lam1(1) = 0.0D0
                  lam1(2) = 0.0D0
               ENDIF
               IF ( ivect==1 ) THEN
!
!     USE PREVIOUSLY STORED VECTOR FOR STARTING VECTOR
!
                  ifile = filevc(1)
                  CALL gopen(filevc,z(iobuf),rd)
                  CALL bckrec(filevc(1))
                  in1 = 1
                  IF ( comflg==1 ) THEN
                     in1 = jj5
                     CALL bckrec(filevc(1))
                  ENDIF
                  CALL fread(filevc,dz(in1),ncol4,1)
                  IF ( comflg==1 ) THEN
!
!     PICK UP LAST ITERATED VECTOR FOR A STARTING VECTOR
!
                     CALL fread(filevc,dz,ncol4,1)
                     CALL skprec(filevc,-2)
                     CALL close(filevc(1),norew)
                  ELSE
                     CALL bckrec(filevc(1))
                     CALL close(filevc(1),norew)
                     ivect = 0
                  ENDIF
               ELSE
                  k = iabs(ind)
                  DO i = 1 , ncol2 , 2
                     dz(i) = (mod(k,13)+1)*(1+5*i/ncol)
                     k = k + 1
                     dz(i+1) = 0.D0
                     dz(i) = 1.D0/dz(i)
                  ENDDO
!
!     FORM V0 = LAMBDA*U0
!
                  CALL cnorm1(dz(ii1),ncol)
               ENDIF
               DO iu = 1 , ncol2 , 2
                  j = kk1 + iu - 1
                  dz(j) = dz(iu)*lambda(1) - dz(iu+1)*lambda(2)
                  dz(j+1) = dz(iu)*lambda(2) + dz(iu+1)*lambda(1)
               ENDDO
               IF ( northo/=0 ) THEN
                  CALL ortho(dz(ii1),dz(kk1),dz(kk2),dz(kk3),dz(kk4),dz(kk5),dz(kk6),nzz,z(iobuf),z(ibuf1),z(ibuf2),z(ibuf3))
                  IF ( fileb(1)==0 ) THEN
                     DO iu = 1 , ncol2 , 2
                        j = kk1 + iu - 1
                        dz(j) = dz(iu)*lambda(1) - dz(iu+1)*lambda(2)
                        dz(j+1) = dz(iu)*lambda(2) + dz(iu+1)*lambda(1)
                     ENDDO
                  ENDIF
               ENDIF
               CALL cmtimu(dz(ii1),dz(jj1),0,z(iobuf))
               IF ( fileb(1)/=0 ) THEN
                  file(1) = fileb(1)
                  CALL cmtimu(dz(ii1),dz(kk2),file,z(ibuf1))
                  con1(1) = 2.0D0*lambda(1)
                  con1(2) = 2.0D0*lambda(2)
                  CALL cdivid(plus1,con2,con1,2)
                  con2(1) = -con2(1)
                  con2(2) = -con2(2)
                  CALL csub(dz(jj1),dz(kk2),dz(jj1),plus1,con2)
               ENDIF
               CALL cxtrny(dz(ii1),dz(jj1),aln(1))
               CALL csqrtx(aln(1),aln(1))
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         SPAG_Loop_1_2: DO
!
!     COMPUTE THE R.H.S. OF THE SYSTEM OF EQUATIONS
!
            file(1) = sr2fil
            IF ( switch==1 ) file(1) = s11fil
            CALL cmtimu(dz(ii1),dz(ll1),file(1),z(iobuf))
            CALL cmtimu(dz(kk1),dz(ll2),0,z(iobuf))
            CALL csub(dz(ll1),dz(ll2),dz(ll2),plus1(1),plus1(1))
!
!     SHIFT POINTERS
!
            ii = ii1
            ii1 = ii2
            ii2 = ii
            ii = jj1
            jj1 = jj2
            jj2 = jj3
            jj3 = ii
!
!     SAVE THE N-1 VECTOR
!
            IF ( switch==0 ) THEN
               ixx = jj5 + ncol2 - 1
               ixz = ii2
               DO i = jj5 , ixx
                  dz(i) = dz(ixz)
                  ixz = ixz + 1
               ENDDO
            ENDIF
            CALL tmtogo(ixx)
            IF ( ixx<=0 ) THEN
               comflg = 8
               EXIT SPAG_Loop_1_2
            ELSE
!
!     SHIFT PARAMETERS
!
               alnm1(1) = aln(1)
               alnm1(2) = aln(2)
               etanm1(1) = xyz(1)
               etanm1(2) = xyz(2)
               h2nm1(1) = h2n(1)
               h2nm1(2) = h2n(2)
               lm1nm1(1) = lam1(1)
               lm1nm1(2) = lam1(2)
               lm2nm1(1) = lam2(1)
               lm2nm1(2) = lam2(2)
!
!     CALL CINFBS TO MAKE ONE ITERATION
!
               CALL cinfbs(dz(ll2),dz(ii1),z(iobuf))
               iterto = iterto + 1
               iter = iter + 1
               iepcnt = iepcnt + 1
               CALL cnorm(dz(ii1),cn(1),dz(ii2))
!
               IF ( idiag/=0 ) THEN
                  kkkk = ii1 + ncol2 - 1
                  WRITE (nout,99001) iterto , iter , cn , timed , timeit , (dz(kx),kx=ii1,kkkk)
99001             FORMAT (15H ITERTO =      ,I5,10H ITER =   ,I5,' CN =    ',2D15.5,10H TIMED =  ,I5,10H TIMEIT=  ,I5,//,           &
                         &20H ITERATER VECTOR    ,//,(10D12.4))
               ENDIF
!
!     COMPUTE V(N)BAR
!
               con1(1) = -cn(1)/(cn(1)**2+cn(2)**2)
               con1(2) = cn(2)/(cn(1)**2+cn(2)**2)
               CALL csub(dz(ii1),dz(ii2),dz(kk1),lambda,con1)
!
!     ORTHOGONALIZE CURRENT ITERANT WITH RESPECT TO VECTORS FOUND IN
!     THE CURRENT AND PREVIOUS REGIONS
!
               IF ( northo/=0 ) CALL ortho(dz(ii1),dz(kk1),dz(kk2),dz(kk3),dz(kk4),dz(kk5),dz(kk6),nzz,z(iobuf),z(ibuf1),z(ibuf2),  &
                  & z(ibuf3))
!
!     COMPUTE V(N)
!
               IF ( fileb(1)==0 ) CALL csub(dz(ii1),dz(ii2),dz(kk1),lambda,con1(1))
!
!     BEGIN TESTING CONVERGENCE CRITERIA
!
!     COMPUTE F(N)
!
               CALL cmtimu(dz(ii1),dz(jj1),0,z(iobuf))
               IF ( fileb(1)/=0 ) THEN
                  file(1) = fileb(1)
                  CALL cmtimu(dz(ii1),dz(kk2),file,z(ibuf1))
                  con1(1) = 2.0D0*lambda(1)
                  con1(2) = 2.0D0*lambda(2)
                  CALL cdivid(plus1,con2,con1,2)
                  con2(1) = -con2(1)
                  con2(2) = -con2(2)
                  CALL csub(dz(jj1),dz(kk2),dz(jj1),plus1,con2)
               ENDIF
!
!     COMPUTE ALPHA(N)
!
               CALL cxtrny(dz(ii1),dz(jj1),aln(1))
               CALL csqrtx(aln(1),aln(1))
!
!     COMPUTE DELTA U(N)
!
               con1(1) = aln(1)/(aln(1)**2+aln(2)**2)
               con1(2) = -aln(2)/(aln(1)**2+aln(2)**2)
               con2(1) = alnm1(1)/(alnm1(1)**2+alnm1(2)**2)
               con2(2) = -alnm1(2)/(alnm1(1)**2+alnm1(2)**2)
               CALL csub(dz(ii1),dz(ii2),dz(ii2),con1(1),con2(1))
!
!     COMPUTE DELTA F(N)
!
               CALL csub(dz(jj1),dz(jj3),dz(jj3),con1(1),con2(1))
               con1(1) = cn(1)*aln(1) - cn(2)*aln(2)
               con1(2) = cn(2)*aln(1) + cn(1)*aln(2)
               lam1(1) = (alnm1(1)*con1(1)+alnm1(2)*con1(2))/(con1(1)**2+con1(2)**2)
               lam1(2) = (alnm1(2)*con1(1)-alnm1(1)*con1(2))/(con1(1)**2+con1(2)**2)
               IF ( irapid/=1 ) THEN
                  CALL cxtrny(dz(ii2),dz(jj3),eta(1))
                  CALL csqrtx(eta(1),xyz(1))
!
                  IF ( idiag/=0 ) THEN
                     WRITE (nout,99002) lam1 , xyz , aln
99002                FORMAT (12H LAMBDA =   ,2D15.5,12H  ETA =     ,2D15.5,12H ALPHA =    ,2D15.5)
                  ENDIF
                  IF ( iter/=1 ) THEN
!
!     RAPID CONVERGENCE TEST
!
!     IF (ETA.GE.A*EPS*GAMMA*(1.+LAMBDA/LAM1)
!
                     con1(1) = (lambda(1)*lam1(1)+lambda(2)*lam1(2))/(lam1(1)**2+lam1(2)**2)
                     con1(2) = (lambda(2)*lam1(1)-lambda(1)*lam1(2))/(lam1(1)**2+lam1(2)**2)
                     IF ( dsqrt(xyz(1)**2+xyz(2)**2)>=a*eps*gamma*dsqrt(1.+con1(1)**2+con1(1)**2+con1(2)**2) ) THEN
                        IF ( dsqrt(etanm1(1)**2+etanm1(2)**2)>=1.E-06 ) GOTO 2
                        IF ( dsqrt(xyz(1)**2+xyz(2)**2)<=1.01*dsqrt(etanm1(1)**2+etanm1(2)**2) ) GOTO 2
                        irapid = 1
                     ELSE
                        irapid = 1
                     ENDIF
                  ENDIF
                  CYCLE
!
!     EPSILON 2 TEST
!
 2                IF ( iep2/=1 ) THEN
                     CALL cxtrny(dz(ii2),dz(jj2),con1(1))
                     con2(1) = con1(1)*lam1(1) - con1(2)*lam1(2)
                     con1(2) = con1(1)*lam1(2) + con1(2)*lam1(1)
                     con1(1) = con2(1)
                     lam2(1) = (con1(1)*eta(1)+con1(2)*eta(2))/(eta(1)**2+eta(2)**2)
                     lam2(2) = (con1(2)*eta(1)-con1(1)*eta(2))/(eta(1)**2+eta(2)**2)
                     con1(1) = lam2(1) - lm2nm1(1)
                     con1(2) = lam2(2) - lm2nm1(2)
                     h2n(1) = (con1(1)*lambda(1)+con1(2)*lambda(2))/(lambda(1)**2+lambda(2)**2)
                     h2n(2) = (con1(2)*lambda(1)-con1(1)*lambda(2))/(lambda(1)**2+lambda(2)**2)
                     IF ( iter>=4 ) THEN
                        IF ( ep2>dsqrt(h2n(1)**2+h2n(2)**2) .AND. dsqrt(h2n(1)**2+h2n(2)**2)>dsqrt(h2nm1(1)**2+h2nm1(2)**2) ) THEN
                           iep2 = 1
                           lam2(1) = lm2nm1(1)
                           lam2(2) = lm2nm1(2)
                        ENDIF
                     ENDIF
                  ENDIF
                  con1(1) = 1. - (lam2(1)*lam1(1)+lam2(2)*lam1(2))/(lam1(1)**2+lam1(2)**2)
                  con1(2) = (lam2(2)*lam1(1)-lam2(1)*lam1(2))/(lam1(1)**2+lam1(2)**2)
                  con2(1) = con1(1)*con1(1) - con1(2)*con1(2)
                  con1(2) = 2.*con1(2)*con1(1)
                  con1(1) = con2(1)
                  con1(1) = dmin1(dsqrt(con1(1)**2+con1(2)**2),10.0D0)
                  delta(1) = eta(1)/con1(1)
                  delta(2) = eta(2)/con1(1)
!
                  IF ( idiag/=0 ) THEN
                     WRITE (nout,99003) lam2 , h2n , delta
99003                FORMAT (12H  LAMBDA =  ,2D15.5,12H  H2N =     ,2D15.5,12H DELTA =    ,2D15.5)
                  ENDIF
!
!     VECTOR CONVERGENCE TEST
!
                  IF ( dsqrt(delta(1)**2+delta(2)**2)>(a*eps)**2 ) THEN
                     IF ( iter<=3 ) CYCLE
!
!     EPSILON 1 TEST
!
                     IF ( iepcnt>=100 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( iepcnt<10 ) THEN
                        IF ( dsqrt((lam1(1)-lm1nm1(1))**2+(lam1(2)-lm1nm1(2))**2)                                                   &
                           & /dsqrt((lambda(1)+dabs(lam1(1)))**2+(lambda(2)+dabs(lam1(2)))**2)>=ep1 ) CYCLE
                        iepcnt = 0
                     ENDIF
!
!     SHIFT DECISION
!
                     CALL klock(t2)
                     timeit = t2 - t1
                     IF ( idiag/=0 ) THEN
                        WRITE (nout,99004) t2 , t1 , timeit
99004                   FORMAT (3I15)
                     ENDIF
                     k = dlog(dsqrt(delta(1)**2+delta(2)**2)/(a*eps)**2)                                                            &
                       & /dabs(dlog(dsqrt(lam1(1)**2+lam1(2)**2)/dsqrt(lam2(1)**2+lam2(2)**2))) + 1.
                     k = k/2
                     IF ( idiag/=0 ) THEN
                        WRITE (nout,99005) k
99005                   FORMAT (I5)
                     ENDIF
                     ir1 = float(k-3)*float(timeit)/float(iter)
                     IF ( timed>=ir1 ) CYCLE
                     lambda(1) = lambda(1) + lam1(1)
                     lambda(2) = lambda(2) + lam1(2)
!
!     STORE THE LAST VECTOR BEFORE A SHIFT FOR USE AS A STARTING VECTOR
!
                     IF ( switch==1 ) THEN
                        in1 = jj5
                     ELSE
                        in1 = ii1
                     ENDIF
                     ifile = filevc(1)
                     CALL gopen(ifile,z(iobuf),wrt)
                     CALL write(ifile,dz(in1),ncol4,1)
                     ivect = 1
                     comflg = 1
!
!     STORE  THE CURRENT VECTOR ON THE EIGENVECTOR FILE SO IT CAN BE
!     USED AS THE STARTING VECTOR
!
                     CALL write(ifile,dz(ii1),ncol4,1)
                     CALL close(ifile,eofnrw)
                     EXIT SPAG_Loop_1_2
                  ENDIF
               ENDIF
!
!     M  RAPID CONVERGENCE MAKE SURE LAMD1 PASSES EP1 TEST
!
               IF ( dsqrt((lam1(1)-lm1nm1(1))**2+(lam1(2)-lm1nm1(2))**2)                                                            &
                  & /dsqrt((lambda(1)+dabs(lam1(1)))**2+(lambda(2)+dabs(lam1(2)))**2)>=ep1 ) CYCLE
!
!     CONVERGENCE ACHIEVED, NORMALIZE THE VECTOR
!
!     STORE THE EIGENVECTOR AND EIGENVALUE ON THE OUTPUT FILES
!
               CALL cnorm1(dz(ii1),ncol)
               lam1(1) = lam1(1) + lambda(1)
               lam1(2) = lam1(2) + lambda(2)
               inu = ii1 + ncol2 - 1
               IF ( idiag/=0 ) THEN
                  WRITE (nout,99006) lam1 , (dz(i),i=ii1,inu)
99006             FORMAT (1H1,20H CONVERGENCE        ,//,' LAMBDA = ',2D15.5,//,(10D12.4))
               ENDIF
               ifile = filevc(1)
               CALL gopen(ifile,z(iobuf),wrt)
               CALL write(ifile,dz(ii1),ncol4,1)
               CALL close(ifile,eofnrw)
               ifile = filelm(1)
               CALL gopen(ifile,z(iobuf),wrt)
               CALL write(ifile,lam1(1),4,1)
               CALL close(ifile,eofnrw)
               northo = northo + 1
               noroot = noroot + 1
               iep2 = 0
               irapid = 0
               nochng = 0
               comflg = 0
               IF ( switch==0 ) THEN
                  ivect = 0
                  IF ( iter<=5 ) GOTO 5
               ELSE
                  switch = 0
                  lambda(1) = lmbda(1)
                  lambda(2) = lmbda(2)
               ENDIF
               in1 = jj5
               ifile = filevc(1)
               CALL gopen(ifile,z(iobuf),wrt)
               CALL write(ifile,dz(in1),ncol4,1)
               CALL close(ifile,eofnrw)
               ivect = 1
 5             iter = 0
!
!     COMPUTE PSEUDO LEFT VECTOR
!
               CALL cmtimu(dz(ii1),dz(jj3),0,z(iobuf))
               IF ( fileb(1)/=0 ) THEN
                  CALL cmtimu(dz(ii1),dz(jj2),fileb,z(ibuf1))
                  con1(1) = 2.0D0*lam1(1)
                  con1(2) = 2.0D0*lam1(2)
                  con2(1) = -1.0D0
                  con2(2) = 0.0D0
                  CALL csub(dz(jj3),dz(jj2),dz(jj3),con1,con2)
               ENDIF
               IF ( isym/=1 ) THEN
!
!     LEFT = RIGHT FINISH JOB
!
                  CALL cxtrny(dz(ii1),dz(jj3),con1)
                  CALL cdivid(dz(ii1),dz(jj3),con1,ncol2)
               ENDIF
!
!     PUT SCALED VECTOR ON LEFT VECTOR FILE
!
               ifile = scrfil(10)
               CALL gopen(ifile,z(ibuf1),wrt)
               CALL write(ifile,dz(jj3),ncol4,1)
               CALL close(ifile,eofnrw)
               left = 1
               IF ( isym/=0 ) EXIT SPAG_Loop_1_2
               spag_nextblock_1 = 1
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
!
! 490 CALL CLOSE (FILEL,1)
!     CALL CLOSE (FILEU,1)
!     CALL CLOSE (FILEM,1)
         RETURN
      CASE (3)
         IF ( nodes<=noroot ) THEN
            comflg = 6
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( lam1(1)**2+lam1(2)**2>=maxmod ) THEN
!
!     ONE OR MORE ROOTS OUTSIDE REGION
!
            comflg = 7
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     GET NEW STARTING  POINT
!
         comflg = 0
         ind = -ind
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cinvp3
