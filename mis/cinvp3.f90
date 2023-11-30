
SUBROUTINE cinvp3
   IMPLICIT NONE
   INTEGER Cdp , Comflg , Fileb(7) , Filek(7) , Filel(7) , Filelm(7) , Fileu(7) , Filevc(7) , Ind , Ind1 , Iofff , Ireg , Istart ,  &
         & Isym , Iter , Iterto , Ivect , Left , Ncol , Nochng , Nodes , Noest , Noreg , Norew , Noroot , Northo , Nout , Nzero ,   &
         & Real , S11fil , Scrfil(11) , Sqr , Sr1fil , Sr2fil , Sr3fil , Sr4fil , Sr8fil , Sr9fil , Switch , Sysbuf , Timed
   REAL Csp , Dudxx , Eofnrw , Eps , Filem(7) , Maxmod , Rd , Rdp , Rdrew , Rew , Rsp , Rzero , Wrt , Wrtrew , Xxyy(20) , Z(1)
   DOUBLE PRECISION Dz(1) , Lam1(2) , Lambda(2) , Lmbda(2)
   COMMON /cdcmpx/ Xxyy , Iofff
   COMMON /cinfbx/ Filel , Fileu
   COMMON /cinvpx/ Filek , Filem , Fileb , Filelm , Filevc , Dudxx , Scrfil , Noreg , Eps
   COMMON /cinvxx/ Lambda , Switch , Comflg , Lmbda , Iterto , Timed , Nochng , Rzero , Ind , Ivect , Ireg , Real , Left , Northo , &
                 & Noroot , Nzero , Lam1 , Maxmod , Nodes , Noest , Istart , Ind1 , Iter , Isym
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr
   COMMON /system/ Sysbuf , Nout
   COMMON /zzzzzz/ Z
   REAL a , aaa , ep1 , ep2 , ep3 , gamma
   DOUBLE PRECISION aln(2) , alnm1(2) , cn(2) , con1(2) , con2(2) , delta(2) , eta(2) , etanm1(2) , h2n(2) , h2nm1(2) , lam2(2) ,   &
                  & lm1nm1(2) , lm2nm1(2) , plus1(2) , xyz(2)
   INTEGER end , file(7) , i , ibuf1 , ibuf2 , ibuf3 , icurnt , idiag , iep2 , iepcnt , ifile , ii , ii1 , ii2 , iijjkk , in1 ,     &
         & inu , iobuf , ir1 , irapid , iu , ixx , ixz , j , jj1 , jj2 , jj3 , jj5 , k , kk1 , kk2 , kk3 , kk4 , kk5 , kk6 , kkkk , &
         & kx , ll1 , ll2 , name(2) , navg , ncol2 , ncol4 , ncount , nz , nzz , t1 , t2 , timeit
   INTEGER korsz
!
!     SUBROUTINE CINVP3, THE MAIN LINK OF CINVPR, SOLVES FOR THE
!     EIGENVALUES AND EIGENVECTORS OF (LAMBDA**2*M + LAMBDA*B*K)
!
!     TYPE DECLARATIONS
!
   EQUIVALENCE (Filek(2),Ncol) , (Scrfil(1),Sr1fil) , (Scrfil(2),Sr2fil) , (Scrfil(3),Sr3fil) , (Scrfil(4),Sr4fil) ,                &
    & (Scrfil(8),Sr8fil) , (Scrfil(9),Sr9fil) , (Dz(1),Z(1)) , (Scrfil(11),S11fil)
   DATA name/4HCINV , 4HP3  /
   DATA plus1/ + 1.D0 , 0.D0/
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
 100  timeit = 0
   nz = korsz(Z)
   ncol2 = Ncol + Ncol
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
   Lam1(1) = 0.0D0
   Lam1(2) = 0.0D0
   Iter = 0
   CALL klock(t1)
   irapid = 0
   iep2 = 0
   DO
      ncount = 0
      iepcnt = 0
      IF ( Switch==1 ) THEN
         Filel(1) = Sr8fil
         Fileu(1) = Sr9fil
      ELSE
         Filel(1) = Sr3fil
         Fileu(1) = Sr4fil
      ENDIF
      Filel(5) = Cdp
      Filel(3) = Filek(3)
      Fileu(7) = Iofff
      file(4) = Sqr
      file(5) = Cdp
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
      iobuf = nz - Sysbuf + 1
      ibuf1 = iobuf - Sysbuf
      ibuf2 = ibuf1 - Sysbuf
      ibuf3 = ibuf2 - Sysbuf
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
         GOTO 99999
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
         IF ( Left==1 ) THEN
!
!     RETURN TO MAIN DRIVER TO COMPUTE THE LEFT EIGENVECTOR
!
!
!     ENTRY POINT UPON RETURNING FROM OBTAINING THE LEFT VECTOR
!
            Left = 0
            IF ( Nodes<=Noroot ) THEN
!
!     ALL ROOTS IN PROBLEM FOUND
!
!     COMFLG = 5
!     GO TO 176
!
!     NO. DES. ROOTS FOUND IN REGION OF CONVERGENCE OUTSIDE REGION
!
               Comflg = 6
               GOTO 200
            ELSEIF ( Noroot>=3*Noest ) THEN
!
!     3*NOEST FOUND
!
               Comflg = 4
               GOTO 200
            ELSE
               aaa = dsqrt((Lambda(1)-Lam1(1))**2+(Lambda(2)-Lam1(2))**2)
               IF ( aaa<=Rzero ) THEN
!
!     FOUND ROOT OUTSIDE REGION OF CURRENT START POINT
!
                  Ind = iabs(Ind)
                  Ireg = 1
                  IF ( Eps*Rzero/dsqrt((Lam1(1)-Lambda(1))**2+(Lam1(2)-Lambda(2))**2)<ep3 ) CYCLE
!
!     CURRENT SHIFT POINT IS TOO CLOSE TO AN EIGENVALUE
!
                  IF ( Comflg/=2 ) THEN
                     Lambda(1) = Lambda(1) + .02*Rzero
                     Lambda(2) = Lambda(2) + .02*Rzero
                     Comflg = 2
                  ELSE
                     Comflg = 9
                  ENDIF
                  GOTO 200
               ELSE
                  IF ( Ireg==0 ) GOTO 300
                  IF ( Ind<=0 ) GOTO 300
!
!     GENERATE NEW ARBITRARY STARTING VECTOR
!
                  Ind = -(Ind+1)
                  Ivect = 0
                  IF ( Ind==-13 ) Ind = -1
                  Lam1(1) = 0.0D0
                  Lam1(2) = 0.0D0
                  IF ( Northo/=0 ) THEN
!
!     TEST FOR INSUFFICIENT TIME
!
                     CALL klock(icurnt)
                     CALL tmtogo(iijjkk)
                     navg = (icurnt-Istart)/Northo
                     IF ( iijjkk<2*navg ) THEN
                        Comflg = 8
                        GOTO 200
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            Lam1(1) = 0.0D0
            Lam1(2) = 0.0D0
         ENDIF
         IF ( Ivect==1 ) THEN
!
!     USE PREVIOUSLY STORED VECTOR FOR STARTING VECTOR
!
            ifile = Filevc(1)
            CALL gopen(Filevc,Z(iobuf),Rd)
            CALL bckrec(Filevc(1))
            in1 = 1
            IF ( Comflg==1 ) THEN
               in1 = jj5
               CALL bckrec(Filevc(1))
            ENDIF
            CALL fread(Filevc,Dz(in1),ncol4,1)
            IF ( Comflg==1 ) THEN
!
!     PICK UP LAST ITERATED VECTOR FOR A STARTING VECTOR
!
               CALL fread(Filevc,Dz,ncol4,1)
               CALL skprec(Filevc,-2)
               CALL close(Filevc(1),Norew)
            ELSE
               CALL bckrec(Filevc(1))
               CALL close(Filevc(1),Norew)
               Ivect = 0
            ENDIF
         ELSE
            k = iabs(Ind)
            DO i = 1 , ncol2 , 2
               Dz(i) = (mod(k,13)+1)*(1+5*i/Ncol)
               k = k + 1
               Dz(i+1) = 0.D0
               Dz(i) = 1.D0/Dz(i)
            ENDDO
!
!     FORM V0 = LAMBDA*U0
!
            CALL cnorm1(Dz(ii1),Ncol)
         ENDIF
         DO iu = 1 , ncol2 , 2
            j = kk1 + iu - 1
            Dz(j) = Dz(iu)*Lambda(1) - Dz(iu+1)*Lambda(2)
            Dz(j+1) = Dz(iu)*Lambda(2) + Dz(iu+1)*Lambda(1)
         ENDDO
         IF ( Northo/=0 ) THEN
            CALL ortho(Dz(ii1),Dz(kk1),Dz(kk2),Dz(kk3),Dz(kk4),Dz(kk5),Dz(kk6),nzz,Z(iobuf),Z(ibuf1),Z(ibuf2),Z(ibuf3))
            IF ( Fileb(1)==0 ) THEN
               DO iu = 1 , ncol2 , 2
                  j = kk1 + iu - 1
                  Dz(j) = Dz(iu)*Lambda(1) - Dz(iu+1)*Lambda(2)
                  Dz(j+1) = Dz(iu)*Lambda(2) + Dz(iu+1)*Lambda(1)
               ENDDO
            ENDIF
         ENDIF
         CALL cmtimu(Dz(ii1),Dz(jj1),0,Z(iobuf))
         IF ( Fileb(1)/=0 ) THEN
            file(1) = Fileb(1)
            CALL cmtimu(Dz(ii1),Dz(kk2),file,Z(ibuf1))
            con1(1) = 2.0D0*Lambda(1)
            con1(2) = 2.0D0*Lambda(2)
            CALL cdivid(plus1,con2,con1,2)
            con2(1) = -con2(1)
            con2(2) = -con2(2)
            CALL csub(Dz(jj1),Dz(kk2),Dz(jj1),plus1,con2)
         ENDIF
         CALL cxtrny(Dz(ii1),Dz(jj1),aln(1))
         CALL csqrtx(aln(1),aln(1))
         EXIT
      ENDIF
   ENDDO
   DO
!
!     COMPUTE THE R.H.S. OF THE SYSTEM OF EQUATIONS
!
      file(1) = Sr2fil
      IF ( Switch==1 ) file(1) = S11fil
      CALL cmtimu(Dz(ii1),Dz(ll1),file(1),Z(iobuf))
      CALL cmtimu(Dz(kk1),Dz(ll2),0,Z(iobuf))
      CALL csub(Dz(ll1),Dz(ll2),Dz(ll2),plus1(1),plus1(1))
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
      IF ( Switch==0 ) THEN
         ixx = jj5 + ncol2 - 1
         ixz = ii2
         DO i = jj5 , ixx
            Dz(i) = Dz(ixz)
            ixz = ixz + 1
         ENDDO
      ENDIF
      CALL tmtogo(ixx)
      IF ( ixx<=0 ) THEN
         Comflg = 8
         EXIT
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
         lm1nm1(1) = Lam1(1)
         lm1nm1(2) = Lam1(2)
         lm2nm1(1) = lam2(1)
         lm2nm1(2) = lam2(2)
!
!     CALL CINFBS TO MAKE ONE ITERATION
!
         CALL cinfbs(Dz(ll2),Dz(ii1),Z(iobuf))
         Iterto = Iterto + 1
         Iter = Iter + 1
         iepcnt = iepcnt + 1
         CALL cnorm(Dz(ii1),cn(1),Dz(ii2))
!
         IF ( idiag/=0 ) THEN
            kkkk = ii1 + ncol2 - 1
            WRITE (Nout,99001) Iterto , Iter , cn , Timed , timeit , (Dz(kx),kx=ii1,kkkk)
99001       FORMAT (15H ITERTO =      ,I5,10H ITER =   ,I5,' CN =    ',2D15.5,10H TIMED =  ,I5,10H TIMEIT=  ,I5,//,                 &
                   &20H ITERATER VECTOR    ,//,(10D12.4))
         ENDIF
!
!     COMPUTE V(N)BAR
!
         con1(1) = -cn(1)/(cn(1)**2+cn(2)**2)
         con1(2) = cn(2)/(cn(1)**2+cn(2)**2)
         CALL csub(Dz(ii1),Dz(ii2),Dz(kk1),Lambda,con1)
!
!     ORTHOGONALIZE CURRENT ITERANT WITH RESPECT TO VECTORS FOUND IN
!     THE CURRENT AND PREVIOUS REGIONS
!
         IF ( Northo/=0 ) CALL ortho(Dz(ii1),Dz(kk1),Dz(kk2),Dz(kk3),Dz(kk4),Dz(kk5),Dz(kk6),nzz,Z(iobuf),Z(ibuf1),Z(ibuf2),Z(ibuf3)&
                                   & )
!
!     COMPUTE V(N)
!
         IF ( Fileb(1)==0 ) CALL csub(Dz(ii1),Dz(ii2),Dz(kk1),Lambda,con1(1))
!
!     BEGIN TESTING CONVERGENCE CRITERIA
!
!     COMPUTE F(N)
!
         CALL cmtimu(Dz(ii1),Dz(jj1),0,Z(iobuf))
         IF ( Fileb(1)/=0 ) THEN
            file(1) = Fileb(1)
            CALL cmtimu(Dz(ii1),Dz(kk2),file,Z(ibuf1))
            con1(1) = 2.0D0*Lambda(1)
            con1(2) = 2.0D0*Lambda(2)
            CALL cdivid(plus1,con2,con1,2)
            con2(1) = -con2(1)
            con2(2) = -con2(2)
            CALL csub(Dz(jj1),Dz(kk2),Dz(jj1),plus1,con2)
         ENDIF
!
!     COMPUTE ALPHA(N)
!
         CALL cxtrny(Dz(ii1),Dz(jj1),aln(1))
         CALL csqrtx(aln(1),aln(1))
!
!     COMPUTE DELTA U(N)
!
         con1(1) = aln(1)/(aln(1)**2+aln(2)**2)
         con1(2) = -aln(2)/(aln(1)**2+aln(2)**2)
         con2(1) = alnm1(1)/(alnm1(1)**2+alnm1(2)**2)
         con2(2) = -alnm1(2)/(alnm1(1)**2+alnm1(2)**2)
         CALL csub(Dz(ii1),Dz(ii2),Dz(ii2),con1(1),con2(1))
!
!     COMPUTE DELTA F(N)
!
         CALL csub(Dz(jj1),Dz(jj3),Dz(jj3),con1(1),con2(1))
         con1(1) = cn(1)*aln(1) - cn(2)*aln(2)
         con1(2) = cn(2)*aln(1) + cn(1)*aln(2)
         Lam1(1) = (alnm1(1)*con1(1)+alnm1(2)*con1(2))/(con1(1)**2+con1(2)**2)
         Lam1(2) = (alnm1(2)*con1(1)-alnm1(1)*con1(2))/(con1(1)**2+con1(2)**2)
         IF ( irapid/=1 ) THEN
            CALL cxtrny(Dz(ii2),Dz(jj3),eta(1))
            CALL csqrtx(eta(1),xyz(1))
!
            IF ( idiag/=0 ) THEN
               WRITE (Nout,99002) Lam1 , xyz , aln
99002          FORMAT (12H LAMBDA =   ,2D15.5,12H  ETA =     ,2D15.5,12H ALPHA =    ,2D15.5)
            ENDIF
            IF ( Iter/=1 ) THEN
!
!     RAPID CONVERGENCE TEST
!
!     IF (ETA.GE.A*EPS*GAMMA*(1.+LAMBDA/LAM1)
!
               con1(1) = (Lambda(1)*Lam1(1)+Lambda(2)*Lam1(2))/(Lam1(1)**2+Lam1(2)**2)
               con1(2) = (Lambda(2)*Lam1(1)-Lambda(1)*Lam1(2))/(Lam1(1)**2+Lam1(2)**2)
               IF ( dsqrt(xyz(1)**2+xyz(2)**2)>=a*Eps*gamma*dsqrt(1.+con1(1)**2+con1(1)**2+con1(2)**2) ) THEN
                  IF ( dsqrt(etanm1(1)**2+etanm1(2)**2)>=1.E-06 ) GOTO 110
                  IF ( dsqrt(xyz(1)**2+xyz(2)**2)<=1.01*dsqrt(etanm1(1)**2+etanm1(2)**2) ) GOTO 110
                  irapid = 1
               ELSE
                  irapid = 1
               ENDIF
            ENDIF
            CYCLE
!
!     EPSILON 2 TEST
!
 110        IF ( iep2/=1 ) THEN
               CALL cxtrny(Dz(ii2),Dz(jj2),con1(1))
               con2(1) = con1(1)*Lam1(1) - con1(2)*Lam1(2)
               con1(2) = con1(1)*Lam1(2) + con1(2)*Lam1(1)
               con1(1) = con2(1)
               lam2(1) = (con1(1)*eta(1)+con1(2)*eta(2))/(eta(1)**2+eta(2)**2)
               lam2(2) = (con1(2)*eta(1)-con1(1)*eta(2))/(eta(1)**2+eta(2)**2)
               con1(1) = lam2(1) - lm2nm1(1)
               con1(2) = lam2(2) - lm2nm1(2)
               h2n(1) = (con1(1)*Lambda(1)+con1(2)*Lambda(2))/(Lambda(1)**2+Lambda(2)**2)
               h2n(2) = (con1(2)*Lambda(1)-con1(1)*Lambda(2))/(Lambda(1)**2+Lambda(2)**2)
               IF ( Iter>=4 ) THEN
                  IF ( ep2>dsqrt(h2n(1)**2+h2n(2)**2) .AND. dsqrt(h2n(1)**2+h2n(2)**2)>dsqrt(h2nm1(1)**2+h2nm1(2)**2) ) THEN
                     iep2 = 1
                     lam2(1) = lm2nm1(1)
                     lam2(2) = lm2nm1(2)
                  ENDIF
               ENDIF
            ENDIF
            con1(1) = 1. - (lam2(1)*Lam1(1)+lam2(2)*Lam1(2))/(Lam1(1)**2+Lam1(2)**2)
            con1(2) = (lam2(2)*Lam1(1)-lam2(1)*Lam1(2))/(Lam1(1)**2+Lam1(2)**2)
            con2(1) = con1(1)*con1(1) - con1(2)*con1(2)
            con1(2) = 2.*con1(2)*con1(1)
            con1(1) = con2(1)
            con1(1) = dmin1(dsqrt(con1(1)**2+con1(2)**2),10.0D0)
            delta(1) = eta(1)/con1(1)
            delta(2) = eta(2)/con1(1)
!
            IF ( idiag/=0 ) THEN
               WRITE (Nout,99003) lam2 , h2n , delta
99003          FORMAT (12H  LAMBDA =  ,2D15.5,12H  H2N =     ,2D15.5,12H DELTA =    ,2D15.5)
            ENDIF
!
!     VECTOR CONVERGENCE TEST
!
            IF ( dsqrt(delta(1)**2+delta(2)**2)>(a*Eps)**2 ) THEN
               IF ( Iter<=3 ) CYCLE
!
!     EPSILON 1 TEST
!
               IF ( iepcnt>=100 ) GOTO 400
               IF ( iepcnt<10 ) THEN
                  IF ( dsqrt((Lam1(1)-lm1nm1(1))**2+(Lam1(2)-lm1nm1(2))**2)                                                         &
                     & /dsqrt((Lambda(1)+dabs(Lam1(1)))**2+(Lambda(2)+dabs(Lam1(2)))**2)>=ep1 ) CYCLE
                  iepcnt = 0
               ENDIF
!
!     SHIFT DECISION
!
               CALL klock(t2)
               timeit = t2 - t1
               IF ( idiag/=0 ) THEN
                  WRITE (Nout,99004) t2 , t1 , timeit
99004             FORMAT (3I15)
               ENDIF
               k = dlog(dsqrt(delta(1)**2+delta(2)**2)/(a*Eps)**2)                                                                  &
                 & /dabs(dlog(dsqrt(Lam1(1)**2+Lam1(2)**2)/dsqrt(lam2(1)**2+lam2(2)**2))) + 1.
               k = k/2
               IF ( idiag/=0 ) THEN
                  WRITE (Nout,99005) k
99005             FORMAT (I5)
               ENDIF
               ir1 = float(k-3)*float(timeit)/float(Iter)
               IF ( Timed>=ir1 ) CYCLE
               Lambda(1) = Lambda(1) + Lam1(1)
               Lambda(2) = Lambda(2) + Lam1(2)
!
!     STORE THE LAST VECTOR BEFORE A SHIFT FOR USE AS A STARTING VECTOR
!
               IF ( Switch==1 ) THEN
                  in1 = jj5
               ELSE
                  in1 = ii1
               ENDIF
               ifile = Filevc(1)
               CALL gopen(ifile,Z(iobuf),Wrt)
               CALL write(ifile,Dz(in1),ncol4,1)
               Ivect = 1
               Comflg = 1
!
!     STORE  THE CURRENT VECTOR ON THE EIGENVECTOR FILE SO IT CAN BE
!     USED AS THE STARTING VECTOR
!
               CALL write(ifile,Dz(ii1),ncol4,1)
               CALL close(ifile,Eofnrw)
               EXIT
            ENDIF
         ENDIF
!
!     M  RAPID CONVERGENCE MAKE SURE LAMD1 PASSES EP1 TEST
!
         IF ( dsqrt((Lam1(1)-lm1nm1(1))**2+(Lam1(2)-lm1nm1(2))**2)/dsqrt((Lambda(1)+dabs(Lam1(1)))**2+(Lambda(2)+dabs(Lam1(2)))**2) &
            & >=ep1 ) CYCLE
!
!     CONVERGENCE ACHIEVED, NORMALIZE THE VECTOR
!
!     STORE THE EIGENVECTOR AND EIGENVALUE ON THE OUTPUT FILES
!
         CALL cnorm1(Dz(ii1),Ncol)
         Lam1(1) = Lam1(1) + Lambda(1)
         Lam1(2) = Lam1(2) + Lambda(2)
         inu = ii1 + ncol2 - 1
         IF ( idiag/=0 ) THEN
            WRITE (Nout,99006) Lam1 , (Dz(i),i=ii1,inu)
99006       FORMAT (1H1,20H CONVERGENCE        ,//,' LAMBDA = ',2D15.5,//,(10D12.4))
         ENDIF
         ifile = Filevc(1)
         CALL gopen(ifile,Z(iobuf),Wrt)
         CALL write(ifile,Dz(ii1),ncol4,1)
         CALL close(ifile,Eofnrw)
         ifile = Filelm(1)
         CALL gopen(ifile,Z(iobuf),Wrt)
         CALL write(ifile,Lam1(1),4,1)
         CALL close(ifile,Eofnrw)
         Northo = Northo + 1
         Noroot = Noroot + 1
         iep2 = 0
         irapid = 0
         Nochng = 0
         Comflg = 0
         IF ( Switch==0 ) THEN
            Ivect = 0
            IF ( Iter<=5 ) GOTO 120
         ELSE
            Switch = 0
            Lambda(1) = Lmbda(1)
            Lambda(2) = Lmbda(2)
         ENDIF
         in1 = jj5
         ifile = Filevc(1)
         CALL gopen(ifile,Z(iobuf),Wrt)
         CALL write(ifile,Dz(in1),ncol4,1)
         CALL close(ifile,Eofnrw)
         Ivect = 1
 120     Iter = 0
!
!     COMPUTE PSEUDO LEFT VECTOR
!
         CALL cmtimu(Dz(ii1),Dz(jj3),0,Z(iobuf))
         IF ( Fileb(1)/=0 ) THEN
            CALL cmtimu(Dz(ii1),Dz(jj2),Fileb,Z(ibuf1))
            con1(1) = 2.0D0*Lam1(1)
            con1(2) = 2.0D0*Lam1(2)
            con2(1) = -1.0D0
            con2(2) = 0.0D0
            CALL csub(Dz(jj3),Dz(jj2),Dz(jj3),con1,con2)
         ENDIF
         IF ( Isym/=1 ) THEN
!
!     LEFT = RIGHT FINISH JOB
!
            CALL cxtrny(Dz(ii1),Dz(jj3),con1)
            CALL cdivid(Dz(ii1),Dz(jj3),con1,ncol2)
         ENDIF
!
!     PUT SCALED VECTOR ON LEFT VECTOR FILE
!
         ifile = Scrfil(10)
         CALL gopen(ifile,Z(ibuf1),Wrt)
         CALL write(ifile,Dz(jj3),ncol4,1)
         CALL close(ifile,Eofnrw)
         Left = 1
         IF ( Isym/=0 ) EXIT
         GOTO 100
      ENDIF
   ENDDO
!
! 490 CALL CLOSE (FILEL,1)
!     CALL CLOSE (FILEU,1)
!     CALL CLOSE (FILEM,1)
 200  RETURN
 300  IF ( Nodes<=Noroot ) THEN
      Comflg = 6
      GOTO 200
   ELSEIF ( Lam1(1)**2+Lam1(2)**2>=Maxmod ) THEN
!
!     ONE OR MORE ROOTS OUTSIDE REGION
!
      Comflg = 7
      GOTO 200
   ENDIF
!
!     GET NEW STARTING  POINT
!
 400  Comflg = 0
   Ind = -Ind
   GOTO 200
99999 RETURN
END SUBROUTINE cinvp3
