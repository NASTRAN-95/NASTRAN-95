!*==trht1c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trht1c(Ngroup,Udvt,Pd,Rdd,Iloop)
   USE c_blank
   USE c_fbsx
   USE c_infbsx
   USE c_packx
   USE c_system
   USE c_trdd1
   USE c_trdxx
   USE c_trhtx
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ngroup
   INTEGER :: Udvt
   INTEGER :: Pd
   INTEGER :: Rdd
   INTEGER :: Iloop
!
! Local variable declarations rewritten by SPAG
!
   REAL :: delta1 , h , h1 , h2 , ombeta , opbeta , tim
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: file , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibuf5 , ibuf6 , ibuf7 , ibuf8 , icor , igroup , in1 , iopen , ip1 , ip2 ,  &
            & iprec , iscr5 , itleft , iu2 , iuk , j , k , l , m , mrow , nbust , newgrp , nf , nolin , noload , nprt , nrow ,      &
            & nwds , sysbuf
   INTEGER , DIMENSION(7) :: ifn , mcb
   INTEGER , DIMENSION(4) :: itab
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bldpk , bldpkn , close , fbs1 , fbs21 , fread , fwdrec , gopen , intfbs , korsz , makmcb , matvec , mesage , pack ,     &
          & rdtrl , read , rewind , tmtogo , trd1d , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE  STEPS INTEGRATION PROCEDURE
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Ktrdxx(28),Iopen) , (Z(1),Iz(1),Dz(1)) , (Ill1(3),Mrow) , (Ksystm(2),Nprt)
   DATA name/4HTRHT , 4H1C  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SYMBOL TABLE
!
!     ICR1 IS LLL
!     ICR2 IS ULL
!     ICR5 IS INITIAL CONDITIONS
!     ICR6 IS THE  A  MATRIX
!
!     NROW     PROBLEM ORDER
!     NGROUP   NUMBER OF TRIPLES OF TIME STEPS
!     UDVT     DISPLACEMENTS AND VELOCITIES
!     PD       LOADS
!     RDD      RADIATION MATRIX
!     ILOOP    CURRENT TIME STEP GROUP
!     IBUF1    UDVT BUFFER
!     IBUF2    A    BUFFER
!     IBUF3    LLL  BUFFER
!     IBUF4    ULL  BUFFER
!     IBUF5    PD   BUFFER
!     IBUF6    PNL1 BUFFER
!     IBUF7    RDD  BUFFER
!     IBUF8    SCRATCH BUFFER(DIT,NLLOADS,SAVE STUFF ETC)
!     NZ       OPEN CORE
!     IST      OUTPUT FLAG
!     IU1,IU2  DISPLACMENT VECTOR POINTERS
!     IP1,IP2  LOAD VECTOR POINTERS
!     IN1,IN2  NON-LINEAR LOAD POINTERS
!     NOLIN    =0  MEAN NO NON-LINEAR LOADS
!     IPNT     POINTER FOR INTERNAL ZERO ROUTINE
!     FILE     FILE    FOR INTERNAL ZERO ROUTINE
!     NSTEP    NUMBER OF TIME STEPS
!     DELTAT   DELTA  T
!     NOUT     OUTPUT INCREMENT
!     H        1/ 2*DELTAT
!     ICOUNT   STEP COUNTER
!     ITLEFT   TIME LEFT
!     NORAD    =-1  NO RADIATION
!     RADLIN   =-1  NON LINEAR RADIATION
!     NLFTP1   NONLINEAR SET SELECTED BY THE USER
!     BETA,OMBETA,OPBETA  --USER BETA 1-BETA, 1+BETA
!     ISYM     0    UNSYMETRIC   1  SYMMETRIC
!     DELTA1   OLD DELTA  T
!
         iscr5 = icr5
         noload = 0
         nbust = 0
         mcb(1) = Pd
         CALL rdtrl(mcb)
         IF ( mcb(1)<=0 ) noload = -1
         nrow = ik(2)
         it1 = 1
         it2 = 1
         ii = 1
         jj = nrow
         incr = 1
         it3 = 1
         iii = 1
         jjj = nrow
         incr1 = 1
         tabs1 = tabs
         sigma1 = sigma
         nz = korsz(z)
         igroup = nz - 3*Ngroup + 1
         ibuf1 = igroup - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
         ibuf5 = ibuf4 - sysbuf
         ibuf6 = ibuf5 - sysbuf
         ibuf7 = ibuf6 - sysbuf
         ibuf8 = ibuf7 - sysbuf
         nz = ibuf8 - 1
         iloop1 = Iloop
         ist = 0
         ill1(1) = icr1
         CALL rdtrl(ill1)
         ifn(1) = icr1
         CALL rdtrl(ifn)
         iu1 = 0
         iu2 = iu1 + nrow
         ip1 = iu2 + nrow
         ip2 = ip1 + nrow
         iuk = ip2 + nrow
         nolin = 0
         IF ( nlftp1/=0 .OR. norad/=-1 ) nolin = 1
         IF ( nolin==0 ) THEN
!
!     NO NON-LINEAR EFFECTS
!
            nz = nz - 4*nrow
            in2 = ip2
         ELSE
            in1 = iuk + nrow
            in2 = in1 + nrow
            nz = nz - 7*nrow
         ENDIF
         IF ( nz<0 ) CALL mesage(-8,0,name)
         icore = in2 + nrow
         iul1(1) = icr2
         CALL rdtrl(iul1)
         ombeta = 1.0 - beta
         opbeta = 1.0 + beta
!
!     SET UP FOR CORE I/O
!
         IF ( nlftp1/=0 ) THEN
            ifrst = 0
            CALL trd1d
            ifrst = 1
         ENDIF
         itab(1) = a
         itab(2) = ill1(1)
         itab(3) = iul1(1)
         itab(4) = Rdd
         icor = in2 + nrow + 1
         nf = 4
         CALL gopen(a,iz(ibuf2),0)
         CALL rewind(a)
         IF ( nolin/=0 .AND. radlin==-1 .AND. norad/=-1 ) THEN
            CALL gopen(Rdd,iz(ibuf7),0)
            CALL rewind(Rdd)
         ENDIF
         CALL gopen(ill1,iz(ibuf3),0)
         CALL rewind(ill1)
         IF ( isym/=1 ) THEN
            CALL gopen(iul1,iz(ibuf4),0)
            CALL rewind(iul1)
         ENDIF
!
!     IS  THIS  A TIME  STEP CHANGE
!
         IF ( Iloop/=1 ) THEN
!
!     REENTRY FROM CHANGE OF TIME STEP
!
            CALL gopen(iscr5,iz(ibuf8),0)
            CALL fread(iscr5,iz(igroup),3*Ngroup,1)
            newgrp = igroup + (Iloop-1)*3
            delta1 = z(newgrp-2)
            nstep = iz(newgrp)
            deltat = z(newgrp+1)
            nout = iz(newgrp+2)
            CALL gopen(Pd,iz(ibuf5),2)
            h = 1.0/deltat
            CALL gopen(Udvt,iz(ibuf1),3)
            mcb(1) = Udvt
            CALL rdtrl(mcb(1))
            IF ( nolin/=0 ) THEN
               CALL gopen(pnl1,iz(ibuf6),3)
               ipnl(1) = pnl1
               CALL rdtrl(ipnl)
            ENDIF
!
!     RESTORE  STUFF  SAVED
!
            IF ( nolin/=0 ) CALL fread(iscr5,z(iuk+1),nrow,1)
            CALL fread(iscr5,z(iu2+1),nrow,1)
            CALL fread(iscr5,z(iu1+1),nrow,1)
            IF ( nolin/=0 ) THEN
               CALL fread(iscr5,z(in1+1),nrow,1)
               CALL fread(iscr5,z(in2+1),nrow,1)
            ENDIF
            CALL close(iscr5,1)
!
!     COMPUTE  PBAR
!
            DO i = 1 , nrow
               l = ip1 + i
               z(l) = 0.0
               IF ( nolin/=0 ) THEN
                  m = in2 + i
                  z(l) = -z(m)
               ENDIF
            ENDDO
            iopen = 0
            CALL matvec(z(iu1+1),z(ip1+1),ik,iz(ibuf8))
            IF ( ib(1)/=0 ) THEN
               DO i = 1 , nrow
                  l = iu2 + i
                  m = iu1 + i
                  z(l) = (z(m)-z(l))/delta1
               ENDDO
               iopen = 0
               CALL matvec(z(iu2+1),z(ip1+1),ib,iz(ibuf8))
            ENDIF
            IF ( nolin/=0 ) THEN
               h1 = 1.0 - deltat/delta1
               h2 = deltat/delta1
               DO i = 1 , nrow
                  l = in1 + i
                  m = in2 + i
                  z(l) = h2*z(l) + h1*z(m)
               ENDDO
            ENDIF
            icount = 0
            spag_nextblock_1 = 4
         ELSE
            IF ( noload==0 ) THEN
               CALL gopen(Pd,iz(ibuf5),0)
               CALL fwdrec(*100,Pd)
            ENDIF
            ist = -1
            CALL gopen(icr5,iz(ibuf1),0)
!
            CALL fread(icr5,iz(igroup),3*Ngroup,1)
!
!     BRING IN  U0 AND UK
!
            CALL read(*120,*20,icr5,z(iu1+1),nrow,1,nwds)
            spag_nextblock_1 = 2
         ENDIF
         CYCLE
!
!     SHORT VECTOR ENCOUNTERED
!
 20      k = nwds + 1
         DO l = k , nrow
            m = iu1 + l
            z(m) = 0.0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         IF ( norad==-1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*120,*40,icr5,z(iuk+1),nrow,1,nwds)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     SHORT VECTOR ENCOUNTERED
!
 40      k = nwds + 1
         DO l = k , nrow
            m = iuk + l
            z(m) = 0.0
         ENDDO
         spag_nextblock_1 = 9
      CASE (3)
         CALL close(icr5,1)
         nstep = iz(igroup) + 1
         deltat = z(igroup+1)
         nout = iz(igroup+2)
         h = 1.0/deltat
         CALL gopen(Udvt,iz(ibuf1),1)
         CALL makmcb(mcb,Udvt,nrow,2,1)
         IF ( nolin/=0 ) THEN
            CALL gopen(pnl1,iz(ibuf6),1)
            CALL makmcb(ipnl,pnl1,nrow,2,1)
         ENDIF
!
!     LETS  GO
!
         icount = 1
         spag_nextblock_1 = 4
      CASE (4)
!
!     TOP OF LOOP
!
         CALL tmtogo(itleft)
         IF ( itleft<=0 ) THEN
            j = 1
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     COMPUTE  NR
!
            IF ( norad==-1 ) THEN
               IF ( nlftp1==0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , nrow
                  m = in2 + i
                  z(m) = 0.0
               ENDDO
            ELSEIF ( radlin==-1 ) THEN
!
!     NON-CONSTANT RADIATION
!
               DO i = 1 , nrow
                  l = iu1 + i
                  k = iuk + i
                  m = in2 + i
                  j = iu2 + i
!
!     CHECK FOR UNSTABLE SOLUTION ABOUT TO CAUSE ARITHMETIC OVERFLOWS.
!
                  IF ( z(l)>=1.0E8 ) THEN
                     nbust = nbust + 1
                     IF ( nbust>10 ) THEN
                        WRITE (nprt,99001) ufm
99001                   FORMAT (A23,' 3103, SUBROUTINE TRHT1C TERMINATING DUE TO ERROR ','COUNT FOR MESSAGE 3102.')
                        CALL mesage(-61,0,name)
                     ELSE
                        WRITE (nprt,99002) uwm , z(l) , icount , i
99002                   FORMAT (A25,' 3102, SUBROUTINE TRHT1C, UNSTABLE TEMP. VALUE OF',E20.8,' COMPUTED FOR TIME STEP',I5,/5X,     &
                               &'AT POINT NUMBER',I6,' IN THE ANALYSIS SET.')
                        z(l) = 1.0E6
                     ENDIF
                  ENDIF
!
                  z(j) = -(z(l)+tabs)**4 + 4.0*(z(k)+tabs)**3*z(l)
                  z(m) = 0.0
               ENDDO
               iopen = 1
               ifn(1) = Rdd
               CALL matvec(z(iu2+1),z(in2+1),ifn,iz(ibuf7))
            ELSE
               DO i = 1 , nrow
                  l = in2 + i
                  k = iuk + i
                  z(l) = z(k)
               ENDDO
            ENDIF
            IF ( nlftp1/=0 ) THEN
               tim1 = tim
               CALL trd1d
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( icount==1 .AND. Iloop==1 ) THEN
            DO i = 1 , nrow
               k = ip1 + i
               z(k) = 0.0
               IF ( nolin/=0 ) THEN
                  l = in2 + i
                  m = in1 + i
                  z(m) = z(l)
                  z(k) = -z(l)
               ENDIF
            ENDDO
            iopen = 0
            CALL matvec(z(iu1+1),z(ip1+1),ik,z(ibuf8))
         ENDIF
!
!     BRING IN  NEXT P
!
         IF ( noload==0 ) THEN
            CALL unpack(*60,Pd,z(ip2+1))
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      DO i = 1 , nrow
            k = ip2 + i
            z(k) = 0.0
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
!
!     ADD ALL LOAD CONTRIBUTIONS
!
         DO i = 1 , nrow
            l = ip1 + i
            m = ip2 + i
            z(l) = ombeta*z(l) + beta*z(m)
            IF ( nolin/=0 ) THEN
               m = in1 + i
               j = in2 + i
               z(l) = z(l) + opbeta*z(j) - beta*z(m)
            ENDIF
         ENDDO
!
!     MULTIPLY  IN  A MATRIX
!
         iopen = 1
         ifn(1) = a
         CALL matvec(z(iu1+1),z(ip1+1),ifn,iz(ibuf2))
!
!     SOLVE  FOR NEXT DISPLACEMENT
!
         iopen = 1
         IF ( isym==0 ) CALL intfbs(z(ip1+1),z(iu2+1),iz(ibuf4))
         IF ( isym==1 ) THEN
!
!     ABSORBED SUBROUTINE FBSINT   SEE ALSO EQUIV.   DATA.
!
            DO i = 1 , mrow
               z(i+iu2) = z(i+ip1)
            ENDDO
!
!     FORWARD PASS
!
            CALL rewind(ill1)
            CALL fwdrec(*80,ill1)
            iz(ibuf4) = ill1(1)
            ll1(1) = ill1(1)
            CALL rdtrl(ll1)
            IF ( iprec/=1 ) THEN
               CALL fbs21(iz(ibuf4),z(iu2+1),z(iu2+1),mrow)
            ELSE
               CALL fbs1(iz(ibuf4),z(iu2+1),z(iu2+1),mrow)
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 80      CALL mesage(-2,ill1,name)
         spag_nextblock_1 = 7
      CASE (7)
!
!     ABSORBED SUBROUTINE FBSINT    SEE ALSO EQUIV.   DATA.
!
         IF ( icount==1 .OR. icount==nstep .OR. mod(icount+ist,nout)==0 ) THEN
!
!     IT  IS OUTPUT TIME
!
            CALL pack(z(iu1+1),Udvt,mcb)
!
!     COMPUTE  U DOT
!
            DO i = 1 , nrow
               l = ip1 + i
               m = iu1 + i
               j = iu2 + i
               z(l) = (z(j)-z(m))*h
            ENDDO
            CALL pack(z(ip1+1),Udvt,mcb)
!
!     PUT OUT ZERO ACCERERATION VECTOR FOR LATER MODULES
!
            CALL bldpk(1,1,Udvt,0,0)
            CALL bldpkn(Udvt,0,mcb)
            IF ( nolin/=0 ) CALL pack(z(in2+1),pnl1,ipnl)
         ENDIF
!
!     ROTATE POINTERS
!
         j = ip1
         ip1 = ip2
         ip2 = j
         j = iu1
         iu1 = iu2
         iu2 = j
         j = in1
         in1 = in2
         in2 = j
         tim = tim + deltat
         icount = icount + 1
         IF ( icount<nstep ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( icount==nstep ) THEN
!
!     END OF 1 GROUP
!
            IF ( Iloop==Ngroup ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     MORE GROUPS TO COME  SAVE STUFF
!
            j = 2
            CALL gopen(iscr5,iz(ibuf8),1)
            CALL write(iscr5,iz(igroup),3*Ngroup,1)
            IF ( nolin/=0 ) CALL write(iscr5,iz(iuk+1),nrow,1)
!
!     SAVE   UI -1
!
            CALL write(iscr5,z(iu2+1),nrow,1)
!
!     SAVE   UI
!
            CALL write(iscr5,z(iu1+1),nrow,1)
            IF ( nolin/=0 ) THEN
!
!     SAVE    NI - 1
!
               CALL write(iscr5,z(in2+1),nrow,1)
!
!     SAVE    NI
!
               CALL write(iscr5,z(in1+1),nrow,1)
            ENDIF
            CALL close(iscr5,1)
         ELSE
            j = 1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         CALL close(Udvt,j)
         CALL close(Pd,j)
         CALL close(ill1,1)
         CALL close(iul1,1)
         CALL close(a,1)
         CALL wrttrl(mcb)
         IF ( norad/=-1 ) CALL close(Rdd,1)
         IF ( nolin/=0 ) THEN
            CALL close(pnl1,j)
            CALL wrttrl(ipnl)
         ENDIF
         RETURN
      CASE (9)
!
!     CONSTANT RADIATION
!
         IF ( radlin/=-1 ) THEN
            DO i = 1 , nrow
               l = iuk + i
               k = in2 + i
               z(l) = -(z(l)+tabs)**4 + 4.0*(z(l)+tabs)**3*z(l)
               z(k) = 0.0
            ENDDO
            iopen = 1
            ifn(1) = Rdd
            CALL matvec(z(iuk+1),z(in2+1),ifn,iz(ibuf7))
            DO i = 1 , nrow
               l = iuk + i
               m = in2 + i
               z(l) = z(m)
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     I/O ERROR
!
 100     file = Pd
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 120     file = icr5
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(-2,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trht1c
