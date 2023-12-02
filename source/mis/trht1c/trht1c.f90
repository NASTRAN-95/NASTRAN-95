!*==trht1c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trht1c(Ngroup,Udvt,Pd,Rdd,Iloop)
USE C_BLANK
USE C_FBSX
USE C_INFBSX
USE C_PACKX
USE C_SYSTEM
USE C_TRDD1
USE C_TRDXX
USE C_TRHTX
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
         iscr5 = Icr5
         noload = 0
         nbust = 0
         mcb(1) = Pd
         CALL rdtrl(mcb)
         IF ( mcb(1)<=0 ) noload = -1
         nrow = Ik(2)
         It1 = 1
         It2 = 1
         Ii = 1
         Jj = nrow
         Incr = 1
         It3 = 1
         Iii = 1
         Jjj = nrow
         Incr1 = 1
         Tabs1 = Tabs
         Sigma1 = Sigma
         Nz = korsz(Z)
         igroup = Nz - 3*Ngroup + 1
         ibuf1 = igroup - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
         ibuf5 = ibuf4 - sysbuf
         ibuf6 = ibuf5 - sysbuf
         ibuf7 = ibuf6 - sysbuf
         ibuf8 = ibuf7 - sysbuf
         Nz = ibuf8 - 1
         Iloop1 = Iloop
         Ist = 0
         Ill1(1) = Icr1
         CALL rdtrl(Ill1)
         ifn(1) = Icr1
         CALL rdtrl(ifn)
         Iu1 = 0
         iu2 = Iu1 + nrow
         ip1 = iu2 + nrow
         ip2 = ip1 + nrow
         iuk = ip2 + nrow
         nolin = 0
         IF ( Nlftp1/=0 .OR. Norad/=-1 ) nolin = 1
         IF ( nolin==0 ) THEN
!
!     NO NON-LINEAR EFFECTS
!
            Nz = Nz - 4*nrow
            In2 = ip2
         ELSE
            in1 = iuk + nrow
            In2 = in1 + nrow
            Nz = Nz - 7*nrow
         ENDIF
         IF ( Nz<0 ) CALL mesage(-8,0,name)
         Icore = In2 + nrow
         Iul1(1) = Icr2
         CALL rdtrl(Iul1)
         ombeta = 1.0 - Beta
         opbeta = 1.0 + Beta
!
!     SET UP FOR CORE I/O
!
         IF ( Nlftp1/=0 ) THEN
            Ifrst = 0
            CALL trd1d
            Ifrst = 1
         ENDIF
         itab(1) = A
         itab(2) = Ill1(1)
         itab(3) = Iul1(1)
         itab(4) = Rdd
         icor = In2 + nrow + 1
         nf = 4
         CALL gopen(A,iz(ibuf2),0)
         CALL rewind(A)
         IF ( nolin/=0 .AND. Radlin==-1 .AND. Norad/=-1 ) THEN
            CALL gopen(Rdd,iz(ibuf7),0)
            CALL rewind(Rdd)
         ENDIF
         CALL gopen(Ill1,iz(ibuf3),0)
         CALL rewind(Ill1)
         IF ( Isym/=1 ) THEN
            CALL gopen(Iul1,iz(ibuf4),0)
            CALL rewind(Iul1)
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
            delta1 = Z(newgrp-2)
            Nstep = iz(newgrp)
            Deltat = Z(newgrp+1)
            Nout = iz(newgrp+2)
            CALL gopen(Pd,iz(ibuf5),2)
            h = 1.0/Deltat
            CALL gopen(Udvt,iz(ibuf1),3)
            mcb(1) = Udvt
            CALL rdtrl(mcb(1))
            IF ( nolin/=0 ) THEN
               CALL gopen(Pnl1,iz(ibuf6),3)
               Ipnl(1) = Pnl1
               CALL rdtrl(Ipnl)
            ENDIF
!
!     RESTORE  STUFF  SAVED
!
            IF ( nolin/=0 ) CALL fread(iscr5,Z(iuk+1),nrow,1)
            CALL fread(iscr5,Z(iu2+1),nrow,1)
            CALL fread(iscr5,Z(Iu1+1),nrow,1)
            IF ( nolin/=0 ) THEN
               CALL fread(iscr5,Z(in1+1),nrow,1)
               CALL fread(iscr5,Z(In2+1),nrow,1)
            ENDIF
            CALL close(iscr5,1)
!
!     COMPUTE  PBAR
!
            DO i = 1 , nrow
               l = ip1 + i
               Z(l) = 0.0
               IF ( nolin/=0 ) THEN
                  m = In2 + i
                  Z(l) = -Z(m)
               ENDIF
            ENDDO
            iopen = 0
            CALL matvec(Z(Iu1+1),Z(ip1+1),Ik,iz(ibuf8))
            IF ( Ib(1)/=0 ) THEN
               DO i = 1 , nrow
                  l = iu2 + i
                  m = Iu1 + i
                  Z(l) = (Z(m)-Z(l))/delta1
               ENDDO
               iopen = 0
               CALL matvec(Z(iu2+1),Z(ip1+1),Ib,iz(ibuf8))
            ENDIF
            IF ( nolin/=0 ) THEN
               h1 = 1.0 - Deltat/delta1
               h2 = Deltat/delta1
               DO i = 1 , nrow
                  l = in1 + i
                  m = In2 + i
                  Z(l) = h2*Z(l) + h1*Z(m)
               ENDDO
            ENDIF
            Icount = 0
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( noload==0 ) THEN
               CALL gopen(Pd,iz(ibuf5),0)
               CALL fwdrec(*100,Pd)
            ENDIF
            Ist = -1
            CALL gopen(Icr5,iz(ibuf1),0)
!
            CALL fread(Icr5,iz(igroup),3*Ngroup,1)
!
!     BRING IN  U0 AND UK
!
            CALL read(*120,*20,Icr5,Z(Iu1+1),nrow,1,nwds)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SHORT VECTOR ENCOUNTERED
!
 20      k = nwds + 1
         DO l = k , nrow
            m = Iu1 + l
            Z(m) = 0.0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Norad==-1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*120,*40,Icr5,Z(iuk+1),nrow,1,nwds)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     SHORT VECTOR ENCOUNTERED
!
 40      k = nwds + 1
         DO l = k , nrow
            m = iuk + l
            Z(m) = 0.0
         ENDDO
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         CALL close(Icr5,1)
         Nstep = iz(igroup) + 1
         Deltat = Z(igroup+1)
         Nout = iz(igroup+2)
         h = 1.0/Deltat
         CALL gopen(Udvt,iz(ibuf1),1)
         CALL makmcb(mcb,Udvt,nrow,2,1)
         IF ( nolin/=0 ) THEN
            CALL gopen(Pnl1,iz(ibuf6),1)
            CALL makmcb(Ipnl,Pnl1,nrow,2,1)
         ENDIF
!
!     LETS  GO
!
         Icount = 1
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
            IF ( Norad==-1 ) THEN
               IF ( Nlftp1==0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , nrow
                  m = In2 + i
                  Z(m) = 0.0
               ENDDO
            ELSEIF ( Radlin==-1 ) THEN
!
!     NON-CONSTANT RADIATION
!
               DO i = 1 , nrow
                  l = Iu1 + i
                  k = iuk + i
                  m = In2 + i
                  j = iu2 + i
!
!     CHECK FOR UNSTABLE SOLUTION ABOUT TO CAUSE ARITHMETIC OVERFLOWS.
!
                  IF ( Z(l)>=1.0E8 ) THEN
                     nbust = nbust + 1
                     IF ( nbust>10 ) THEN
                        WRITE (nprt,99001) Ufm
99001                   FORMAT (A23,' 3103, SUBROUTINE TRHT1C TERMINATING DUE TO ERROR ','COUNT FOR MESSAGE 3102.')
                        CALL mesage(-61,0,name)
                     ELSE
                        WRITE (nprt,99002) Uwm , Z(l) , Icount , i
99002                   FORMAT (A25,' 3102, SUBROUTINE TRHT1C, UNSTABLE TEMP. VALUE OF',E20.8,' COMPUTED FOR TIME STEP',I5,/5X,     &
                               &'AT POINT NUMBER',I6,' IN THE ANALYSIS SET.')
                        Z(l) = 1.0E6
                     ENDIF
                  ENDIF
!
                  Z(j) = -(Z(l)+Tabs)**4 + 4.0*(Z(k)+Tabs)**3*Z(l)
                  Z(m) = 0.0
               ENDDO
               iopen = 1
               ifn(1) = Rdd
               CALL matvec(Z(iu2+1),Z(In2+1),ifn,iz(ibuf7))
            ELSE
               DO i = 1 , nrow
                  l = In2 + i
                  k = iuk + i
                  Z(l) = Z(k)
               ENDDO
            ENDIF
            IF ( Nlftp1/=0 ) THEN
               Tim1 = tim
               CALL trd1d
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Icount==1 .AND. Iloop==1 ) THEN
            DO i = 1 , nrow
               k = ip1 + i
               Z(k) = 0.0
               IF ( nolin/=0 ) THEN
                  l = In2 + i
                  m = in1 + i
                  Z(m) = Z(l)
                  Z(k) = -Z(l)
               ENDIF
            ENDDO
            iopen = 0
            CALL matvec(Z(Iu1+1),Z(ip1+1),Ik,Z(ibuf8))
         ENDIF
!
!     BRING IN  NEXT P
!
         IF ( noload==0 ) THEN
            CALL unpack(*60,Pd,Z(ip2+1))
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      DO i = 1 , nrow
            k = ip2 + i
            Z(k) = 0.0
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
!
!     ADD ALL LOAD CONTRIBUTIONS
!
         DO i = 1 , nrow
            l = ip1 + i
            m = ip2 + i
            Z(l) = ombeta*Z(l) + Beta*Z(m)
            IF ( nolin/=0 ) THEN
               m = in1 + i
               j = In2 + i
               Z(l) = Z(l) + opbeta*Z(j) - Beta*Z(m)
            ENDIF
         ENDDO
!
!     MULTIPLY  IN  A MATRIX
!
         iopen = 1
         ifn(1) = A
         CALL matvec(Z(Iu1+1),Z(ip1+1),ifn,iz(ibuf2))
!
!     SOLVE  FOR NEXT DISPLACEMENT
!
         iopen = 1
         IF ( Isym==0 ) CALL intfbs(Z(ip1+1),Z(iu2+1),iz(ibuf4))
         IF ( Isym==1 ) THEN
!
!     ABSORBED SUBROUTINE FBSINT   SEE ALSO EQUIV.   DATA.
!
            DO i = 1 , mrow
               Z(i+iu2) = Z(i+ip1)
            ENDDO
!
!     FORWARD PASS
!
            CALL rewind(Ill1)
            CALL fwdrec(*80,Ill1)
            iz(ibuf4) = Ill1(1)
            Ll1(1) = Ill1(1)
            CALL rdtrl(Ll1)
            IF ( iprec/=1 ) THEN
               CALL fbs21(iz(ibuf4),Z(iu2+1),Z(iu2+1),mrow)
            ELSE
               CALL fbs1(iz(ibuf4),Z(iu2+1),Z(iu2+1),mrow)
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 80      CALL mesage(-2,Ill1,name)
         spag_nextblock_1 = 7
      CASE (7)
!
!     ABSORBED SUBROUTINE FBSINT    SEE ALSO EQUIV.   DATA.
!
         IF ( Icount==1 .OR. Icount==Nstep .OR. mod(Icount+Ist,Nout)==0 ) THEN
!
!     IT  IS OUTPUT TIME
!
            CALL pack(Z(Iu1+1),Udvt,mcb)
!
!     COMPUTE  U DOT
!
            DO i = 1 , nrow
               l = ip1 + i
               m = Iu1 + i
               j = iu2 + i
               Z(l) = (Z(j)-Z(m))*h
            ENDDO
            CALL pack(Z(ip1+1),Udvt,mcb)
!
!     PUT OUT ZERO ACCERERATION VECTOR FOR LATER MODULES
!
            CALL bldpk(1,1,Udvt,0,0)
            CALL bldpkn(Udvt,0,mcb)
            IF ( nolin/=0 ) CALL pack(Z(In2+1),Pnl1,Ipnl)
         ENDIF
!
!     ROTATE POINTERS
!
         j = ip1
         ip1 = ip2
         ip2 = j
         j = Iu1
         Iu1 = iu2
         iu2 = j
         j = in1
         in1 = In2
         In2 = j
         tim = tim + Deltat
         Icount = Icount + 1
         IF ( Icount<Nstep ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Icount==Nstep ) THEN
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
            CALL write(iscr5,Z(iu2+1),nrow,1)
!
!     SAVE   UI
!
            CALL write(iscr5,Z(Iu1+1),nrow,1)
            IF ( nolin/=0 ) THEN
!
!     SAVE    NI - 1
!
               CALL write(iscr5,Z(In2+1),nrow,1)
!
!     SAVE    NI
!
               CALL write(iscr5,Z(in1+1),nrow,1)
            ENDIF
            CALL close(iscr5,1)
         ELSE
            j = 1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         CALL close(Udvt,j)
         CALL close(Pd,j)
         CALL close(Ill1,1)
         CALL close(Iul1,1)
         CALL close(A,1)
         CALL wrttrl(mcb)
         IF ( Norad/=-1 ) CALL close(Rdd,1)
         IF ( nolin/=0 ) THEN
            CALL close(Pnl1,j)
            CALL wrttrl(Ipnl)
         ENDIF
         RETURN
      CASE (9)
!
!     CONSTANT RADIATION
!
         IF ( Radlin/=-1 ) THEN
            DO i = 1 , nrow
               l = iuk + i
               k = In2 + i
               Z(l) = -(Z(l)+Tabs)**4 + 4.0*(Z(l)+Tabs)**3*Z(l)
               Z(k) = 0.0
            ENDDO
            iopen = 1
            ifn(1) = Rdd
            CALL matvec(Z(iuk+1),Z(In2+1),ifn,iz(ibuf7))
            DO i = 1 , nrow
               l = iuk + i
               m = In2 + i
               Z(l) = Z(m)
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
 120     file = Icr5
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(-2,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trht1c
