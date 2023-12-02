!*==feer.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer
   USE c_blank
   USE c_feercx
   USE c_feerxx
   USE c_names
   USE c_ntime
   USE c_opinv
   USE c_packx
   USE c_reigkr
   USE c_sturmx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: drsm , drsn , dsm , epxm , scale
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , ibuf1 , ibuf2 , ifk , ifl , ifm , ij , ik , io , iprec , ising , it , j , jj , mrk , nbpw , nodcmp , npr , nt ,   &
            & ntms , ntot , ntz , nz , sysbuf , t1 , t2 , t3 , timet
   INTEGER , SAVE :: i0 , i1 , i2 , i3 , i4 , ibegn , iend , mode
   INTEGER , DIMENSION(2) , SAVE :: icr , jcr
   INTEGER , DIMENSION(12) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(3) , SAVE :: name
   REAL :: sp , xi , xm , xmp , xn , xt
   REAL , DIMENSION(4) :: tml , tmt
   EXTERNAL close , conmsg , feer1 , feer2 , feer3 , feer4 , feerdd , frmax , gopen , klock , korsz , makmcb , mesage , pack ,      &
          & page2 , rdtrl , sswtch , tmtogo , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     DRIVER FOR THE FEER (FAST EIGENVALUE EXTRACTION ROUTINE) METHOD.
!     THIS ROUTINE WAS CALLED FCNTL BEFORE
!
!     GIVEN A REAL SYMETRIC MATRIX, FEER WILL SOLVE FOR THE EIGENVALUES
!     AND EIGENVECTORS AROUND THE CENTER OF INTEREST
!
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!
!     IFKAA(7) = 101, MATRIX GINO BLOCK FOR THE INPUT STIFFNESS MATRIX K
!     IFMAA(7) = 102, MATRIX GINO BLOCK FOR THE INPUT MASS MATRIX M
!     IFLELM(7)= 201, MATRIX GINO BLOCK FOR THE OUTPUT EIGENVALUES
!     IFLVEC(7)= 202, MATRIX GINO BLOCK FOR THE OUTPUT EIGENVECTORS
!            ? = 203
!     DMPFLE   = 204, EIGENVALUE SUMMARY FILE
!     SR1FLE-SR8FLE = 301-308, SCRATCH FILES REQUIRED INTERNALLY
!     XLMBDA   =  INPUT, CENTER OF RANGE OF INTEREST.
!                 (USER SPECIFIED SHIFT)
!     NEIG     =  NUMBER OF DESIRED EIGENVALUES AROUND THE CENTER
!                 OF INTEREST. (EIGENVALUES SPECIFIED BY USER)
!     NORD     =  PROBLEM SIZE (SET INTERNALLY USING THE DIMENSION OF
!                 THE STIFFNESS MATRIX)
!     MORD     =  ORDER OF THE REDUCED PROBLEM (SET INTERNALLY)
!     NORTHO   =  NO. OF ORTHOGONAL VECTORS IN PRESENT SET (INCLUDE
!                 PREVISOUSLY COMPUTED VECTORS)
!     EPXM     =  ZERO MASS CRITERIA TO DETERMINE RANK
!     EPX      =  ORTHOGONALITY CONVERGENCE CRITERIA
!     IBK      =  BUCKLING OPTION INDICATOR (SET INTERNALLY)
!     CRITF    =  THE USER SPECIFIED (OR DEFAULT) DESIRED THEORETICAL
!                 ACCURACY OF THE EIGENVALUES EXPRESSED AS A PERCENTAGE
!     LAMBDA   =  VALUE OF THE SHIFT ACTUALLY USED (D.P.)
!     CNDFLG   =  TERMINATION INDICATOR
!     ITER     =  NO. OF STARTING POINTS USED
!     IOPTF    =  SPECIFIED SHIFT OPTION INDICATOR, SET INTERNALLY
!     NOCHNG   =  THEORETICAL ERROR PARAMETER
!     IFSET    =  INTERNALLY COMPUTED SHIFT INDICTOR
!     NONUL    =  NO. OF VETOR ITERATIONS
!     MRANK    =  MATRIX RANK OF THE PROBLEM
!     IND,LMBDA,IDAIG = NOT ACTIVEATED
!
!     EIGENVALUES AND EIGENVECTORS WILL BE STORED ON THE ACTUAL SR1FLE
!     AND SR2FLE. THE SELECTION OF ACCURATE EIGENVALUES AND VECTORS WILL
!     PUT THEM ON IFLELM AND IFLVEC IN THE CORRECT SEQUENCE AT THE END
!     OF PROCESSING
!
!     IFLELM        CONTAINS (K+LAMBDA*M) OR KAA
!     IFLVEC        CONTAINS THE LOWER TRIANGLE L OR C
!     SR4FLE        IS USED AS SCRATCH IN SDCOMP
!     SR5FLE        IS USED AS SCRATCH IN SDCOMP
!     SR6FLE        IS USED AS SCRATCH IN SDCOMP
!     SR7FLE        CONTAINS THE VECTORS WHICH ARE USED TO ORTHOGONALIZE
!     SR8FLE        CONTAINS THE CONTITIONED MAA MATRIX
!     IFLRVA = 301
!     IFLRVC = 302
!     MCBLT         LOWER TRAINGULAR MATRIX L CONTROL BLOCK
!     MCBSMA        CONTITIONED MASTRIX M CONTROL BLOCK
!     MCBVEC        ORTHOGONAL VECTOR FILE CONTROL BLOCK
!     MCBRM         TRIAL VECTOR V OR C(INVERSE-TRANSPOSE)*V CONTROL
!                   BLOCK
!
   !>>>>EQUIVALENCE (Iz(1),Z(1),Dz(1)) , (Ksystm(1),Sysbuf) , (Ksystm(2),Io) , (Ksystm(55),Iprec) , (Tcons(8),Tmt(1)) ,                  &
!>>>>    & (Tcons(12),Tml(4)) , (Ksystm(40),Nbpw)
   DATA name/4HFEER , 2*2H  / , ibegn/4HBEGN/
   DATA iend/4HEND / , mode/4HMODE/
   DATA i1 , i2 , i3 , i4 , i0/1H1 , 1H2 , 1H3 , 1H4 , 1H /
   DATA icr/4HPASS , 4HFAIL/ , jcr/4HFREQ , 4HBUCK/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SET PRECISION DIGITS TO 12, ALL MACHINES (NEW 1/92)
!
         it = 12
         epx = 10.**(2-it)
         dsm = 10.0D0**(-2*it/3)
         name(3) = ibegn
         CALL conmsg(name,3,0)
         CALL feerdd
!
!     INITIALIZE FEERCX
!     DEFINITION OF INTERNAL PARAMETERS
!
         ibk = 0
         IF ( iprob(1)/=mode ) ibk = 1
         ioptf = ibk
         timed = 0
         timet = 0
         CALL sswtch(16,l16)
         IF ( l16==1 ) WRITE (io,99001)
99001    FORMAT (//,' *** DIAG16 - ALL TERMS USED ARE DESCRIBED IN ','PROGRAMMER MANUAL  P. 4.48-19I THRU K',/)
         lambda = -xlmbda
         IF ( ibk/=0 ) THEN
            IF ( xlmbda/=0.0 ) THEN
               CALL page2(3)
               WRITE (io,99002) uwm
99002          FORMAT (A25,' 2388',/5X,'USER SPECIFIED RANGE NOT USED FOR FEER',                                                    &
                      &' BUCKLING. THE ROOTS OF LOWEST MAGNITUDE ARE OBTAINED')
            ENDIF
            lambda = 0.0D+0
         ENDIF
         ifset = 0
         IF ( xlmbda==0. .AND. ibk==0 ) ifset = 1
         IF ( ifset==1 ) ioptf = 1
         cndflg = 0
         nodcmp = 0
         CALL rdtrl(ifkaa(1))
         CALL rdtrl(ifmaa(1))
         ifk = ifkaa(1)
         ifm = ifmaa(1)
         iprc = iprec
         nord = ifkaa(2)
         incr = 1
         incrp = incr
         itp1 = iprc
         itp2 = iprc
         nz = korsz(z)
         ibuf1 = nz - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ntot = iprc*(5*nord+1) + 4*sysbuf - nz
         IF ( ntot>0 ) CALL mesage(-8,ntot,name)
         CALL klock(istart)
         mrank = 0
         CALL gopen(ifm,z(ibuf1),rdrew)
         CALL makmcb(mcb,sr8fle,nord,6,iprc)
         CALL gopen(sr8fle,z(ibuf2),wrtrew)
         mcb(2) = 0
         mcb(6) = 0
         IF ( iprc==2 ) THEN
            DO j = 1 , nord
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     ii = 0
                     CALL unpack(*2,ifm,dz(1))
                     nt = nn - ii + 1
                     epxm = 0.0D+0
                     IF ( ii<=j .AND. nn>=j ) epxm = dz(j-ii+1)*dsm
                     ntz = 0
                     DO jj = 1 , nt
                        IF ( dabs(dz(jj))<=epxm ) THEN
                           dz(jj) = 0.0D+0
                           ntz = ntz + 1
                        ENDIF
                     ENDDO
                     IF ( ntz<nt ) mrank = mrank + 1
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
 2                   ii = 1
                     nn = 1
                     nt = 1
                     dz(1) = 0.0D+0
                     spag_nextblock_2 = 2
                  CASE (2)
                     iip = ii
                     nnp = nn
                     CALL pack(dz(1),sr8fle,mcb(1))
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
         ELSE
            DO j = 1 , nord
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     ii = 0
                     CALL unpack(*4,ifm,z(1))
                     nt = nn - ii + 1
                     epxm = 0.0D+0
                     IF ( ii<=j .AND. nn>=j ) epxm = z(j-ii+1)*dsm
                     ntz = 0
                     DO jj = 1 , nt
                        IF ( abs(z(jj))<=epxm ) THEN
                           z(jj) = 0.
                           ntz = ntz + 1
                        ENDIF
                     ENDDO
                     IF ( ntz<nt ) mrank = mrank + 1
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
 4                   ii = 1
                     nn = 1
                     nt = 1
                     z(1) = 0.
                     spag_nextblock_3 = 2
                  CASE (2)
                     iip = ii
                     nnp = nn
                     CALL pack(z(1),sr8fle,mcb(1))
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
         ENDIF
         CALL wrttrl(mcb)
         mord = 2*(neig-northo) + 10
         mrk = mrank - northo
         nzero = northo
         IF ( mord>mrk ) mord = mrk
         IF ( neig>mrank ) THEN
            CALL page2(3)
            WRITE (io,99003) uwm
99003       FORMAT (A25,' 2385',/5X,'DESIRED NUMBER OF EIGENVALUES EXCEED ',                                                        &
                   &'THE EXISTING NUMBER, ALL EIGENSOLUTIONS WILL BE SOUGHT.')
         ENDIF
         CALL close(sr8fle,norew)
         CALL close(ifm,rew)
         DO i = 1 , 7
            mcbsma(i) = mcb(i)
            ifmaa(i) = mcbsma(i)
         ENDDO
         ifm = ifmaa(1)
         IF ( ibk/=0 ) THEN
!
!     SET UP TO DECOMPOSE KAA
!
            iflelm(1) = ifkaa(1)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ifset/=0 ) THEN
!
!     CALCULATE INITIAL SHIFT
!
            CALL gopen(ifk,z(ibuf1),rdrew)
            CALL gopen(ifm,z(ibuf2),rdrew)
            CALL frmax(ifk,ifm,nord,iprc,drsn,drsm)
            CALL close(ifk,rew)
            CALL close(ifm,rew)
            scale = dble(float(nord))*10.0D0**(-it)*drsm
            lambda = 10.0D0**(-it/3)*drsn
            IF ( lambda<scale ) lambda = scale
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     CALL IN ADD LINK TO FORM  (K+LAMBDA*M)
!
         name(2) = i1
         CALL conmsg(name,3,0)
         CALL feer1
         name(3) = iend
         CALL conmsg(name,3,0)
         spag_nextblock_1 = 3
      CASE (3)
!
!     CALL IN SDCOMP TO DECOMPOSE THIS MATRIX
!
         nodcmp = nodcmp + 1
         shftpt = dabs(lambda)
         name(2) = i2
         name(3) = ibegn
         CALL conmsg(name,3,0)
         CALL feer2(ising)
         name(3) = iend
         CALL conmsg(name,3,0)
         ik = ibk + 1
         ij = ising + 1
         IF ( ising==1 .OR. l16/=0 ) THEN
            CALL page2(4)
            WRITE (io,99004) jcr(ik) , nord , mrank , mord , northo , neig , nzero , xlmbda , lambda , icr(ij)
99004       FORMAT ('0*** DIAG 16 OUTPUT FOR FEER ANALYSIS, OPTION =',A4,/5X,'ORDER =',I5,',  MAX RANK =',I5,',  REDUCED ORDER =',  &
                  & I5,',  ORTH VCT =',I5,',  NEIG =',I4,',  NZERO =',I4,/5X,'USER SHIFT =',1P,E16.8,',  INTERNAL SHIFT =',D16.8,   &
                   &',  SINGULARITY CHECK ',A4)
         ENDIF
         IF ( ising==0 ) THEN
!
!     DETERMINE THE TIME REQUIRED TO COMPLETE FEER PROCESS
!
            CALL tmtogo(t1)
            xm = mord
            xmp = northo
            xn = nord
            xi = ifset
            ifl = mcblt(1)
            CALL gopen(ifl,z(ibuf1),rdrew)
            ntms = 0
            DO i = 1 , nord
               ii = 0
               CALL unpack(*10,ifl,z(1))
               ntms = ntms + nn - ii + 1
 10         ENDDO
            CALL close(ifl,rew)
            xt = ntms
            sp = (xt*(1.-xi)*(xm+xmp)+2.*xm) + xn*(2.+xi)*.5*(3.*xm**2+2.*xmp) + (16.+11.*xi*.5)*xn*xm + 14.*xm**2
!
!     OBTAIN TRIDIAGONAL REDUCTION
!
            name(2) = i3
            name(3) = ibegn
            CALL conmsg(name,3,0)
            CALL feer3
            name(3) = iend
            CALL conmsg(name,3,0)
            IF ( cndflg==3 ) THEN
               CALL page2(3)
               WRITE (io,99005) uwm
99005          FORMAT (A25,' 2389',/5X,'PROBLEM SIZE REDUCED - NO MORE TRIAL ','VECTORS CAN BE OBTAINED.')
            ENDIF
            IF ( mord/=0 ) THEN
               CALL tmtogo(t2)
               timet = t3 - t1
!
!     OBTAIN EIGENVALUES AND EIGENVECTORS
!
               name(2) = i4
               name(3) = ibegn
               CALL conmsg(name,3,0)
               CALL feer4(it)
               name(3) = iend
               CALL conmsg(name,3,0)
               CALL tmtogo(t3)
               IF ( l16/=0 ) WRITE (io,99006) t1 , t2 , t3 , sp
99006          FORMAT (' FEER COMPLETE,  T1,T2,T3 =',3I9,',  SP = ',1P,E16.8)
               IF ( cndflg/=4 ) THEN
                  IF ( mord+nzero<neig ) THEN
                     npr = neig - mord - nzero
                     CALL page2(3)
                     WRITE (io,99007) uwm , npr , neig
99007                FORMAT (A25,' 2390',/4X,I5,' FEWER ACCURATE EIGENSOLUTIONS THAN',' THE',I5,' REQUESTED HAVE BEEN FOUND.')
                     cndflg = 1
                  ELSEIF ( mord+nzero/=neig ) THEN
                     npr = mord + nzero - neig
                     CALL page2(3)
                     WRITE (io,99008) uim , npr , neig
99008                FORMAT (A29,' 2392',/4X,I5,' MORE ACCURATE EIGENSOLUTIONS THAN ','THE',I5,' REQUESTED HAVE BEEN FOUND.')
                     IF ( l16==0 ) WRITE (io,99009)
99009                FORMAT (5X,'USE DIAG 16 TO DETERMINE ERROR BOUNDS')
                  ENDIF
                  CALL gopen(dmpfle,z(ibuf1),wrtrew)
!
!    SET IZ(1) TO 2 (FOR INVPWR) THEN IZ(7) TO 1 (POINTS TO FEER METHOD)
!
                  iz(1) = 2
                  iz(2) = mord + nzero
                  iz(3) = iter
                  iz(4) = 0
                  iz(5) = nodcmp
                  iz(6) = nonul
                  iz(7) = 1
                  iz(8) = cndflg
                  iz(9) = 0
                  iz(10) = 0
                  iz(11) = 0
                  iz(12) = 0
                  CALL write(dmpfle,iz,12,1)
                  CALL close(dmpfle,rew)
                  critf = xn*10.0**(-it)
                  name(2) = i0
                  CALL conmsg(name,3,0)
                  RETURN
               ENDIF
            ENDIF
            WRITE (io,99010) ufm
99010       FORMAT (A23,' 2391, PROGRAM LOGIC ERROR IN FEER')
            CALL mesage(-37,0,name)
!
!     SINGULAR MATRIX. ADJUST LAMBDA
!
         ELSEIF ( ibk==1 ) THEN
!
            WRITE (io,99011) ufm
99011       FORMAT (A23,' 2436, SINGULAR MATRIX IN FEER BUCKLING SOLUTION.')
            CALL mesage(-37,0,name)
         ELSE
            cndflg = cndflg + 1
            IF ( nodcmp==3 ) THEN
               WRITE (io,99012) ufm
99012          FORMAT (A23,' 2386',/5X,'STIFFNESS MATRIX SINGULARITY CANNOT BE',' REMOVED BY SHIFTING.')
               CALL mesage(-37,0,name)
            ELSE
               lambda = 100.0D0*lambda
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE feer
