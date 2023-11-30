
SUBROUTINE feer
   IMPLICIT NONE
   INTEGER Cndflg , Dmpfle , Ibk , Icase , Idiag , Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Ifset , Ii ,     &
         & Iip , Incr , Incrp , Ind , Io , Ioptf , Iprc , Iprec , Iprob(2) , Istart , Iter , Itp1 , Itp2 , Iz(12) , Keep(2) ,       &
         & Ksystm(65) , L16 , Lntime , Mcblt(7) , Mcbrm(7) , Mcbsma(7) , Mcbvec(7) , Mord , Mrank , Nbpw , Neig , Nn , Nnp ,        &
         & Nochng , Nonul , Nord , Norew , Northo , Nummod , Nz3 , Nzero , Sr8fle , Sturm , Sysbuf , Timed
   REAL Critf , Eofnrw , Epx , Option(2) , Rd , Rdrew , Rew , Shftpt , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle ,        &
      & Sr7fle , Tcons(15) , Tml(4) , Tmt(4) , Wrt , Wrtrew , Xlmbda , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda , Lmbda
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Iprob , Nummod , Icase
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc
   COMMON /feerxx/ Lambda , Cndflg , Iter , Timed , L16 , Ioptf , Epx , Nochng , Ind , Lmbda , Ifset , Nzero , Nonul , Idiag ,      &
                 & Mrank , Istart , Nz3
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /ntime / Lntime , Tcons
   COMMON /opinv / Mcblt , Mcbsma , Mcbvec , Mcbrm
   COMMON /packx / Itp1 , Itp2 , Iip , Nnp , Incrp
   COMMON /reigkr/ Option
   COMMON /sturmx/ Sturm , Shftpt , Keep
   COMMON /system/ Ksystm
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION drsm , drsn , dsm , epxm , scale
   INTEGER i , i0 , i1 , i2 , i3 , i4 , ibegn , ibuf1 , ibuf2 , icr(2) , iend , ifk , ifl , ifm , ij , ik , ising , it , j , jcr(2) &
         & , jj , mcb(7) , mode , mrk , name(3) , nodcmp , npr , nt , ntms , ntot , ntz , nz , t1 , t2 , t3 , timet
   INTEGER korsz
   REAL sp , xi , xm , xmp , xn , xt
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
!
!     SET PRECISION DIGITS TO 12, ALL MACHINES (NEW 1/92)
!
   it = 12
   Epx = 10.**(2-it)
   dsm = 10.0D0**(-2*it/3)
   name(3) = ibegn
   CALL conmsg(name,3,0)
   CALL feerdd
!
!     INITIALIZE FEERCX
!     DEFINITION OF INTERNAL PARAMETERS
!
   Ibk = 0
   IF ( Iprob(1)/=mode ) Ibk = 1
   Ioptf = Ibk
   Timed = 0
   timet = 0
   CALL sswtch(16,L16)
   IF ( L16==1 ) WRITE (Io,99001)
99001 FORMAT (//,' *** DIAG16 - ALL TERMS USED ARE DESCRIBED IN ','PROGRAMMER MANUAL  P. 4.48-19I THRU K',/)
   Lambda = -Xlmbda
   IF ( Ibk/=0 ) THEN
      IF ( Xlmbda/=0.0 ) THEN
         CALL page2(3)
         WRITE (Io,99002) Uwm
99002    FORMAT (A25,' 2388',/5X,'USER SPECIFIED RANGE NOT USED FOR FEER',' BUCKLING. THE ROOTS OF LOWEST MAGNITUDE ARE OBTAINED')
      ENDIF
      Lambda = 0.0D+0
   ENDIF
   Ifset = 0
   IF ( Xlmbda==0. .AND. Ibk==0 ) Ifset = 1
   IF ( Ifset==1 ) Ioptf = 1
   Cndflg = 0
   nodcmp = 0
   CALL rdtrl(Ifkaa(1))
   CALL rdtrl(Ifmaa(1))
   ifk = Ifkaa(1)
   ifm = Ifmaa(1)
   Iprc = Iprec
   Nord = Ifkaa(2)
   Incr = 1
   Incrp = Incr
   Itp1 = Iprc
   Itp2 = Iprc
   nz = korsz(Z)
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ntot = Iprc*(5*Nord+1) + 4*Sysbuf - nz
   IF ( ntot>0 ) CALL mesage(-8,ntot,name)
   CALL klock(Istart)
   Mrank = 0
   CALL gopen(ifm,Z(ibuf1),Rdrew)
   CALL makmcb(mcb,Sr8fle,Nord,6,Iprc)
   CALL gopen(Sr8fle,Z(ibuf2),Wrtrew)
   mcb(2) = 0
   mcb(6) = 0
   IF ( Iprc==2 ) THEN
      DO j = 1 , Nord
         Ii = 0
         CALL unpack(*20,ifm,Dz(1))
         nt = Nn - Ii + 1
         epxm = 0.0D+0
         IF ( Ii<=j .AND. Nn>=j ) epxm = Dz(j-Ii+1)*dsm
         ntz = 0
         DO jj = 1 , nt
            IF ( dabs(Dz(jj))<=epxm ) THEN
               Dz(jj) = 0.0D+0
               ntz = ntz + 1
            ENDIF
         ENDDO
         IF ( ntz<nt ) Mrank = Mrank + 1
         GOTO 40
 20      Ii = 1
         Nn = 1
         nt = 1
         Dz(1) = 0.0D+0
 40      Iip = Ii
         Nnp = Nn
         CALL pack(Dz(1),Sr8fle,mcb(1))
      ENDDO
   ELSE
      DO j = 1 , Nord
         Ii = 0
         CALL unpack(*60,ifm,Z(1))
         nt = Nn - Ii + 1
         epxm = 0.0D+0
         IF ( Ii<=j .AND. Nn>=j ) epxm = Z(j-Ii+1)*dsm
         ntz = 0
         DO jj = 1 , nt
            IF ( abs(Z(jj))<=epxm ) THEN
               Z(jj) = 0.
               ntz = ntz + 1
            ENDIF
         ENDDO
         IF ( ntz<nt ) Mrank = Mrank + 1
         GOTO 80
 60      Ii = 1
         Nn = 1
         nt = 1
         Z(1) = 0.
 80      Iip = Ii
         Nnp = Nn
         CALL pack(Z(1),Sr8fle,mcb(1))
      ENDDO
   ENDIF
   CALL wrttrl(mcb)
   Mord = 2*(Neig-Northo) + 10
   mrk = Mrank - Northo
   Nzero = Northo
   IF ( Mord>mrk ) Mord = mrk
   IF ( Neig>Mrank ) THEN
      CALL page2(3)
      WRITE (Io,99003) Uwm
99003 FORMAT (A25,' 2385',/5X,'DESIRED NUMBER OF EIGENVALUES EXCEED ','THE EXISTING NUMBER, ALL EIGENSOLUTIONS WILL BE SOUGHT.')
   ENDIF
   CALL close(Sr8fle,Norew)
   CALL close(ifm,Rew)
   DO i = 1 , 7
      Mcbsma(i) = mcb(i)
      Ifmaa(i) = Mcbsma(i)
   ENDDO
   ifm = Ifmaa(1)
   IF ( Ibk/=0 ) THEN
!
!     SET UP TO DECOMPOSE KAA
!
      Iflelm(1) = Ifkaa(1)
      GOTO 200
   ELSEIF ( Ifset/=0 ) THEN
!
!     CALCULATE INITIAL SHIFT
!
      CALL gopen(ifk,Z(ibuf1),Rdrew)
      CALL gopen(ifm,Z(ibuf2),Rdrew)
      CALL frmax(ifk,ifm,Nord,Iprc,drsn,drsm)
      CALL close(ifk,Rew)
      CALL close(ifm,Rew)
      scale = dble(float(Nord))*10.0D0**(-it)*drsm
      Lambda = 10.0D0**(-it/3)*drsn
      IF ( Lambda<scale ) Lambda = scale
   ENDIF
!
!     CALL IN ADD LINK TO FORM  (K+LAMBDA*M)
!
 100  name(2) = i1
   CALL conmsg(name,3,0)
   CALL feer1
   name(3) = iend
   CALL conmsg(name,3,0)
!
!     CALL IN SDCOMP TO DECOMPOSE THIS MATRIX
!
 200  nodcmp = nodcmp + 1
   Shftpt = dabs(Lambda)
   name(2) = i2
   name(3) = ibegn
   CALL conmsg(name,3,0)
   CALL feer2(ising)
   name(3) = iend
   CALL conmsg(name,3,0)
   ik = Ibk + 1
   ij = ising + 1
   IF ( ising==1 .OR. L16/=0 ) THEN
      CALL page2(4)
      WRITE (Io,99004) jcr(ik) , Nord , Mrank , Mord , Northo , Neig , Nzero , Xlmbda , Lambda , icr(ij)
99004 FORMAT ('0*** DIAG 16 OUTPUT FOR FEER ANALYSIS, OPTION =',A4,/5X,'ORDER =',I5,',  MAX RANK =',I5,',  REDUCED ORDER =',I5,     &
             &',  ORTH VCT =',I5,',  NEIG =',I4,',  NZERO =',I4,/5X,'USER SHIFT =',1P,E16.8,',  INTERNAL SHIFT =',D16.8,            &
             &',  SINGULARITY CHECK ',A4)
   ENDIF
   IF ( ising==0 ) THEN
!
!     DETERMINE THE TIME REQUIRED TO COMPLETE FEER PROCESS
!
      CALL tmtogo(t1)
      xm = Mord
      xmp = Northo
      xn = Nord
      xi = Ifset
      ifl = Mcblt(1)
      CALL gopen(ifl,Z(ibuf1),Rdrew)
      ntms = 0
      DO i = 1 , Nord
         Ii = 0
         CALL unpack(*250,ifl,Z(1))
         ntms = ntms + Nn - Ii + 1
 250  ENDDO
      CALL close(ifl,Rew)
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
      IF ( Cndflg==3 ) THEN
         CALL page2(3)
         WRITE (Io,99005) Uwm
99005    FORMAT (A25,' 2389',/5X,'PROBLEM SIZE REDUCED - NO MORE TRIAL ','VECTORS CAN BE OBTAINED.')
      ENDIF
      IF ( Mord/=0 ) THEN
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
         IF ( L16/=0 ) WRITE (Io,99006) t1 , t2 , t3 , sp
99006    FORMAT (' FEER COMPLETE,  T1,T2,T3 =',3I9,',  SP = ',1P,E16.8)
         IF ( Cndflg/=4 ) THEN
            IF ( Mord+Nzero<Neig ) THEN
               npr = Neig - Mord - Nzero
               CALL page2(3)
               WRITE (Io,99007) Uwm , npr , Neig
99007          FORMAT (A25,' 2390',/4X,I5,' FEWER ACCURATE EIGENSOLUTIONS THAN',' THE',I5,' REQUESTED HAVE BEEN FOUND.')
               Cndflg = 1
            ELSEIF ( Mord+Nzero/=Neig ) THEN
               npr = Mord + Nzero - Neig
               CALL page2(3)
               WRITE (Io,99008) Uim , npr , Neig
99008          FORMAT (A29,' 2392',/4X,I5,' MORE ACCURATE EIGENSOLUTIONS THAN ','THE',I5,' REQUESTED HAVE BEEN FOUND.')
               IF ( L16==0 ) WRITE (Io,99009)
99009          FORMAT (5X,'USE DIAG 16 TO DETERMINE ERROR BOUNDS')
            ENDIF
            CALL gopen(Dmpfle,Z(ibuf1),Wrtrew)
!
!    SET IZ(1) TO 2 (FOR INVPWR) THEN IZ(7) TO 1 (POINTS TO FEER METHOD)
!
            Iz(1) = 2
            Iz(2) = Mord + Nzero
            Iz(3) = Iter
            Iz(4) = 0
            Iz(5) = nodcmp
            Iz(6) = Nonul
            Iz(7) = 1
            Iz(8) = Cndflg
            Iz(9) = 0
            Iz(10) = 0
            Iz(11) = 0
            Iz(12) = 0
            CALL write(Dmpfle,Iz,12,1)
            CALL close(Dmpfle,Rew)
            Critf = xn*10.0**(-it)
            name(2) = i0
            CALL conmsg(name,3,0)
            RETURN
         ENDIF
      ENDIF
      WRITE (Io,99010) Ufm
99010 FORMAT (A23,' 2391, PROGRAM LOGIC ERROR IN FEER')
      CALL mesage(-37,0,name)
!
!     SINGULAR MATRIX. ADJUST LAMBDA
!
   ELSEIF ( Ibk==1 ) THEN
!
      WRITE (Io,99011) Ufm
99011 FORMAT (A23,' 2436, SINGULAR MATRIX IN FEER BUCKLING SOLUTION.')
      CALL mesage(-37,0,name)
   ELSE
      Cndflg = Cndflg + 1
      IF ( nodcmp==3 ) THEN
         WRITE (Io,99012) Ufm
99012    FORMAT (A23,' 2386',/5X,'STIFFNESS MATRIX SINGULARITY CANNOT BE',' REMOVED BY SHIFTING.')
         CALL mesage(-37,0,name)
      ELSE
         Lambda = 100.0D0*Lambda
         GOTO 100
      ENDIF
   ENDIF
END SUBROUTINE feer