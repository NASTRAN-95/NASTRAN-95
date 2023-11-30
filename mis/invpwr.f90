
SUBROUTINE invpwr
   IMPLICIT NONE
   INTEGER Comflg , Dmpfil , Filek(7) , Ind , Iprec , Ireg , Istart , Isym , Iter , Ivect , Iz(12) , Keep(2) , Ksystm(65) , Ndmnus ,&
         & Ndplus , Neg , Nochng , Noest , Noneg , Nopos , Norew , Northo , Nzero , Rew , Sr1fil , Sr2fil , Sturm , Switch ,        &
         & Sysbuf , Timed , Wrtrew
   REAL Dumxx(35) , Eofnrw , Eps , Filelm(7) , Filem(7) , Filevc(7) , Lammax , Lammin , Rd , Rdrew , Rzero , Shftpt , Sr3fil ,      &
      & Sr4fil , Sr5fil , Sr6fil , Sr7fil , Sr8fil , Wrt , Z(1) , Zz(1)
   DOUBLE PRECISION Lambda , Lmbda
   COMMON /dcompx/ Dumxx , Isym
   COMMON /invpwx/ Filek , Filem , Filelm , Filevc , Sr1fil , Sr2fil , Sr3fil , Sr4fil , Sr5fil , Sr6fil , Sr7fil , Sr8fil ,        &
                 & Dmpfil , Lammin , Lammax , Noest , Ndplus , Ndmnus , Eps , Northo
   COMMON /invpxx/ Lambda , Comflg , Iter , Timed , Nopos , Rzero , Neg , Nochng , Ind , Lmbda , Switch , Nzero , Noneg , Ivect ,   &
                 & Ireg , Istart
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /sturmx/ Sturm , Shftpt , Keep
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   REAL dellam , lmin , x , y
   INTEGER ibuf1 , icrq , ifile , ishift , ising , iterm , ix , mxchng , name(2) , ncol , ncol2 , no , nodcmp , nomovs , nostrt ,   &
         & nshift , nz , t1 , t2
   INTEGER korsz , mtimsu , mtmsu1 , norm1 , norm11
   REAL sub , sub1 , xtrnsy , xtrny1
   EXTERNAL mtimsu , mtmsu1 , norm1 , norm11 , sub , sub1 , xtrnsy , xtrny1
!
!     GIVEN A REAL SYMETRIC MATRIX, INVPWR WILL SOLVE FOR ALL OF THE
!     EIGENVALUES AND EIGENVECTORS WITHIN A SPECIFIED RANGE
!
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!
!     FILEK(7) =  MATRIX CONTROL BLOCK FOR THE INPUT STIFFNESS MATRIX K
!     FILEM(7) =  MATRIX CONTROL BLOCK FOR THE INPUT MASS MATRIX M
!     FILELM(7)=  MATRIX CONTROL BLOCK FOR THE OUTPUT EIGENVALUES
!     FILEVC(7)=  MATRIX CONTROL BLOCK FOR THE OUTPUT EIGENVECTORS
!     SR1FIL-
!     SR7FIL   =  SCRATCH FILES REQUIRED INTERNALLY
!     LAMMIN   =  MINIMUM VALUE FOR THE EIGENVALUE
!     LAMMAX   =  MAXIMUM VALUE FOR THE EIGENVALUE
!     NOEST    =  NUMBER OF ESTIMATED EIGENVALUES WITHIN THE SPECIFIED
!                 RANGE
!     NDPLUS   =  NUMBER OF DESIRED EIGENVALUES IN THE POSITIVE RANGE
!     NDMNUS   =  NUMBER OF DESIRED EIGENVALUES IN THE NEGATIVE RANGE
!     EPS      =  CONVERGENCE CRITERIA
!
!     FILELM AND FILEVC WILL BE USED AS SR1FIL AND SR2FIL WHILE THE
!     EIGENVALUES AND EIGENVECTORS WILL BE STORED ON THE ACTUAL SR1FIL
!     AND SR2FIL. THE ORDERING OF THE EIGENVALUES AND EIGENVECTORS WILL
!     PUT THEM ON FILELM AND FILEVC IN THE CORRECT SEQUENCE AT THE END
!     OF THE SUBROUTINE
!
!     SR1FIL-FILELM CONTAINS (K-LAMBDA*M)
!     SR2FIL-FILEVC CONTAINS THE LOWER TRIANGLE L
!     SR3FIL        CONTAINS THE UPPER TRIANGLE U
!     SR4FIL        IS USED AS SCRATCH IN DECOMP
!     SR5FIL        IS USED AS SCRATCH IN DECOMP
!     SR6FIL        IS USED AS SCRATCH IN DECOMP
!     SR7FIL        CONTAINS THE VECTORS WHICH ARE USED TO ORTHOGONALIZE
!                   THE CURRENT ITERATE
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec)
   DATA name/4HINVP , 4HWR  /
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     NSHIFT =  NUMBER OF SHIFT POINTS
!     ISHIFT =  CURRENT SHIFT REGION
!     NOVECT =  NUMBER OF EIGENVECTORS FOUND IN A GIVEN REGION
!     NOSKIP =  NUMBER OF VECTORS TO SKIP TO REACH THE LAST SHIFT REGION
!     NEG    =  1 = FIND NEGATIVE ROOTS
!               0 = FIND ONLY POSITIVE ROOTS
!              -1 = WE ARE NOW SEARCHING FOR THE NEGATIVE ROOTS
!     LAMBDA =  THE CURRENT SHIFT POINT
!     RZEROP =  THE CURRENT EIGENVALUE MUST BE .LT. LAMBDA + RZEROP
!     RZEROM =  THE CURRENT EIGENVALUE MUST BE .GT. LAMBDA - RZEROM
!     LMBDA  =  THE ORIGINAL VALUE OF LAMBDA IN A GIVEN REGION
!     COMFLG =  0 = INITIAL ENTRY WITH NEW LAMBDA
!               1 = NEW SHIFT POINT WITHIN THE SEARCH REGION
!               2 = NEW SHIFT DUE TO CLOSENESS TO AN EIGENVALUE
!               3 = NUMBER OF DESIRED POSITIVE ROOTS FOUND
!               4 = NUMBER FOUND EXCEEDS 3*NOEST
!     ISING  =  SINGULARITY FLAG  0 = NO SINGULARITY
!                                 1 = SINGULAR MATRIX - CHANGE LAMBDA
!                                     AND TRY ONE MORE TIME
!     ITER   =  TOTAL NUMBER OF ITERATIONS
!     NOCHNG =  NUMBER OF SHIFTS WITHIN ONE REGION
!     TIMED  =  TIME REQUIRED TO FORM AND DECOMPOSE (K-LAMBDA*M)
!     NFIRST =  NUMBER OF VECTORS IN THE FIRST POSITIVE SEARCH REGION
!
   Isym = 1
   nshift = (Noest+5)/6
   mxchng = max0(10,nshift)
   ncol = Filek(2)
   ncol2 = 2*ncol
   ishift = 1
   nz = korsz(Zz(1))
   icrq = ncol*(1+7*Iprec) + 4*Sysbuf - nz
   IF ( icrq>0 ) GOTO 700
   nz = korsz(Z(1))
   ibuf1 = nz - Sysbuf
   icrq = ncol2 - ibuf1
   IF ( ibuf1<=ncol2 ) GOTO 700
   Nopos = Northo
   Noneg = 0
   Neg = 0
   Ind = 0
   Iter = 0
   nodcmp = 0
   nostrt = 0
   nomovs = 0
   IF ( Northo<=0 ) THEN
      CALL gopen(Sr1fil,Z(ibuf1),Wrtrew)
      CALL close(Sr1fil,Norew)
      CALL gopen(Sr2fil,Z(ibuf1),Wrtrew)
      CALL close(Sr2fil,Norew)
   ENDIF
   lmin = Lammin
   IF ( Lammin>=0.0 ) THEN
!
!     EVALUATE THE VALUE OF LAMBDA IN THE CENTER OF THE CURRENT SEARCH
!     REGION
!
      dellam = Lammax - lmin
   ELSE
      lmin = 0.
      Neg = 1
      IF ( Lammax>0.0 ) THEN
         dellam = Lammax - lmin
      ELSE
         lmin = Lammax
         Neg = -1
         dellam = Lammin - Lammax
      ENDIF
   ENDIF
 100  Lambda = lmin + (ishift-0.5)*dellam/nshift
   Rzero = abs(0.55*dellam/nshift)
   nostrt = nostrt + 1
 200  Comflg = 0
   Lmbda = Lambda
!
!     INITIATE CLOCK TIME
!
   CALL klock(Istart)
   Nochng = 0
   Switch = 0
   Ivect = 0
   Ireg = 0
   Ind = Ind + 1
   IF ( iabs(Ind)==13 ) Ind = 1
   ising = 0
 300  DO WHILE ( Nochng<mxchng )
      Nochng = Nochng + 1
      CALL klock(t1)
!
!     CALL IN ADD LINK TO FORM  (K-LAMBDA*M)
!
      CALL invp1
!
!     CALL IN DECOMP TO DECOMPOSE THIS MATRIX
!
      nodcmp = nodcmp + 1
      Shftpt = Lambda
      CALL invp2(*400)
      CALL klock(t2)
!
!     DETERMINE THE TIME REQUIRED TO FORM AND DECOMPOSE (K-LAMBDA*M)
!
      Timed = t2 - t1
!
!     CALL IN THE MAIN LINK TO ITERATE FOR EIGENVALUES
!
      IF ( Iprec==1 ) CALL invp3(norm11,sub1,mtmsu1,xtrny1)
      IF ( Iprec==2 ) CALL invp3(norm1,sub,mtimsu,xtrnsy)
      IF ( Comflg==2 ) THEN
         nomovs = nomovs + 1
         GOTO 200
      ELSEIF ( Comflg==1 ) THEN
         ising = 0
         Switch = 1
      ELSE
         IF ( Comflg==3 ) GOTO 500
         IF ( Comflg==0 ) THEN
            ishift = ishift + 1
            IF ( ishift<=nshift ) GOTO 100
            GOTO 500
         ELSE
            iterm = Comflg
            GOTO 600
         ENDIF
      ENDIF
   ENDDO
   iterm = 2
   GOTO 600
!
!     SINGULAR MATRIX. INCREMENT LAMBDA AND TRY ONCE MORE
!
 400  IF ( ising==1 ) THEN
      iterm = 1
      GOTO 600
   ELSE
      ising = 1
      Lambda = Lambda + .02*Rzero
      GOTO 300
   ENDIF
 500  IF ( Neg<=0 ) THEN
      iterm = 3
   ELSE
!
!     INITIALIZE PARAMETERS TO SOLVE FOR NEGATIVE EIGENVALUES
!
      x = nshift*(-Lammin/Lammax)
      ix = x
      y = ix
      IF ( x/=y ) ix = ix + 1
      nshift = ix
      Neg = -1
      dellam = Lammin
      ishift = 1
      GOTO 100
   ENDIF
!
!     RE-ORDER EIGENVALUES AND EIGENVECTORS
!
 600  CALL gopen(Dmpfil,Z(ibuf1),Wrtrew)
   Iz(1) = 2
   Iz(2) = Northo
   Iz(3) = nostrt
   Iz(4) = nomovs
   Iz(5) = nodcmp
   Iz(6) = Iter
   Iz(7) = 0
   Iz(8) = iterm
   Iz(9) = 0
   Iz(10) = 0
   Iz(11) = 0
   Iz(12) = 0
   CALL write(Dmpfil,Iz,12,1)
   CALL close(Dmpfil,Rew)
   RETURN
 700  no = -8
   ifile = icrq
   CALL mesage(no,ifile,name)
END SUBROUTINE invpwr