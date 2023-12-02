!*==invpwr.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invpwr
   USE c_dcompx
   USE c_invpwx
   USE c_invpxx
   USE c_names
   USE c_sturmx
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dellam , lmin , x , y
   INTEGER :: ibuf1 , icrq , ifile , iprec , ishift , ising , iterm , ix , mxchng , ncol , ncol2 , no , nodcmp , nomovs , nostrt ,  &
            & nshift , nz , sysbuf , t1 , t2
   INTEGER , DIMENSION(12) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: zz
   EXTERNAL close , gopen , invp1 , invp2 , invp3 , klock , korsz , mesage , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         isym = 1
         nshift = (noest+5)/6
         mxchng = max0(10,nshift)
         ncol = filek(2)
         ncol2 = 2*ncol
         ishift = 1
         nz = korsz(zz(1))
         icrq = ncol*(1+7*iprec) + 4*sysbuf - nz
         IF ( icrq>0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nz = korsz(z(1))
         ibuf1 = nz - sysbuf
         icrq = ncol2 - ibuf1
         IF ( ibuf1<=ncol2 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nopos = northo
         noneg = 0
         neg = 0
         ind = 0
         iter = 0
         nodcmp = 0
         nostrt = 0
         nomovs = 0
         IF ( northo<=0 ) THEN
            CALL gopen(sr1fil,z(ibuf1),wrtrew)
            CALL close(sr1fil,norew)
            CALL gopen(sr2fil,z(ibuf1),wrtrew)
            CALL close(sr2fil,norew)
         ENDIF
         lmin = lammin
         IF ( lammin>=0.0 ) THEN
!
!     EVALUATE THE VALUE OF LAMBDA IN THE CENTER OF THE CURRENT SEARCH
!     REGION
!
            dellam = lammax - lmin
         ELSE
            lmin = 0.
            neg = 1
            IF ( lammax>0.0 ) THEN
               dellam = lammax - lmin
            ELSE
               lmin = lammax
               neg = -1
               dellam = lammin - lammax
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         lambda = lmin + (ishift-0.5)*dellam/nshift
         rzero = abs(0.55*dellam/nshift)
         nostrt = nostrt + 1
         spag_nextblock_1 = 3
      CASE (3)
         comflg = 0
         lmbda = lambda
!
!     INITIATE CLOCK TIME
!
         CALL klock(istart)
         nochng = 0
         switch = 0
         ivect = 0
         ireg = 0
         ind = ind + 1
         IF ( iabs(ind)==13 ) ind = 1
         ising = 0
         spag_nextblock_1 = 4
      CASE (4)
         DO WHILE ( nochng<mxchng )
            nochng = nochng + 1
            CALL klock(t1)
!
!     CALL IN ADD LINK TO FORM  (K-LAMBDA*M)
!
            CALL invp1
!
!     CALL IN DECOMP TO DECOMPOSE THIS MATRIX
!
            nodcmp = nodcmp + 1
            shftpt = lambda
            CALL invp2(*20)
            CALL klock(t2)
!
!     DETERMINE THE TIME REQUIRED TO FORM AND DECOMPOSE (K-LAMBDA*M)
!
            timed = t2 - t1
!
!     CALL IN THE MAIN LINK TO ITERATE FOR EIGENVALUES
!
            IF ( iprec==1 ) CALL invp3(norm11,sub1,mtmsu1,xtrny1)
            IF ( iprec==2 ) CALL invp3(norm1,sub,mtimsu,xtrnsy)
            IF ( comflg==2 ) THEN
               nomovs = nomovs + 1
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( comflg==1 ) THEN
               ising = 0
               switch = 1
            ELSE
               IF ( comflg==3 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( comflg==0 ) THEN
                  ishift = ishift + 1
                  IF ( ishift>nshift ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 2
               ELSE
                  iterm = comflg
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         iterm = 2
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     SINGULAR MATRIX. INCREMENT LAMBDA AND TRY ONCE MORE
!
 20      IF ( ising==1 ) THEN
            iterm = 1
            spag_nextblock_1 = 6
         ELSE
            ising = 1
            lambda = lambda + .02*rzero
            spag_nextblock_1 = 4
         ENDIF
      CASE (5)
         IF ( neg<=0 ) THEN
            iterm = 3
         ELSE
!
!     INITIALIZE PARAMETERS TO SOLVE FOR NEGATIVE EIGENVALUES
!
            x = nshift*(-lammin/lammax)
            ix = x
            y = ix
            IF ( x/=y ) ix = ix + 1
            nshift = ix
            neg = -1
            dellam = lammin
            ishift = 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     RE-ORDER EIGENVALUES AND EIGENVECTORS
!
         CALL gopen(dmpfil,z(ibuf1),wrtrew)
         iz(1) = 2
         iz(2) = northo
         iz(3) = nostrt
         iz(4) = nomovs
         iz(5) = nodcmp
         iz(6) = iter
         iz(7) = 0
         iz(8) = iterm
         iz(9) = 0
         iz(10) = 0
         iz(11) = 0
         iz(12) = 0
         CALL write(dmpfil,iz,12,1)
         CALL close(dmpfil,rew)
         RETURN
      CASE (7)
         no = -8
         ifile = icrq
         CALL mesage(no,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE invpwr
