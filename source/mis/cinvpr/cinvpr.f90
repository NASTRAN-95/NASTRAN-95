!*==cinvpr.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvpr(Eed,Method,Nfound)
   USE c_cdcmpx
   USE c_cinvpx
   USE c_cinvxx
   USE c_names
   USE c_output
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Eed
   INTEGER :: Method
   INTEGER :: Nfound
!
! Local variable declarations rewritten by SPAG
!
   REAL :: anodes , anoest , anum1 , anum2 , arg , d , d1 , d2 , deltx , delty , flag , l , l1 , r , rang , range , shift , slope , &
         & x1 , x2 , xl2 , xx , y1 , y2 , yl2 , yy
   REAL(REAL64) , DIMENSION(2) :: dtemp
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(2) , SAVE :: eigc , name
   INTEGER :: i , ibuf , idiag , ifile , imax , imin , ishift , ising , isys , iterm , ixx , j , jreg , k , n , nodcmp , nomovs ,   &
            & nostrt , nout , nrow , nrow2 , nshift , nz , t1 , t2 , typeb , typek , typem
   INTEGER , DIMENSION(10) , SAVE :: ihead
   INTEGER , DIMENSION(7,1) :: ireg
   INTEGER , DIMENSION(1) :: iz
   LOGICAL :: noleft
   REAL , SAVE :: sign
   EXTERNAL bckrec , cdifbs , cdivid , cinvp1 , cinvp2 , cinvp3 , close , cnorm1 , cxtrny , fread , gopen , klock , korsz , locate ,&
          & mesage , open , preloc , rdtrl , sswtch , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GIVEN REAL OR COMPLEX MATRICIES, CINVPR WILL SOLVE FOR ALL OF
!     THE EIGENVALUES AND EIGENVECTORS WITHIN A SPECIFIED REGION
!
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!
!     FILEK(7) = MATRIX CONTROL BLOCK FOR THE INPUT STIFFNESS MATRIX K
!     FILEM(7) = MATRIX CONTROL BLOCK FOR THE INPUT MASS MATRIX M
!     FILEB(7) = MATRIX CONTROL BLOCK FOR THE INPUT DAMPING MATRIX B
!     FILELM(7)= MATRIX CONTROL BLOCK FOR THE OUTPUT EIGENVALUES
!     FILEVC(7)= MATRIX CONTROL BLOCK FOR THE OUTPUT EIGENVECTORS
!     DMPFIL   = FILE CONTAINING THE EIGENVALUE SUMMARY
!     SR1FIL-  = SCRATCH FILES USED INTERNALLY
!     SR0FIL
!     EPS      = CONVERGENCE CRITERIA
!     NOREG    = NUMBER OF REGIONS INPUT
!     REG(1,I) = X1 FOR REGION I
!     REG(2,I) = Y1 FOR REGION I
!     REG(3,I) = X2 FOR REGION I
!     REG(4,I) = Y2 FOR REGION I
!     REG(5,I) = L1 FOR REGION I
!     REG(6,I) = NO. OF DESIRED  ROOTS FOR REGION I
!     REG(7,I) = NO. OF ESTIMATED ROOTS IN REGION I
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Isys) , (Ireg(1,1),Reg(1,1))
   !>>>>EQUIVALENCE (Filek(5),Typek) , (Filem(5),Typem) , (Fileb(5),Typeb) , (Iz(1),Z(1))
   !>>>>EQUIVALENCE (Anodes,Nodes) , (Anoest,Noest) , (Z(1),Dz(1)) , (Ksystm(2),Nout)
   DATA ihead/0 , 1009 , 2 , 7*0/
   DATA eigc/207 , 2/
   DATA name/4HCINV , 4HPR  /
   DATA sign/1.0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     REAL     = 0 - ALL MATRICIES ARE REAL
!                1 - AT LEAST ONE MATRIX IS COMPLEX
!     NSHIFT   = NO. OF SHIFT POINTS IN A REGION
!     NODES    = NO. OF DESIRED ROOTS IN A REGION
!     NOEST    = NO. OF ESTIMATED ROOTS IN A REGION
!     SHIFT    = INDEX OF THE CURRENT SHIFT POINT
!     ISHIFT   = INDEX OF THE CURRENT SHIFT POINT
!     IMIN     = LOWEST INDEX OF THE COMPLETED SHIFT POINTS
!     IMAX     = HIGHEST INDEX OF COMPLETED SHIFT POINTS
!
!     FILE ALLOCATION
!
!     SR1FIL CONTAINS (LAMBDA**2*M + LAMBDA*B + K)
!     SR2FIL CONTAINS -(B+LAMBDA*M)
!     SR3FIL CONTAINS THE LOWER TRIANGLE OF THE DECOMPOSED DYNAMIC MTRX
!     SR4FIL CONTAINS THE UPPER TRIANGLE OF THE DECOMPOSED DYNAMIC MTRX
!     SR5FIL IS USED AS A SCRATCH FOR CDCOMP
!     SR6FIL IS USED AS A SCRATCH FOR CDCOMP
!     SR7FIL IS USED AS A SCRATCH FOR CDCOMP
!     SR8FIL CONTAINS THE LOWER TRIANGLE L
!     SR9FIL CONTAINS THE UPPER TRIANGLE U
!     SR0FIL CONTAINS THE LEFT EIGENVECTORS
!     S11FIL CONTAINS  -(B + LAMBDA*M)
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     IND      = AN INDEX FOR GENERATING VARIOUS STARTING VECTORS
!     ITER     = TOTAL NUMBER OF ITERATIONS
!     NODCMP   = TOTAL NUMBER OF DECOMPOSITIONS
!     NOSTRT   = NUMBER OF STARTING POINTS USED
!     NOMOVS   = NUMBER OF TIMES A STARTING POINT HAD TO BE MOVED
!     RZERO    = DISTANCE FROM THE STARTING POINT TO THE CORNER OF THE
!                PARALELOGRAM
!     NOCHNG   = COUNT OF THE NUMBER OF MOVES WHILE LOOKING FOR ONE ROO
!     COMFLG   = 0 -
!              = 1 -
!              = 2 -
!              = 3 -
!              = 4 -
!              = 5 -
!              = 6 -
!     SWITCH   =
!     IVECT    =
!     KREG     = 0-NO VECTORS FOUND IN SEARCH AREA YET
!                1- A VECTOR HAS BEEN FOUND IN THE SEARCH AREA
!     ISING    = SINGULARITY FLAG
!     ITERM    = REASON FOR TERMINATING
!              = 1 - 2SINGULARITIES IN A ROW
!              = 2 - 4 MOVES WHILE TRACKING ONE ROOT
!              = 3 - ALL REGIONS COMPLETED
!              = 4 - 3*NOEST FOUND
!              = 5 - ALL ROOTS FOUND
!              = 8 - 200 ITERATIONS WITH ONE MOVE WITHOUR CONVERGING
!     TIMED    = TIME TOO FORM AND DECOMPOSE THE DYNAMIC MATRIX
!     LEFT     = 1 - DECOMPOSE MATRIX FOR THE COMPUTATION OF THE LEFT
!                EIGENVECTORS
!
         CALL sswtch(12,idiag)
         ind1 = 0
         nz = korsz(z)
         CALL klock(istart)
         ibuf = nz - isys - 2
         ifile = filelm(1)
         CALL open(*60,filelm,z(ibuf),wrtrew)
         CALL close(filelm,rew)
         ifile = filevc(1)
         CALL open(*60,filevc,z(ibuf),wrtrew)
         CALL close(filevc,rew)
         CALL gopen(dmpfil,z(ibuf),wrtrew)
         CALL close(dmpfil,eofnrw)
         ifile = scrfil(10)
         CALL open(*60,ifile,z(ibuf),wrtrew)
         CALL close(ifile,rew)
         noleft = .FALSE.
         iz(1) = 204
         CALL rdtrl(iz)
         IF ( iz(1)<0 ) noleft = .TRUE.
         northo = 0
         nrow = 2*filek(3)
         nrow2 = 2*nrow
         isym = 1
         IF ( filek(1)==0 .OR. filek(4)==6 ) THEN
            IF ( filem(1)==0 .OR. filem(4)==6 ) THEN
               IF ( fileb(1)==0 .OR. fileb(4)==6 ) isym = 0
            ENDIF
         ENDIF
!
!     PICK UP REGION PARAMETERS
!
         CALL preloc(*60,z(ibuf),Eed)
         CALL locate(*60,z(ibuf),eigc(1),flag)
         SPAG_Loop_1_2: DO
            CALL fread(Eed,ireg,10,0)
            IF ( Method==ireg(1,1) .OR. Method==-1 ) THEN
               jreg = 1
               eps = .0001
               IF ( reg(1,2)/=0. ) eps = reg(1,2)
               SPAG_Loop_2_1: DO
                  CALL fread(Eed,ireg(1,jreg),7,0)
                  IF ( ireg(6,jreg)==-1 ) EXIT SPAG_Loop_2_1
                  jreg = jreg + 1
                  IF ( jreg>10 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
               CALL close(Eed,rew)
               noreg = jreg - 1
               jreg = 0
               EXIT SPAG_Loop_1_2
            ELSE
               SPAG_Loop_2_3: DO
                  CALL fread(Eed,ireg,7,0)
                  IF ( ireg(6,1)==-1 ) EXIT SPAG_Loop_2_3
               ENDDO SPAG_Loop_2_3
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
!
!     PICK UP PARAMETERS FOR REGION I
!
         jreg = jreg + 1
         iter = 0
         nodcmp = 0
         nostrt = 0
         nomovs = 0
         x1 = reg(1,jreg)
         y1 = reg(2,jreg)
         x2 = reg(3,jreg)
         y2 = reg(4,jreg)
         l = reg(5,jreg)
         anoest = reg(6,jreg)
         anodes = reg(7,jreg)
         IF ( nodes==0 ) nodes = 3*noest
         nshift = sqrt((x1-x2)**2+(y1-y2)**2)/l + 1.
         l1 = l*.5
         noroot = 0
!
!
!     FIND SHIFT POINT CLOSEST TO THE ORIGIN
!
         r = sqrt((x1-x2)**2+(y1-y2)**2)
         IF ( r<=0 ) THEN
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 2366, REGION IMPROPERLY DEFINED ON EIGC CARD.')
            CALL mesage(-61,0,0)
         ENDIF
         d = (float(nshift)*l-r)/2.0
         xx = x1 + d*(x1-x2)/r
         x2 = x2 + d*(x2-x1)/r
         x1 = xx
         yy = y1 + d*(y1-y2)/r
         y2 = y2 + d*(y2-y1)/r
         y1 = yy
         IF ( idiag/=0 ) THEN
            WRITE (nout,99002) x1 , y1 , x2 , y2 , l1 , nodes , noest , nshift
99002       FORMAT (1H1,5F10.2,3I5)
         ENDIF
         deltx = (x1-x2)/float(nshift)
         delty = (y1-y2)/float(nshift)
         xx = x2 + deltx/2.
         yy = y2 + delty/2.
         range = xx**2 + yy**2
         n = nshift - 1
         shift = 1.
         IF ( deltx/=0. ) THEN
            slope = delty/deltx
            arg = sqrt(1.+slope**2)
            anum1 = slope*l1/arg
            anum2 = l1/arg
         ELSE
            anum1 = l1
            anum2 = 0.
         ENDIF
         IF ( n/=0 ) THEN
            SPAG_Loop_1_4: DO i = 1 , n
               xx = xx + deltx
               yy = yy + delty
               rang = xx**2 + yy**2
               IF ( rang>=range ) EXIT SPAG_Loop_1_4
               range = rang
               shift = i + 1
            ENDDO SPAG_Loop_1_4
         ENDIF
!
!     COMPUTE COORDINATES OF CORNERS OF THE REGION
!
         xl2 = x2 + anum1
         yl2 = y2 - anum2
         imin = shift
         imax = shift
!
!     FIND THE MAXIMUM MODULUS OF THE SEARCH REGION
!
         maxmod = xl2**2 + yl2**2
         xx = x2 - anum1
         yy = y2 + anum2
         maxmod = amax1(maxmod,xx**2+yy**2)
         xx = x1 + anum1
         yy = y1 - anum2
         maxmod = amax1(maxmod,xx**2+yy**2)
         xx = x1 - anum1
         yy = y1 + anum2
         maxmod = amax1(maxmod,xx**2+yy**2)
!
!     INITIALIZE
!
         ind = 0
         left = 0
         spag_nextblock_1 = 3
      CASE (3)
         ishift = shift
!
!     EVALUATE THE VALUE OF LAMBDA IN THE CENTER OF THE CURRENT SEARCH
!     REGION
!
         lambda(1) = x2 + (shift-.5)*deltx
         lambda(2) = y2 + (shift-.5)*delty
         IF ( lambda(2)==0.0D0 ) lambda(2) = .01*delty
!
!     COMPUTE DISTANCE TO FARTHEST CORNER OF THE SQUARE SEARCH REGION
!
         xx = xl2 + shift*deltx
         yy = yl2 + shift*delty
         rzero = (lambda(1)-xx)**2 + (lambda(2)-yy)**2
         rzero = sqrt(rzero)*1.05
         IF ( idiag/=0 ) THEN
            WRITE (nout,99003) rzero
99003       FORMAT (//,10H RZERO =  ,F10.4)
         ENDIF
         nostrt = nostrt + 1
         comflg = 0
         spag_nextblock_1 = 4
      CASE (4)
         lmbda(1) = lambda(1)
         lmbda(2) = lambda(2)
         nochng = 0
         switch = 0
         ivect = 0
         kreg = 0
         ind = ind + 1
         IF ( iabs(ind)==13 ) ind = 1
         ising = 0
         spag_nextblock_1 = 5
      CASE (5)
         IF ( nochng>=4 ) THEN
!
!     4 MOVES WHILE TRACKING ONE ROOT
!
            iterm = 2
            spag_nextblock_1 = 11
         ELSE
            nochng = nochng + 1
            CALL klock(t1)
!
!     CALL IN ADD LINK TO FORM (LAMBDA**2*M + LAMBDA*B + K)
!
            CALL cinvp1
!
!     CALL IN CD COMP TO DECOMPOSE THE MATRIX
!
            IF ( idiag/=0 ) WRITE (nout,99007) lambda
            nodcmp = nodcmp + 1
            CALL cinvp2(*20)
            CALL klock(t2)
!
!     DETERMINE THE TIME REQUIRED TO FORM AND DECOMPOSE THE DYNAMIC
!     MATRIX
!
            timed = t2 - t1
            IF ( timed==0 ) timed = 1
            spag_nextblock_1 = 6
         ENDIF
         CYCLE
 20      IF ( ising==1 ) THEN
!
!     SINGULARITY ENCOUNTERED TWICE IN A ROW
!
            iterm = 1
            spag_nextblock_1 = 11
         ELSE
!
!     SINGULAR MATRIX. INCREMENT LAMBDA AND TRY ONCE MORE
!
            ising = 1
            lambda(1) = lambda(1) + .02*rzero
            lambda(2) = lambda(2) + .02*rzero
            spag_nextblock_1 = 5
         ENDIF
      CASE (6)
!
!     CALL IN MAIN LINK TO ITERATE FOR EIGENVALUES
!
         CALL cinvp3
         IF ( left==1 ) THEN
!
!     CALL IN LINK TO COMPUTE THE LEFT EIGENVECTOR
!
            dtemp(1) = lambda(1)
            dtemp(2) = lambda(2)
            lambda(1) = lam1(1)
            lambda(2) = lam1(2)
         ELSEIF ( comflg==2 ) THEN
            nomovs = nomovs + 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( comflg==1 ) THEN
            ising = 0
            switch = 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( comflg>=3 ) THEN
            iterm = comflg
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( comflg==0 ) THEN
!
!     FIND NEXT SHIFT POINT WHICH IS CLOSEST TO THE ORIGIN
!
            IF ( imin/=1 ) THEN
               IF ( imax==nshift ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               xx = lmbda(1) - deltx
               yy = lmbda(2) - delty
               rang = xx**2 + yy**2
               xx = lmbda(1) + deltx
               yy = lmbda(2) + delty
               range = xx**2 + yy**2
               IF ( range<=rang ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 9
            ELSE
               IF ( imax/=nshift ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     REGIONS COMPLETED
!
               iterm = 3
               spag_nextblock_1 = 11
            ENDIF
            CYCLE
         ELSE
            IF ( idiag/=0 ) THEN
               WRITE (nout,99004) noreg , jreg
99004          FORMAT (2I10)
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         switch = -1
         CALL cinvp1
!
!     DECOMPOSE THE DYNAMIC MATRIX AT THE EIGENVALUE TO OBTAIN THE LEFT
!     EIGENVECTOR BY THE DETERMINATE METHOD
!
         IF ( idiag/=0 ) WRITE (nout,99007) lambda
         CALL cinvp2(*40)
!
!     BUILD LOAD FOR FBS
!
         d1 = nrow/2
         d2 = northo
         DO i = 1 , nrow , 2
            k = (i+1)/2
            dz(i) = sign*mindia/(1.D0+(1.D0-float(k)/d1)*d2)
            dz(i+1) = 0.0D0
         ENDDO
         sign = -sign
         CALL cdifbs(dz(1),z(ibuf))
         lambda(1) = dtemp(1)
         lambda(2) = dtemp(2)
         switch = 0
!
!     NORMALIZE AND STORE THE LEFT EIGENVECTOR
!
         CALL cnorm1(dz(1),filek(2))
         IF ( idiag/=0 ) THEN
            WRITE (nout,99005) (dz(i),i=1,nrow)
99005       FORMAT (///,15H LEFT VECTOR   ,//,(10D12.4))
         ENDIF
         IF ( .NOT.(noleft .OR. isym==0) ) THEN
            ifile = phidli
            CALL open(*60,ifile,z(ibuf),wrt)
            CALL write(ifile,dz(1),nrow2,1)
            CALL close(ifile,norew)
         ENDIF
         ifile = scrfil(10)
         CALL gopen(ifile,z(ibuf),rd)
         CALL bckrec(ifile)
         CALL fread(ifile,dz(nrow+2),nrow2,1)
         CALL bckrec(ifile)
         CALL close(ifile,norew)
!
!     COMPUTE REAL LEFT VECTOR SCALED
!
         CALL cxtrny(dz(1),dz(nrow+2),dtemp)
         CALL cdivid(dz(1),dz(1),dtemp,nrow)
         CALL open(*60,ifile,z(ibuf),wrt)
         CALL write(ifile,dz(1),nrow2,1)
         CALL close(ifile,rew)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 40      lambda(1) = 1.01*lambda(1)
         lambda(2) = 1.01*lambda(2)
         spag_nextblock_1 = 7
      CASE (8)
         IF ( noreg==jreg ) RETURN
         spag_nextblock_1 = 2
      CASE (9)
         shift = shift + 1.
         imax = imax + 1
         lambda(1) = lmbda(1) + deltx
         lambda(2) = lmbda(2) + delty
         spag_nextblock_1 = 3
      CASE (10)
         shift = shift - 1.
         imin = imin - 1
         lambda(1) = lmbda(1) - deltx
         lambda(2) = lmbda(2) - delty
         spag_nextblock_1 = 3
      CASE (11)
!
!     SET UP THE SUMMARY FILE
!
         ifile = dmpfil
         CALL open(*60,dmpfil,z(ibuf),wrt)
         CALL write(dmpfil,ihead(1),10,0)
         i = 0
         iz(i+2) = northo
         iz(i+3) = nostrt
         iz(i+4) = nomovs
         iz(i+5) = nodcmp
         iz(i+6) = iter
         iz(i+7) = iterm
         DO i = 8 , 12
            iz(i) = 0
         ENDDO
         i = 2
         CALL write(dmpfil,iz(i),40,0)
         CALL write(dmpfil,head(1),96,1)
         CALL write(dmpfil,iz(1),0,1)
         CALL close(dmpfil,eofnrw)
!
!     WRITE DUMMY TRAILER
         ixx = filek(1)
         filek(1) = dmpfil
         CALL wrttrl(filek(1))
         filek(1) = ixx
         Nfound = northo
         IF ( idiag/=0 ) THEN
            j = 12
            WRITE (nout,99006) (iz(i),i=1,j)
99006       FORMAT (///,12I10)
         ENDIF
         IF ( iterm==5 ) RETURN
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
 60      CALL mesage(-1,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (10H1LAMBDA = ,2D15.5)
END SUBROUTINE cinvpr
