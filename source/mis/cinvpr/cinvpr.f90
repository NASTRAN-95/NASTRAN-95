!*==cinvpr.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvpr(Eed,Method,Nfound)
USE C_CDCMPX
USE C_CINVPX
USE C_CINVXX
USE C_NAMES
USE C_OUTPUT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
         Ind1 = 0
         nz = korsz(Z)
         CALL klock(Istart)
         ibuf = nz - isys - 2
         ifile = Filelm(1)
         CALL open(*60,Filelm,Z(ibuf),Wrtrew)
         CALL close(Filelm,Rew)
         ifile = Filevc(1)
         CALL open(*60,Filevc,Z(ibuf),Wrtrew)
         CALL close(Filevc,Rew)
         CALL gopen(Dmpfil,Z(ibuf),Wrtrew)
         CALL close(Dmpfil,Eofnrw)
         ifile = Scrfil(10)
         CALL open(*60,ifile,Z(ibuf),Wrtrew)
         CALL close(ifile,Rew)
         noleft = .FALSE.
         iz(1) = 204
         CALL rdtrl(iz)
         IF ( iz(1)<0 ) noleft = .TRUE.
         Northo = 0
         nrow = 2*Filek(3)
         nrow2 = 2*nrow
         Isym = 1
         IF ( Filek(1)==0 .OR. Filek(4)==6 ) THEN
            IF ( Filem(1)==0 .OR. Filem(4)==6 ) THEN
               IF ( Fileb(1)==0 .OR. Fileb(4)==6 ) Isym = 0
            ENDIF
         ENDIF
!
!     PICK UP REGION PARAMETERS
!
         CALL preloc(*60,Z(ibuf),Eed)
         CALL locate(*60,Z(ibuf),eigc(1),flag)
         SPAG_Loop_1_2: DO
            CALL fread(Eed,ireg,10,0)
            IF ( Method==ireg(1,1) .OR. Method==-1 ) THEN
               jreg = 1
               Eps = .0001
               IF ( Reg(1,2)/=0. ) Eps = Reg(1,2)
               SPAG_Loop_2_1: DO
                  CALL fread(Eed,ireg(1,jreg),7,0)
                  IF ( ireg(6,jreg)==-1 ) EXIT SPAG_Loop_2_1
                  jreg = jreg + 1
                  IF ( jreg>10 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
               CALL close(Eed,Rew)
               Noreg = jreg - 1
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
         Iter = 0
         nodcmp = 0
         nostrt = 0
         nomovs = 0
         x1 = Reg(1,jreg)
         y1 = Reg(2,jreg)
         x2 = Reg(3,jreg)
         y2 = Reg(4,jreg)
         l = Reg(5,jreg)
         anoest = Reg(6,jreg)
         anodes = Reg(7,jreg)
         IF ( Nodes==0 ) Nodes = 3*Noest
         nshift = sqrt((x1-x2)**2+(y1-y2)**2)/l + 1.
         l1 = l*.5
         Noroot = 0
!
!
!     FIND SHIFT POINT CLOSEST TO THE ORIGIN
!
         r = sqrt((x1-x2)**2+(y1-y2)**2)
         IF ( r<=0 ) THEN
            WRITE (nout,99001) Ufm
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
            WRITE (nout,99002) x1 , y1 , x2 , y2 , l1 , Nodes , Noest , nshift
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
         Maxmod = xl2**2 + yl2**2
         xx = x2 - anum1
         yy = y2 + anum2
         Maxmod = amax1(Maxmod,xx**2+yy**2)
         xx = x1 + anum1
         yy = y1 - anum2
         Maxmod = amax1(Maxmod,xx**2+yy**2)
         xx = x1 - anum1
         yy = y1 + anum2
         Maxmod = amax1(Maxmod,xx**2+yy**2)
!
!     INITIALIZE
!
         Ind = 0
         Left = 0
         spag_nextblock_1 = 3
      CASE (3)
         ishift = shift
!
!     EVALUATE THE VALUE OF LAMBDA IN THE CENTER OF THE CURRENT SEARCH
!     REGION
!
         Lambda(1) = x2 + (shift-.5)*deltx
         Lambda(2) = y2 + (shift-.5)*delty
         IF ( Lambda(2)==0.0D0 ) Lambda(2) = .01*delty
!
!     COMPUTE DISTANCE TO FARTHEST CORNER OF THE SQUARE SEARCH REGION
!
         xx = xl2 + shift*deltx
         yy = yl2 + shift*delty
         Rzero = (Lambda(1)-xx)**2 + (Lambda(2)-yy)**2
         Rzero = sqrt(Rzero)*1.05
         IF ( idiag/=0 ) THEN
            WRITE (nout,99003) Rzero
99003       FORMAT (//,10H RZERO =  ,F10.4)
         ENDIF
         nostrt = nostrt + 1
         Comflg = 0
         spag_nextblock_1 = 4
      CASE (4)
         Lmbda(1) = Lambda(1)
         Lmbda(2) = Lambda(2)
         Nochng = 0
         Switch = 0
         Ivect = 0
         Kreg = 0
         Ind = Ind + 1
         IF ( iabs(Ind)==13 ) Ind = 1
         ising = 0
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Nochng>=4 ) THEN
!
!     4 MOVES WHILE TRACKING ONE ROOT
!
            iterm = 2
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Nochng = Nochng + 1
            CALL klock(t1)
!
!     CALL IN ADD LINK TO FORM (LAMBDA**2*M + LAMBDA*B + K)
!
            CALL cinvp1
!
!     CALL IN CD COMP TO DECOMPOSE THE MATRIX
!
            IF ( idiag/=0 ) WRITE (nout,99007) Lambda
            nodcmp = nodcmp + 1
            CALL cinvp2(*20)
            CALL klock(t2)
!
!     DETERMINE THE TIME REQUIRED TO FORM AND DECOMPOSE THE DYNAMIC
!     MATRIX
!
            Timed = t2 - t1
            IF ( Timed==0 ) Timed = 1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      IF ( ising==1 ) THEN
!
!     SINGULARITY ENCOUNTERED TWICE IN A ROW
!
            iterm = 1
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SINGULAR MATRIX. INCREMENT LAMBDA AND TRY ONCE MORE
!
            ising = 1
            Lambda(1) = Lambda(1) + .02*Rzero
            Lambda(2) = Lambda(2) + .02*Rzero
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (6)
!
!     CALL IN MAIN LINK TO ITERATE FOR EIGENVALUES
!
         CALL cinvp3
         IF ( Left==1 ) THEN
!
!     CALL IN LINK TO COMPUTE THE LEFT EIGENVECTOR
!
            dtemp(1) = Lambda(1)
            dtemp(2) = Lambda(2)
            Lambda(1) = Lam1(1)
            Lambda(2) = Lam1(2)
         ELSEIF ( Comflg==2 ) THEN
            nomovs = nomovs + 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Comflg==1 ) THEN
            ising = 0
            Switch = 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Comflg>=3 ) THEN
            iterm = Comflg
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Comflg==0 ) THEN
!
!     FIND NEXT SHIFT POINT WHICH IS CLOSEST TO THE ORIGIN
!
            IF ( imin/=1 ) THEN
               IF ( imax==nshift ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               xx = Lmbda(1) - deltx
               yy = Lmbda(2) - delty
               rang = xx**2 + yy**2
               xx = Lmbda(1) + deltx
               yy = Lmbda(2) + delty
               range = xx**2 + yy**2
               IF ( range<=rang ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
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
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            IF ( idiag/=0 ) THEN
               WRITE (nout,99004) Noreg , jreg
99004          FORMAT (2I10)
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         Switch = -1
         CALL cinvp1
!
!     DECOMPOSE THE DYNAMIC MATRIX AT THE EIGENVALUE TO OBTAIN THE LEFT
!     EIGENVECTOR BY THE DETERMINATE METHOD
!
         IF ( idiag/=0 ) WRITE (nout,99007) Lambda
         CALL cinvp2(*40)
!
!     BUILD LOAD FOR FBS
!
         d1 = nrow/2
         d2 = Northo
         DO i = 1 , nrow , 2
            k = (i+1)/2
            dz(i) = sign*Mindia/(1.D0+(1.D0-float(k)/d1)*d2)
            dz(i+1) = 0.0D0
         ENDDO
         sign = -sign
         CALL cdifbs(dz(1),Z(ibuf))
         Lambda(1) = dtemp(1)
         Lambda(2) = dtemp(2)
         Switch = 0
!
!     NORMALIZE AND STORE THE LEFT EIGENVECTOR
!
         CALL cnorm1(dz(1),Filek(2))
         IF ( idiag/=0 ) THEN
            WRITE (nout,99005) (dz(i),i=1,nrow)
99005       FORMAT (///,15H LEFT VECTOR   ,//,(10D12.4))
         ENDIF
         IF ( .NOT.(noleft .OR. Isym==0) ) THEN
            ifile = Phidli
            CALL open(*60,ifile,Z(ibuf),Wrt)
            CALL write(ifile,dz(1),nrow2,1)
            CALL close(ifile,Norew)
         ENDIF
         ifile = Scrfil(10)
         CALL gopen(ifile,Z(ibuf),Rd)
         CALL bckrec(ifile)
         CALL fread(ifile,dz(nrow+2),nrow2,1)
         CALL bckrec(ifile)
         CALL close(ifile,Norew)
!
!     COMPUTE REAL LEFT VECTOR SCALED
!
         CALL cxtrny(dz(1),dz(nrow+2),dtemp)
         CALL cdivid(dz(1),dz(1),dtemp,nrow)
         CALL open(*60,ifile,Z(ibuf),Wrt)
         CALL write(ifile,dz(1),nrow2,1)
         CALL close(ifile,Rew)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 40      Lambda(1) = 1.01*Lambda(1)
         Lambda(2) = 1.01*Lambda(2)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         IF ( Noreg==jreg ) RETURN
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
         shift = shift + 1.
         imax = imax + 1
         Lambda(1) = Lmbda(1) + deltx
         Lambda(2) = Lmbda(2) + delty
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         shift = shift - 1.
         imin = imin - 1
         Lambda(1) = Lmbda(1) - deltx
         Lambda(2) = Lmbda(2) - delty
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
!
!     SET UP THE SUMMARY FILE
!
         ifile = Dmpfil
         CALL open(*60,Dmpfil,Z(ibuf),Wrt)
         CALL write(Dmpfil,ihead(1),10,0)
         i = 0
         iz(i+2) = Northo
         iz(i+3) = nostrt
         iz(i+4) = nomovs
         iz(i+5) = nodcmp
         iz(i+6) = Iter
         iz(i+7) = iterm
         DO i = 8 , 12
            iz(i) = 0
         ENDDO
         i = 2
         CALL write(Dmpfil,iz(i),40,0)
         CALL write(Dmpfil,Head(1),96,1)
         CALL write(Dmpfil,iz(1),0,1)
         CALL close(Dmpfil,Eofnrw)
!
!     WRITE DUMMY TRAILER
         ixx = Filek(1)
         Filek(1) = Dmpfil
         CALL wrttrl(Filek(1))
         Filek(1) = ixx
         Nfound = Northo
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
