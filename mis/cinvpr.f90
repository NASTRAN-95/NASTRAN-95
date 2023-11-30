
SUBROUTINE cinvpr(Eed,Method,Nfound)
   IMPLICIT NONE
   REAL Anodes , Anoest , Eofnrw , Eps , Head(1) , Maxmod , Rd , Rdrew , Reg(7,10) , Rew , Rsp , Rzero , Wrt , Wrtrew , Z(1)
   INTEGER Comflg , Dmpfil , Fileb(7) , Filek(7) , Filelm(7) , Filem(7) , Filevc(7) , Idum(30) , Ind , Ind1 , Ireg(7,1) , Istart ,  &
         & Isym , Isys , Iter , Iterx , Ivect , Iz(1) , Kreg , Ksystm(65) , Left , Nochng , Nodes , Noest , Noreg , Norew , Noroot ,&
         & Northo , Nout , Nzero , Phidli , Rdp , Real , Scrfil(11) , Switch , Timed , Typeb , Typek , Typem
   DOUBLE PRECISION Dz(1) , Lam1(2) , Lambda(2) , Lmbda(2) , Mindia
   CHARACTER*23 Ufm
   COMMON /cdcmpx/ Idum , Mindia
   COMMON /cinvpx/ Filek , Filem , Fileb , Filelm , Filevc , Dmpfil , Scrfil , Noreg , Eps , Reg , Phidli
   COMMON /cinvxx/ Lambda , Switch , Comflg , Lmbda , Iter , Timed , Nochng , Rzero , Ind , Ivect , Kreg , Real , Left , Northo ,   &
                 & Noroot , Nzero , Lam1 , Maxmod , Nodes , Noest , Istart , Ind1 , Iterx , Isym
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp
   COMMON /output/ Head
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Eed , Method , Nfound
   REAL anum1 , anum2 , arg , d , d1 , d2 , deltx , delty , flag , l , l1 , r , rang , range , shift , sign , slope , x1 , x2 ,     &
      & xl2 , xx , y1 , y2 , yl2 , yy
   DOUBLE PRECISION dtemp(2)
   INTEGER eigc(2) , i , ibuf , idiag , ifile , ihead(10) , imax , imin , ishift , ising , iterm , ixx , j , jreg , k , n , name(2) &
         & , nodcmp , nomovs , nostrt , nrow , nrow2 , nshift , nz , t1 , t2
   INTEGER korsz
   LOGICAL noleft
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
   ibuf = nz - Isys - 2
   ifile = Filelm(1)
   CALL open(*1300,Filelm,Z(ibuf),Wrtrew)
   CALL close(Filelm,Rew)
   ifile = Filevc(1)
   CALL open(*1300,Filevc,Z(ibuf),Wrtrew)
   CALL close(Filevc,Rew)
   CALL gopen(Dmpfil,Z(ibuf),Wrtrew)
   CALL close(Dmpfil,Eofnrw)
   ifile = Scrfil(10)
   CALL open(*1300,ifile,Z(ibuf),Wrtrew)
   CALL close(ifile,Rew)
   noleft = .FALSE.
   Iz(1) = 204
   CALL rdtrl(Iz)
   IF ( Iz(1)<0 ) noleft = .TRUE.
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
   CALL preloc(*1300,Z(ibuf),Eed)
   CALL locate(*1300,Z(ibuf),eigc(1),flag)
   DO
      CALL fread(Eed,Ireg,10,0)
      IF ( Method==Ireg(1,1) .OR. Method==-1 ) THEN
         jreg = 1
         Eps = .0001
         IF ( Reg(1,2)/=0. ) Eps = Reg(1,2)
         DO
            CALL fread(Eed,Ireg(1,jreg),7,0)
            IF ( Ireg(6,jreg)==-1 ) EXIT
            jreg = jreg + 1
            IF ( jreg>10 ) EXIT
         ENDDO
         CALL close(Eed,Rew)
         Noreg = jreg - 1
         jreg = 0
         EXIT
      ELSE
         DO
            CALL fread(Eed,Ireg,7,0)
            IF ( Ireg(6,1)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
!
!     PICK UP PARAMETERS FOR REGION I
!
 100  jreg = jreg + 1
   Iter = 0
   nodcmp = 0
   nostrt = 0
   nomovs = 0
   x1 = Reg(1,jreg)
   y1 = Reg(2,jreg)
   x2 = Reg(3,jreg)
   y2 = Reg(4,jreg)
   l = Reg(5,jreg)
   Anoest = Reg(6,jreg)
   Anodes = Reg(7,jreg)
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
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 2366, REGION IMPROPERLY DEFINED ON EIGC CARD.')
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
      WRITE (Nout,99002) x1 , y1 , x2 , y2 , l1 , Nodes , Noest , nshift
99002 FORMAT (1H1,5F10.2,3I5)
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
      DO i = 1 , n
         xx = xx + deltx
         yy = yy + delty
         rang = xx**2 + yy**2
         IF ( rang>=range ) EXIT
         range = rang
         shift = i + 1
      ENDDO
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
 200  ishift = shift
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
      WRITE (Nout,99003) Rzero
99003 FORMAT (//,10H RZERO =  ,F10.4)
   ENDIF
   nostrt = nostrt + 1
   Comflg = 0
 300  Lmbda(1) = Lambda(1)
   Lmbda(2) = Lambda(2)
   Nochng = 0
   Switch = 0
   Ivect = 0
   Kreg = 0
   Ind = Ind + 1
   IF ( iabs(Ind)==13 ) Ind = 1
   ising = 0
 400  IF ( Nochng>=4 ) THEN
!
!     4 MOVES WHILE TRACKING ONE ROOT
!
      iterm = 2
      GOTO 1200
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
      IF ( idiag/=0 ) WRITE (Nout,99007) Lambda
      nodcmp = nodcmp + 1
      CALL cinvp2(*500)
      CALL klock(t2)
!
!     DETERMINE THE TIME REQUIRED TO FORM AND DECOMPOSE THE DYNAMIC
!     MATRIX
!
      Timed = t2 - t1
      IF ( Timed==0 ) Timed = 1
      GOTO 600
   ENDIF
 500  IF ( ising==1 ) THEN
!
!     SINGULARITY ENCOUNTERED TWICE IN A ROW
!
      iterm = 1
      GOTO 1200
   ELSE
!
!     SINGULAR MATRIX. INCREMENT LAMBDA AND TRY ONCE MORE
!
      ising = 1
      Lambda(1) = Lambda(1) + .02*Rzero
      Lambda(2) = Lambda(2) + .02*Rzero
      GOTO 400
   ENDIF
!
!     CALL IN MAIN LINK TO ITERATE FOR EIGENVALUES
!
 600  CALL cinvp3
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
      GOTO 300
   ELSEIF ( Comflg==1 ) THEN
      ising = 0
      Switch = 1
      GOTO 400
   ELSEIF ( Comflg>=3 ) THEN
      iterm = Comflg
      GOTO 1200
   ELSEIF ( Comflg==0 ) THEN
!
!     FIND NEXT SHIFT POINT WHICH IS CLOSEST TO THE ORIGIN
!
      IF ( imin/=1 ) THEN
         IF ( imax==nshift ) GOTO 1100
         xx = Lmbda(1) - deltx
         yy = Lmbda(2) - delty
         rang = xx**2 + yy**2
         xx = Lmbda(1) + deltx
         yy = Lmbda(2) + delty
         range = xx**2 + yy**2
         IF ( range>rang ) GOTO 1000
         GOTO 1100
      ELSE
         IF ( imax/=nshift ) GOTO 1000
!
!     REGIONS COMPLETED
!
         iterm = 3
         GOTO 1200
      ENDIF
   ELSE
      IF ( idiag/=0 ) THEN
         WRITE (Nout,99004) Noreg , jreg
99004    FORMAT (2I10)
      ENDIF
      GOTO 900
   ENDIF
 700  Switch = -1
   CALL cinvp1
!
!     DECOMPOSE THE DYNAMIC MATRIX AT THE EIGENVALUE TO OBTAIN THE LEFT
!     EIGENVECTOR BY THE DETERMINATE METHOD
!
   IF ( idiag/=0 ) WRITE (Nout,99007) Lambda
   CALL cinvp2(*800)
!
!     BUILD LOAD FOR FBS
!
   d1 = nrow/2
   d2 = Northo
   DO i = 1 , nrow , 2
      k = (i+1)/2
      Dz(i) = sign*Mindia/(1.D0+(1.D0-float(k)/d1)*d2)
      Dz(i+1) = 0.0D0
   ENDDO
   sign = -sign
   CALL cdifbs(Dz(1),Z(ibuf))
   Lambda(1) = dtemp(1)
   Lambda(2) = dtemp(2)
   Switch = 0
!
!     NORMALIZE AND STORE THE LEFT EIGENVECTOR
!
   CALL cnorm1(Dz(1),Filek(2))
   IF ( idiag/=0 ) THEN
      WRITE (Nout,99005) (Dz(i),i=1,nrow)
99005 FORMAT (///,15H LEFT VECTOR   ,//,(10D12.4))
   ENDIF
   IF ( .NOT.(noleft .OR. Isym==0) ) THEN
      ifile = Phidli
      CALL open(*1300,ifile,Z(ibuf),Wrt)
      CALL write(ifile,Dz(1),nrow2,1)
      CALL close(ifile,Norew)
   ENDIF
   ifile = Scrfil(10)
   CALL gopen(ifile,Z(ibuf),Rd)
   CALL bckrec(ifile)
   CALL fread(ifile,Dz(nrow+2),nrow2,1)
   CALL bckrec(ifile)
   CALL close(ifile,Norew)
!
!     COMPUTE REAL LEFT VECTOR SCALED
!
   CALL cxtrny(Dz(1),Dz(nrow+2),dtemp)
   CALL cdivid(Dz(1),Dz(1),dtemp,nrow)
   CALL open(*1300,ifile,Z(ibuf),Wrt)
   CALL write(ifile,Dz(1),nrow2,1)
   CALL close(ifile,Rew)
   GOTO 600
 800  Lambda(1) = 1.01*Lambda(1)
   Lambda(2) = 1.01*Lambda(2)
   GOTO 700
 900  IF ( Noreg==jreg ) RETURN
   GOTO 100
 1000 shift = shift + 1.
   imax = imax + 1
   Lambda(1) = Lmbda(1) + deltx
   Lambda(2) = Lmbda(2) + delty
   GOTO 200
 1100 shift = shift - 1.
   imin = imin - 1
   Lambda(1) = Lmbda(1) - deltx
   Lambda(2) = Lmbda(2) - delty
   GOTO 200
!
!     SET UP THE SUMMARY FILE
!
 1200 ifile = Dmpfil
   CALL open(*1300,Dmpfil,Z(ibuf),Wrt)
   CALL write(Dmpfil,ihead(1),10,0)
   i = 0
   Iz(i+2) = Northo
   Iz(i+3) = nostrt
   Iz(i+4) = nomovs
   Iz(i+5) = nodcmp
   Iz(i+6) = Iter
   Iz(i+7) = iterm
   DO i = 8 , 12
      Iz(i) = 0
   ENDDO
   i = 2
   CALL write(Dmpfil,Iz(i),40,0)
   CALL write(Dmpfil,Head(1),96,1)
   CALL write(Dmpfil,Iz(1),0,1)
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
      WRITE (Nout,99006) (Iz(i),i=1,j)
99006 FORMAT (///,12I10)
   ENDIF
   IF ( iterm==5 ) RETURN
   GOTO 900
!
 1300 CALL mesage(-1,ifile,name)
99007 FORMAT (10H1LAMBDA = ,2D15.5)
END SUBROUTINE cinvpr