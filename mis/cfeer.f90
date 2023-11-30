
SUBROUTINE cfeer(Eed,Method,Nfound)
   IMPLICIT NONE
   REAL Eofnrw , Head(1) , Rd , Rdp , Rdrew , Reg(7,10) , Rew , Rsp , Ten2mt , Tenmht , Tenmtt , Wrt , Wrtrew , Z(1)
   DOUBLE PRECISION Eps , Lambda(2)
   INTEGER Ib(7) , Idiag , Idmpfl , Ik(7) , Ilam(7) , Im(7) , Iphi(7) , Ireg(7,1) , Iscr(11) , It , Iz(1) , Jreg , Jskip ,          &
         & Ksystm(65) , Mcblmb(7) , Mcblt(7) , Mcbut(7) , Mcbvec(7) , Minopn , Mreduc , Nbpw , Nord , Nord2 , Nord4 , Nordp1 ,      &
         & Noreg , Norew , Northo , Nout , Nstart , Nswp , Numort , Numran , Nzero
   LOGICAL Nob , Qpr , Symmet
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /feeraa/ Ik , Im , Ib , Ilam , Iphi , Idmpfl , Iscr , Reg , Mcblt , Mcbut , Mcbvec , Mcblmb
   COMMON /feerxc/ Lambda , Symmet , Mreduc , Nord , Idiag , Eps , Northo , Nord2 , Nord4 , Nordp1 , Nswp , Jskip , Nob , It ,      &
                 & Ten2mt , Tenmht , Nstart , Qpr , Jreg , Noreg , Nzero , Tenmtt , Minopn , Numort , Numran
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp
   COMMON /output/ Head
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER Eed , Method , Nfound
   REAL anodes , asym , flag , unidum , x1 , y1
   INTEGER eigc(2) , have(10) , i , ibuf , ifile , ihead(10) , iopn , iret , ising , iterm , ixx , limsum , name(2) , nodcmp ,      &
         & nodes , nomnf , nonsym , nprint , nz , want(10)
   INTEGER korsz
!
!     PREVIOUS THIS ROUITNE IS CALLED CFCNTL
!
!     GIVEN REAL OR COMPLEX MATRICES, CFEER WILL SOLVE FOR THE
!     REQUESTED NUMBER OF EIGENVALUES AND EIGENVECTORS CLOSEST TO A
!     SPECIFIED POINT IN THE COMPLEX PLANE, FOR UP TO TEN POINTS,
!     VIA THE TRIDIAGONAL REDUCTION (FEER) METHOD.
!     THE SUBROUTINE NAME  CFEER  STANDS FOR COMPLEX FEER CONTROL.
!
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!
!     IK(7)    = MATRIX CONTROL BLOCK FOR THE INPUT STIFFNESS MATRIX K
!     IM(7)    = MATRIX CONTROL BLOCK FOR THE INPUT MASS      MATRIX M
!     IB(7)    = MATRIX CONTROL BLOCK FOR THE INPUT DAMPING   MATRIX B
!     ILAM(7)  = MATRIX CONTROL BLOCK FOR THE OUTPUT EIGENVALUES
!     IPHI(7)  = MATRIX CONTROL BLOCK FOR THE OUTPUT EIGENVECTORS
!     IDMPFL   = FILE CONTAINING THE EIGENVALUE SUMMARY
!     ISCR(11) = SCRATCH FILES USED INTERNALLY
!     REG(1,I) = INPUT REAL      PART OF CENTER I (LAMBDA)
!     REG(2,I) = INPUT IMAGINARY PART OF CENTER I (LAMBDA)
!     REG(5,I) = PROBLEM SIZE MAXIMUM FOR SETTING QPR
!     REG(6,I) = SUPPRESSES ANY SPECIAL SYMMETRY LOGIC
!     REG(7,I) = NUMBER OF DESIRED ROOTS AROUND CENTER I
!     REG(8,1) = CONVERGENCE CRITERION (EQUIV. TO REG(1,2) TEMPORARILY)
!
   EQUIVALENCE (Ireg(1,1),Reg(1,1)) , (anodes,nodes) , (Ksystm(2),Nout) , (Ksystm(40),Nbpw) , (asym,nonsym) , (Z(1),Iz(1))
   DATA eigc/207 , 2/
   DATA ihead/0 , 1009 , 2 , 7*0/
   DATA name/4HCFCN , 4HTL  /
!
!     FILE ALLOCATION
!
!     ISCR( 1)  CONTAINS  (LAMBDA**2*M + LAMBDA*B + K) = DYNAMIC MATRIX
!     ISCR( 2)  CONTAINS -(LAMBDA*M + B) = NOT REQUIRED WHEN B = 0
!     ISCR( 3)  CONTAINS LOWER TRIANGLE OF DECOMPOSED DYNAMIC MATRIX
!     ISCR( 4)  CONTAINS UPPER TRIANGLE OF DECOMPOSED DYNAMIC MATRIX
!     ISCR( 5)  CONTAINS REDUCED TRIDIAGONAL MATRIX ELEMENTS
!     ISCR( 6)  CONTAINS SPECIAL UPPER TRIANGLE FOR TRANSPOSED SWEEP
!     ISCR( 7)  CONTAINS THE ORTHOGONAL VECTORS
!     ISCR( 8)  CONTAINS OUTPUT EIGENVALUES , FOR INPUT TO CEAD1A
!     ISCR( 9)  CONTAINS OUTPUT EIGENVECTORS, FOR INPUT TO CEAD1A
!     ISCR(10)  SCRATCH FILE USED IN CFEER4
!     ISCR(11)  NOT USED
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     NODES  = NUMBER OF DESIRED ROOTS IN CURRENT NEIGHBORHOOD
!     EPS    = ACCURACY CRITERION - USED FOR REJECTING EIGENSOLUTIONS
!     NOREG  = TOTAL NUMBER OF CENTERS (NEIGHBORHOODS) INPUT,
!              EQUIVALENT TO THE NUMBER OF EIGC CONTINUATION CARDS
!     JREG   = COUNTER FOR CURRENT NEIGHBORHOOD
!     MREDUC = SIZE OF THE REDUCED PROBLEM IN CURRENT NEIGHBORHOOD
!     NFOUND = ACCUMULATED NUMBER OF ACCEPTABLE EIGENSOLUTIONS
!     NORD   = 2*N IF B.NE.0 AND = N IF B.EQ.0, WHERE B IS THE
!              DAMPING MATRIX AND N IS THE PROBLEM SIZE
!     NORD2  = VECTOR SIZE OF ORIGINAL PROBLEM (COMPLEX SINGLE
!              PRECISION OR COMPLEX DOUBLE PRECISION)
!     NSWP   = COMPLEX VECTOR SIZE FOR SWEEP ALGORITHM
!     NO B   = LOGICAL INDICATOR FOR ABSENCE OF DAMPING MATRIX B
!     SYMMET = LOGICAL INDICATOR FOR SYMMETRIC DYNAMIC MATRIX
!     NONSYM = PROGRAM INPUT WHICH FORCES THE PROGRAM TO CONSIDER
!              THE DYNAMIC MATRIX AS NON-SYMMETRIC
!     IT     = NUMBER OF DECIMAL DIGITS OF ACCURACY FOR THE COMPUTER
!     TEN2MT = 10**(2-T) CONVERGENCE CRITERION
!     TENMHT = 10**(-HALF*T) CONVERGENCE CRITERION
!     TENMTT = 10**(-THIRD*T) RIGID BODY ROOT CRITERION
!     NORTHO = TOTAL CURRENT NUMBER OF ORTHOGONAL VECTOR PAIRS ON
!              ORTHOGONAL VECTOR FILE. INITIALIZED TO NUMBER OF
!              EIGENVECTOR PAIRS ON THE RESTART FILE.
!     MINOPN = MINIMUM OPEN CORE NOT USED (WORDS)
!     NSTART = NUMBER OF INITIAL REORTHOGONALIZATION ATTEMPTS
!     IDIAG  = DIAG 12 PRINT CONTROL
!     QPR    = LOGICAL INDICATOR FOR VERY DETAILED PRINTOUT
!     WANT   = ARRAY OF DESIRED NUMBER OF ROOTS IN EACH NEIGHBORHOOD
!     HAVE   = ARRAY OF ACTUAL  NUMBER OF ROOTS IN EACH NEIGHBORHOOD
!
   Northo = 0
   Nfound = Northo
   Nzero = Northo
   Jskip = 0
   CALL sswtch(12,Idiag)
!
!     TEST COMPUTING MACHINE TYPE AND SET PRECISION PARAMETERS
!
   IF ( Nbpw>=60 ) THEN
      It = 14*Ksystm(55)
   ELSE
      It = 8*Ksystm(55)
   ENDIF
   Ten2mt = 10.**(2-It)
   Tenmht = 10.**(-It/2)
   Tenmtt = 10.**(-It/3)
   Ik(1) = 101
   CALL rdtrl(Ik)
   Im(1) = 103
   CALL rdtrl(Im)
   Ib(1) = 102
   CALL rdtrl(Ib)
   IF ( Ib(1)<0 .OR. Ib(6)==0 ) Ib(1) = 0
!
!     DETERMINE IF THE DYNAMIC MATRIX IS SYMMETRIC
!
   Symmet = .FALSE.
   IF ( Ik(1)==0 .OR. Ik(4)==6 ) THEN
      IF ( Im(1)==0 .OR. Im(4)==6 ) THEN
         IF ( Ib(1)==0 .OR. Ib(4)==6 ) Symmet = .TRUE.
      ENDIF
   ENDIF
   DO i = 1 , 11
      Iscr(i) = 300 + i
   ENDDO
   Idmpfl = 203
   nz = korsz(Z)
   ibuf = nz - Ksystm(1) - 2
   limsum = 12
   iopn = ibuf - limsum
   IF ( Idiag/=0 ) WRITE (Nout,99001) iopn
!
!
99001 FORMAT (1H1,27X,'*****  F E E R  *****  (FAST EIGENVALUE',' EXTRACTION ROUTINE)  *****',////,1H ,I10,' SINGLE ',              &
             &'PRECISION WORDS OF OPEN CORE, NOT USED (SUBROUTINE ','CFEER)',//)
   IF ( iopn<=0 ) CALL mesage(-8,0,name)
   Minopn = iopn
   Ilam(1) = 308
   Iphi(1) = 309
   ifile = Ilam(1)
   CALL open(*300,Ilam,Z(ibuf),Wrtrew)
   CALL close(Ilam,Rew)
   ifile = Iphi(1)
   CALL open(*300,Iphi,Z(ibuf),Wrtrew)
   CALL close(Iphi,Rew)
   CALL gopen(Idmpfl,Z(ibuf),Wrtrew)
   CALL close(Idmpfl,Eofnrw)
!
!     PROCURE DATA FROM MAIN EIGC CARD
!
   ifile = Eed
   CALL preloc(*300,Z(ibuf),Eed)
   CALL locate(*300,Z(ibuf),eigc(1),flag)
   DO
      CALL fread(Eed,Ireg,10,0)
      IF ( Ireg(1,1)==Method ) THEN
         Jreg = 1
         Eps = .1D0/Ik(2)/100.D0
         IF ( Reg(1,2)>0. ) Eps = dble(Reg(1,2))/100.D0
         unidum = sngl(Eps)*100.
         IF ( Idiag/=0 ) WRITE (Nout,99002) unidum , Reg(1,2)
99002    FORMAT (1H0,5HCFEER,6X,18HACCURACY CRITERION,1P,E16.8,8X,12H(INPUT VALUE,E16.8,1H))
         DO
!
!     PROCURE DATA FROM EIGC CONTINUATION CARDS
!
            CALL fread(Eed,Ireg(1,Jreg),7,0)
            IF ( Ireg(6,Jreg)==-1 ) EXIT
            Jreg = Jreg + 1
            IF ( Jreg>10 ) EXIT
         ENDDO
         CALL close(Eed,Rew)
         Noreg = Jreg - 1
         nodcmp = 0
         Numort = 0
         Numran = 0
         Jreg = 0
         DO
!
!     PICK UP PARAMETERS FOR NEIGHBORHOOD I
!
            Jreg = Jreg + 1
            IF ( Jreg<=Noreg ) THEN
               x1 = Reg(1,Jreg)
               y1 = Reg(2,Jreg)
               anodes = Reg(7,Jreg)
               asym = Reg(6,Jreg)
               IF ( nonsym/=0 ) Symmet = .FALSE.
               nprint = ifix(Reg(5,Jreg))
               Qpr = .FALSE.
               IF ( Idiag/=0 .AND. nprint>=Ik(2) ) Qpr = .TRUE.
               IF ( Idiag/=0 ) WRITE (Nout,99003) Jreg , x1 , y1 , nodes , nonsym
99003          FORMAT (1H0,5HCFEER,6X,12HNEIGHBORHOOD,I3,8X,8HCENTER =,2F18.8,8X,15HNO. DES. RTS. =,I5,8X,8HNONSYM =,I2/1H )
!
!     TEST IF USER PICKED THE ORIGIN
!
               IF ( x1==0. .AND. y1==0. ) THEN
                  x1 = x1 + .001
                  WRITE (Nout,99004) Uwm
99004             FORMAT (A25,' 3149',//5X,'USER SPECIFIED NEIGHBORHOOD CENTERED AT',                                               &
                         &' ORIGIN NOT ALLOWED, CENTER SHIFTED TO THE RIGHT .001',//)
               ENDIF
               IF ( nodes<=0 ) THEN
                  WRITE (Nout,99005) Uwm , nodes
99005             FORMAT (A25,' 3150',//5X,'DESIRED NUMBER OF EIGENVALUES',I8,3X,'INVALID. SET = 1.',//)
                  nodes = 1
               ENDIF
               want(Jreg) = nodes
               have(Jreg) = 0
               Nord = 2*Ik(2)
               Nob = .FALSE.
               IF ( Ib(1)<=0 ) THEN
                  Nob = .TRUE.
                  Nord = Ik(2)
               ENDIF
               Nswp = Ik(2)
               Nord2 = 2*Nord
               Nord4 = 2*Nord2
               Nordp1 = Nord + 1
               Mreduc = 2*nodes + 10
               nomnf = Nord - Nfound
               IF ( Mreduc>nomnf ) Mreduc = nomnf
               Lambda(1) = x1
               Lambda(2) = y1
               IF ( nodes>Nord ) WRITE (Nout,99006) Uwm , nodes , Jreg , Noreg , Lambda , Nord
99006          FORMAT (A25,' 3161',//5X,'DESIRED NUMBER OF EIGENSOLUTIONS',I5,' FOR NEIGHBORHOOD',I3,' OF',I3,' CENTERED AT ',1P,   &
                     & 2D16.8,//5X,'EXCEEDS THE EXISTING NUMBER',I5,', ALL EIGENSOLUTIONS WILL BE SOUGHT.',//)
               ising = 0
               DO
!
!      FORM (LAMBDA**2*M + LAMBDA*B + K) = THE DYNAMIC MATRIX
!
                  CALL cfeer1
!
!     CALL IN CDCOMP TO DECOMPOSE THE DYNAMIC MATRIX
!
                  nodcmp = nodcmp + 1
                  CALL cfeer2(iret)
                  IF ( iret/=0 ) THEN
                     iret = iret + ising
                     WRITE (Nout,99007) Uwm , iret , Lambda
99007                FORMAT (A25,' 3151',//5X,'DYNAMIC MATRIX IS SINGULAR (OCCURRENCE',I3,') IN NEIGHBORHOOD CENTERED AT ',1P,      &
                           & 2D16.8,//)
                     IF ( ising==1 ) EXIT
!
!     SINGULAR MATRIX. INCREMENT LAMBDA AND TRY ONCE MORE.
!
                     ising = 1
                     Lambda(1) = Lambda(1) + .02D0
                     Lambda(2) = Lambda(2) + .02D0
                  ELSE
!
!     CALL IN DRIVER TO GENERATE REDUCED TRIDIAGONAL MATRIX
!
                     CALL cfeer3
                     IF ( Nstart>2 ) EXIT
!
!     OBTAIN EIGENVALUES AND EIGENVECTORS
!
                     CALL cfeer4
                     have(Jreg) = Mreduc
                     IF ( Mreduc>nodes ) THEN
                        i = Mreduc - nodes
                        WRITE (Nout,99008) Uim , i , nodes , Jreg , Noreg , Lambda
99008                   FORMAT (A29,' 3166',//1X,I5,' MORE ACCURATE EIGENSOLUTIONS THAN ','THE',I5,                                 &
                               &' REQUESTED HAVE BEEN FOUND FOR NEIGHBORHOOD',I3,' OF',I3,//5X,'CENTERED AT ',1P,2D16.8,            &
                               &'. USE DIAG 12 TO DETERMINE ERROR ESTIMATES.',//)
                     ENDIF
                     Nfound = Nfound + Mreduc
                     IF ( Jreg>=Noreg .OR. Nfound>=Nord ) GOTO 20
                     EXIT
                  ENDIF
               ENDDO
            ELSE
               Jreg = Noreg
               IF ( Nzero>0 ) Jskip = -1
               EXIT
            ENDIF
         ENDDO
!
!     FEER IS FINISHED. PERFORM WRAP-UP OPERATIONS.
!
 20      IF ( Jskip<0 ) CALL cfeer4
         IF ( Nfound==0 ) THEN
!
!     ABNORMAL TERMINATION. NO ROOTS FOUND.
!
            iterm = 2
         ELSE
            IF ( Nfound>=Nord ) THEN
!
!     ALL SOLUTIONS FOUND
!
               WRITE (Nout,99009) Uim
99009          FORMAT (A29,' 3159',//5X,'ALL SOLUTIONS HAVE BEEN FOUND.',//)
               IF ( Jreg<Noreg ) EXIT
            ENDIF
            DO i = 1 , Jreg
               IF ( have(i)<want(i) ) GOTO 100
            ENDDO
!
!     EACH REQUESTED NEIGHBORHOOD HAS THE DESIRED NUMBER OF ROOTS
!
            iterm = 0
         ENDIF
         GOTO 200
      ELSE
         DO
            CALL fread(Eed,Ireg,7,0)
            IF ( Ireg(6,1)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
!
!     AT LEAST ONE REQUESTED NEIGHBORHOOD FAILS TO HAVE THE DESIRED
!     NUMBER OF ROOTS
!
 100  iterm = 1
!
!     WRITE INFORMATION ON NASTRAN SUMMARY FILE
!
 200  ifile = Idmpfl
   CALL open(*300,Idmpfl,Z(ibuf),Wrt)
   DO i = 1 , limsum
      Iz(i) = 0
   ENDDO
   i = 0
   Iz(i+2) = Northo
   Iz(i+3) = Numran
   Iz(i+5) = nodcmp
   Iz(i+6) = Numort
   Iz(i+7) = iterm
   Iz(i+8) = 1
   i = 2
   CALL write(Idmpfl,ihead(1),10,0)
   CALL write(Idmpfl,Iz(i),40,0)
   CALL write(Idmpfl,Head(1),96,1)
   CALL write(Idmpfl,Iz(1),0,1)
   CALL close(Idmpfl,Eofnrw)
!
!     WRITE DUMMY TRAILER
!
   ixx = Ik(1)
   Ik(1) = Idmpfl
   CALL wrttrl(Ik(1))
   Ik(1) = ixx
!
!     INFORM USER IF RUN REGION SIZE CAN BE REDUCED
!
   IF ( Nbpw<36 ) THEN
      i = 4
   ELSEIF ( Nbpw==36 ) THEN
      i = 6
   ELSE
      i = 10
      IF ( Nbpw==64 ) i = 8
   ENDIF
   i = (i*Minopn)/1000
   IF ( i<0 ) i = 0
   WRITE (Nout,99010) Uim , Minopn , i
99010 FORMAT (A29,' 3160',//5X,'MINIMUM OPEN CORE NOT USED BY FEER',I9,' WORDS (',I9,'K BYTES).',//)
   RETURN
!
 300  CALL mesage(-1,ifile,name)
   RETURN
END SUBROUTINE cfeer
