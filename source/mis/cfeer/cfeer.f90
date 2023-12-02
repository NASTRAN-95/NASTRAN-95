!*==cfeer.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfeer(Eed,Method,Nfound)
USE C_FEERAA
USE C_FEERXC
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
   REAL :: anodes , asym , flag , unidum , x1 , y1
   INTEGER , DIMENSION(2) , SAVE :: eigc , name
   INTEGER , DIMENSION(10) :: have , want
   INTEGER :: i , ibuf , ifile , iopn , iret , ising , iterm , ixx , limsum , nbpw , nodcmp , nodes , nomnf , nonsym , nout ,       &
            & nprint , nz
   INTEGER , DIMENSION(10) , SAVE :: ihead
   INTEGER , DIMENSION(7,1) :: ireg
   INTEGER , DIMENSION(1) :: iz
   EXTERNAL cfeer1 , cfeer2 , cfeer3 , cfeer4 , close , fread , gopen , korsz , locate , mesage , open , preloc , rdtrl , sswtch ,  &
          & write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Ireg(1,1),Reg(1,1)) , (anodes,nodes) , (Ksystm(2),Nout) , (Ksystm(40),Nbpw) , (asym,nonsym) , (Z(1),Iz(1))
   DATA eigc/207 , 2/
   DATA ihead/0 , 1009 , 2 , 7*0/
   DATA name/4HCFCN , 4HTL  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         IF ( nbpw>=60 ) THEN
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
         IF ( Idiag/=0 ) WRITE (nout,99001) iopn
!
!
99001    FORMAT (1H1,27X,'*****  F E E R  *****  (FAST EIGENVALUE',' EXTRACTION ROUTINE)  *****',////,1H ,I10,' SINGLE ',           &
                &'PRECISION WORDS OF OPEN CORE, NOT USED (SUBROUTINE ','CFEER)',//)
         IF ( iopn<=0 ) CALL mesage(-8,0,name)
         Minopn = iopn
         Ilam(1) = 308
         Iphi(1) = 309
         ifile = Ilam(1)
         CALL open(*20,Ilam,Z(ibuf),Wrtrew)
         CALL close(Ilam,Rew)
         ifile = Iphi(1)
         CALL open(*20,Iphi,Z(ibuf),Wrtrew)
         CALL close(Iphi,Rew)
         CALL gopen(Idmpfl,Z(ibuf),Wrtrew)
         CALL close(Idmpfl,Eofnrw)
!
!     PROCURE DATA FROM MAIN EIGC CARD
!
         ifile = Eed
         CALL preloc(*20,Z(ibuf),Eed)
         CALL locate(*20,Z(ibuf),eigc(1),flag)
         SPAG_Loop_1_4: DO
            CALL fread(Eed,ireg,10,0)
            IF ( ireg(1,1)==Method ) THEN
               Jreg = 1
               Eps = .1D0/Ik(2)/100.D0
               IF ( Reg(1,2)>0. ) Eps = dble(Reg(1,2))/100.D0
               unidum = sngl(Eps)*100.
               IF ( Idiag/=0 ) WRITE (nout,99002) unidum , Reg(1,2)
99002          FORMAT (1H0,5HCFEER,6X,18HACCURACY CRITERION,1P,E16.8,8X,12H(INPUT VALUE,E16.8,1H))
               SPAG_Loop_2_1: DO
!
!     PROCURE DATA FROM EIGC CONTINUATION CARDS
!
                  CALL fread(Eed,ireg(1,Jreg),7,0)
                  IF ( ireg(6,Jreg)==-1 ) EXIT SPAG_Loop_2_1
                  Jreg = Jreg + 1
                  IF ( Jreg>10 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
               CALL close(Eed,Rew)
               Noreg = Jreg - 1
               nodcmp = 0
               Numort = 0
               Numran = 0
               Jreg = 0
               SPAG_Loop_2_3: DO
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
                     IF ( Idiag/=0 ) WRITE (nout,99003) Jreg , x1 , y1 , nodes , nonsym
99003                FORMAT (1H0,5HCFEER,6X,12HNEIGHBORHOOD,I3,8X,8HCENTER =,2F18.8,8X,15HNO. DES. RTS. =,I5,8X,8HNONSYM =,I2/1H )
!
!     TEST IF USER PICKED THE ORIGIN
!
                     IF ( x1==0. .AND. y1==0. ) THEN
                        x1 = x1 + .001
                        WRITE (nout,99004) Uwm
99004                   FORMAT (A25,' 3149',//5X,'USER SPECIFIED NEIGHBORHOOD CENTERED AT',                                         &
                               &' ORIGIN NOT ALLOWED, CENTER SHIFTED TO THE RIGHT .001',//)
                     ENDIF
                     IF ( nodes<=0 ) THEN
                        WRITE (nout,99005) Uwm , nodes
99005                   FORMAT (A25,' 3150',//5X,'DESIRED NUMBER OF EIGENVALUES',I8,3X,'INVALID. SET = 1.',//)
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
                     IF ( nodes>Nord ) WRITE (nout,99006) Uwm , nodes , Jreg , Noreg , Lambda , Nord
99006                FORMAT (A25,' 3161',//5X,'DESIRED NUMBER OF EIGENSOLUTIONS',I5,' FOR NEIGHBORHOOD',I3,' OF',I3,' CENTERED AT ',&
                           & 1P,2D16.8,//5X,'EXCEEDS THE EXISTING NUMBER',I5,', ALL EIGENSOLUTIONS WILL BE SOUGHT.',//)
                     ising = 0
                     SPAG_Loop_3_2: DO
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
                           WRITE (nout,99007) Uwm , iret , Lambda
99007                      FORMAT (A25,' 3151',//5X,'DYNAMIC MATRIX IS SINGULAR (OCCURRENCE',I3,') IN NEIGHBORHOOD CENTERED AT ',1P,&
                                 & 2D16.8,//)
                           IF ( ising==1 ) EXIT SPAG_Loop_3_2
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
                           IF ( Nstart>2 ) EXIT SPAG_Loop_3_2
!
!     OBTAIN EIGENVALUES AND EIGENVECTORS
!
                           CALL cfeer4
                           have(Jreg) = Mreduc
                           IF ( Mreduc>nodes ) THEN
                              i = Mreduc - nodes
                              WRITE (nout,99008) Uim , i , nodes , Jreg , Noreg , Lambda
99008                         FORMAT (A29,' 3166',//1X,I5,' MORE ACCURATE EIGENSOLUTIONS THAN ','THE',I5,                           &
                                     &' REQUESTED HAVE BEEN FOUND FOR NEIGHBORHOOD',I3,' OF',I3,//5X,'CENTERED AT ',1P,2D16.8,      &
                                     &'. USE DIAG 12 TO DETERMINE ERROR ESTIMATES.',//)
                           ENDIF
                           Nfound = Nfound + Mreduc
                           IF ( Jreg<Noreg .AND. Nfound<Nord ) EXIT SPAG_Loop_3_2
                           EXIT SPAG_Loop_2_3
                        ENDIF
                     ENDDO SPAG_Loop_3_2
                  ELSE
                     Jreg = Noreg
                     IF ( Nzero>0 ) Jskip = -1
                     EXIT SPAG_Loop_2_3
                  ENDIF
               ENDDO SPAG_Loop_2_3
!
!     FEER IS FINISHED. PERFORM WRAP-UP OPERATIONS.
!
               IF ( Jskip<0 ) CALL cfeer4
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
                     WRITE (nout,99009) Uim
99009                FORMAT (A29,' 3159',//5X,'ALL SOLUTIONS HAVE BEEN FOUND.',//)
                     IF ( Jreg<Noreg ) EXIT SPAG_Loop_1_4
                  ENDIF
                  DO i = 1 , Jreg
                     IF ( have(i)<want(i) ) EXIT SPAG_Loop_1_4
                  ENDDO
!
!     EACH REQUESTED NEIGHBORHOOD HAS THE DESIRED NUMBER OF ROOTS
!
                  iterm = 0
               ENDIF
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               SPAG_Loop_2_5: DO
                  CALL fread(Eed,ireg,7,0)
                  IF ( ireg(6,1)==-1 ) EXIT SPAG_Loop_2_5
               ENDDO SPAG_Loop_2_5
            ENDIF
         ENDDO SPAG_Loop_1_4
!
!     AT LEAST ONE REQUESTED NEIGHBORHOOD FAILS TO HAVE THE DESIRED
!     NUMBER OF ROOTS
!
         iterm = 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     WRITE INFORMATION ON NASTRAN SUMMARY FILE
!
         ifile = Idmpfl
         CALL open(*20,Idmpfl,Z(ibuf),Wrt)
         DO i = 1 , limsum
            iz(i) = 0
         ENDDO
         i = 0
         iz(i+2) = Northo
         iz(i+3) = Numran
         iz(i+5) = nodcmp
         iz(i+6) = Numort
         iz(i+7) = iterm
         iz(i+8) = 1
         i = 2
         CALL write(Idmpfl,ihead(1),10,0)
         CALL write(Idmpfl,iz(i),40,0)
         CALL write(Idmpfl,Head(1),96,1)
         CALL write(Idmpfl,iz(1),0,1)
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
         IF ( nbpw<36 ) THEN
            i = 4
         ELSEIF ( nbpw==36 ) THEN
            i = 6
         ELSE
            i = 10
            IF ( nbpw==64 ) i = 8
         ENDIF
         i = (i*Minopn)/1000
         IF ( i<0 ) i = 0
         WRITE (nout,99010) Uim , Minopn , i
99010    FORMAT (A29,' 3160',//5X,'MINIMUM OPEN CORE NOT USED BY FEER',I9,' WORDS (',I9,'K BYTES).',//)
         RETURN
!
 20      CALL mesage(-1,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cfeer
