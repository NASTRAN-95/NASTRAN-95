!*==cmrd2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: casecc , i , ifile , ihorg , imsg , itest , j , k , nozwds , npass , nrange , nwdscc , nwdsrd , phissl
   INTEGER , SAVE :: iblank , kaa , nhhlft , nhhorg , nhloap , nhlods , yes
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , DIMENSION(2) , SAVE :: modnam
   INTEGER , DIMENSION(8) , SAVE :: nmonic
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , cmrd2a , cmrd2b , cmrd2c , cmrd2d , cmrd2e , cmrd2f , cmrd2g , fwdrec , korsz , mesage , open , orf , rdtrl ,   &
          & read , sfetch , sofcls , sofopn , softrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE IS THE CMRED2 MODULE WHICH PERFORMS THE MAJOR
!     COMPUTATIONS FOR THE COMPLEX MODAL REDUCE COMMAND.
!
!     DMAP CALLING SEQUENCE
!     CMRED2   CASECC,LAMAMR,PHISSR,PHISSL,EQST,USETMR,KAA,MAA,BAA,K4AA,
!              PAA/KHH,MHH,BHH,K4HH,PHH,POVE/STEP/S,N,DRY/POPT $
!
!     INPUT  DATA
!     GINO - CASECC - CASE CONTROL DATA
!            LAMAMR - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!            PHISSR - RIGHT HAND EIGENVECTORS FOR SUBSTRUCTURE BEING
!                     REDUCED
!            PHISSL - LEFT HAND EIGENVECTORS FOR SUBSTRUCTURE BEING
!                     REDUCED
!            EQST   - EQSS DATA FOR BOUNDARY SET FOR SUBSTRUCTURE BEING
!                     REDUCED
!            USETMR - USET TABLE FOR REDUCED SUBSTRUCTURE
!            KAA    - SUBSTRUCTURE STIFFNESS MATRIX
!            MAA    - SUBSTRUCTURE MASS MATRIX
!            BAA    - SUBSTRUCTURE VISCOUS DAMPING MATRIX
!            K4AA   - SUBSTRUCTURE STRUCTURE DAMPINF MATRIX
!            PAA    - SUBSTRUCTURE LOAD MATRIX
!     SOF  - LAMS   - EIGENVALUE TABLE FOR ORIGINAL SUBSTRUCTURE
!            PHIS   - RIGHT HAND EIGENVECTOR TABLE FOR ORIGINAL
!                     SUBSTRUCTURE
!            PHIL   - LEFT HAND EIGENVECTOR TABLE FOR ORIGINAL
!                     SUBSTRUCTURE
!            HORG   - RIGHT HAND H TRANSFORMATION MATRIX FOR ORIGINAL
!                     SUBSTRUCTURE
!            HLFT   - LEFT HAND H TRANSFORMATION MATRIX FOR ORIGINAL
!                     SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO - KHH    - REDUCED STIFFNESS MATRIX
!            MHH    - REDUCED MASS MATRIX
!            BHH    - REDUCED VISCOUS DAMPING MATRIX
!            K4HH   - REDUCED STRUCTURE DAMPING MATRIX
!            PHH    - REDUCED LOAD MATRIX
!            POVE   - INTERIOR POINT LOAD MATRIX
!     SOF  - LAMS   - EIGENVALUE TABLE FOR ORIGINAL SUBSTRUCTURE
!            PHIS   - RIGHT HAND EIGENVECTOR TABLE FOR ORIG.SUBSTRUCTURE
!            PHIL   - LEFT HAND EIGENVECTOR TABLE FOR ORIG. SUBSTRUCTURE
!            GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS FOR
!                     ORIGINAL SUBSTRUCTURE
!            HORG   - RIGHT HAND H TRANSFORMATION MATRIX FOR ORIGINAL
!                     SUBSTRUCTURE
!            HLFT   - LEFT HAND H TRANSFORMATION MATRIX FOR ORIGINAL
!                     SUBSTRUCTURE
!            UPRT   - PARTITIONING VECTOR FOR CREDUCE FOR ORIGINAL
!                     SUBSTRUCTURE
!            POVE   - INTERNAL POINT LOADS FOR ORIGINAL SUBSTRUCTURE
!            POAP   - INTERNAL POINTS APPENDED LOADS FOR ORIGINAL
!                     SUBSTRUCTURE
!            EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE FOR REDUCED
!                     SUBSTRUCTURE
!            BGSS   - BASIC GRID POINT DEFINITION TABLE FOR REDUCED
!                     SUBSTRUCTURE
!            CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRICES FOR
!                     REDUCED SUBSTRUCTURE
!            LODS   - LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!            LOAP   - APPENDED LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!            PLTS   - PLOT SET DATA FOR REDUCED SUBSTRUCTURE
!            KMTX   - STIFFNESS MATRIX FOR REDUCED SUBSTRUCTURE
!            MMTX   - MASS MATRIX FOR REDUCED SUBSTRUCTURE
!            PVEC   - LOAD MATRIX FOR REDUCED SUBSTRUCTURE
!            PAPD   - APPENDED LOAD MATRIX FOR REDUCED SUBSTRUCTURE
!            BMTX   - VISCOUS DAMPING MATRIX FOR REDUCED SUBSTRUCTURE
!            K4MX   - STRUCTURE DAMPING MATRIX FOR REDUCED SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT  - STEP   - CONTROL DATA CASECC RECORD (INTEGER)
!              POPT   - PVEC OR PAPP OPTION FLAG (BCD)
!     OUTPUT - DRY    - MODULE OPERATION FLAG (INTEGER)
!     OTHERS - GBUF   - GINO BUFFERS
!              SBUF   - SOF BUFFERS
!              INFILE - INPUT FILE NUMBERS
!              OTFILE - OUTPUT FILE NUMBERS
!              ISCR   - ARRAY OF SCRATCH FILE NUMBERS
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!              NEWNAM - NAME OF REDUCED SUBSTRUCTURE
!              SYMTRY - SYMMETRY FLAG
!              RANGE  - RANGE OF FREQUENCIES TO BE USED
!              NMAX   - MAXIMUM NUMBER OF FREQUENCIES TO BE USED
!              IO     - IO OPTIONS FLAG
!              MODES  - OLDMODES OPTION FLAG
!              RSAVE  - SAVE REDUCTION PRODUCT FLAG
!              LAMSAP - BEGINNING ADDRESS OF MODE USE DESCRIPTION ARRAY
!              MODLEN - LENGTH OF MODE USE ARRAY
!              MODPTS - NUMBER OF MODAL POINTS
!
   !>>>>EQUIVALENCE (Casecc,Infile(1)) , (Phissl,Infile(4)) , (Rz(1),Z(1))
   DATA nmonic/4HNAMA , 4HNAMB , 4HSYMF , 4HRANG , 4HNMAX , 4HOUTP , 4HOLDM , 4HRSAV/
   DATA kaa/107/ , iblank , yes/4H     , 4HYES /
   DATA modnam/4HCMRD , 4H2   /
   DATA nhlods , nhloap , nhhorg , nhhlft/4HLODS , 4HLOAP , 4HHORG , 4HHLFT/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     COMPUTE OPEN CORE AND DEFINE GINO, SOF BUFFERS
!
         IF ( Dry==-2 ) RETURN
         nozwds = korsz(Z(1))
         Lstzwd = nozwds - 1
         Gbuf1 = nozwds - Sysbuf - 2
         Gbuf2 = Gbuf1 - Sysbuf
         Gbuf3 = Gbuf2 - Sysbuf
         Sbuf1 = Gbuf3 - Sysbuf
         Sbuf2 = Sbuf1 - Sysbuf - 1
         Sbuf3 = Sbuf2 - Sysbuf
         Korlen = Sbuf3 - 1
         Korbgn = 1
         IF ( Korlen<=Korbgn ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     INITIALIZE SOF
!
            CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
!
!     INITIALIZE CASE CONTROL PARAMETERS
!
            DO i = 1 , 11
               IF ( i>6 ) THEN
                  Infile(i) = 100 + i
                  Iscr(i) = 300 + i
               ELSE
                  Infile(i) = 100 + i
                  Otfile(i) = 200 + i
                  Iscr(i) = 300 + i
               ENDIF
            ENDDO
            DO i = 1 , 2
               Oldnam(i) = iblank
               Newnam(i) = iblank
            ENDDO
            Range(1) = -1.0E+35
            Range(2) = 1.0E+35
            Symtry = .FALSE.
            Nmax = 2147483647
            Io = 0
            Modes = .FALSE.
            Rsave = .FALSE.
            nrange = 0
            Ponly = .FALSE.
!
!     PROCESS CASE CONTROL
!
            ifile = casecc
            CALL open(*20,casecc,Z(Gbuf2),0)
            IF ( Step/=0 ) THEN
               DO i = 1 , Step
                  CALL fwdrec(*60,casecc)
               ENDDO
            ENDIF
!
!     READ CASECC
!
            CALL read(*40,*60,casecc,Z(Korbgn),2,0,nwdsrd)
            nwdscc = Z(Korbgn+1)
            DO i = 1 , nwdscc , 3
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     CALL read(*40,*60,casecc,Z(Korbgn),3,0,nwdsrd)
!
!     TEST CASE CONTROL MNEMONICS
!
                     DO j = 1 , 8
                        IF ( Z(Korbgn)==nmonic(j) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     CYCLE
                  CASE (2)
!
!     SELECT DATA TO EXTRACT
!
                     IF ( j==2 ) THEN
!
!     EXTRACT NAME OF REDUCED SUBSTRUCTURE
!
                        DO k = 1 , 2
                           Newnam(k) = Z(Korbgn+k)
                        ENDDO
                     ELSEIF ( j==3 ) THEN
!
!     EXTRACT SYMMETRY FLAG
!
                        IF ( Z(Korbgn+1)==yes ) Symtry = .TRUE.
                     ELSEIF ( j==4 ) THEN
!
!     EXTRACT FREQUENCY RANGE
!
                        IF ( nrange==1 ) THEN
                           Range(2) = rz(Korbgn+2)
                        ELSE
                           nrange = 1
                           Range(1) = rz(Korbgn+2)
                        ENDIF
                     ELSEIF ( j==5 ) THEN
!
!     EXTRACT MAXIMUM NUMBER OF FREQUENCIES
!
                        IF ( Z(Korbgn)/=0 ) Nmax = Z(Korbgn+2)
                     ELSEIF ( j==6 ) THEN
!
!     EXTRACT OUTPUT FLAGS
!
                        Io = orf(Io,Z(Korbgn+2))
                     ELSEIF ( j==7 ) THEN
!
!     EXTRACT OLDMODES FLAG
!
                        IF ( Z(Korbgn+1)==yes ) Modes = .TRUE.
                     ELSEIF ( j==8 ) THEN
!
!     EXTRACT REDUCTION SAVE FLAG
!
                        IF ( Z(Korbgn+1)==yes ) Rsave = .TRUE.
                     ELSE
!
!     EXTRACT NAME OF SUBSTRUCTURE BEING REDUCED
!
                        DO k = 1 , 2
                           Oldnam(k) = Z(Korbgn+k)
                        ENDDO
                     ENDIF
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            CALL close(casecc,1)
!
!     CHECK FOR SYMMETRY
!
            itrlr(1) = phissl
            CALL rdtrl(itrlr)
            npass = 2
            IF ( itrlr(1)<=0 ) THEN
               Symtry = .TRUE.
               npass = 1
            ENDIF
!
!     CHECK FOR RUN = GO
!
            ihorg = 0
            IF ( Dry/=0 ) THEN
!
!     CHECK FOR STIFFNESS PROCESSING
!
               itrlr(1) = kaa
               CALL rdtrl(itrlr)
               IF ( itrlr(1)>0 ) THEN
!
!     PROCESS STIFFNESS MATRIX
!
                  CALL cmrd2a
!
!     BEGIN COMPLEX MODAL REDUCTION
!     NPASS .EQ. 1, SYMMETRIC REDUCTION
!     NPASS .EQ. 2, UNSYMMETRIC REDUCTION
!
                  DO j = 1 , npass
!
!     TEST FOR H TRANSFORMATION MATRICES
!
                     IF ( j==2 ) THEN
                        CALL softrl(Oldnam,nhhlft,itrlr)
                        IF ( itrlr(1)==1 ) CYCLE
                        ihorg = ihorg + 2
                     ELSE
                        CALL softrl(Oldnam,nhhorg,itrlr)
                        IF ( itrlr(1)==1 ) CYCLE
                        ihorg = ihorg + 1
                     ENDIF
!
!     PREFORM GUYAN REDUCTION
!
                     CALL cmrd2c(j)
!
!     PROCESS OLDMODES FLAG
!
                     CALL cmrd2b(j)
!
!     CALCULATE MODAL TRANSFORMATION MATRIX
!
                     CALL cmrd2d(j)
                     IF ( j==1 ) CALL cmrd2b(3)
!
!     CALCULATE H TRANSFORMATION MATRIX
!
                     CALL cmrd2e(j)
                  ENDDO
               ELSE
!
!     CHECK FOR LOADS ONLY PROCESSING
!
                  CALL sfetch(Newnam,nhlods,3,itest)
                  IF ( itest==3 ) Ponly = .TRUE.
                  CALL sfetch(Newnam,nhloap,3,itest)
                  IF ( itest==3 ) Ponly = .TRUE.
               ENDIF
            ENDIF
!
!     CALCULATE STRUCTURAL MATRICES
!     IHORG .EQ. 0, BOTH HORG, HLFT ON SOF
!     IHORG .EQ. 1, HORG CALCULATED, HLFT ON SOF
!     IHORG .EQ. 2, HORG ON SOF, HLFT CALCULATED
!     IHORG .EQ. 3, BOTH HORG, HLFT CALCULATED
!
            CALL cmrd2f(ihorg)
!
!     PROCESS NEW TABLE ITEMS
!
            IF ( ihorg/=0 ) CALL cmrd2g
!
!     CLOSE ANY OPEN FILES
!
            CALL sofcls
            IF ( Dry==-2 ) WRITE (Iprntr,99001)
!
99001       FORMAT (50H0  MODULE CREDUCE TERMINATING DUE TO ABOVE ERRORS.)
            RETURN
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      imsg = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE cmrd2
