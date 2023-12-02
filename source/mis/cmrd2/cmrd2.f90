!*==cmrd2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2
   USE c_blank
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
         IF ( dry==-2 ) RETURN
         nozwds = korsz(z(1))
         lstzwd = nozwds - 1
         gbuf1 = nozwds - sysbuf - 2
         gbuf2 = gbuf1 - sysbuf
         gbuf3 = gbuf2 - sysbuf
         sbuf1 = gbuf3 - sysbuf
         sbuf2 = sbuf1 - sysbuf - 1
         sbuf3 = sbuf2 - sysbuf
         korlen = sbuf3 - 1
         korbgn = 1
         IF ( korlen<=korbgn ) THEN
            imsg = -8
            ifile = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     INITIALIZE SOF
!
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
!
!     INITIALIZE CASE CONTROL PARAMETERS
!
            DO i = 1 , 11
               IF ( i>6 ) THEN
                  infile(i) = 100 + i
                  iscr(i) = 300 + i
               ELSE
                  infile(i) = 100 + i
                  otfile(i) = 200 + i
                  iscr(i) = 300 + i
               ENDIF
            ENDDO
            DO i = 1 , 2
               oldnam(i) = iblank
               newnam(i) = iblank
            ENDDO
            range(1) = -1.0E+35
            range(2) = 1.0E+35
            symtry = .FALSE.
            nmax = 2147483647
            io = 0
            modes = .FALSE.
            rsave = .FALSE.
            nrange = 0
            ponly = .FALSE.
!
!     PROCESS CASE CONTROL
!
            ifile = casecc
            CALL open(*20,casecc,z(gbuf2),0)
            IF ( step/=0 ) THEN
               DO i = 1 , step
                  CALL fwdrec(*60,casecc)
               ENDDO
            ENDIF
!
!     READ CASECC
!
            CALL read(*40,*60,casecc,z(korbgn),2,0,nwdsrd)
            nwdscc = z(korbgn+1)
            DO i = 1 , nwdscc , 3
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     CALL read(*40,*60,casecc,z(korbgn),3,0,nwdsrd)
!
!     TEST CASE CONTROL MNEMONICS
!
                     SPAG_Loop_4_1: DO j = 1 , 8
                        IF ( z(korbgn)==nmonic(j) ) THEN
                           spag_nextblock_2 = 2
                           EXIT SPAG_Loop_4_1
                        ENDIF
                     ENDDO SPAG_Loop_4_1
                  CASE (2)
!
!     SELECT DATA TO EXTRACT
!
                     IF ( j==2 ) THEN
!
!     EXTRACT NAME OF REDUCED SUBSTRUCTURE
!
                        DO k = 1 , 2
                           newnam(k) = z(korbgn+k)
                        ENDDO
                     ELSEIF ( j==3 ) THEN
!
!     EXTRACT SYMMETRY FLAG
!
                        IF ( z(korbgn+1)==yes ) symtry = .TRUE.
                     ELSEIF ( j==4 ) THEN
!
!     EXTRACT FREQUENCY RANGE
!
                        IF ( nrange==1 ) THEN
                           range(2) = rz(korbgn+2)
                        ELSE
                           nrange = 1
                           range(1) = rz(korbgn+2)
                        ENDIF
                     ELSEIF ( j==5 ) THEN
!
!     EXTRACT MAXIMUM NUMBER OF FREQUENCIES
!
                        IF ( z(korbgn)/=0 ) nmax = z(korbgn+2)
                     ELSEIF ( j==6 ) THEN
!
!     EXTRACT OUTPUT FLAGS
!
                        io = orf(io,z(korbgn+2))
                     ELSEIF ( j==7 ) THEN
!
!     EXTRACT OLDMODES FLAG
!
                        IF ( z(korbgn+1)==yes ) modes = .TRUE.
                     ELSEIF ( j==8 ) THEN
!
!     EXTRACT REDUCTION SAVE FLAG
!
                        IF ( z(korbgn+1)==yes ) rsave = .TRUE.
                     ELSE
!
!     EXTRACT NAME OF SUBSTRUCTURE BEING REDUCED
!
                        DO k = 1 , 2
                           oldnam(k) = z(korbgn+k)
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
               symtry = .TRUE.
               npass = 1
            ENDIF
!
!     CHECK FOR RUN = GO
!
            ihorg = 0
            IF ( dry/=0 ) THEN
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
                        CALL softrl(oldnam,nhhlft,itrlr)
                        IF ( itrlr(1)==1 ) CYCLE
                        ihorg = ihorg + 2
                     ELSE
                        CALL softrl(oldnam,nhhorg,itrlr)
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
                  CALL sfetch(newnam,nhlods,3,itest)
                  IF ( itest==3 ) ponly = .TRUE.
                  CALL sfetch(newnam,nhloap,3,itest)
                  IF ( itest==3 ) ponly = .TRUE.
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
            IF ( dry==-2 ) WRITE (iprntr,99001)
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
