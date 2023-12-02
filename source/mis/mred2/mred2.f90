!*==mred2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2
   USE c_blank
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: casecc , i , ifile , imsg , itest , j , k , mrd2g , nozwds , nrange , nwdscc , nwdsrd
   INTEGER , SAVE :: iblank , nhloap , nhlods
   INTEGER , DIMENSION(7) , SAVE :: itrlr
   INTEGER , DIMENSION(2) , SAVE :: modnam
   INTEGER , DIMENSION(10) , SAVE :: nmonic
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , fwdrec , korsz , mesage , mred2a , mred2b , mred2c , mred2d , mred2e , mred2f , mred2g , mred2h , open , orf ,  &
          & rdtrl , read , sfetch , sofcls , sofopn
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE IS THE MRED2 MODULE WHICH PERFORMS THE MAJOR
!     COMPUTATIONS FOR THE REDUCE COMMAND.
!
!     DMAP CALLING SEQUENCE
!     MRED2    CASECC,LAMAMR,PHISS,EQST,USETMR,KAA,MAA,BAA,K4AA,PAA,DMR,
!              QSM/KHH,MHH,BHH,K4HH,PHH,POVE/STEP/S,N,DRY/POPT $
!
!     12 INPUT DATA BLOCKS
!     GINO -   CASECC - CASE CONTROL DATA
!              LAMAMR - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!              PHISS  - EIGENVECTORS FOR SUBSTRUCTURE BEING REDUCED
!              EQST   - EQSS DATA FOR BOUNDARY SET FOR SUBSTRUCTURE
!                       BEINGREDUCED
!              USETMR - USET TABLE FOR REDUCED SUBSTRUCTURE
!              KAA    - SUBSTRUCTURE STIFFNESS MATRIX
!              MAA    - SUBSTRUCTURE MASS MATRIX
!              BAA    - SUBSTRUCTURE VISCOUS DAMPING MATRIX
!              K4AA   - SUBSTRUCTURE STRUCTURE DAMPINF MATRIX
!              PAA    - SUBSTRUCTURE LOAD MATRIX
!              DMR    - FREE BODY MATRIX
!              QSM    - MODEL REACTION MATRIX
!     SOF  -   LAMS   - EIGENVALUE TABLE FOR ORIGINAL SUBSTRUCTURE
!              PHIS   - EIGENVECTOR TABLE FOR ORIGINAL SUBSTRUCTURE
!              LMTX   - STIFFNESS DECOMPOSITION PRODUCT FOR ORIGINAL
!                       SUBSTRUCTURE
!              GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS FOR
!                       ORIGINAL SUBSTRUCTURE
!              HORG   - H TRANSFORMATION MATRIX FOR ORIGINAL
!                       SUBSTRUCTURE
!
!     6 OUTPUT DATA BLOCKS
!     GINO -   KHH    - REDUCED STIFFNESS MATRIX
!              MHH    - REDUCED MASS MATRIX
!              BHH    - REDUCED VISCOUS DAMPING MATRIX
!              K4HH   - REDUCED STRUCTURE DAMPING MATRIX
!              PHH    - REDUCED LOAD MATRIX
!              POVE   - INTERIOR POINT LOAD MATRIX
!     SOF  -   LAMS   - EIGENVALUE TABLE FOR ORIGINAL SUBSTRUCTURE
!              PHIS   - EIGENVECTOR TABLE FOR ORIGINAL SUBSTRUCTURE
!              LMTX   - STIFFNESS DECOMPOSITION PRODUCT FOR ORIGINAL
!                       SUBSTRUCTURE
!              GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS FOR
!                       ORIGINAL SUBSTRUCTURE
!              HORG   - H TRANSFORMATION MATRIX FOR ORIGINAL
!                       SUBSTRUCTURE
!              UPRT   - PARTITIONING VECTOR FOR MREDUCE FOR ORIGINAL
!                       SUBSTRUCTURE
!              POVE   - INTERNAL POINT LOADS FOR ORIGINAL SUBSTRUCTURE
!              POAP   - INTERNAL POINTS APPENDED LOADS FOR ORIGINAL
!                       SUBSTRUCTURE
!              EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE FOR REDUCED
!                       SUBSTRUCTURE
!              BGSS   - BASIC GRID POINT DEFINITION TABLE FOR REDUCED
!                       SUBSTRUCTURE
!              CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRICES FOR
!                       REDUCED SUBSTRUCTURE
!              LODS   - LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!              LOAP   - APPENDED LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!              PLTS   - PLOT SET DATA FOR REDUCED SUBSTRUCTURE
!              KMTX   - STIFFNESS MATRIX FOR REDUCED SUBSTRUCTURE
!              MMTX   - MASS MATRIX FOR REDUCED SUBSTRUCTURE
!              PVEC   - LOAD MATRIX FOR REDUCED SUBSTRUCTURE
!              PAPD   - APPENDED LOAD MATRIX FOR REDUCED SUBSTRUCTURE
!              BMTX   - VISCOUS DAMPING MATRIX FOR REDUCED SUBSTRUCTURE
!              K4MX   - STRUCTURE DAMPING MATRIX FOR REDUCED
!                       SUBSTRUCTURE
!
!     11 SCRATCH DATA BLOCKS
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
!              ISCR11 - LII PARTITION MATRIX USED IN MRED2B AND MRED2F
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!              NEWNAM - NAME OF REDUCED SUBSTRUCTURE
!              FREBDY - FREE BODY MODES CALCULATION FLAG
!              RANGE  - RANGE OF FREQUENCIES TO BE USED
!              NMAX   - MAXIMUM NUMBER OF FREQUENCIES TO BE USED
!              USRMOD - USERMODES CALCULATION FLAG
!              IO     - IO OPTIONS FLAG
!              BOUNDS - OLDBOUNDS OPTION FLAG
!              MODES  - OLDMODES OPTION FLAG
!              RSAVE  - SAVE REDUCTION PRODUCT FLAG
!              LAMSAP - BEGINNING ADDRESS OF MODE USE DESCRIPTION ARRAY
!              MODPTS - NUMBER OF MODAL POINTS
!              MODLEN - LENGTH OF MODE USE ARRAY
!
   !>>>>EQUIVALENCE (Casecc,Infile(1)) , (Rz(1),Z(1))
   DATA nmonic/4HNAMA , 4HNAMB , 4HFREE , 4HRANG , 4HNMAX , 4HUSER , 4HOUTP , 4HOLDB , 4HOLDM , 4HRSAV/
   DATA iblank , nhlods , nhloap/4H     , 4HLODS , 4HLOAP/
   DATA modnam/4HMRED , 4H2   /
   DATA itrlr/106 , 6*0/
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
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     INITIALIZE SOF
!
            CALL sofopn(z(sbuf1),z(sbuf2),z(sbuf3))
!
!     INITIALIZE CASE CONTROL PARAMETERS
!
            DO i = 1 , 12
               infile(i) = 100 + i
            ENDDO
            DO i = 1 , 6
               otfile(i) = 200 + i
            ENDDO
            DO i = 1 , 10
               iscr(i) = 300 + i
            ENDDO
            iscr11 = 311
            DO i = 1 , 2
               oldnam(i) = iblank
               newnam(i) = iblank
            ENDDO
            range(1) = 0.0
            range(2) = 1.0E+35
            frebdy = .FALSE.
            nmax = 2147483647
            usrmod = -1
            io = 0
            nrange = 0
            bounds = .FALSE.
            modes = .FALSE.
            rsave = .FALSE.
            ponly = .FALSE.
!
!     ** PROCESS CASE CONTROL
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
                     SPAG_Loop_4_1: DO j = 1 , 10
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
!     EXTRACT FREEBODY MODES FLAG
!
                        frebdy = .TRUE.
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
                        IF ( z(korbgn+2)/=0 ) nmax = z(korbgn+2)
                     ELSEIF ( j==6 ) THEN
!
!     EXTRACT USERMODE FLAG
!
                        usrmod = z(korbgn+2)
                     ELSEIF ( j==7 ) THEN
!
!     EXTRACT OUTPUT FLAGS
!
                        io = orf(io,z(korbgn+2))
                     ELSEIF ( j==8 ) THEN
!
!     EXTRACT OLDBOUND FLAG
!
                        bounds = .TRUE.
                     ELSEIF ( j==9 ) THEN
!
!     EXTRACT OLDMODES FLAG
!
                        modes = .TRUE.
                     ELSEIF ( j==10 ) THEN
!
!     EXTRACT REDUCTION SAVE FLAG
!
                        rsave = .TRUE.
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
!
            ENDDO
            CALL close(casecc,1)
!
!     TEST FOR RUN = GO
!
            mrd2g = 1
            IF ( dry/=0 ) THEN
!
!     CHECK FOR USERMODE = TYPE 2
!
               IF ( usrmod==2 ) THEN
!
!     PROCESS USERMODES FLAG
!
                  CALL mred2d
                  CALL mred2c(3)
!
!     PROCESS NEW TABLE ITEMS
!
                  CALL mred2h
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     CHECK FOR STIFFNESS PROCESSING
!
                  CALL rdtrl(itrlr)
                  IF ( itrlr(1)>0 ) THEN
!
!     PROCESS STIFFNESS MATRIX
!
                     mrd2g = 2
                     CALL mred2a
!
!     PROCESS OLDBOUND FLAG
!
                     CALL mred2b
!
!     PROCESS OLDMODES FLAG
!
                     CALL mred2c(1)
!
!     CALCULATE MODAL TRANSFORMATION MATRIX
!
                     CALL mred2e
                     CALL mred2c(2)
!
!     CALCULATE FREE BODY EFFECTS
!
                     CALL mred2f
                  ELSE
!
!     CHECK FOR LOADS ONLY
!
                     CALL sfetch(newnam,nhlods,3,itest)
                     IF ( itest/=3 ) THEN
                        CALL sfetch(newnam,nhloap,3,itest)
                        IF ( itest/=3 ) THEN
                           mrd2g = 4
                           GOTO 10
                        ENDIF
                     ENDIF
                     mrd2g = 3
                     ponly = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
!
!     CALCULATE STRUCTURAL MATRICES
!
!     MRD2G .EQ. 1, M,B,K4,P/PA PROCESSING (RUN = GO)
!     MRD2G .EQ. 2, K,M,B,K4,P/PA PROCESSING
!     MRD2G .EQ. 3, P/PA PROCESSING (ONLY)
!     MRD2G .EQ. 4, M,B,K4,P/PA PROCESSING (RUN = STEP)
!
 10         CALL mred2g(mrd2g)
            IF ( mrd2g/=1 ) CALL mred2h
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     CLOSE ANY OPEN FILES
!
         CALL sofcls
         IF ( dry==-2 ) WRITE (iprntr,99001)
!
99001    FORMAT (//,'  MODULE MREDUCE TERMINATING DUE TO ABOVE ERRORS.')
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      imsg = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mred2
