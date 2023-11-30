
SUBROUTINE mred2
   IMPLICIT NONE
   LOGICAL Bounds , Frebdy , Modes , Ponly , Rsave
   INTEGER Casecc , Dry , Gbuf1 , Gbuf2 , Gbuf3 , Infile(12) , Io , Iprntr , Iscr(10) , Iscr11 , Korbgn , Korlen , Lamsap , Lstzwd ,&
         & Modlen , Modpts , Newnam(2) , Nmax , Oldnam(2) , Otfile(6) , Popt , Sbuf1 , Sbuf2 , Sbuf3 , Step , Sysbuf , Usrmod , Z(1)
   REAL Range(2) , Rz(1)
   COMMON /blank / Step , Dry , Popt , Gbuf1 , Gbuf2 , Gbuf3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn ,   &
                 & Oldnam , Newnam , Frebdy , Range , Nmax , Usrmod , Io , Bounds , Modes , Rsave , Lamsap , Modpts , Modlen ,      &
                 & Ponly , Lstzwd , Iscr11
   COMMON /system/ Sysbuf , Iprntr
   COMMON /zzzzzz/ Z
   INTEGER i , iblank , ifile , imsg , itest , itrlr(7) , j , k , modnam(2) , mrd2g , nhloap , nhlods , nmonic(10) , nozwds ,       &
         & nrange , nwdscc , nwdsrd
   INTEGER korsz , orf
   EXTERNAL orf
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
   EQUIVALENCE (Casecc,Infile(1)) , (Rz(1),Z(1))
   DATA nmonic/4HNAMA , 4HNAMB , 4HFREE , 4HRANG , 4HNMAX , 4HUSER , 4HOUTP , 4HOLDB , 4HOLDM , 4HRSAV/
   DATA iblank , nhlods , nhloap/4H     , 4HLODS , 4HLOAP/
   DATA modnam/4HMRED , 4H2   /
   DATA itrlr/106 , 6*0/
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
      GOTO 500
   ELSE
!
!     INITIALIZE SOF
!
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
!
!     INITIALIZE CASE CONTROL PARAMETERS
!
      DO i = 1 , 12
         Infile(i) = 100 + i
      ENDDO
      DO i = 1 , 6
         Otfile(i) = 200 + i
      ENDDO
      DO i = 1 , 10
         Iscr(i) = 300 + i
      ENDDO
      Iscr11 = 311
      DO i = 1 , 2
         Oldnam(i) = iblank
         Newnam(i) = iblank
      ENDDO
      Range(1) = 0.0
      Range(2) = 1.0E+35
      Frebdy = .FALSE.
      Nmax = 2147483647
      Usrmod = -1
      Io = 0
      nrange = 0
      Bounds = .FALSE.
      Modes = .FALSE.
      Rsave = .FALSE.
      Ponly = .FALSE.
!
!     ** PROCESS CASE CONTROL
!
      ifile = Casecc
      CALL open(*200,Casecc,Z(Gbuf2),0)
      IF ( Step/=0 ) THEN
         DO i = 1 , Step
            CALL fwdrec(*400,Casecc)
         ENDDO
      ENDIF
!
!     READ CASECC
!
      CALL read(*300,*400,Casecc,Z(Korbgn),2,0,nwdsrd)
      nwdscc = Z(Korbgn+1)
      DO i = 1 , nwdscc , 3
         CALL read(*300,*400,Casecc,Z(Korbgn),3,0,nwdsrd)
!
!     TEST CASE CONTROL MNEMONICS
!
         DO j = 1 , 10
            IF ( Z(Korbgn)==nmonic(j) ) GOTO 20
         ENDDO
         CYCLE
!
!     SELECT DATA TO EXTRACT
!
 20      IF ( j==2 ) THEN
!
!     EXTRACT NAME OF REDUCED SUBSTRUCTURE
!
            DO k = 1 , 2
               Newnam(k) = Z(Korbgn+k)
            ENDDO
         ELSEIF ( j==3 ) THEN
!
!     EXTRACT FREEBODY MODES FLAG
!
            Frebdy = .TRUE.
         ELSEIF ( j==4 ) THEN
!
!     EXTRACT FREQUENCY RANGE
!
            IF ( nrange==1 ) THEN
               Range(2) = Rz(Korbgn+2)
            ELSE
               nrange = 1
               Range(1) = Rz(Korbgn+2)
            ENDIF
         ELSEIF ( j==5 ) THEN
!
!     EXTRACT MAXIMUM NUMBER OF FREQUENCIES
!
            IF ( Z(Korbgn+2)/=0 ) Nmax = Z(Korbgn+2)
         ELSEIF ( j==6 ) THEN
!
!     EXTRACT USERMODE FLAG
!
            Usrmod = Z(Korbgn+2)
         ELSEIF ( j==7 ) THEN
!
!     EXTRACT OUTPUT FLAGS
!
            Io = orf(Io,Z(Korbgn+2))
         ELSEIF ( j==8 ) THEN
!
!     EXTRACT OLDBOUND FLAG
!
            Bounds = .TRUE.
         ELSEIF ( j==9 ) THEN
!
!     EXTRACT OLDMODES FLAG
!
            Modes = .TRUE.
         ELSEIF ( j==10 ) THEN
!
!     EXTRACT REDUCTION SAVE FLAG
!
            Rsave = .TRUE.
         ELSE
!
!     EXTRACT NAME OF SUBSTRUCTURE BEING REDUCED
!
            DO k = 1 , 2
               Oldnam(k) = Z(Korbgn+k)
            ENDDO
         ENDIF
!
      ENDDO
      CALL close(Casecc,1)
!
!     TEST FOR RUN = GO
!
      mrd2g = 1
      IF ( Dry/=0 ) THEN
!
!     CHECK FOR USERMODE = TYPE 2
!
         IF ( Usrmod==2 ) THEN
!
!     PROCESS USERMODES FLAG
!
            CALL mred2d
            CALL mred2c(3)
!
!     PROCESS NEW TABLE ITEMS
!
            CALL mred2h
            GOTO 100
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
               CALL sfetch(Newnam,nhlods,3,itest)
               IF ( itest/=3 ) THEN
                  CALL sfetch(Newnam,nhloap,3,itest)
                  IF ( itest/=3 ) THEN
                     mrd2g = 4
                     GOTO 50
                  ENDIF
               ENDIF
               mrd2g = 3
               Ponly = .TRUE.
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
 50   CALL mred2g(mrd2g)
      IF ( mrd2g/=1 ) CALL mred2h
   ENDIF
!
!     CLOSE ANY OPEN FILES
!
 100  CALL sofcls
   IF ( Dry==-2 ) WRITE (Iprntr,99001)
!
99001 FORMAT (//,'  MODULE MREDUCE TERMINATING DUE TO ABOVE ERRORS.')
   RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 200  imsg = -1
   GOTO 500
 300  imsg = -2
   GOTO 500
 400  imsg = -3
 500  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   RETURN
!
END SUBROUTINE mred2
