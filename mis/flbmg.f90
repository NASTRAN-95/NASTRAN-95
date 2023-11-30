
SUBROUTINE flbmg
   IMPLICIT NONE
   INTEGER Af , Afdict , Afmat , Bgpdt , Conect , Cstm , Dkgg , Ect , Eqexin , Fbelm , Frelm , Geom2 , Geom3 , Ibgpdt , Ibuf1 ,     &
         & Ibuf2 , Ibuf3 , Ibuf4 , Ibuf5 , Icore , Igrav , Igrid , Isil , Kgdict , Kgmat , Lcore , Mpt , Nbgpdt , Ngrav , Ngrid ,   &
         & Nofree , Nograv , Nout , Nsil , Sil , Sysbuf , Uset , Usetf , Usets , Z1(1) , Z2(1)
   LOGICAL Error
   REAL Tilt(2)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Nograv , Nofree , Tilt
   COMMON /flbfil/ Geom2 , Ect , Bgpdt , Sil , Mpt , Geom3 , Cstm , Uset , Eqexin , Usetf , Usets , Af , Dkgg , Fbelm , Frelm ,     &
                 & Conect , Afmat , Afdict , Kgmat , Kgdict
   COMMON /flbptr/ Error , Icore , Lcore , Ibgpdt , Nbgpdt , Isil , Nsil , Igrav , Ngrav , Igrid , Ngrid , Ibuf1 , Ibuf2 , Ibuf3 ,  &
                 & Ibuf4 , Ibuf5
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z1
   INTEGER korsz
!
!     DRIVER FOR MODULE FLBMG
!
!     COMPUTES THE HYDROELASTIC AREA FACTOR MATRIX AND THE
!     GRAVITATIONAL STIFFNESS MATRIX.
!
!     THE HYDROELASTIC USET VECTOR IA ALSO BUILT.
!
!     DMAP CALL
!
!        FLBMG    GEOM2,ECT,BGPDT,SIL,MPT,GEOM3,CSTM,USET,EQEXIN/
!                 USETF,USETS,AF,DKGG/S,N,NOGRAV/S,N,NOFREE/S,N,TILT $
!
!     INPUT DATA BLOCKS
!
!        GEOM2  - FLUID ELEMENT BOUNDARY DATA
!        ECT    - ELEMENT CONNECTION TABLE
!        BGPDT  - BASIC GRID POINT DEFINITION TABLE
!        SIL    - SCALAR INDEX LIST
!        MPT    - MATERIAL PROPERTIES TABLE
!        GEOM3  - GRAVITY LOAD DATA
!        CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRICES
!        USET   - DISPLACEMENT SET DEFINITION TABLE
!        EQEXIN - EQUIVALENCE BETWEEN EXTERNAL AND INTERNAL GRID POINTS
!
!     OUTPUT DATA BLOCK
!
!        USETF  - FLUID AND STRUCTURAL POINT SET DEFINITION TABLE
!        USETS  - STRUCTURAL POINT SET DEFINITION TABLE
!        AF     - FLUID AREA FACTOR MATRIX
!        DKGG   - STRUCTURAL GRAVITY STIFFNESS AMTRIX
!
!     PARAMETERS
!
!        NOGRAV - INPUT  - FLAG WHICH SPECIFIES WHETHER GRAVITY
!                          EFFECTS ARE TO BE COMPUTED.
!        NOFREE - OUTPUT - FLAG WHICH SPECIFIES WHETHER A FLUID FREE
!                          SURFACE EXISTS.
!        TILT   - OUTPUT - FREE SURFACE TILT VECTOR USED IN PLOTTING
!
!     USER PRINT OPTIONS
!
!        DIAG 32 - PRINTS HYDROELASTIC SET DEFINITION.
!        DIAG 33 - PRINTS HYDROELASTIC DEGREE OF FREEDOM DEFINITION.
!
!
   EQUIVALENCE (Z2(1),Z1(1))
!
!
!     INITILIZE OPEN CORE FOR ELEMENT MATRIX GENERATION PHASE
!
   Error = .FALSE.
   Lcore = korsz(Z1(1))
   Icore = 1
   Ibuf1 = Lcore - Sysbuf - 1
   Ibuf2 = Ibuf1 - Sysbuf
   Ibuf3 = Ibuf2 - Sysbuf
   Ibuf4 = Ibuf3 - Sysbuf
   Ibuf5 = Ibuf4 - Sysbuf
!
!     PROCESS FLUID ELEMENTS ON THE FLUID / STRUCTURE BOUNDARY
!     AND THE FREE SURFACE .
!
   CALL flbelm
   IF ( .NOT.(Error) ) THEN
!
!     BUILD THE HYDROELASTIC USET VECTOR
!
      CALL flbset
      IF ( .NOT.(Error) ) THEN
!
!     GENERATE THE ELEMENT MATRICES
!
         CALL flbemg
         IF ( .NOT.(Error) ) THEN
!
!     INITIALIZE CORE FOR THE MATRIX ASSEMBLY PHASE
!
            Lcore = korsz(Z2(1))
            Icore = 1
            Ibuf1 = Lcore - Sysbuf - 1
            Ibuf2 = Ibuf1 - Sysbuf
!
!     ASSEMBLE THE AREA FACTOR MATRIX
!
            CALL flbema(1)
!
!     IF GRAVITY LOADS - ASSEMBLE THE GRAVITY STIFFNESS MATRIX
!
            IF ( Nograv>=0 ) CALL flbema(2)
!
!     MODULE COMPLETION
!
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
!     FATAL ERROR OCCURED DURING PROCESSING - TERMINATE RUN
!
   WRITE (Nout,99001) Uim
99001 FORMAT (A29,' 8000, MODULE FLBMG TERMINATED DUE TO ABOVE ERRORS.')
   CALL mesage(-61,0,0)
END SUBROUTINE flbmg
