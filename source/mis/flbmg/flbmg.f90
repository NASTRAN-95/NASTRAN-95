!*==flbmg.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbmg
   USE c_blank
   USE c_flbfil
   USE c_flbptr
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: z2
   EXTERNAL flbelm , flbema , flbemg , flbset , korsz , mesage
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Z2(1),Z1(1))
!
!
!     INITILIZE OPEN CORE FOR ELEMENT MATRIX GENERATION PHASE
!
   error = .FALSE.
   lcore = korsz(z1(1))
   icore = 1
   ibuf1 = lcore - sysbuf - 1
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = ibuf2 - sysbuf
   ibuf4 = ibuf3 - sysbuf
   ibuf5 = ibuf4 - sysbuf
!
!     PROCESS FLUID ELEMENTS ON THE FLUID / STRUCTURE BOUNDARY
!     AND THE FREE SURFACE .
!
   CALL flbelm
   IF ( .NOT.(error) ) THEN
!
!     BUILD THE HYDROELASTIC USET VECTOR
!
      CALL flbset
      IF ( .NOT.(error) ) THEN
!
!     GENERATE THE ELEMENT MATRICES
!
         CALL flbemg
         IF ( .NOT.(error) ) THEN
!
!     INITIALIZE CORE FOR THE MATRIX ASSEMBLY PHASE
!
            lcore = korsz(z2(1))
            icore = 1
            ibuf1 = lcore - sysbuf - 1
            ibuf2 = ibuf1 - sysbuf
!
!     ASSEMBLE THE AREA FACTOR MATRIX
!
            CALL flbema(1)
!
!     IF GRAVITY LOADS - ASSEMBLE THE GRAVITY STIFFNESS MATRIX
!
            IF ( nograv>=0 ) CALL flbema(2)
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
   WRITE (nout,99001) uim
99001 FORMAT (A29,' 8000, MODULE FLBMG TERMINATED DUE TO ABOVE ERRORS.')
   CALL mesage(-61,0,0)
END SUBROUTINE flbmg
