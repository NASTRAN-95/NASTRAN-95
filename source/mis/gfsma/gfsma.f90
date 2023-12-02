!*==gfsma.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gfsma
   IMPLICIT NONE
   USE C_BLANK
   USE C_GFSMOX
   EXTERNAL gfsdir , gfsmo2 , gfsmod
!
! End of declarations rewritten by SPAG
!
!
!     MODULE GFSMA  ( GENERAL FLUID / STRUCTURE MATRIX ASSEMBLER )
!
!
!     DMAP CALL
!
!        GFSMA  AXY,AFRY,KYY,DKAA,DKFRFR,KAA,MAA,GM,GO,USETS,USETF,
!               PHIA,PHIX,LAMA/KMAT,MMAT,GIA,POUT,HC/V,N,NOGRAV/
!               V,N,NOFREE/V,Y,KCOMP/V,Y,COMPTYP/V,N,FORM/V,Y,LMODES $
!
!     INPUT DATA BLOCKS
!
!        AXY    - STRUCTURE / FLUID AREA MATRIX
!        AFRY   - FREE SURFACE AREA MATRIX
!        KYY    - FLUID STIFFNESS MATRIX
!        DKAA   - STRUCTURE GRAVITY STIFFNESS MATRIX
!        DKFRFR - FREE SURFACE GRAVITY STIFFNESS MATRIX
!        KAA    - REDUCED STRUCTURE STIFFNESS MATRIX
!        MAA    - REDUCED STRUCTURE MASS MATRIX
!        GM     - MULTIPOINT CONSTRAINT TRANSFORMATION MATRIX
!        GO     - OMIT POINT TRANSFORMATION MATRIX
!        USETS  - STRUCTURE ONLY SET DEFINITION TABLE
!        USETF  - FLUID AND STRUCTURE SET DEFINITION TABLE
!        PHIA   - SOLUTION EIGENVECTORS  A - SET
!        PHIX   - SOLUTION EIGENVECTORS  X - SET
!        LAMA   - SOLUTION EIGENVALUE TABLE
!
!     OUTPUT DATA BLOCKS
!
!        KMAT   - COMBINATION FLUID / STRUCTURE STIFFNESS MATRIX
!        MMAT   - COMBINATION FLUID / STRUCTURE MASS MATRIX
!        GIA    - PRESSURE TRANSFORMATION MATRIX
!        POUT   - PARTITIONING VECTOR FOR MODAL DISPLACEMENTS
!        HC     - CONSTRAINT TRANSFORMATION MATRIX FOR INCOMPRESSIBLE
!                 APPROACH
!
!     PARAMETERS
!
!        NOGRAV  - GRAVITY FLAG  (-1 FOR NO GRAVITY)
!        NOFREE  - FREE SURFACE FLAG  (-1 FOR NO FREE SURFACE)
!        KCOMP   - COMPRESSIBILITY FACTOR  (DEFAULT = 1.0)
!        COMPTYP - TYPE OF COMPRESSIBLILITY COMPUTATIONS
!                       -1  STRUCTURE AND FREE SURFACE ARE COUPLED
!                           WITH A SPRING TO RESIST VOLUME CHANGE
!                        1  PURE INCOMPRESSIBLE - CONSTRAINT EQUATION
!                           IS GENERATED TO RESTRICT VOLUME CHANGE
!        FORM    - TYPE OF FORMULATION TO BE USED
!                       -1  DIRECT FORMULATION
!                        1  MODAL FORMULATION
!        LMODES  - NUMBER OF MODES USED IN MODAL FORMULATION
!                  ( -1 IF ALL STRUCTURE MODES ARE TO BE USED (
!
!
!     MODULE PARAMETERS
!
!
!     LOCAL VARIABLES FOR GFSMOD AND GFSMO2
!
!***********************************************************************
!
   IF ( Form>0 ) THEN
!
!     MODAL FORMULATION
!
      CALL gfsmod
      CALL gfsmo2
   ELSE
!
!     DIRECT FORMULATION
!
      CALL gfsdir
   ENDIF
!
!     MODULE COMPLETION
!
END SUBROUTINE gfsma
