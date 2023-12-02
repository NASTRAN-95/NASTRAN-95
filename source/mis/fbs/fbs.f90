!*==fbs.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbs(Zs,Zd)
   USE c_fbsx
   USE c_system
   USE c_type
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Zs
   REAL(REAL64) , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iremain , l46 , memavl , ncol , need , nrhv , nrhvwd , rc , typeb , typel
   REAL :: subnam
   EXTERNAL fbsf , fbsi , mesage , sswtch
!
! End of declarations rewritten by SPAG
!
!
!     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND
!     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM
!     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR
!     SYSTEM OF EQUATIONS.
!
!     THE ARE TWO METHODS AVAILABLE FOR THIS PROCESS.
!     METHOD 1 - THIS METHOD READS AS MANY RIGHT HAND VECTORS INTO MEMORY
!                AS POSSIBLE, AND THEN READS THE LOWER TRIANGULAR MATRIX
!                USING GETSTR AND GETSTB TO SOLVE FOR THE SOLUTION VECTORS.
!                MORE THAN ONE PASS MAY BE REQUIRED IF INSUFFICIENT MEMORY
!                EXISTS FOR LOADING ALL RIGHT HAND VECTORS AT ONE TIME.
!                THIS METHOD IS THE OLDER OF THE TWO METHODS.
!                (SEE SUBROUTINES FBSF, FBSF1, FBSF2, FBSF3 AND FBSF4)
!     METHOD 2 - THIS METHOD IS THE SAME AS METHOD 1 WITH THE EXCEPTION
!                THAT MEMORY EXISTS FOR LOADING PART OR ALL OF THE LOWER
!                TRIANGULAR MATRIX INTO OPEN CORE AFTER LOADING ALL OF
!                THE RIGHT HAND VECTORS INTO OPEN CORE.  THIS METHOD
!                WILL ELIMINATE THE NEED TO READ THE LOWER TRIANGULAR
!                MATRIX TWICE (ONCE FORWARD AND ONCE BACKWARD).
!                (SEE SUBROUTINES FBSI, FBSI1, FBSI2, FBSI3 AND FBSI4)
!
!     THE SELECTION OF METHOD 1 OR 2 IS DEPENDENT UPON WHETHER
!     MEMORY EXISTS FOR READING THE LOWER TRIANGULAR MATRIX INTO MEMORY
!
!     SEE SUBROUTINES FBSF AND FBSI FOR OPEN CORE LAYOUTS
!
!
!     GENERAL INITIALIZATION
!
!   DIAG 46 FORCES METHOD ONE
!
   CALL sswtch(46,l46)
   IF ( l46==0 ) THEN
      ncol = dbl(2)
      typel = dbl(5)
      typeb = dbb(5)
      rc = rlcmpx(typeb)
!
!     NRHVWD = NUMBER OF WORDS REQUIRED FOR EACH RIGHT HAND VECTOR
!     NRHV   = NUMBER OF RIGHT HAND VECTORS
!
      nrhvwd = words(typel)*ncol
      nrhv = dbb(2)
!
! CHECK FOR RIGHT HAND VECTORS BEING THE IDENTITY MATRIX
!
      IF ( dbb(4)==8 ) nrhv = ncol
!
! COMPUTE THE MEMORY TO READ ALL OF THE RIGHT HAND VECTORS INTO MEMORY
!
      need = nrhv*nrhvwd
      iremain = lcore - 2*sysbuf - need
!
! IF LESS THAN ONE COLUMN WORTH OF MEMORY AVAILABLE, USE METHOD ONE
!
      IF ( iremain>=nrhvwd ) THEN
!
!  METHOD TWO
!
         CALL fbsi(Zs,Zd)
         RETURN
      ENDIF
   ENDIF
!
!  METHOD ONE - FIRST, CHECK FOR SUFFICIENT MEMORY FOR PROCESS
!
   memavl = lcore - 2*sysbuf - nrhvwd
   IF ( memavl<=0 ) CALL mesage(-8,-memavl,subnam)
   CALL fbsf(Zs,Zd)
END SUBROUTINE fbs
