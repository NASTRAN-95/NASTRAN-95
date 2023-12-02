!*==mxcids.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mxcids(Z,Mset,Msze,Nwds,Uset,Nstrt,Snam) !HIDESTARS (*,Z,Mset,Msze,Nwds,Uset,Nstrt,Snam)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Z
   INTEGER :: Mset
   INTEGER :: Msze
   INTEGER :: Nwds
   REAL :: Uset
   INTEGER :: Nstrt
   REAL :: Snam
!
! End of declarations rewritten by SPAG
!
!-----
!   THIS SUBROUTINE CREATES A LIST OF SUBSTRUCTURE NAMES AT Z(1)
! AND AN ARRAY AT Z(NSTRT) OF LENGTH MSZE*NWDS WITH THE FIRST TWO WORDS
! OF EACH ENTRY AS,
!     1 - EXTERNAL ID * 10 + COMPONENT
!     2 - POINTER TO FIRST WORD OF SUBSTRUCTURE NAME...
!
! INPUT
!     Z     = OPEN CORE - Z(1) TO Z(NSTRT-1)
!     MSET  = ONE WORD BCD IDENTIFING SET
!     MSZE  = NUMBER OF COLUMNS IN MATRIX
!     NWDS  = NUMBER WORDS/ENTRY DESIRED (2 MINIMUM)
!     USET  = USET GINO FILE NAME
!     NSTRT = LOCATION OF FIRST OF FOUR (4) BUFFERS.
!     SNAM  = SUBSTRUCTURE NAME BEING SOLVED (2 WORD BCD).
! OUTPUT
!     *     = TASK NOT COMPLETED
!     Z     = SUBSTRUCTURE NAMES + COLUMN IDENTIFIERS (SEE ABOVE)
!     NSTRT = FIRST WORD OF COLUMN IDENTIFIERS
!-----
!
!       N O T   C O D E D   Y E T
!
   IF ( Mset==1 ) RETURN
   RETURN 1
END SUBROUTINE mxcids
