!*==sdr2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k
   EXTERNAL sdr2a , sdr2aa , sdr2b , sdr2c , sdr2d
!
! End of declarations rewritten by SPAG
!
!
!     SDR2 IS THE EXECUTIVE CONTROL PROGRAM FOR THE SDR2 MODULE.
!
!
!     EXECUTE THE PHASES OF SDR2.
!
   casecc = 101
   CALL sdr2aa
   CALL sdr2a
   IF ( any/=0 ) CALL sdr2b
   k = loads + spcf + displ + vel + acc + plots
   IF ( k/=0 ) CALL sdr2c
   IF ( any/=0 ) CALL sdr2d
END SUBROUTINE sdr2
