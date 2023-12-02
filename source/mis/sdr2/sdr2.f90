!*==sdr2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2
   IMPLICIT NONE
   USE C_SDR2X2
   USE C_SDR2X4
   USE C_SYSTEM
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
   Casecc = 101
   CALL sdr2aa
   CALL sdr2a
   IF ( Any/=0 ) CALL sdr2b
   k = Loads + Spcf + Displ + Vel + Acc + Plots
   IF ( k/=0 ) CALL sdr2c
   IF ( Any/=0 ) CALL sdr2d
END SUBROUTINE sdr2
