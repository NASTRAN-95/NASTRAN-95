!*==shpsts.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shpsts(Sigma,Vonms,Sigp)
   IMPLICIT NONE
   USE C_BLANK
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: Sigma
   LOGICAL :: Vonms
   REAL , DIMENSION(4) :: Sigp
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: eps
   REAL :: proj , sig , taumax , txy2
!
! End of declarations rewritten by SPAG
!
!
!     TO CALCULATE PRINCIPAL STRESSES AND THEIR ANGLES FOR THE
!     ISOPARAMETRIC SHELL ELEMENTS
!
!
!     INPUT :
!           SIGMA  - ARRAY OF 3 STRESS COMPONENTS
!           VONMS  - LOGICAL FLAG INDICATING THE PRESENCE OF VON-MISES
!                    STRESS REQUEST
!     OUTPUT:
!           SIGP   - ARRAY OF PRINCIPAL STRESSES
!
!
!WKBNB 7/94 SPR94004
!WKBNE 7/94 SPR94004
   DATA eps/1.0E-11/
!
!
!     CALCULATE PRINCIPAL STRESSES
!
   sig = 0.5*(Sigma(1)+Sigma(2))
   proj = 0.5*(Sigma(1)-Sigma(2))
   taumax = proj*proj + Sigma(3)*Sigma(3)
!WKBI 7/94 SPR94004
   IF ( Ostrai ) taumax = proj*proj + Sigma(3)*Sigma(3)/4.
   IF ( taumax/=0.0 ) taumax = sqrt(taumax)
   IF ( taumax<=eps ) taumax = 0.0
!
!     CALCULATE THE PRINCIPAL ANGLE
!
   txy2 = Sigma(3)*2.0
   proj = proj*2.0
   Sigp(1) = 0.0
   IF ( abs(txy2)>eps .OR. abs(proj)>eps ) Sigp(1) = 28.64788976*atan2(txy2,proj)
!                   28.64788976 = 90./PI
!
   Sigp(2) = sig + taumax
   Sigp(3) = sig - taumax
   Sigp(4) = taumax
!
!     OUTPUT VON MISES YIELD STRESS IF REQUESTED
!
   IF ( .NOT.Vonms ) RETURN
   sig = Sigp(2)*Sigp(2) + Sigp(3)*Sigp(3) - Sigp(2)*Sigp(3)
   IF ( sig/=0.0 ) sig = sqrt(sig)
   IF ( sig<=eps ) sig = 0.0
   Sigp(4) = sig
!
END SUBROUTINE shpsts
