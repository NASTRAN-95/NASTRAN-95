
SUBROUTINE shpsts(Sigma,Vonms,Sigp)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL App(2) , Comps , Sk2(39) , Skp(4) , Sort2
   INTEGER Idum(2) , Midve
   LOGICAL Ostrai
   COMMON /blank / App , Sort2 , Idum , Comps , Skp , Ostrai , Sk2 , Midve
!
! Dummy argument declarations
!
   LOGICAL Vonms
   REAL Sigma(3) , Sigp(4)
!
! Local variable declarations
!
   REAL eps , proj , sig , taumax , txy2
!
! End of declarations
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
