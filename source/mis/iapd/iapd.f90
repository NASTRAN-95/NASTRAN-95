!*==iapd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION iapd(I,J,Nc,Ncrd)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: iapd
   INTEGER :: I
   INTEGER :: J
   INTEGER :: Nc
   INTEGER :: Ncrd
!
! End of declarations rewritten by SPAG
!
   IF ( J==1 ) THEN
      iapd = Ncrd + 1
      IF ( I==1 ) RETURN
      iapd = iapd + 1
      IF ( I==2 ) RETURN
      iapd = 3 + 3*(I-2) + Ncrd
      RETURN
   ELSEIF ( J/=2 ) THEN
      iapd = J + Nc*(2*J-3) + Ncrd
      IF ( I==1 ) RETURN
      iapd = iapd + 1
      IF ( I==2 ) RETURN
      iapd = iapd + 2*(I-2)
      RETURN
   ENDIF
   iapd = 3 + Ncrd
   IF ( I==1 ) RETURN
   iapd = 4 + Ncrd
   IF ( I==2 ) RETURN
   iapd = 4 + 3*(I-2) + Ncrd
   RETURN
END FUNCTION iapd
