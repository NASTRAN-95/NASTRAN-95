
FUNCTION iapd(I,J,Nc,Ncrd)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER I , J , Nc , Ncrd
   INTEGER iapd
!
! End of declarations
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
      GOTO 99999
   ENDIF
   iapd = 3 + Ncrd
   IF ( I==1 ) RETURN
   iapd = 4 + Ncrd
   IF ( I==2 ) RETURN
   iapd = 4 + 3*(I-2) + Ncrd
   RETURN
99999 END FUNCTION iapd
