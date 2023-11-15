
FUNCTION apdf(F,In,Ns)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER In , Ns
   REAL apdf
   REAL F(1)
!
! End of declarations
!
!
!
!     IF (NS .EQ. 0) GO TO 10
!     APDF = FLOAT(IN-1)/FLOAT(NS)
!     RETURN
!  10 APDF = F(IN)
!     RETURN
!
   apdf = F(In)
   IF ( Ns/=0 ) apdf = float(In-1)/float(Ns)
END FUNCTION apdf
