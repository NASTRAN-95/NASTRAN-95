!*==apdf.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION apdf(F,In,Ns)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: apdf
   REAL , DIMENSION(1) :: F
   INTEGER :: In
   INTEGER :: Ns
!
! End of declarations rewritten by SPAG
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
