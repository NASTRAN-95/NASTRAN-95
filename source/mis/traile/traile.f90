!*==traile.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION traile(X,J,N,P,M,Boxl)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: M
   COMPLEX :: traile
   REAL :: X
   INTEGER :: J
   INTEGER , DIMENSION(1) :: N
   COMPLEX , DIMENSION(3,M) :: P
   REAL :: Boxl
!
! Local variable declarations rewritten by SPAG
!
   REAL :: xa
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO FIND PHI FOR TRAILING EDGE
!
!
!     CHECK TO SEE IF TRAILING EDGE HAS BEEN COMPUTED
!
   IF ( N(J)>=0 ) THEN
!
      xa = X/Boxl + 0.5 - float(N(J))
      IF ( real(P(2,J))/=0.0 ) THEN
         traile = P(1,J) + xa*(P(1,J)-P(2,J))
         RETURN
      ENDIF
   ENDIF
   traile = P(1,J)
END FUNCTION traile
