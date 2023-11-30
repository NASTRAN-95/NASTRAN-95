
COMPLEX FUNCTION traile(X,J,N,P,M,Boxl)
   IMPLICIT NONE
   REAL Boxl , X
   INTEGER J , M
   INTEGER N(1)
   COMPLEX P(3,M)
   REAL xa
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
         GOTO 99999
      ENDIF
   ENDIF
   traile = P(1,J)
   RETURN
99999 RETURN
END FUNCTION traile