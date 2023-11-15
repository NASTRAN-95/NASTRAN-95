
SUBROUTINE sdcom4(P,Ac,Wa,Wb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER C , Lasti , Lastpl , Row , Sc , Start
   REAL Frstpc , Spflg
   COMMON /sdcomx/ Row , C , Spflg , Start , Frstpc , Lastpl , Lasti , Sc
!
! Dummy argument declarations
!
   INTEGER Ac(1)
   DOUBLE PRECISION P(2) , Wa(1) , Wb(1)
!
! Local variable declarations
!
   INTEGER i , j , k , k1 , l
   DOUBLE PRECISION p1 , p2 , pii , pir
!
! End of declarations
!
!******
!
! SDCOM4 COMPUTES THE CONTRIBUTIONS OF THE PIVOT ROW FOR SDCOMP IN CDP
!
!******
!
!
!
   j = 1
   l = 1
!
! FOR THE OUTER LOOP I RUNS FROM START TO LASTI.
! BEGIN BY FORMING -P(I)/P(1). THEN DECIDE WHICH INNER LOOP TO EXECUTE
!
   p2 = P(1)**2 + P(2)**2
   p1 = P(1)/p2
   p2 = P(2)/p2
   DO i = Start , Lasti
      pir = -P(2*i-1)*p1 - P(2*i)*p2
      pii = P(2*i-1)*p2 - P(2*i)*p1
      IF ( i<=Lastpl ) THEN
!
! LOOP 3 -- K RUNS FROM I TO LASTPL AND L IS INCREMENTED EVERY TIME
!           THEN, IF LASTPL .LT. C, LOOP 1 IS EXECUTED TO FINISH IT UP
!
         DO k = i , Lastpl
            Wb(j) = pir*P(2*k-1) - pii*P(2*k) + Wa(l)
            Wb(j+1) = pir*P(2*k) + pii*P(2*k-1) + Wa(l+1)
            l = l + 2
            j = j + 2
         ENDDO
         IF ( Lastpl==C ) GOTO 50
         k1 = Lastpl + 1
      ELSEIF ( Ac(i)<0 ) THEN
!
! LOOP 2 -- L IS NEVER INCREMENTED
!
         DO k = i , C
            Wb(j) = pir*P(2*k-1) - pii*P(2*k)
            Wb(j+1) = pir*P(2*k) + pii*P(2*k-1)
            j = j + 2
         ENDDO
         GOTO 50
      ELSE
         k1 = i
      ENDIF
!
! LOOP 1 -- L IS INCREMENTED WHENEVER AC(K) .GT. 0
!
      DO k = k1 , C
         IF ( Ac(k)>0 ) THEN
            Wb(j) = pir*P(2*k-1) - pii*P(2*k) + Wa(l)
            Wb(j+1) = pir*P(2*k) + pii*P(2*k-1) + Wa(l+1)
            l = l + 2
         ELSE
            Wb(j) = pir*P(2*k-1) - pii*P(2*k)
            Wb(j+1) = pir*P(2*k) + pii*P(2*k-1)
         ENDIF
         j = j + 2
      ENDDO
!
! END OUTER LOOP BY STORING -P(I)/P(1) AT P(1).
!
 50   P(2*i-1) = pir
      P(2*i) = pii
   ENDDO
END SUBROUTINE sdcom4
