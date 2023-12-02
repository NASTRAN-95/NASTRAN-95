!*==sdcom3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcom3(P,Ac,Wa,Wb)
   USE c_sdcomx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: P
   INTEGER , DIMENSION(1) :: Ac
   REAL , DIMENSION(1) :: Wa
   REAL , DIMENSION(1) :: Wb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , k , k1 , l
   REAL :: p1 , p2 , pii , pir
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!******
!
! SDCOM3 COMPUTES THE CONTRIBUTIONS OF THE PIVOT ROW FOR SDCOMP IN CSP
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
   DO i = start , lasti
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            pir = -P(2*i-1)*p1 - P(2*i)*p2
            pii = P(2*i-1)*p2 - P(2*i)*p1
            IF ( i<=lastpl ) THEN
!
! LOOP 3 -- K RUNS FROM I TO LASTPL AND L IS INCREMENTED EVERY TIME
!           THEN, IF LASTPL .LT. C, LOOP 1 IS EXECUTED TO FINISH IT UP
!
               DO k = i , lastpl
                  Wb(j) = pir*P(2*k-1) - pii*P(2*k) + Wa(l)
                  Wb(j+1) = pir*P(2*k) + pii*P(2*k-1) + Wa(l+1)
                  l = l + 2
                  j = j + 2
               ENDDO
               IF ( lastpl==c ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k1 = lastpl + 1
            ELSEIF ( Ac(i)<0 ) THEN
!
! LOOP 2 -- L IS NEVER INCREMENTED
!
               DO k = i , c
                  Wb(j) = pir*P(2*k-1) - pii*P(2*k)
                  Wb(j+1) = pir*P(2*k) + pii*P(2*k-1)
                  j = j + 2
               ENDDO
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               k1 = i
            ENDIF
!
! LOOP 1 -- L IS INCREMENTED WHENEVER AC(K) .GT. 0
!
            DO k = k1 , c
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
            spag_nextblock_1 = 2
         CASE (2)
!
! END OUTER LOOP BY STORING -P(I)/P(1) AT P(1).
!
            P(2*i-1) = pir
            P(2*i) = pii
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE sdcom3
