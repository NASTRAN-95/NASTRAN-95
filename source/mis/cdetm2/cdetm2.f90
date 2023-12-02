!*==cdetm2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cdetm2(P,D,Ip,Pr,Pi,Dr,Di,Ips1)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(6) :: P
   REAL(REAL64) , DIMENSION(6) :: D
   INTEGER , DIMENSION(6) :: Ip
   REAL(REAL64) , DIMENSION(3) :: Pr
   REAL(REAL64) , DIMENSION(3) :: Pi
   REAL(REAL64) , DIMENSION(3) :: Dr
   REAL(REAL64) , DIMENSION(3) :: Di
   INTEGER , DIMENSION(3) :: Ips1
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: d1 , d2 , d3 , d4
   REAL(REAL64) , DIMENSION(3) :: dd
   INTEGER :: i , is1 , is2 , isret , k , nx , spag_nextblock_1
   INTEGER , DIMENSION(3) :: ips
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ARRANGES  P,D,IP  IN ORDER BY MAGNITUDE OF DETERMINANT
!
   !>>>>EQUIVALENCE (d1,dd(1)) , (d2,dd(2)) , (d3,dd(3))
!
         d1 = D(1)*D(1) + D(2)*D(2)
         d2 = D(3)*D(3) + D(4)*D(4)
         d3 = D(5)*D(5) + D(6)*D(6)
         DO i = 1 , 3
            dd(i) = dsqrt(dd(i))
         ENDDO
         DO i = 2 , 6 , 2
            k = i/2
            ips(k) = Ip(i)
            Ips1(k) = Ip(i)
         ENDDO
!
!     SAVE STUFF IN OUTPUT AREAS
!
         DO i = 1 , 3
            Pr(i) = P(2*i-1)
            Pi(i) = P(2*i)
            Dr(i) = D(2*i-1)
            Di(i) = D(2*i)
         ENDDO
!
!     SCALE  MAGNITUDES
!
         DO i = 1 , 3
            DO WHILE ( dd(i)>10.0D0 )
               dd(i) = dd(i)*0.1D0
               ips(i) = ips(i) + 1
            ENDDO
            DO WHILE ( dd(i)<1.0D0 )
               dd(i) = dd(i)*10.0D0
               ips(i) = ips(i) - 1
            ENDDO
         ENDDO
!
!     START COMPARISON TESTS
!
         IF ( ips(1)>ips(2) .AND. ips(2)>ips(3) ) GOTO 40
         IF ( ips(1)>ips(2) .AND. ips(1)>ips(3) ) GOTO 20
         IF ( ips(2)<ips(3) ) THEN
         ELSEIF ( ips(2)==ips(3) ) THEN
            IF ( d2>=d3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ips(1)<ips(3) ) THEN
         ELSEIF ( ips(1)==ips(3) ) THEN
            IF ( d1>=d3 ) GOTO 20
         ELSE
            GOTO 20
         ENDIF
         is1 = 1
         is2 = 3
         ASSIGN 20 TO isret
         spag_nextblock_1 = 3
      CASE (2)
         IF ( ips(1)<ips(2) ) THEN
         ELSEIF ( ips(1)==ips(2) ) THEN
            IF ( d1>=d2 ) GOTO 20
         ELSE
            GOTO 20
         ENDIF
         is1 = 1
         is2 = 2
         ASSIGN 20 TO isret
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      IF ( ips(2)<ips(3) ) THEN
         ELSEIF ( ips(2)==ips(3) ) THEN
            IF ( d2>=d3 ) GOTO 40
         ELSE
            GOTO 40
         ENDIF
         is1 = 2
         is2 = 3
         ASSIGN 40 TO isret
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      RETURN
      CASE (3)
!
!      SWITCHES VALUES ON IS1, IS2
!
         nx = ips(is1)
         ips(is1) = ips(is2)
         ips(is2) = nx
         nx = Ips1(is1)
         Ips1(is1) = Ips1(is2)
         Ips1(is2) = nx
         d4 = Pr(is1)
         Pr(is1) = Pr(is2)
         Pr(is2) = d4
         d4 = Pi(is1)
         Pi(is1) = Pi(is2)
         Pi(is2) = d4
         d4 = Dr(is1)
         Dr(is1) = Dr(is2)
         Dr(is2) = d4
         d4 = Di(is1)
         Di(is1) = Di(is2)
         Di(is2) = d4
         d4 = dd(is1)
         dd(is1) = dd(is2)
         dd(is2) = d4
         GOTO isret
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cdetm2
