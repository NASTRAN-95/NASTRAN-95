
SUBROUTINE cdetm2(P,D,Ip,Pr,Pi,Dr,Di,Ips1)
   IMPLICIT NONE
   DOUBLE PRECISION D(6) , Di(3) , Dr(3) , P(6) , Pi(3) , Pr(3)
   INTEGER Ip(6) , Ips1(3)
   DOUBLE PRECISION d1 , d2 , d3 , d4 , dd(3)
   INTEGER i , ips(3) , is1 , is2 , isret , k , nx
!
!     ARRANGES  P,D,IP  IN ORDER BY MAGNITUDE OF DETERMINANT
!
   EQUIVALENCE (d1,dd(1)) , (d2,dd(2)) , (d3,dd(3))
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
   IF ( ips(1)>ips(2) .AND. ips(2)>ips(3) ) GOTO 300
   IF ( ips(1)>ips(2) .AND. ips(1)>ips(3) ) GOTO 200
   IF ( ips(2)<ips(3) ) THEN
   ELSEIF ( ips(2)==ips(3) ) THEN
      IF ( d2>=d3 ) GOTO 100
   ELSE
      GOTO 100
   ENDIF
   IF ( ips(1)<ips(3) ) THEN
   ELSEIF ( ips(1)==ips(3) ) THEN
      IF ( d1>=d3 ) GOTO 200
   ELSE
      GOTO 200
   ENDIF
   is1 = 1
   is2 = 3
   ASSIGN 200 TO isret
   GOTO 400
 100  IF ( ips(1)<ips(2) ) THEN
   ELSEIF ( ips(1)==ips(2) ) THEN
      IF ( d1>=d2 ) GOTO 200
   ELSE
      GOTO 200
   ENDIF
   is1 = 1
   is2 = 2
   ASSIGN 200 TO isret
   GOTO 400
 200  IF ( ips(2)<ips(3) ) THEN
   ELSEIF ( ips(2)==ips(3) ) THEN
      IF ( d2>=d3 ) GOTO 300
   ELSE
      GOTO 300
   ENDIF
   is1 = 2
   is2 = 3
   ASSIGN 300 TO isret
   GOTO 400
 300  RETURN
!
!      SWITCHES VALUES ON IS1, IS2
!
 400  nx = ips(is1)
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
END SUBROUTINE cdetm2
