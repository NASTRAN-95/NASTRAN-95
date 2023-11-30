
SUBROUTINE arrm(P,D,Nd)
   IMPLICIT NONE
   DOUBLE PRECISION D(3) , P(3)
   INTEGER Nd(3)
   DOUBLE PRECISION dx , px
   INTEGER i , nx
!
!     SCALED ARITHMETIC ROUTINES--ARRANGING ROUTINE
!
   DO i = 1 , 3
      IF ( D(i)/=0.0D0 ) THEN
         DO WHILE ( dabs(D(i))<1.0 )
            D(i) = D(i)*10.0
            Nd(i) = Nd(i) - 1
         ENDDO
         DO WHILE ( dabs(D(i))>=10.0 )
            D(i) = D(i)*0.1
            Nd(i) = Nd(i) + 1
         ENDDO
      ENDIF
   ENDDO
   IF ( Nd(1)>Nd(2) .AND. Nd(2)>Nd(3) ) RETURN
   IF ( Nd(1)<=Nd(2) .OR. Nd(1)<=Nd(3) ) THEN
      IF ( Nd(2)<Nd(3) ) THEN
      ELSEIF ( Nd(2)==Nd(3) ) THEN
         IF ( dabs(D(2))>=dabs(D(3)) ) GOTO 100
      ELSE
         GOTO 100
      ENDIF
      IF ( Nd(1)<Nd(3) ) THEN
      ELSEIF ( Nd(1)==Nd(3) ) THEN
         IF ( dabs(D(1))>=dabs(D(3)) ) GOTO 200
      ELSE
         GOTO 200
      ENDIF
      nx = Nd(1)
      dx = D(1)
      px = P(1)
      Nd(1) = Nd(3)
      D(1) = D(3)
      P(1) = P(3)
      Nd(3) = nx
      D(3) = dx
      P(3) = px
   ENDIF
   GOTO 200
 100  IF ( Nd(1)<Nd(2) ) THEN
   ELSEIF ( Nd(1)==Nd(2) ) THEN
      IF ( dabs(D(1))>=dabs(D(2)) ) GOTO 200
   ELSE
      GOTO 200
   ENDIF
   nx = Nd(1)
   dx = D(1)
   px = P(1)
   Nd(1) = Nd(2)
   D(1) = D(2)
   P(1) = P(2)
   Nd(2) = nx
   D(2) = dx
   P(2) = px
 200  IF ( Nd(2)<Nd(3) ) THEN
   ELSEIF ( Nd(2)==Nd(3) ) THEN
      IF ( dabs(D(2))>=dabs(D(3)) ) RETURN
   ELSE
      GOTO 99999
   ENDIF
   nx = Nd(2)
   dx = D(2)
   px = P(2)
   Nd(2) = Nd(3)
   D(2) = D(3)
   P(2) = P(3)
   Nd(3) = nx
   D(3) = dx
   P(3) = px
99999 RETURN
END SUBROUTINE arrm
