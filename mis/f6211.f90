
FUNCTION f6211(I,A,B,X)
   IMPLICIT NONE
   REAL A , B
   INTEGER I
   REAL f6211
   REAL X(1)
   REAL aaj , c1 , c2 , c3 , xx
   INTEGER j
   xx = X(I)
   IF ( (B*xx)**2<A**2 ) THEN
      f6211 = alog(abs(A))*alog(abs(xx))
      c1 = -B*xx/A
      c2 = 1.0
      j = 0
      DO
         j = j + 1
         aaj = j
         c2 = c2*c1
         c3 = c2/(aaj**2)
         f6211 = f6211 - c3
         IF ( abs(c3)<=0.000001 ) RETURN
      ENDDO
   ELSEIF ( (B*xx)**2==A**2 ) THEN
      IF ( A/=B*xx ) THEN
         f6211 = 0.0
         RETURN
      ELSE
         f6211 = 0.5*(alog(abs(2.0*B*xx)))**2
         RETURN
      ENDIF
   ELSE
      f6211 = (alog(abs(B*xx))**2)/2.0
      c1 = -A/(B*xx)
      c2 = 1.0
      j = 0
      DO
         j = j + 1
         aaj = j
         c2 = c2*c1
         c3 = c2/(aaj**2)
         f6211 = f6211 + c3
         IF ( abs(c3)<=0.000001 ) EXIT
      ENDDO
   ENDIF
END FUNCTION f6211