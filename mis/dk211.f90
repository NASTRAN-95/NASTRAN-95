
DOUBLE PRECISION FUNCTION dk211(I,A,B,X)
   IMPLICIT NONE
   DOUBLE PRECISION A , B
   INTEGER I
   DOUBLE PRECISION X(1)
   DOUBLE PRECISION aaj , c1 , c2 , c3 , f6211 , xx
   INTEGER j
   xx = X(I)
   IF ( (B*xx)**2<A**2 ) THEN
      f6211 = dlog(dabs(A))*dlog(dabs(xx))
      c1 = -B*xx/A
      c2 = 1.0D0
      j = 0
      DO
         j = j + 1
         aaj = j
         c2 = c2*c1
         c3 = c2/(aaj**2)
         f6211 = f6211 - c3
         IF ( dabs(c3)<=0.1D-5 ) THEN
            dk211 = f6211
            RETURN
         ENDIF
      ENDDO
   ELSEIF ( (B*xx)**2==A**2 ) THEN
      IF ( A/=B*xx ) THEN
         f6211 = 0.0D0
         dk211 = f6211
         RETURN
      ELSE
         f6211 = 0.5D0*(dlog(dabs(2.0D0*B*xx)))**2
         dk211 = f6211
         RETURN
      ENDIF
   ELSE
      f6211 = (dlog(dabs(B*xx))**2)/2.0D0
      c1 = -A/(B*xx)
      c2 = 1.0D0
      j = 0
      DO
         j = j + 1
         aaj = j
         c2 = c2*c1
         c3 = c2/(aaj**2)
         f6211 = f6211 + c3
         IF ( dabs(c3)<=0.1D-5 ) THEN
            dk211 = f6211
            EXIT
         ENDIF
      ENDDO
   ENDIF
END FUNCTION dk211
