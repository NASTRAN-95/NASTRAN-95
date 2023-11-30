
DOUBLE PRECISION FUNCTION dk89(I,A,B,M,N,X)
   IMPLICIT NONE
   DOUBLE PRECISION A , B
   INTEGER I , M , N
   DOUBLE PRECISION X(1)
   DOUBLE PRECISION amf , ammsf , amn1f , an1 , an2 , anm1f , capx , f89 , s , sf
   INTEGER iret , is , kfac , lfac , n1 , n2 , n3 , nfac
   f89 = 0.0D0
   capx = A + B*X(I)
   nfac = M
   ASSIGN 100 TO iret
   GOTO 600
 100  amf = kfac
   n1 = M + 1
   n2 = n1 - N
   an1 = n1
   an2 = n2
   is = 0
   s = 0.0D0
   sf = 1.0D0
   ammsf = amf
 200  n3 = n2 - is
   IF ( n3==0 ) THEN
      nfac = n2
      ASSIGN 300 TO iret
      GOTO 600
   ELSE
      f89 = f89 + amf*((-A)**is)*(capx**n3)/(ammsf*sf*(an2-s))
      GOTO 500
   ENDIF
 300  amn1f = kfac
   nfac = N - 1
   ASSIGN 400 TO iret
   GOTO 600
 400  anm1f = kfac
   f89 = f89 + amf*((-A)**n2)*dlog(dabs(capx))/(amn1f*anm1f)
 500  IF ( is<M ) THEN
      is = is + 1
      s = is
      sf = sf*s
      ammsf = ammsf/(an1-s)
      GOTO 200
   ELSEIF ( B==0.0D0 ) THEN
      dk89 = 0.0D0
      RETURN
   ELSE
      f89 = f89/(B**n1)
      dk89 = f89
      RETURN
   ENDIF
 600  kfac = 1
   IF ( nfac>=2 ) THEN
      DO lfac = 2 , nfac
         kfac = kfac*lfac
      ENDDO
   ENDIF
   GOTO iret
END FUNCTION dk89
