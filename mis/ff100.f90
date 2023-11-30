
FUNCTION ff100(I,A,B,M,N,X)
   IMPLICIT NONE
   REAL A , B
   INTEGER I , M , N
   REAL ff100
   REAL X(1)
   REAL am1f , amn2f , amn2sf , an1 , an1f , an1p1 , an2 , capx , f100 , s , sf , xx
   INTEGER ifac , iret , is , lfac , n1 , n2 , n3 , n4 , nfac
   f100 = 0.0
   capx = A + B*X(I)
   xx = X(I)
   n1 = M + N - 2
   n2 = M - 1
   n3 = n1 + 1
   an1 = n1
   an2 = n2
   nfac = n1
   ASSIGN 100 TO iret
   GOTO 600
 100  amn2f = ifac
   an1p1 = an1 + 1.0
   is = 0
   s = 0.0
   sf = 1.0
   amn2sf = amn2f
 200  n4 = n2 - is
   IF ( n4==0 ) THEN
      nfac = n2
      ASSIGN 300 TO iret
      GOTO 600
   ELSE
      f100 = f100 + amn2f*(capx**n4)*((-B)**is)/(amn2sf*sf*(an2-s)*(xx**n4))
      GOTO 500
   ENDIF
 300  am1f = ifac
   nfac = N - 1
   ASSIGN 400 TO iret
   GOTO 600
 400  an1f = ifac
   f100 = f100 + amn2f*((-B)**n2)*alog(abs(capx/xx))/(am1f*an1f)
 500  IF ( is<n1 ) THEN
      is = is + 1
      s = is
      sf = sf*s
      amn2sf = amn2sf/(an1p1-s)
      GOTO 200
   ELSE
      f100 = -f100/(A**n3)
      ff100 = f100
      RETURN
   ENDIF
 600  ifac = 1
   IF ( nfac>=2 ) THEN
      DO lfac = 2 , nfac
         ifac = ifac*lfac
      ENDDO
   ENDIF
   GOTO iret
END FUNCTION ff100