
SUBROUTINE csumm(D1,D2,Id1,D3,D4,Id2,D5,D6,Id5)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   DOUBLE PRECISION D1 , D2 , D3 , D4 , D5 , D6
   INTEGER Id1 , Id2 , Id5
!
! Local variable declarations
!
   REAL factor
   INTEGER mult
   DOUBLE PRECISION t1 , t2 , t3 , t4
!
! End of declarations
!
!
!     ADDS  D1+D2 TO  D3+D4 SCALING OUTPUT
!
   mult = iabs(Id1-Id2)
   IF ( mult<=38 ) factor = 10.0**mult
   t1 = D1
   t2 = D2
   t3 = D3
   t4 = D4
   Id5 = Id1
   IF ( Id1<Id2 ) THEN
      IF ( mult>38 ) THEN
         D5 = D1
         D6 = D2
         GOTO 100
      ELSE
         t3 = t3*factor
         t4 = t4*factor
      ENDIF
   ELSEIF ( Id1/=Id2 ) THEN
      IF ( mult>38 ) THEN
         D5 = D3
         D6 = D4
         Id5 = Id2
         GOTO 100
      ELSE
         t1 = t1*factor
         t2 = t2*factor
         Id5 = Id2
      ENDIF
   ENDIF
   D5 = t1 + t3
   D6 = t2 + t4
 100  RETURN
   ENTRY csqrtn(D1,D2,Id1,D3,D4,Id2)
!
!     COMPUTES COMPLEX SQRT = SCALED
!
   Id2 = Id1
   D3 = D1
   D4 = D2
   IF ( mod(Id1,2)/=0 ) THEN
      Id2 = Id2 - 1
!
!     NEGATIVE EXPONENT
!
      IF ( Id2<0 ) Id2 = Id2 + 1
      D3 = D3*10.0
      D4 = D4*10.0
   ENDIF
   Id2 = Id2/2
   t1 = dsqrt(D3*D3+D4*D4)
   t2 = dsqrt(dabs(D3+t1)/2.0)
   t3 = dsqrt(dabs(-D3+t1)/2.0)
   D3 = t2
   D4 = t3
   IF ( D2/=0.0D0 ) D4 = dsign(t3,D2)
   GOTO 100
!
!     SCALES DETERMINANT
!
   ENTRY cdetm3(D1,D2,Id1)
   t1 = dmax1(dabs(D1),dabs(D2))
   IF ( t1==0.0D0 ) GOTO 100
   DO WHILE ( t1>10.0D0 )
      D1 = D1*0.1D0
      D2 = D2*0.1D0
      t1 = t1*0.1D0
      Id1 = Id1 + 1
   ENDDO
   DO WHILE ( t1<1.0D0 )
      D1 = D1*10.0D0
      D2 = D2*10.0D0
      t1 = t1*10.0D0
      Id1 = Id1 - 1
   ENDDO
   GOTO 100
END SUBROUTINE csumm
