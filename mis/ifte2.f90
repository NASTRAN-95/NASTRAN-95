
SUBROUTINE ifte2(Tha,Rp,Cp)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Cp , Rp , Tha
!
! Local variable declarations
!
   REAL d , epsi , rn , rps , sign , t1 , t2 , thao , trm , tsq
   INTEGER i , it
!
! End of declarations
!
   DATA thao , epsi/.1 , 1.E-9/
   IF ( abs(Tha)<thao ) THEN
!
!     EVALUATE SERIES
!
      rn = 1.0
      d = 1.0
      sign = -1.
      rps = 1.0
      tsq = Tha*Tha
      t1 = 3.
      t2 = 4.
      it = 1
   ELSE
      d = .5*Tha*Tha
      Rp = (1.-cos(Tha))/d
      Cp = (Tha-sin(Tha))/d
      RETURN
   ENDIF
 100  DO i = 1 , 50
      rn = rn*tsq
      d = d*t1*t2
      trm = rn/d*sign
      rps = rps + trm
      IF ( abs(trm)<epsi ) EXIT
      sign = -sign
      t1 = t1 + 2.
      t2 = t2 + 2.
   ENDDO
   IF ( it==2 ) THEN
      Cp = rps
   ELSE
      Rp = rps
      rn = Tha
      d = 3.0
      sign = -1.
      rps = Tha/3.
      t1 = 4.
      t2 = 5.
      it = 2
      GOTO 100
   ENDIF
END SUBROUTINE ifte2
