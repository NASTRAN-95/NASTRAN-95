
SUBROUTINE frr1a1(Rz,Cz,Ib,Reb,Ceb)
   IMPLICIT NONE
   REAL Ceb , Cz , Reb , Rz
   INTEGER Ib
   REAL bf , bf1 , den
   INTEGER i , n
   COMPLEX sum , term , z , zk
!
!
   z = cmplx(Rz,Cz)
   IF ( cabs(z)<.1 ) THEN
!
      zk = z
      den = float(Ib) + 1.
      sum = cmplx(1.,0.)
      DO i = 1 , 30
         term = zk/den
         sum = sum + term
         IF ( cabs(term)<1.E-9 ) EXIT
         zk = zk*z
         den = den*(float(Ib)+float(i+1))
      ENDDO
      Reb = real(sum)
      Ceb = aimag(sum)
      GOTO 99999
   ENDIF
   zk = cmplx(1.,0.)
   n = Ib
   bf = 1.
   bf1 = 0.
   sum = cmplx(0.,0.)
   DO i = 1 , n
      sum = sum + zk/cmplx(bf,0.)
      zk = zk*z
      bf1 = bf1 + 1.
      bf = bf*bf1
   ENDDO
   zk = cmplx(bf,0.)/zk*(cexp(z)-sum)
   Reb = real(zk)
   Ceb = aimag(zk)
   RETURN
99999 RETURN
END SUBROUTINE frr1a1
