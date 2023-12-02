!*==frr1a1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frr1a1(Rz,Cz,Ib,Reb,Ceb)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Rz
   REAL :: Cz
   INTEGER :: Ib
   REAL :: Reb
   REAL :: Ceb
!
! Local variable declarations rewritten by SPAG
!
   REAL :: bf , bf1 , den
   INTEGER :: i , n
   COMPLEX :: sum , term , z , zk
!
! End of declarations rewritten by SPAG
!
!
!
   z = cmplx(Rz,Cz)
   IF ( cabs(z)<.1 ) THEN
!
      zk = z
      den = float(Ib) + 1.
      sum = cmplx(1.,0.)
      SPAG_Loop_1_1: DO i = 1 , 30
         term = zk/den
         sum = sum + term
         IF ( cabs(term)<1.E-9 ) EXIT SPAG_Loop_1_1
         zk = zk*z
         den = den*(float(Ib)+float(i+1))
      ENDDO SPAG_Loop_1_1
      Reb = real(sum)
      Ceb = aimag(sum)
      RETURN
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
END SUBROUTINE frr1a1
