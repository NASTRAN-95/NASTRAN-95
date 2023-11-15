
FUNCTION zj(Arg)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Arg
   REAL zj
!
! Local variable declarations
!
   REAL a , an , dbslj , pf
   INTEGER i
!
! End of declarations
!
!
!     ZERO ORDER BESSEL FUNCTION OF FIRST KIND
!
   dbslj = 1.0E-10
   a = -(Arg/2.0)**2
   zj = 1.0
   pf = 1.0
   an = 1.0
   DO i = 1 , 20
      an = an*a/pf**2
      pf = pf + 1.0
      IF ( abs(an)<=dbslj ) RETURN
      zj = zj + an
   ENDDO
END FUNCTION zj
