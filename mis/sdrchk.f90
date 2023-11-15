
SUBROUTINE sdrchk(Forvec,Cfrvec,Lvec,Kont)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Fnchk , Twotop
   INTEGER Nchk(5)
   COMMON /sdr2x9/ Nchk , Twotop , Fnchk
!
! Dummy argument declarations
!
   INTEGER Kont , Lvec
   REAL Cfrvec(Lvec) , Forvec(Lvec)
!
! Local variable declarations
!
   INTEGER i
   REAL r , rj
!
! End of declarations
!
!   THIS ROUTINE IS USED BY ELEMENT SUBROUTINES THAT DETERMINE IF THE
! REQUESTED STRESS/FORCE PRECISION IS AVAILABLE...
!-----
!
   DO i = 1 , Lvec
      IF ( Cfrvec(i)==0.0 ) r = 1.0E0
      IF ( Cfrvec(i)/=0.0 ) r = abs(Forvec(i)/Cfrvec(i))
      IF ( r>1.001 ) r = 1.0E0
      IF ( r==0.0 ) rj = Twotop
      IF ( r/=0.0 ) rj = Twotop + alog10(r)
      IF ( rj<0.0 ) rj = 0.0
      Cfrvec(i) = rj
      IF ( rj<Fnchk ) Kont = Kont + 1
   ENDDO
!-----
END SUBROUTINE sdrchk
