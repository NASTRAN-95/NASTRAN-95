!*==sdrchk.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdrchk(Forvec,Cfrvec,Lvec,Kont)
   IMPLICIT NONE
   USE C_SDR2X9
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lvec
   REAL , DIMENSION(Lvec) :: Forvec
   REAL , DIMENSION(Lvec) :: Cfrvec
   INTEGER :: Kont
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   REAL :: r , rj
!
! End of declarations rewritten by SPAG
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
