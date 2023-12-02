!*==step2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE step2(U2,U1,U0,P,Ibuf)
USE C_INFBSX
USE C_NAMES
USE C_TRDXX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: U2
   REAL(REAL64) , DIMENSION(1) :: U1
   REAL(REAL64) , DIMENSION(1) :: U0
   REAL(REAL64) , DIMENSION(1) :: P
   INTEGER , DIMENSION(1) :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: file
   EXTERNAL fbsint , invfbs , matvc2 , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!     STEP2 WILL INTEGRATE FORWARD ONE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
!
   file(1) = Iscr1
   file(2) = Dum(2)
   file(4) = Sqr
!
!     TELL MATVC2/INVFBS FILES ARE OPEN
!
   Iopen = 1
!
!     FORM R.H.S. OF THE INTEGRATION EQUATION
!
   CALL matvc2(U1(1),P(1),file,Ibuf)
   file(1) = Iscr4
   CALL matvc2(U0(1),P(1),file,Ibuf)
!
!     CALL INVFBS/FBSINT TO DO THE FORWARD/BACKWARD PASS
!
   Ifil(1) = Iscr2
   Ifilu(1) = Iscr3
   CALL rdtrl(Ifil)
   CALL rdtrl(Ifilu)
   Ifil(5) = Rdp
   Ifil(3) = Dum(3)
   IF ( Isym==1 ) Iopen = -20
   IF ( Isym==1 ) CALL invfbs(P(1),U2(1),Ibuf)
   IF ( Isym==0 ) CALL fbsint(P(1),U2(1))
   Iopen = 0
END SUBROUTINE step2
