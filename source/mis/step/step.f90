!*==step.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE step(U2,U1,U0,P,Ibuf)
   IMPLICIT NONE
   USE C_INFBSX
   USE C_NAMES
   USE C_TRDXX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: U2
   REAL , DIMENSION(1) :: U1
   REAL , DIMENSION(1) :: U0
   REAL , DIMENSION(1) :: P
   INTEGER , DIMENSION(1) :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: file
   EXTERNAL fbsint , intfbs , matvec , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!     STEP WILL INTEGRATE FORWARD ONE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!
!
   file(1) = Iscr1
   file(2) = Dum(2)
   file(4) = Sqr
!
!     TELL MATVEC/INTFBS FILES ARE OPEN
!
   Iopen = 1
!
!     FORM R.H.S. OF THE INTEGRATION EQUATION
!
   CALL matvec(U1(1),P(1),file,Ibuf)
   file(1) = Iscr4
   CALL matvec(U0(1),P(1),file,Ibuf)
!
!     CALL INTFBS/FBSINT TO DO THE FORWARD/BACKWARD PASS
!
   Ifil(1) = Iscr2
   Ifilu(1) = Iscr3
   CALL rdtrl(Ifil)
   CALL rdtrl(Ifilu)
   Ifil(5) = Rsp
   Ifil(3) = Dum(2)
   IF ( Isym==1 ) CALL intfbs(P(1),U2(1),Ibuf)
   IF ( Isym==0 ) CALL fbsint(P(1),U2(1))
   Iopen = 0
END SUBROUTINE step
