!*==step.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE step(U2,U1,U0,P,Ibuf)
   USE c_infbsx
   USE c_names
   USE c_trdxx
   IMPLICIT NONE
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
   file(1) = iscr1
   file(2) = dum(2)
   file(4) = sqr
!
!     TELL MATVEC/INTFBS FILES ARE OPEN
!
   iopen = 1
!
!     FORM R.H.S. OF THE INTEGRATION EQUATION
!
   CALL matvec(U1(1),P(1),file,Ibuf)
   file(1) = iscr4
   CALL matvec(U0(1),P(1),file,Ibuf)
!
!     CALL INTFBS/FBSINT TO DO THE FORWARD/BACKWARD PASS
!
   ifil(1) = iscr2
   ifilu(1) = iscr3
   CALL rdtrl(ifil)
   CALL rdtrl(ifilu)
   ifil(5) = rsp
   ifil(3) = dum(2)
   IF ( isym==1 ) CALL intfbs(P(1),U2(1),Ibuf)
   IF ( isym==0 ) CALL fbsint(P(1),U2(1))
   iopen = 0
END SUBROUTINE step
