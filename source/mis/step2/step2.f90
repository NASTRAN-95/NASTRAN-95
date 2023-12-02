!*==step2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE step2(U2,U1,U0,P,Ibuf)
   USE c_infbsx
   USE c_names
   USE c_trdxx
   USE iso_fortran_env
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
   file(1) = iscr1
   file(2) = dum(2)
   file(4) = sqr
!
!     TELL MATVC2/INVFBS FILES ARE OPEN
!
   iopen = 1
!
!     FORM R.H.S. OF THE INTEGRATION EQUATION
!
   CALL matvc2(U1(1),P(1),file,Ibuf)
   file(1) = iscr4
   CALL matvc2(U0(1),P(1),file,Ibuf)
!
!     CALL INVFBS/FBSINT TO DO THE FORWARD/BACKWARD PASS
!
   ifil(1) = iscr2
   ifilu(1) = iscr3
   CALL rdtrl(ifil)
   CALL rdtrl(ifilu)
   ifil(5) = rdp
   ifil(3) = dum(3)
   IF ( isym==1 ) iopen = -20
   IF ( isym==1 ) CALL invfbs(P(1),U2(1),Ibuf)
   IF ( isym==0 ) CALL fbsint(P(1),U2(1))
   iopen = 0
END SUBROUTINE step2
