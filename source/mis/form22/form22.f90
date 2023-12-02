!*==form22.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE form22(Uddip1,Udiprm,Uiprm,Piprm,Ibuf)
   USE c_trdxx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: Uddip1
   REAL(REAL64) , DIMENSION(1) :: Udiprm
   REAL(REAL64) , DIMENSION(1) :: Uiprm
   REAL(REAL64) , DIMENSION(1) :: Piprm
   INTEGER , DIMENSION(1) :: Ibuf
   EXTERNAL matvc2
!
! End of declarations rewritten by SPAG
!
!*******
!     FORM22 GENERATES THE VECTORS NECESSARY TO CHANGE THE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!*******
!
!
!*******
!     FORM UDOT(I+1), UDDOT(I+1), UDOT-(I), AND U-(I)
!*******
   CALL matvc2(Uddip1(1),Piprm(1),ifilm(1),Ibuf)
   CALL matvc2(Udiprm(1),Piprm(1),ifilb(1),Ibuf)
   CALL matvc2(Uiprm(1),Piprm(1),ifilk(1),Ibuf)
END SUBROUTINE form22
