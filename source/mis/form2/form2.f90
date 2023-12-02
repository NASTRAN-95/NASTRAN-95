!*==form2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE form2(Uddip1,Udiprm,Uiprm,Piprm,Ibuf)
   IMPLICIT NONE
   USE C_TRDXX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Uddip1
   REAL , DIMENSION(1) :: Udiprm
   REAL , DIMENSION(1) :: Uiprm
   REAL , DIMENSION(1) :: Piprm
   INTEGER , DIMENSION(1) :: Ibuf
   EXTERNAL matvec
!
! End of declarations rewritten by SPAG
!
!*******
!     FORM2 GENERATES THE VECTORS NECESSARY TO CHANGE THE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!*******
!
!*******
!     FORM UDOT(I+1), UDDOT(I+1), UDOT-(I), AND U-(I)
!*******
   CALL matvec(Uddip1(1),Piprm(1),Ifilm(1),Ibuf)
   CALL matvec(Udiprm(1),Piprm(1),Ifilb(1),Ibuf)
   CALL matvec(Uiprm(1),Piprm(1),Ifilk(1),Ibuf)
END SUBROUTINE form2
