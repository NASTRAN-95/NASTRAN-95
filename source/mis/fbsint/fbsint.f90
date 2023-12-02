!*==fbsint.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbsint(X,Y)
   IMPLICIT NONE
   USE C_FBSX
   USE C_INFBSX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Y
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nrow , nrow2
   INTEGER , DIMENSION(15) :: iblk
   EXTERNAL fbs1 , fbs2 , rewind , skprec
!
! End of declarations rewritten by SPAG
!
!
!     GIVEN THE DECOMPOSITION OF A REAL SYMMETRIC MATRIX, FBSINT WILL
!     SOLVE A SYSTEM OF SIMULTANEOUS LINEAR EQUATIONS BY FORWARD-
!     BACKWARD SUBSTITUTION
!
!     THIS ROUTINE IS SUITABLE FOR BOTH SINGLE AND DOUBLE PRECISION
!     OPERATION
!
   !>>>>EQUIVALENCE (Filel(3),Nrow)
!
   nrow2 = nrow
   IF ( Filel(5)==2 ) nrow2 = 2*nrow
   DO i = 1 , nrow2
      Y(i) = X(i)
   ENDDO
   DO i = 1 , 7
      Lfile(i) = Filel(i)
   ENDDO
   CALL rewind(Filel)
   CALL skprec(Filel,1)
   iblk(1) = Filel(1)
   IF ( Filel(5)==1 ) CALL fbs1(iblk,Y,Y,nrow2)
   IF ( Filel(5)==2 ) CALL fbs2(iblk,Y,Y,nrow2)
END SUBROUTINE fbsint
