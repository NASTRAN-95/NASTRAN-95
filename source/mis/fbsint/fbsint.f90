!*==fbsint.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbsint(X,Y)
   USE c_fbsx
   USE c_infbsx
   IMPLICIT NONE
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
   IF ( filel(5)==2 ) nrow2 = 2*nrow
   DO i = 1 , nrow2
      Y(i) = X(i)
   ENDDO
   DO i = 1 , 7
      lfile(i) = filel(i)
   ENDDO
   CALL rewind(filel)
   CALL skprec(filel,1)
   iblk(1) = filel(1)
   IF ( filel(5)==1 ) CALL fbs1(iblk,Y,Y,nrow2)
   IF ( filel(5)==2 ) CALL fbs2(iblk,Y,Y,nrow2)
END SUBROUTINE fbsint
