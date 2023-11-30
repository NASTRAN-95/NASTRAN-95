
SUBROUTINE fbsint(X,Y)
   IMPLICIT NONE
   INTEGER Filel(7) , Lfile(7) , Nrow
   COMMON /fbsx  / Lfile
   COMMON /infbsx/ Filel
   REAL X(1) , Y(1)
   INTEGER i , iblk(15) , nrow2
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
   nrow2 = Nrow
   IF ( Filel(5)==2 ) nrow2 = 2*Nrow
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