!*==fbsinv.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbsinv(X,Y,Iobuff)
   IMPLICIT NONE
   USE C_FBSX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Y
   INTEGER :: Iobuff
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ltype , nrow , nrow2
   INTEGER , DIMENSION(15) :: iblk
   INTEGER , DIMENSION(3) , SAVE :: parm
   EXTERNAL fbs1 , fbs2 , mesage , rewind , skprec
!
! End of declarations rewritten by SPAG
!
!
!     SINGLE PRECISION VERSION
!
!     FBSINV IS A SPECIAL FORWARD-BACKWARD SUBSTITUTION ROUTINE FOR
!     INVPWR. IT OPERATES ON CONJUNCTION WITH SDCOMP.
!     THE ARITHMETIC PRECISION IS THAT OF THE INPUT FILE
!
!     FILEL  = MATRIX CONTROL BLOCK FOR THE LOWER TRIANGLE
!     X      = THE LOAD VECTOR
!     Y      = THE SOLUTION VECTOR
!     IOBUFF = NOT USED
!
   !>>>>EQUIVALENCE (Filel(3),Nrow) , (Filel(5),Ltype)
   DATA parm/4H     , 4HFBSI , 4HNV  /
!
!     FORWARD PASS
!
   parm(1) = Filel(1)
   iblk(1) = Filel(1)
   IF ( ltype==2 ) THEN
!
!     TRANSFER THE DOUBLE PRECISION LOAD VECTOR TO THE SOLUTION VECTOR
!
      nrow2 = 2*nrow
      DO i = 1 , nrow2
         Y(i) = X(i)
      ENDDO
      CALL fbs2(iblk,Y,Y,nrow2)
   ELSEIF ( ltype/=1 ) THEN
!
!     FATAL ERRORS
!
      CALL mesage(-7,parm(1),parm(2))
      RETURN
   ELSE
!
!     TRANSFER THE SINGLE PRECISION LOAD VECTOR TO THE SOLUTION VECTOR
!
      DO i = 1 , nrow
         Y(i) = X(i)
      ENDDO
      CALL fbs1(iblk,Y,Y,nrow)
   ENDIF
!
   CALL rewind(Filel)
   CALL skprec(Filel,1)
   RETURN
END SUBROUTINE fbsinv
