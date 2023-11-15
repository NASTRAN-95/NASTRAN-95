
SUBROUTINE fbsinv(X,Y,Iobuff)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Filel(7) , Ltype , Nrow
   COMMON /fbsx  / Filel
!
! Dummy argument declarations
!
   INTEGER Iobuff
   REAL X(1) , Y(1)
!
! Local variable declarations
!
   INTEGER i , iblk(15) , nrow2 , parm(3)
!
! End of declarations
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
   EQUIVALENCE (Filel(3),Nrow) , (Filel(5),Ltype)
   DATA parm/4H     , 4HFBSI , 4HNV  /
!
!     FORWARD PASS
!
   parm(1) = Filel(1)
   iblk(1) = Filel(1)
   IF ( Ltype==2 ) THEN
!
!     TRANSFER THE DOUBLE PRECISION LOAD VECTOR TO THE SOLUTION VECTOR
!
      nrow2 = 2*Nrow
      DO i = 1 , nrow2
         Y(i) = X(i)
      ENDDO
      CALL fbs2(iblk,Y,Y,nrow2)
   ELSEIF ( Ltype/=1 ) THEN
!
!     FATAL ERRORS
!
      CALL mesage(-7,parm(1),parm(2))
      GOTO 99999
   ELSE
!
!     TRANSFER THE SINGLE PRECISION LOAD VECTOR TO THE SOLUTION VECTOR
!
      DO i = 1 , Nrow
         Y(i) = X(i)
      ENDDO
      CALL fbs1(iblk,Y,Y,Nrow)
   ENDIF
!
   CALL rewind(Filel)
   CALL skprec(Filel,1)
   RETURN
99999 END SUBROUTINE fbsinv
