!*==intprt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE intprt(A,Cr,O,Name)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: A
   INTEGER :: Cr
   INTEGER :: O
   REAL , DIMENSION(2) :: Name
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: colnum , i , icropt
   INTEGER , DIMENSION(3) , SAVE :: crfmt
   INTEGER , DIMENSION(2,2) , SAVE :: cropt
   EXTERNAL matprt , prtmat
!
! End of declarations rewritten by SPAG
!
!
   DATA crfmt/4H(60X , 4H,2A4 , 4H,I5)/
   DATA cropt/4HCOLU , 4HMN   , 4HROW  , 4H    /
!
!     CR   = 0  IF MATRIX BY COLUMNS.
!          = 1  IF MATRIX BY ROWS.
!     IF O = 0, THE MATRIX WILL NOT BE PRINTED.
!     NAME = 8  CHARACTER BCD NAME OF THE MATRIX.
!
   IF ( Cr/=0 ) THEN
      icropt = 2
   ELSE
      icropt = 1
   ENDIF
!
   CALL matprt(*100,*200,A,-1,colnum)
   RETURN
 100  WRITE (mo,99001) Name(1) , Name(2)
99001 FORMAT (50X,24HINTERMEDIATE MATRIX ... ,2A4//)
 200  WRITE (mo,crfmt) (cropt(i,icropt),i=1,2) , colnum
   CALL prtmat(*100,*200,colnum)
!
END SUBROUTINE intprt
