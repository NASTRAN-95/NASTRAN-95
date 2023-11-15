
SUBROUTINE intprt(A,Cr,O,Name)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Mo
   REAL Skip
   COMMON /system/ Skip , Mo
!
! Dummy argument declarations
!
   INTEGER Cr , O
   REAL A(1) , Name(2)
!
! Local variable declarations
!
   INTEGER colnum , crfmt(3) , cropt(2,2) , i , icropt
!
! End of declarations
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
   GOTO 99999
 100  WRITE (Mo,99001) Name(1) , Name(2)
99001 FORMAT (50X,24HINTERMEDIATE MATRIX ... ,2A4//)
 200  WRITE (Mo,crfmt) (cropt(i,icropt),i=1,2) , colnum
   CALL prtmat(*100,*200,colnum)
!
99999 END SUBROUTINE intprt
