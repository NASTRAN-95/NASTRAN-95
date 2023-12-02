!*==prtint.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prtint
   USE c_blank
   USE c_xxmprt
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: name
   EXTERNAL fname , intprt , rdtrl
!
! End of declarations rewritten by SPAG
!
!
!
!     OPT = 0 IF MATRIX BY COLUMNS...1 IF BY ROWS.
!
!
!
   IF ( prt>=0 ) THEN
      trlr(1) = 101
      CALL rdtrl(trlr)
      IF ( trlr(1)>0 ) THEN
         CALL fname(trlr,name)
         CALL intprt(x,opt,1,name)
      ENDIF
   ENDIF
END SUBROUTINE prtint
