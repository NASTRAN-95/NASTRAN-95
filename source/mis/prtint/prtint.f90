!*==prtint.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prtint
   IMPLICIT NONE
   USE C_BLANK
   USE C_XXMPRT
   USE C_ZZZZZZ
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
   IF ( Prt>=0 ) THEN
      Trlr(1) = 101
      CALL rdtrl(Trlr)
      IF ( Trlr(1)>0 ) THEN
         CALL fname(Trlr,name)
         CALL intprt(X,Opt,1,name)
      ENDIF
   ENDIF
END SUBROUTINE prtint
