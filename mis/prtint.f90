
SUBROUTINE prtint
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Opt , Prt , Trlr(7)
   REAL X(1)
   COMMON /blank / Opt , Prt
   COMMON /xxmprt/ Trlr
   COMMON /zzzzzz/ X
!
! Local variable declarations
!
   REAL name(2)
!
! End of declarations
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
