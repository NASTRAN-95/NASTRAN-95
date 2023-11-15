
SUBROUTINE skprec(Ifile,K)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ifile , K
!
! Local variable declarations
!
   INTEGER i , m , name(2)
!
! End of declarations
!
!
!
   DATA name/4HSKPR , 2HEC/
!
! ----------------------------------------------------------------------
!
   IF ( K<0 ) THEN
!
      m = iabs(K)
      DO i = 1 , m
         CALL bckrec(Ifile)
      ENDDO
   ELSEIF ( K/=0 ) THEN
!
      DO i = 1 , K
         CALL fwdrec(*200,Ifile)
      ENDDO
   ENDIF
!
 100  RETURN
!
 200  CALL mesage(-2,Ifile,name)
   GOTO 100
!
END SUBROUTINE skprec
