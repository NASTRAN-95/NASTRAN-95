
SUBROUTINE dsfwr1
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Local variable declarations
!
   INTEGER id
!
! End of declarations
!
   CALL dsskrc
   id = iand(Ibase(Indclr-1),Maskq1)
   IF ( id==Idsef ) THEN
      Iretrn = 1
   ELSE
      Iretrn = 0
   ENDIF
END SUBROUTINE dsfwr1
