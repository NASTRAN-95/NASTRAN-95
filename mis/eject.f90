
INTEGER FUNCTION eject(Lines)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Lincnt , Maxlin
   REAL Skp1(8) , Skp2(2)
   COMMON /system/ Skp1 , Maxlin , Skp2 , Lincnt
!
! Dummy argument declarations
!
   INTEGER Lines
!
! End of declarations
!
!
!     LINES = NUNBER OF LINES TO BE PRINTED.
!     RESULT = 1 IF NEW PAGE IS STARTED.
!
   eject = 0
   IF ( Lincnt+Lines+2>Maxlin ) THEN
      CALL page1
      eject = 1
   ENDIF
END FUNCTION eject