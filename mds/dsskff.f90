
SUBROUTINE dsskff(Nn)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   INTEGER Nn
!
! Local variable declarations
!
   INTEGER n
!
! End of declarations
!
   n = Nn
   DO WHILE ( n/=0 )
      DO
         CALL dsfwr1
         IF ( Iretrn/=0 ) THEN
            n = n - 1
            EXIT
         ENDIF
      ENDDO
   ENDDO
END SUBROUTINE dsskff
