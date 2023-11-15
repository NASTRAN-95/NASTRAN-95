
SUBROUTINE encode(Ii)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ii
!
! Local variable declarations
!
   INTEGER i , idiv(6) , isum , j
!
! End of declarations
!
!
!     THIS SUBROUTINE CONVERTS THE DEGREE OF FREEDOM CODES AS GIVEN
!     IN BULK DATA FORM ( INTEGERS FROM 1-6 ) TO THE BIT PATTERN
!     USED IN SUBSTRUCTURE ANALYSIS.
!
   DATA idiv/100000 , 10000 , 1000 , 100 , 10 , 1/
!
   isum = 0
   DO i = 1 , 6
      j = Ii/idiv(i)
      IF ( j/=0 ) THEN
         isum = isum + 2**(j-1)
         Ii = Ii - j*idiv(i)
      ENDIF
   ENDDO
   Ii = isum
END SUBROUTINE encode
