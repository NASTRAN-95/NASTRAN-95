!*==encode.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE encode(Ii)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , isum , j
   INTEGER , DIMENSION(6) , SAVE :: idiv
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
