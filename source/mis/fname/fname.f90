!*==fname.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fname(File,Name)
   USE c_xfiat
   USE c_xfist
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER , DIMENSION(2) :: Name
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ix , j , k , n
   INTEGER , SAVE :: nblank , non1 , non2
!
! End of declarations rewritten by SPAG
!
!*******
!     GIVEN A FILE NO., FNAME WILL RETURN THE BCD DESCRIPTOR
!*******
   DATA nblank/4H    /
   DATA non1 , non2/4H (NO , 4HNE) /
!*******
!     SEARCH THE FIST FOR THE FILE
!*******
   n = fist(2)*2 + 2
   DO j = 3 , n , 2
      IF ( File==fist(j) ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!*******
!     FILE DOES NOT EXIST, RETURN -(NONE)-
!*******
   Name(1) = non1
   Name(2) = non2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      K = fist(J+1)
      IF ( K<=0 ) THEN
!*******
!     RETURN BCD DESCRIPTOR
!*******
         Name(1) = File
         Name(2) = Nblank
         RETURN
      ELSE
!
         Ix = fist(J+1) + 2
         Name(1) = fiat(Ix)
         Name(2) = fiat(Ix+1)
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE fname
