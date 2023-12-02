!*==fname.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fname(File,Name)
   IMPLICIT NONE
   USE C_XFIAT
   USE C_XFIST
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
   n = Fist(2)*2 + 2
   DO j = 3 , n , 2
      IF ( File==Fist(j) ) THEN
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
      k = Fist(j+1)
      IF ( k<=0 ) THEN
!*******
!     RETURN BCD DESCRIPTOR
!*******
         Name(1) = File
         Name(2) = nblank
         RETURN
      ELSE
!
         ix = Fist(j+1) + 2
         Name(1) = Fiat(ix)
         Name(2) = Fiat(ix+1)
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE fname
