
SUBROUTINE fname(File,Name)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Fiat(1) , Fist(2)
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
!
! Dummy argument declarations
!
   INTEGER File
   INTEGER Name(2)
!
! Local variable declarations
!
   INTEGER ix , j , k , n , nblank , non1 , non2
!
! End of declarations
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
      IF ( File==Fist(j) ) GOTO 100
   ENDDO
!*******
!     FILE DOES NOT EXIST, RETURN -(NONE)-
!*******
   Name(1) = non1
   Name(2) = non2
   RETURN
 100  k = Fist(j+1)
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
END SUBROUTINE fname
