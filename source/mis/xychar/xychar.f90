!*==xychar.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xychar(Row,Col,Char)
!
   USE c_machin
   USE c_system
   USE c_xypppp
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Row
   INTEGER :: Col
   INTEGER :: Char
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ichar , ishift , iword , let , n , nmask
   INTEGER , DIMENSION(4) :: mask
   LOGICAL , SAVE :: pass
   EXTERNAL andf , complf , khrfn1 , lshift , orf , rshift
!
! End of declarations rewritten by SPAG
!
   DATA pass/.FALSE./
!
   IF ( Row<=maxrow ) THEN
!
      IF ( Col>119 .OR. Col<1 .OR. Row<1 ) RETURN
!
!     CHAR COMING IN IS ASSUMED LEFT ADJUSTED
!
      IF ( .NOT.(pass) ) THEN
         pass = .TRUE.
!
!     SET UP MASKS FIRST TIME THROUGH AFTER LOADING
!
         n = 2**bperch - 1
         ishift = bperwd - bperch
         n = lshift(n,ishift)
         nmask = n
         DO i = 1 , 4
            mask(i) = complf(n)
            n = rshift(n,bperch)
         ENDDO
      ENDIF
!
!     COMPUTE WORD AND CHARACTER OF WORD
!
      iword = (Col-1)/4 + 1
      ichar = Col - (iword-1)*4
      iword = (Row-1)*30 + iword
!
!     PACK THE CHARACTER
!
      IF ( mach==5 .OR. mach==6 .OR. mach==21 ) THEN
!
!     VAX, ULTRIX, AND ALPHA
!
         z(iword) = khrfn1(z(iword),ichar,Char,1)
         RETURN
      ENDIF
   ELSE
      exceed = .TRUE.
      RETURN
   ENDIF
   let = rshift(andf(Char,nmask),bperch*(ichar-1))
   z(iword) = orf(andf(z(iword),mask(ichar)),let)
END SUBROUTINE xychar
