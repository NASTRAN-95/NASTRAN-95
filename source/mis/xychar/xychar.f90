!*==xychar.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xychar(Row,Col,Char)
!
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SYSTEM
   USE C_XYPPPP
   USE C_ZZZZZZ
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
   IF ( Row<=Maxrow ) THEN
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
         n = 2**Bperch - 1
         ishift = Bperwd - Bperch
         n = lshift(n,ishift)
         nmask = n
         DO i = 1 , 4
            mask(i) = complf(n)
            n = rshift(n,Bperch)
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
      IF ( Mach==5 .OR. Mach==6 .OR. Mach==21 ) THEN
!
!     VAX, ULTRIX, AND ALPHA
!
         Z(iword) = khrfn1(Z(iword),ichar,Char,1)
         RETURN
      ENDIF
   ELSE
      Exceed = .TRUE.
      RETURN
   ENDIF
   let = rshift(andf(Char,nmask),Bperch*(ichar-1))
   Z(iword) = orf(andf(Z(iword),mask(ichar)),let)
   RETURN
END SUBROUTINE xychar
