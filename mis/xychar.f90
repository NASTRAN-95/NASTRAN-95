
SUBROUTINE xychar(Row,Col,Char)
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bperch , Bperwd , Dum(38) , I123 , Id(300) , Iframe , Mach , Maxplt , Maxrow , Titlec(32) , Titlel(14) , Titler(14) ,    &
         & Xinc , Xmin , Xtitle(32) , Z(1)
   LOGICAL Exceed
   COMMON /machin/ Mach
   COMMON /system/ Dum , Bperch , Bperwd
   COMMON /xypppp/ Iframe , Titlec , Titlel , Titler , Xtitle , Id , Maxplt , Xmin , Xinc , Exceed , I123 , Maxrow
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Char , Col , Row
!
! Local variable declarations
!
   INTEGER andf , complf , khrfn1 , lshift , orf , rshift
   INTEGER i , ichar , ishift , iword , let , mask(4) , n , nmask
   LOGICAL pass
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
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
         GOTO 99999
      ENDIF
   ELSE
      Exceed = .TRUE.
      RETURN
   ENDIF
   let = rshift(andf(Char,nmask),Bperch*(ichar-1))
   Z(iword) = orf(andf(Z(iword),mask(ichar)),let)
   RETURN
99999 RETURN
END SUBROUTINE xychar
