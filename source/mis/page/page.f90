!*==page.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE page
   IMPLICIT NONE
   USE C_CHMACH
   USE C_MACHIN
   USE C_OUTPUT
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(30) :: ahead
   INTEGER :: fchar , i , in , iout , ncmnam , ncmos
   CHARACTER(3) , DIMENSION(12) , SAVE :: month
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(18) :: titlex
!
! End of declarations rewritten by SPAG
!
!
!     MASTER PAGING ROUTINE FOR NASTRAN.
!
   !>>>>EQUIVALENCE (Titlex(1),Title(1))
   DATA month/'JAN' , 'FEB' , 'MAR' , 'APR' , 'MAY' , 'JUN' , 'JUL' , 'AUG' , 'SEP' , 'OCT' , 'NOV' , 'DEC'/
   DATA name/4HPAGE , 4H    /
!
   iout = 1
 100  Ipage = Ipage + 1
   Itline = Itline + Line
   Line = 0
   IF ( Itline>Maxlin ) THEN
!
!     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE
!
      Maxlin = Maxlin + 3000
      CALL mesage(-19,Itline,name)
   ELSE
      in = Date(1)
!
!   ASSEMBLE PAGE HEADING
!
      ahead = ' '
      ncmnam = index(Mchnam,' ') - 1
      IF ( ncmnam<=-1 ) ncmnam = 11
      ncmos = index(Machos,' ') - 1
      IF ( ncmos<=-1 ) ncmos = 7
      fchar = (18-ncmnam-ncmos)/2 + 1
      WRITE (ahead(fchar:fchar+1),99001) Crdate(3)
99001 FORMAT (A2)
      fchar = fchar + 3
      ahead(fchar:30) = Mchnam(1:ncmnam)//' '//Machos(1:ncmos)//' NASTRAN'
!
      WRITE (Otpe,99002) titlex , ahead , month(in) , Date(2) , Date(3) , Ipage
99002 FORMAT (1H1,4X,17A4,A2,' /',A30,'/ ',A3,1X,I2,', ',I2,' / PAGE',I6)
      WRITE (Otpe,99003) Subtit
      WRITE (Otpe,99004) Label
      Line = Line + 4
      IF ( iout/=0 ) THEN
         WRITE (Otpe,99004) (Head1(i),i=1,32)
         WRITE (Otpe,99003) (Head2(i),i=1,32)
         WRITE (Otpe,99003) (Head3(i),i=1,32)
         Line = Line + 4
      ENDIF
   ENDIF
   RETURN
!
!
   ENTRY page1
!     ===========
!
   iout = 0
   GOTO 100
99003 FORMAT (5X,31A4,A3)
99004 FORMAT (1H0,4X,31A4,A3)
END SUBROUTINE page
