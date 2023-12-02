!*==page.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE page
   USE c_chmach
   USE c_machin
   USE c_output
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(30) :: ahead
   INTEGER :: fchar , i , in , iout , ncmnam , ncmos
   CHARACTER(3) , DIMENSION(12) , SAVE :: month
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(18) :: titlex
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MASTER PAGING ROUTINE FOR NASTRAN.
!
   !>>>>EQUIVALENCE (Titlex(1),Title(1))
   DATA month/'JAN' , 'FEB' , 'MAR' , 'APR' , 'MAY' , 'JUN' , 'JUL' , 'AUG' , 'SEP' , 'OCT' , 'NOV' , 'DEC'/
   DATA name/4HPAGE , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         iout = 1
         spag_nextblock_1 = 2
      CASE (2)
         ipage = ipage + 1
         itline = itline + line
         line = 0
         IF ( itline>maxlin ) THEN
!
!     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE
!
            maxlin = maxlin + 3000
            CALL mesage(-19,itline,name)
         ELSE
            in = date(1)
!
!   ASSEMBLE PAGE HEADING
!
            ahead = ' '
            ncmnam = index(mchnam,' ') - 1
            IF ( ncmnam<=-1 ) ncmnam = 11
            ncmos = index(machos,' ') - 1
            IF ( ncmos<=-1 ) ncmos = 7
            fchar = (18-ncmnam-ncmos)/2 + 1
            WRITE (ahead(fchar:fchar+1),99001) crdate(3)
99001       FORMAT (A2)
            fchar = fchar + 3
            ahead(fchar:30) = mchnam(1:ncmnam)//' '//machos(1:ncmos)//' NASTRAN'
!
            WRITE (otpe,99002) titlex , ahead , month(in) , date(2) , date(3) , ipage
99002       FORMAT (1H1,4X,17A4,A2,' /',A30,'/ ',A3,1X,I2,', ',I2,' / PAGE',I6)
            WRITE (otpe,99003) subtit
            WRITE (otpe,99004) label
            line = line + 4
            IF ( iout/=0 ) THEN
               WRITE (otpe,99004) (head1(i),i=1,32)
               WRITE (otpe,99003) (head2(i),i=1,32)
               WRITE (otpe,99003) (head3(i),i=1,32)
               line = line + 4
            ENDIF
         ENDIF
         RETURN
!
!
         ENTRY page1
!     ===========
!
         iout = 0
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99003 FORMAT (5X,31A4,A3)
99004 FORMAT (1H0,4X,31A4,A3)
END SUBROUTINE page
