!*==dbmfdp.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmfdp
   USE i_zzzzzz
   USE i_dsiof
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibase , icnt , index , ival , ival2 , ival3 , ival4 , ivaln , ivalp , lblock , next
!
! End of declarations rewritten by SPAG
!
!********************************************************************
!     DBMFDP- DUMPS THE DIRECTORY CHAIN OF A GIVEN FILE.
!             ARGUMENT IDIR IS THE IN-MEMORY DIRECTORY FOR THE FILE
!********************************************************************
   ibase = locfx(mem)
   ival2 = ibase + fcb(9,ifilex)
   ival3 = ibase + fcb(10,ifilex)
   ival4 = ibase + fcb(11,ifilex)
   index = fcb(10,ifilex)
   lblock = mem(index+3)
   WRITE (iwr,99001) ifilex , ival2 , ival3 , ival4 , fcb(12,ifilex)
99001 FORMAT (///,25X,' DUMP OF FILE CHAIN FOR UNIT=',I6,/,14X,'( BLOCK ADDRESSES ARE IN WORDS,  BLOCK LENGTHS IN WORDS)',/,/,7X,   &
             &' FIRST BLOCK ADDRESS   ',I12,'   LAST BLOCK ADDRESS      ',I12,/,7X,' CURRENT BLOCK ADDRESS ',I12,                   &
             &'   ORIGINAL BUFFER ADDRESS ',I12)
   WRITE (iwr,99002)
99002 FORMAT (/,'  IN-MEM     BUFFER',/,' BLOCK NO.  BLOCK NO  BLOCK ADDRESS  PREV. BLOCK   NEXT BLOCK ',' LENGTH')
   next = fcb(9,ifilex)
   icnt = 0
   IF ( next==0 ) THEN
      WRITE (iwr,99003)
99003 FORMAT (//' *************** NO BLOCK ALLOCATED TO FILE **********')
   ELSE
      SPAG_Loop_1_1: DO
         icnt = icnt + 1
         IF ( next==0 ) EXIT SPAG_Loop_1_1
         ival = ibase + next
         ivalp = ibase + mem(next)
         ivaln = ibase + mem(next+1)
         IF ( mem(next)==0 ) ivalp = 0
         IF ( mem(next+1)==0 ) ivaln = 0
         WRITE (iwr,99004) mem(next+3) , mem(next+7) , ival , ivalp , ivaln , mem(next+2)
99004    FORMAT (I9,I11,5X,I12,7X,I12,5X,I12,I12)
99005    FORMAT (12(8(1X,I8),/))
         next = mem(next+1)
      ENDDO SPAG_Loop_1_1
   ENDIF
   WRITE (iwr,99006)
99006 FORMAT (///)
END SUBROUTINE dbmfdp
