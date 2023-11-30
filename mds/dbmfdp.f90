
SUBROUTINE dbmfdp
   IMPLICIT NONE
   INCLUDE 'ZZZZZZ.COM'
   INCLUDE 'DSIOF.COM'
   INTEGER Isysbf , Iwr
   COMMON /system/ Isysbf , Iwr
   INTEGER ibase , icnt , index , ival , ival2 , ival3 , ival4 , ivaln , ivalp , lblock , next
   INTEGER locfx
!********************************************************************
!     DBMFDP- DUMPS THE DIRECTORY CHAIN OF A GIVEN FILE.
!             ARGUMENT IDIR IS THE IN-MEMORY DIRECTORY FOR THE FILE
!********************************************************************
   ibase = locfx(Mem)
   ival2 = ibase + Fcb(9,Ifilex)
   ival3 = ibase + Fcb(10,Ifilex)
   ival4 = ibase + Fcb(11,Ifilex)
   index = Fcb(10,Ifilex)
   lblock = Mem(index+3)
   WRITE (Iwr,99001) Ifilex , ival2 , ival3 , ival4 , Fcb(12,Ifilex)
99001 FORMAT (///,25X,' DUMP OF FILE CHAIN FOR UNIT=',I6,/,14X,'( BLOCK ADDRESSES ARE IN WORDS,  BLOCK LENGTHS IN WORDS)',/,/,7X,   &
             &' FIRST BLOCK ADDRESS   ',I12,'   LAST BLOCK ADDRESS      ',I12,/,7X,' CURRENT BLOCK ADDRESS ',I12,                   &
             &'   ORIGINAL BUFFER ADDRESS ',I12)
   WRITE (Iwr,99002)
99002 FORMAT (/,'  IN-MEM     BUFFER',/,' BLOCK NO.  BLOCK NO  BLOCK ADDRESS  PREV. BLOCK   NEXT BLOCK ',' LENGTH')
   next = Fcb(9,Ifilex)
   icnt = 0
   IF ( next==0 ) THEN
      WRITE (Iwr,99003)
99003 FORMAT (//' *************** NO BLOCK ALLOCATED TO FILE **********')
   ELSE
      DO
         icnt = icnt + 1
         IF ( next==0 ) EXIT
         ival = ibase + next
         ivalp = ibase + Mem(next)
         ivaln = ibase + Mem(next+1)
         IF ( Mem(next)==0 ) ivalp = 0
         IF ( Mem(next+1)==0 ) ivaln = 0
         WRITE (Iwr,99004) Mem(next+3) , Mem(next+7) , ival , ivalp , ivaln , Mem(next+2)
99004    FORMAT (I9,I11,5X,I12,7X,I12,5X,I12,I12)
99005    FORMAT (12(8(1X,I8),/))
         next = Mem(next+1)
      ENDDO
   ENDIF
   WRITE (Iwr,99006)
99006 FORMAT (///)
   RETURN
END SUBROUTINE dbmfdp