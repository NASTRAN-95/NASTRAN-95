!*==page2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE page2(Lines)
!
!     2ND MASTER PAGING ROUTINE FOR NASTRAN
!
!     IABS(LINES) = NO. OF LINES TO BE ADDED FOR OUTPUT
!     IF CURRENT PAGE CAN NOT ACCOMODATE THE INCOMING LINES, A NEW PAGE
!     IS INITIATED WITH PROPER HEADINGS.
!
!     IF LINES IS NEGATIVE, A 6-LINE HEADER IS PRINTED.
!     IF LINES IS POSITIVE, A 3-LINE HEADER IS PRINTED AND FOLLOWED BY
!        3 BLANK LINES.
!
!     ENTRY POINT PAGE3 -
!     A 3-LINE HEADER IS PRINTED, NO BLANK LINES FOLLOWED. LINES CAN BE
!     NEGATIVE OR POSITIVE.
!
!     SIMPLIFIED BY G.CHAN/UNISYS, AND PAGE3 ADDED  12/92
!
   USE c_chmach
   USE c_machin
   USE c_output
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lines
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(30) :: ahead
   INTEGER :: fchar , flag , i , in , ll , ncmnam , ncmos
   CHARACTER(3) , DIMENSION(12) , SAVE :: month
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(18) :: titlex
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Titlex(1),Title(1))
   DATA month/'JAN' , 'FEB' , 'MAR' , 'APR' , 'MAY' , 'JUN' , 'JUL' , 'AUG' , 'SEP' , 'OCT' , 'NOV' , 'DEC'/
   DATA name/4H PAG , 4HE2  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         flag = 2
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( Lines/=0 ) THEN
            ll = iabs(Lines)
            IF ( sym-line<ll .OR. ofp/=0 ) THEN
!
               page = page + 1
               tline = tline + line
               line = 0
               IF ( tline>maxlin ) THEN
!
!     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE
!
                  maxlin = maxlin + 3000
                  CALL mesage(-19,tline,name)
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
99001             FORMAT (A2)
                  fchar = fchar + 3
                  ahead(fchar:30) = mchnam(1:ncmnam)//' '//machos(1:ncmos)//' NASTRAN'
!
                  WRITE (nout,99002) titlex , ahead , month(in) , date(2) , date(3) , page
99002             FORMAT (1H1,4X,17A4,A2,' /',A30,'/ ',A3,1X,I2,', ',I2,' / PAGE',I6)
                  WRITE (nout,99004) subtit
                  WRITE (nout,99005) label
                  line = line + 4
                  IF ( flag>=0 ) THEN
                     IF ( Lines>0 ) THEN
!
                        WRITE (nout,99003)
99003                   FORMAT (///)
                        line = line + 4
                     ELSE
!
                        WRITE (nout,99005) (head1(i),i=1,32)
                        WRITE (nout,99004) (head2(i),i=1,32)
                        WRITE (nout,99004) (head3(i),i=1,32)
                        line = line + 4
                     ENDIF
                  ENDIF
                  line = line + ll
               ENDIF
            ELSE
               line = line + ll
            ENDIF
         ENDIF
!
         ofp = 0
         RETURN
!
!
         ENTRY page3(Lines)
!     ===================
!
         flag = -3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (5X,31A4,A3)
99005 FORMAT (/5X,31A4,A3)
!
END SUBROUTINE page2
