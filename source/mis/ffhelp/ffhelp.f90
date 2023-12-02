!*==ffhelp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ffhelp(J) !HIDESTARS (*,*,J)
   USE c_machin
   USE c_qmarkq
   USE c_system
   USE c_xechox
   USE c_xreadx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: J
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(4) , SAVE :: help , stop , yes
   CHARACTER(4) :: xx
   EXTERNAL upcase
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA stop , yes , help/'STOP' , 'Y   ' , 'HELP'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS ROUTINE IS CALLED ONLY BY FF
!
         IF ( J==2 ) THEN
         ELSEIF ( J==3 ) THEN
!
            WRITE (nout,99001)
99001       FORMAT (//,24H ENTER 'N' FOR NO PUNCH,,/7X,38H'Y' FOR PUNCH IN FREE-FIELD FORMAT, OR,/7X,                               &
                   &43H'X' FOR PUNCH IN NASTRAN FIXED-FIELD FORMAT,/)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( J==4 ) THEN
!
            WRITE (nout,99002)
99002       FORMAT (/,' MIFYLE - IS A RESERVED WORD.  TRY ANY OTHER NAME')
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( J==5 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            WRITE (nout,99003)
99003       FORMAT (///1X,'GENERATED OUTPUT CARDS ARE SAVED ONLY IF FILE NAME IS GIVEN.',//,                                        &
                   &' YOU MAY ENTER NASTRAN EXECUTIVE CONTROL AND CASE CONTROL',' CARDS FIRST',/,' (NO INPUT ECHO ON SCREEN)',//,   &
                   &' ADDITIONAL INPUT INFORMATION WILL BE GIVEN WHEN YOU ENTER ',12H'BEGIN BULK',//,                               &
                   &' YOU MAY QUIT FREE-FIELD PROGRAM AT ANY TIME BY ENTERING ',6H'STOP',/,' NORMALLY, JOB TERMINATES BY ',         &
                   &9H'ENDDATA',//,' YOU MAY USE ',10H'READFILE',' COMMAND TO READ ANY FILE WHICH',14H WAS 'STOPPED',/,             &
                   &' BEFORE, AND CONTINUE FROM WHERE THE PREVIOUS JOB ENDED',//,                                                   &
                   &' FREE-FIELD INPUT IS AVAILABLE ONLY IN BULK DATA SECTION',/,                                                   &
                   &' AND IS ACTIVATED BY A COMMA OR EQUAL SIGN IN COLS. 1 THRU 10',//,                                             &
                   &' BOTH UPPER-CASE AND LOWER-CASE LETTERS ARE ACCEPTABLE',//,' REFERENCE - G.CHAN: ',1H',                        &
                   &'COSMIC/NASTRAN FREE-FIELD INPUT',2H',,/13X,'12TH NASTRAN USERS',1H',' COLLOQUIUM, MAY 1984')
            WRITE (nout,99008) qmark
            READ (in,99009,END=20) xx
            CALL upcase(xx,4)
            IF ( xx/=yes ) GOTO 20
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         WRITE (nout,99004)
99004    FORMAT (///,' THE FOLLOWING SYMBOLS ARE USED FOR FREE-FIELD INPUT',//10X,'SYMBOL',12X,'FUNCTION',/,9X,2('----'),5X,        &
                &10('----'),/10X,', OR BLANK  FIELD SEPERATORS',/10X,'  =         DUPLICATES ONE CORRESPONDING FIELD',/10X,         &
                &'  ==        DUPLICATES THE REMAINING FIELDS',/10X,'  *(N)      INCREMENT BY N',/10X,                              &
                &'  %(E)      ENDING VALUE BY E',/10X,'  /         THIS INPUT FIELD IS SAME AS PREVIOUS FIELD',/10X,                &
                &'  J)        FIELD INDEX, J-TH FIELD (MUST FOLLOWED BY A VALUE)',/10X,')+ OR 10)   INDEX FOR CONTINUATION FIELD',  &
               & /10X,'  )         (IN COL. 1 ONLY) DUPLICATES THE CONTINUATION ID',/22X,                                           &
                &'OF PREVIOUS CARD INTO FIELD 1 OF CURRENT CARD',/10X,'  ,         COL.1 ONLY, AUTO-CONTINUATION ID GENERATION',    &
               & /10X,'  =(N)      1ST FIELD ONLY, DUPLICATES N CARDS WITH PROPER',/22X,' INCREMENTS',/12X,'+A-I',6X,               &
                &'CONTINUATION ID CAN BE DUPLICATED AUTOMATICALLY',/22X,'ONLY IF IT IS IN PLUS-ALPHA-MINUS-INTEGER FORM',//1X,      &
                &'EXAMPLES:',/1X,'GRID, 101,,  0.  0. ,  7. 8)2  )+ABC-2',/1X,'=(11),*(1)  ,,  *(1.), /  %(23.45),==')
         IF ( J==1 .OR. iechos/=-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (nout,99008) qmark
         READ (in,99009,END=20) xx
         CALL upcase(xx,4)
         IF ( xx==yes ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      IF ( xx==stop ) RETURN 2
         IF ( mach==4 .AND. in==5 ) REWIND in
         spag_nextblock_1 = 5
      CASE (3)
!
         WRITE (nout,99005)
99005    FORMAT (//,' *** FREE-FIELD INPUT IS OPTIONAL.',//5X,'FOUR (4)',                                                           &
                &' CONTROL OPTIONS ARE AVAILABLE - CAN BE ENTERED AT ANY TIME',/7X,                                                 &
                &'1.  PROMPT=ON, PROMPT=OFF, OR PROMPT=YES(DEFAULT)',/7X,'2.  SCALE/10,  OR SCALE/8',/7X,                           &
                &'3.  CANCEL=N,  (TO CANCEL N PREVIOUSLY GENERATED CARDS)',/7X,                                                     &
                &'4.  LIST  =N,  (TO   LIST N PREVIOUSLY GENERATED CARDS)',//7X,'ENTER ''HELP'' IF YOU NEED ADDITIONAL INSTRUCTIONS'&
               & ,//7X,'INTEGER INPUT SHOULD BE LIMITED TO 8 DIGITS',/7X,'UP TO 12 DIGITS ARE ALLOWED FOR FLOATING PT. NUMBER INPUT'&
               & ,/7X,'HOWEVER, ONLY UP TO 8 DIGIT ACCURACY IS KEPT',/7X,'             INPUT           RESULT ',/7X,                &
                &'         ------------       --------',/7X,'E.G.     123.456789         123.4567',/7X,                             &
                &'         123.456789+6       .12345+9',/7X,'         -123.4567D+5       -.1234+8',/7X,                             &
                &'         123.45678E+4       1234567.',/7X,'         0.00123456-3       .12345-5',/7X,                             &
                &'         0.0123456789       .0123456',/7X,'         .00000123456       .12345-5')
         IF ( iechos/=-2 ) WRITE (nout,99006)
99006    FORMAT (/7X,'(3 AND 4 ARE AVAILABLE ONLY IN THE FREE-FIELD STAND','-ALONE VERSION)')
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (nout,99007)
99007    FORMAT (/4X,'UP TO 94 CHARATERS ALLOWABLE ON AN INPUT LINE. ',' C/R TO CONTINUE')
         READ (in,99009,END=20) xx
         CALL upcase(xx,4)
         IF ( xx==help ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( J/=1 ) GOTO 20
         spag_nextblock_1 = 5
      CASE (5)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99008 FORMAT (/,' MORE',A1,' (Y,N) - ')
99009 FORMAT (A4)
END SUBROUTINE ffhelp
