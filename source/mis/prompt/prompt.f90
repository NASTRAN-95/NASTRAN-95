!*==prompt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prompt
!
!     DRIVER FOR  INTERACTIVE MODULE - PROMPT
!
!        PROMPT    //S,N,PEXIT/S,N,PLOT1/S,N,PLOT2/S,N,XYPLOT/
!                    S,N,SCAN1/S,N,SCAN2/DUM1/DUM2/DUM3/DUM4 $
!
   USE c_blank
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: b , c , p , s
   INTEGER :: i , in , intra , j , nout , soln
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Nout,Ksysm(2)) , (Soln,Ksysm(22)) , (In,Ksysm(4)) , (Intra,Ksysm(86))
   DATA p , s , c , b/1HP , 1HS , 1HC , 1H /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         intra = iabs(intra)
         nout = 6
         DO i = 1 , 10
            param(i) = 0
         ENDDO
 20      SPAG_Loop_1_1: DO
!
            WRITE (nout,99001)
!
99001       FORMAT (9X,'1. EXIT',/9X,'2. STRUCTURE PLOTS - UNDEFORMED',/9X,'3. STRUCTURE PLOTS - DEFORMED',/9X,'4. XYPLOTS',/9X,    &
                   &'5. SCAN OUTPUT - SORT1',/9X,'6. SCAN OUTPUT - SORT2',//9X,'SELECT ONE OPTION FROM MENU -')
            READ (in,99002,ERR=20) j
99002       FORMAT (I1)
            IF ( j>=1 .AND. j<=6 ) THEN
               IF ( soln==3 .AND. (j==4 .OR. j==6) ) THEN
                  WRITE (nout,99003) j , soln
99003             FORMAT (/,' OPTION',I3,' IS NOT AVAILABLE FOR SOLUTION',I3)
               ELSE
                  param(j) = -1
                  IF ( j==1 ) RETURN
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         WRITE (nout,99004)
99004    FORMAT (/9X,'OUTPUT TO SCREEN, OR TO PRINTFILE, OR CANCEL OPTION (S/P/C) -')
         i = b
         READ (in,99005,END=40) i
99005    FORMAT (A1)
 40      IF ( i==b ) RETURN
         IF ( i==c ) THEN
!
            param(j) = 0
            GOTO 20
         ELSEIF ( i/=p .AND. i/=s ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( i==s ) intra = mod(intra,10)
         IF ( i==p ) intra = mod(intra,10) + 10
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE prompt
