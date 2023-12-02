!*==mesage.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mesage(No,Parm,Name)
   USE c_machin
   USE c_msgx
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: No
   INTEGER :: Parm
   INTEGER , DIMENSION(2) :: Name
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , k
   INTEGER , SAVE :: link1
   EXTERNAL errtrc , msgwrt , pexit , sswtch
!
! End of declarations rewritten by SPAG
!
!
!     MESAGE IS USED TO QUEUE NON-FATAL MESSAGES DURING THE EXECUTION
!     OF A MODULE, AND EXITS IF MESSAGE IS FATAL
!
!     REVISED 1/92 BY G.CHAN/UNISYS.
!     IF MESSAGE IS FATAL AND DIAG 1 IS ON -
!
!     IBM, CDC AND UNIVAC - PRINT THE MESSAGE(S), GIVE A CORE DUMP AND
!     CALL PEXIT
!
!     VAX OR UNIX (MACHINE TYPE .GE. 5) - IF LAST MESSAGE IS NOT INSUFF.
!     CORE OR INSUFFICIENT TIME, AND FATAL ERROR IS NOT IN LINK 1, PRINT
!     ONLY THE MESSAGE NO(S). AND GIVE AN ERROR TRACEBACK. NO CORE DUMP.
!     TO MAKE SURE THAT THE CURRENT MODULE (WHICH CALLS FATAL MESSAGE)
!     IS UTILL IN CORE, THE MESSAGE PRINTOUT MODULE CAN NOT BE CALLED,
!     AND THEREFORE THE TEXT(S) OF THE MESSAGE(S) CAN NOT BE PRINTED.
!
   DATA link1/4HNS01/
!
!     N        =  CURRENT NUMBER OF MESSAGES STORED
!     M        =  MAXIMUM NUMBER POSSIBLE
!     MSG(4,I) =  STORAGE SPACE FOR THE MESSAGE PARAMETERS
!
   n = n + 1
   IF ( n>m ) THEN
      n = m
      IF ( No>0 ) RETURN
   ENDIF
!
   msg(1,n) = No
   msg(2,n) = Parm
   msg(3,n) = Name(1)
   msg(4,n) = Name(2)
   IF ( No<=0 ) THEN
!
!     MESSAGE IS FATAL, TERMINATE RUN
!
      CALL sswtch(1,j)
      IF ( j/=0 ) THEN
         IF ( mach==5 ) THEN
!
!     VAX, UNIX (MACHINE TYPE 5 AND HIGHER)
!
            IF ( linkno/=link1 ) THEN
               i = iabs(msg(1,n))
               IF ( i/=8 .AND. i/=119 .AND. i/=45 .AND. i/=50 ) THEN
!             INSUFF. CORE             INSUFFICIENT TIME
!
                  IF ( i==30 ) THEN
                     j = msg(2,n)
!
!     INSUFFECIENT CORE
                     IF ( j==142 .OR. j==289 .OR. j==296 .OR. j==253 .OR. j==365 ) GOTO 50
!
!     INSUFFECIENT TIME
                     IF ( j==234 .OR. j==228 ) GOTO 50
                  ENDIF
!
                  WRITE (nout,99001) n
99001             FORMAT ('0*** DUE TO SYSTEM ERROR-TRACEBACK, THE TEXT(S) OF THE ','FOLLOWING',I3,' MSG NO(S). CAN NOT BE PRINTED')
                  DO k = 1 , n
                     i = msg(1,k)
                     IF ( iabs(i)==30 ) THEN
                        i = msg(2,k)
                        j = 2000 + iabs(i)
                     ELSE
                        j = 3000 + iabs(i)
                     ENDIF
                     WRITE (nout,99002) i , j
99002                FORMAT (5X,'ERROR',I4,' (or ',I5,1H))
                     IF ( i/=30 .AND. msg(2,k)>100 .AND. msg(2,k)<400 ) WRITE (nout,99003) msg(2,k)
99003                FORMAT (1H+,30X,'GINO UNIT=',I4)
                  ENDDO
                  WRITE (nout,99004)
99004             FORMAT (/5X,'(SEE MESSAGES IN USER MANUAL SECTIONS 6.4 AND 6.5,',                                                 &
                         &' AND IGNORE ANY COMPUTER FATAL MESSAGE HEREAFTER ','OR IN THE LOG FILE)')
!
!     FORCE A SYSTEM FATAL ERROR FOR TRACEBACK
!
                  CALL errtrc('MESAGE  ',105)
               ENDIF
            ENDIF
!
!     ALL NON-VAX MACHINES
!
         ENDIF
      ENDIF
!
 50   CALL msgwrt
      CALL pexit
   ENDIF
END SUBROUTINE mesage
