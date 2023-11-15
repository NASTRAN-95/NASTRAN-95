
SUBROUTINE apdoe(Id,Z,Start,End,Found,Count)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Count , End , Found , Id , Start
   INTEGER Z(1)
!
! Local variable declarations
!
   INTEGER i , j
   LOGICAL look
!
! End of declarations
!
!
!     APDOE FINDS AND OPEN ENDED CARD FOR ID
!     GIVEN A LIST Z(START ) TO Z(END)
!     FOUND = 0 IF NOT FOUND
!     FOUND = POINTER TO START OF CARD Z(FOUND)
!     COUNT = NUMBER OF DATA ITEMS NOT COUNTING THE ID
!
   Found = 0
   look = .TRUE.
   Count = 0
   IF ( Start/=0 ) THEN
      DO i = Start , End
         IF ( look ) THEN
            IF ( Z(i)==Id ) GOTO 100
            look = .FALSE.
         ELSE
            IF ( Z(i)==-1 ) look = .TRUE.
         ENDIF
      ENDDO
   ENDIF
   GOTO 99999
 100  Found = i
   j = i + 2
   Count = Count + 1
!
!     START COUNT AT + 2 BECAUSE PAERO4 CARD CAN HAVE -1 IN FIELD 2
!
   DO i = j , End
      IF ( Z(i)==-1 ) EXIT
      Count = Count + 1
   ENDDO
99999 END SUBROUTINE apdoe
