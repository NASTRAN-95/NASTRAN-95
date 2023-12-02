!*==apdoe.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdoe(Id,Z,Start,End,Found,Count)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Id
   INTEGER , DIMENSION(1) :: Z
   INTEGER :: Start
   INTEGER :: End
   INTEGER :: Found
   INTEGER :: Count
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j
   LOGICAL :: look
!
! End of declarations rewritten by SPAG
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
            IF ( Z(i)==Id ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            look = .FALSE.
         ELSE
            IF ( Z(i)==-1 ) look = .TRUE.
         ENDIF
      ENDDO
   ENDIF
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Found = i
      j = i + 2
      Count = Count + 1
!
!     START COUNT AT + 2 BECAUSE PAERO4 CARD CAN HAVE -1 IN FIELD 2
!
      SPAG_Loop_1_1: DO i = j , End
         IF ( Z(i)==-1 ) EXIT SPAG_Loop_1_1
         Count = Count + 1
      ENDDO SPAG_Loop_1_1
   END SUBROUTINE spag_block_1
END SUBROUTINE apdoe
