!*==decode.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE decode(Code,List,N)
   IMPLICIT NONE
   USE C_TWO
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Code
   INTEGER , DIMENSION(1) :: List
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   EXTERNAL andf
!
! End of declarations rewritten by SPAG
!
!
!     DECODE DECODES THE BITS IN A WORD AND RETURNS A LIST OF INTEGERS
!     CORRESPONDING TO THE BIT POSITIONS WHICH ARE ON. NUMBERING
!     CONVENTION IS RIGHT (LOW ORDER) TO LEFT (HIGH ORDER) 00 THRU 31.
!
!     ARGUMENTS
!
!     CODE - INPUT  - THE WORD TO BE DECODED
!     LIST - OUTPUT - AN ARRAY OF DIMENSION .GE. 32 WHERE THE INTEGERS
!                     CORRESPONDING TO BIT POSITIONS ARE STORED
!     N    - OUTPUT - THE NUMBER OF ENTRIES IN THE LIST  I.E. THE NO.
!                     OF 1-BITS IN THE WORD
!
!
!
   N = 0
   DO i = 1 , 32
      IF ( andf(Two(33-i),Code)/=0 ) THEN
         N = N + 1
         List(N) = i - 1
      ENDIF
   ENDDO
!
END SUBROUTINE decode
