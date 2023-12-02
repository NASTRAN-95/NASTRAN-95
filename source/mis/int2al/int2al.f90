!*==int2al.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE int2al(Int,Alf,Ch)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Int
   INTEGER , DIMENSION(2) :: Alf
   INTEGER , DIMENSION(9) :: Ch
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , zero
   INTEGER :: j
   CHARACTER(8) :: k8
   EXTERNAL int2k8
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!     ----------
!     THIS ROUTINE CONVERTS AN INTEGER TO ALPHA-NUMERIC WORD. THE
!     NUMBER IS LEFT JUSTIFIED WITH NO BLANKS.
!
!     INPUT/OUTPUT
!
!     INT - INTEGER - INPUT - NOT CHANGED
!     ALF - BCD 2 WORDS - OUTPUT - 2A4 MAY BE USED FOR PRINTING
!     CH  - BCD 9 WORDS - OUTPUT - CH(1) .EQ. NUMBER OF CHARACTERS
!           NEEDED TO CREATE INT. MAY BE PRINTED BY CH(I), I=2,CH(1)
!           IN A1 FORMAT.
!
!     NOTE - ANY INPUT NUMBER OUTSIDE THE RANGE OF -9999999 AND +9999999
!            (I.E. MORE THAN 8 DIGITS) IS SET TO ZERO IN OUTPUT.
!     ----------
!
   DATA blank , zero/1H  , 1H0/
!
   IF ( Int>=-9999999 .AND. Int<=+99999999 ) THEN
      CALL int2k8(*100,Int,k8)
      READ (k8,99001) Alf
99001 FORMAT (2A4)
      READ (k8,99002) (Ch(j),j=2,9)
99002 FORMAT (8A1)
      DO j = 2 , 9
         IF ( Ch(j)==blank ) GOTO 50
      ENDDO
      j = 10
 50   Ch(1) = j - 2
      RETURN
   ENDIF
!
 100  Ch(1) = 1
   Ch(2) = zero
   Alf(1) = zero
   Alf(2) = blank
END SUBROUTINE int2al
