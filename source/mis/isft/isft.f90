!*==isft.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION isft(Bf,Sft,J)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: isft
   INTEGER :: Bf
   INTEGER :: Sft
   INTEGER :: J
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!
   IF ( J==4 ) THEN
      isft = lshift(Bf,Sft)
      RETURN
   ENDIF
   isft = rshift(Bf,Sft)
END FUNCTION isft
