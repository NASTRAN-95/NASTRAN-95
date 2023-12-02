!*==krshft.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION krshft(Iword,N)
   USE c_machin
   USE c_system
   IMPLICIT NONE
   INTEGER :: krshft
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Iword
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: klshft
!
! End of declarations rewritten by SPAG
!
!
!     CHARACTER FUNCTION KRSHFT AND KLSHFT PERFORM LEFT AND RIGHT
!     SHIFTS, BY N CHARACTERS (BYTES).
!     EMPTY BYTES ARE ZERO FILLED.
!
!     NORMALLY, KRSHFT AND KLSHFT WORK ALMOST LIKE RSHIFT AND LSHFIT
!     RESPECTIVELY, EXCEPT THEY MOVE DATA BY BYTE COUNT, NOT BY BITS.
!     HOWEVER, IF THE MACHINE STORES THE BCD WORD DATA IN REVERSE ORDER
!     (SUCH AS VAX AND SILICON GRAPHICS), KRSHFT IS EQUIVALENCED TO
!     LSHFIT, AND KLSFHT TO RSHIFT.
!
!
   IF ( mod(lqro,10)==1 ) THEN
      krshft = lshift(Iword(1),N*nbpc)
      RETURN
   ELSE
      krshft = rshift(Iword(1),N*nbpc)
      RETURN
   ENDIF
!
   ENTRY klshft(Iword,N)
!     ======================
!
   IF ( mod(lqro,10)==1 ) THEN
      klshft = rshift(Iword(1),N*nbpc)
      RETURN
   ENDIF
   klshft = lshift(Iword(1),N*nbpc)
END FUNCTION krshft
