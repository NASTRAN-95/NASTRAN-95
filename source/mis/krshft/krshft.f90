!*==krshft.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
FUNCTION krshft(Iword,N)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SYSTEM
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
   IF ( mod(Lqro,10)==1 ) THEN
      krshft = lshift(Iword(1),N*Nbpc)
      RETURN
   ELSE
      krshft = rshift(Iword(1),N*Nbpc)
      RETURN
   ENDIF
!
   ENTRY klshft(Iword,N)
!     ======================
!
   IF ( mod(Lqro,10)==1 ) THEN
      klshft = rshift(Iword(1),N*Nbpc)
      GOTO 99999
   ENDIF
   klshft = lshift(Iword(1),N*Nbpc)
   RETURN
99999 END FUNCTION krshft
