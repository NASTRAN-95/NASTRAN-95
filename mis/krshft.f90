
FUNCTION krshft(Iword,N)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dummy(38)
   INTEGER Lqro , Mac(3) , Nbpc
   COMMON /machin/ Mac , Lqro
   COMMON /system/ Dummy , Nbpc
!
! Dummy argument declarations
!
   INTEGER N
   INTEGER Iword(1)
   INTEGER krshft
!
! Local variable declarations
!
   INTEGER klshft
   INTEGER lshift , rshift
   EXTERNAL lshift , rshift
!
! End of declarations
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
99999 RETURN
END FUNCTION krshft
