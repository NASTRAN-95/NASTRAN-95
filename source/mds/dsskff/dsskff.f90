!*==dsskff.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsskff(Nn)
   USE i_dsiof
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Nn
   INTEGER n
   n = Nn
   DO WHILE ( n/=0 )
      SPAG_Loop_2_1: DO
         CALL dsfwr1
         IF ( iretrn/=0 ) THEN
            n = n - 1
            EXIT SPAG_Loop_2_1
         ENDIF
      ENDDO SPAG_Loop_2_1
   ENDDO
END SUBROUTINE dsskff
