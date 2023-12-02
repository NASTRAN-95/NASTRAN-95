!*==dsfwr1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsfwr1
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER id
   CALL dsskrc
   id = iand(ibase(indclr-1),maskq1)
   IF ( id==idsef ) THEN
      iretrn = 1
   ELSE
      iretrn = 0
   ENDIF
END SUBROUTINE dsfwr1
