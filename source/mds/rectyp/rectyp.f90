!*==rectyp.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rectyp(File,Itype)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Itype
   INTEGER id
   name = File
   CALL dsgefl
   SPAG_Loop_1_1: DO
      id = iand(ibase(indclr),maskq1)
      IF ( id==idssb ) THEN
         Itype = 1
         EXIT SPAG_Loop_1_1
      ELSEIF ( id==idseb ) THEN
         CALL dsrdnb
         CALL dssdcb
      ELSE
         Itype = 0
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE rectyp
