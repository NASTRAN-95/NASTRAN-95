!*==intpk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE intpk(File,Block,Itypot,Iflag) !HIDESTARS (*,File,Block,Itypot,Iflag)
   USE i_dsiof
   USE i_pakblk
   USE c_zntpkx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER , DIMENSION(15) :: Block
   INTEGER :: Itypot
   INTEGER :: Iflag
!
! End of declarations rewritten by SPAG
!
   name = File
   IF ( Iflag==0 ) THEN
      ieol = 0
      iendrc = 0
      CALL dsipk1(iblkb,Itypot)
   ELSE
      CALL dsipk1(Block,Itypot)
   ENDIF
   IF ( iretrn/=0 ) RETURN 1
END SUBROUTINE intpk
