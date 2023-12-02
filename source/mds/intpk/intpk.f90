!*==intpk.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE intpk(File,Block,Itypot,Iflag) !HIDESTARS (*,File,Block,Itypot,Iflag)
   IMPLICIT NONE
   USE I_DSIOF
   USE I_PAKBLK
   USE C_ZNTPKX
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
      Ieol = 0
      Iendrc = 0
      CALL dsipk1(iblkb,Itypot)
   ELSE
      CALL dsipk1(Block,Itypot)
   ENDIF
   IF ( iretrn/=0 ) RETURN 1
END SUBROUTINE intpk
