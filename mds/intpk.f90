
SUBROUTINE intpk(File,Block,Itypot,Iflag) !HIDESTARS (*,File,Block,Itypot,Iflag)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
   REAL A(4)
   INTEGER Iendrc , Ieol , Irow
   COMMON /zntpkx/ A , Irow , Ieol , Iendrc
   INTEGER File , Iflag , Itypot
   INTEGER Block(15)
   Name = File
   IF ( Iflag==0 ) THEN
      Ieol = 0
      Iendrc = 0
      CALL dsipk1(Iblkb,Itypot)
   ELSE
      CALL dsipk1(Block,Itypot)
   ENDIF
   IF ( Iretrn/=0 ) RETURN 1
END SUBROUTINE intpk