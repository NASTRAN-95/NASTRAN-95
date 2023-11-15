
SUBROUTINE intpk(*,File,Block,Itypot,Iflag)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'PAKBLK.COM'
!
! COMMON variable declarations
!
   REAL A(4)
   INTEGER Iendrc , Ieol , Irow
   COMMON /zntpkx/ A , Irow , Ieol , Iendrc
!
! Dummy argument declarations
!
   INTEGER File , Iflag , Itypot
   INTEGER Block(15)
!
! End of declarations
!
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
