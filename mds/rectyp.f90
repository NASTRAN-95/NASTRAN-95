
SUBROUTINE rectyp(File,Itype)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER File , Itype
!
! Local variable declarations
!
   INTEGER id
!
! End of declarations
!
   Name = File
   CALL dsgefl
   DO
      id = iand(Ibase(Indclr),Maskq1)
      IF ( id==Idssb ) THEN
         Itype = 1
         EXIT
      ELSEIF ( id==Idseb ) THEN
         CALL dsrdnb
         CALL dssdcb
      ELSE
         Itype = 0
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE rectyp
