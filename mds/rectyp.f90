
SUBROUTINE rectyp(File,Itype)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , Itype
   INTEGER id
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
