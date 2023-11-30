
SUBROUTINE rewind(File)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File
   Name = File
   CALL dsgefl
! CALL DBMMGR FOR REWIND SO TO SET BUFFER ADDRESS CORRECTLY
   CALL dbmmgr(3)
   Nblock = Fcb(4,Ifilex)
   IF ( Iprvop/=0 ) THEN
! IF FILE OPEN FOR WRITE, THEN INITIAL BUFFER AND BLOCK NUMBER
      Ibase(Indbas+3) = 1
      Ibase(Indbas+4) = 6
!WKBNB NCL93007 11/94
! SET THE COUNTER FOR NUMBER OF STRINGS AND TERMS TO ZERO
      Fcb(16,Ifilex) = 0
      Fcb(17,Ifilex) = 0
   ENDIF
!WKBNE NCL93007 11/94
   Indclr = Indbas + 5
   Indcbp = Indclr
   CALL dssdcb
END SUBROUTINE rewind
