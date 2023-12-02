!*==rewind.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rewind(File)
   USE i_dsiof
   USE i_xnstrn
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File
   name = File
   CALL dsgefl
! CALL DBMMGR FOR REWIND SO TO SET BUFFER ADDRESS CORRECTLY
   CALL dbmmgr(3)
   nblock = fcb(4,ifilex)
   IF ( iprvop/=0 ) THEN
! IF FILE OPEN FOR WRITE, THEN INITIAL BUFFER AND BLOCK NUMBER
      ibase(indbas+3) = 1
      ibase(indbas+4) = 6
!WKBNB NCL93007 11/94
! SET THE COUNTER FOR NUMBER OF STRINGS AND TERMS TO ZERO
      fcb(16,ifilex) = 0
      fcb(17,ifilex) = 0
   ENDIF
!WKBNE NCL93007 11/94
   indclr = indbas + 5
   indcbp = indclr
   CALL dssdcb
END SUBROUTINE rewind
