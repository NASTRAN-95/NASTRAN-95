!*==dsgefl.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsgefl
   USE i_dsiof
   USE i_xnstrn
   USE c_dsunit
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iobuf
!
! End of declarations rewritten by SPAG
!
   IF ( name>=101 .AND. name<=320 ) THEN
      ifilex = iunit(name-100)
   ELSE
      CALL geturn(name)
   ENDIF
   IF ( ifilex==0 ) THEN
      IF ( iretrn==77 ) RETURN
      CALL dsmsg(107)
   ENDIF
   iobuf = fcb(2,ifilex)
   IF ( iobuf==0 ) THEN
      ifilex = 0
   ELSE
      iprvop = fcb(1,ifilex)
      IF ( iprvop==2 ) iprvop = 0
      indbas = iobuf
      indcbp = indbas + ibase(indbas+1) - 1
      indclr = indbas + ibase(indbas+2) - 1
      nblock = fcb(4,ifilex)
      lcw = ibase(indbas+4)
      lasnam = name
   ENDIF
END SUBROUTINE dsgefl
