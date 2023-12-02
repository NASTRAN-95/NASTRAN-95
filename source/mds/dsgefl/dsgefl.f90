!*==dsgefl.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsgefl
   IMPLICIT NONE
   USE I_DSIOF
   USE I_XNSTRN
   USE C_DSUNIT
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iobuf
!
! End of declarations rewritten by SPAG
!
   IF ( name>=101 .AND. name<=320 ) THEN
      ifilex = Iunit(name-100)
   ELSE
      CALL geturn(name)
   ENDIF
   IF ( ifilex==0 ) THEN
      IF ( iretrn==77 ) GOTO 99999
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
99999 END SUBROUTINE dsgefl
