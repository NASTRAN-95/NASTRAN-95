
SUBROUTINE dsgefl
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! COMMON variable declarations
!
   INTEGER*2 Iunit(220)
   COMMON /dsunit/ Iunit
!
! Local variable declarations
!
   INTEGER iobuf
!
! End of declarations
!
   IF ( Name>=101 .AND. Name<=320 ) THEN
      Ifilex = Iunit(Name-100)
   ELSE
      CALL geturn(Name)
   ENDIF
   IF ( Ifilex==0 ) THEN
      IF ( Iretrn==77 ) GOTO 99999
      CALL dsmsg(107)
   ENDIF
   iobuf = Fcb(2,Ifilex)
   IF ( iobuf==0 ) THEN
      Ifilex = 0
   ELSE
      Iprvop = Fcb(1,Ifilex)
      IF ( Iprvop==2 ) Iprvop = 0
      Indbas = iobuf
      Indcbp = Indbas + Ibase(Indbas+1) - 1
      Indclr = Indbas + Ibase(Indbas+2) - 1
      Nblock = Fcb(4,Ifilex)
      Lcw = Ibase(Indbas+4)
      Lasnam = Name
   ENDIF
99999 END SUBROUTINE dsgefl
