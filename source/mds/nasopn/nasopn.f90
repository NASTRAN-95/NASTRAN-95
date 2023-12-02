!*==nasopn.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE nasopn(Lu,Dsn) !HIDESTARS (*,Lu,Dsn)
   USE i_nasnames
   USE I_NASNAMES
   IMPLICIT NONE
   INCLUDE 'NASNAMES.COM'
   CHARACTER*80 Dsn
   INTEGER Lu
   LOGICAL iexist
   CHARACTER*80 ifile
   INTEGER klen
   klen = index(rfdir,' ')
   ifile = rfdir(1:klen-1)//'/NASINFO'
   Dsn = ifile
   INQUIRE (FILE=ifile,EXIST=iexist)
   IF ( iexist ) THEN
      OPEN (UNIT=Lu,FILE=ifile,STATUS='OLD',ERR=100)
      RETURN
   ENDIF
 100  RETURN 1
END SUBROUTINE nasopn
