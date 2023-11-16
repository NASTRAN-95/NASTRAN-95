
SUBROUTINE nasopn(*,Lu,Dsn)
   IMPLICIT NONE
   INCLUDE 'NASNAMES.COM'
!
! Dummy argument declarations
!
   CHARACTER*80 Dsn
   INTEGER Lu
!
! Local variable declarations
!
   LOGICAL iexist
   CHARACTER*80 ifile
   INTEGER klen
!
! End of declarations
!
   klen = index(Rfdir,' ')
   ifile = Rfdir(1:klen-1)//'/NASINFO'
   Dsn = ifile
   INQUIRE (FILE=ifile,EXIST=iexist)
   IF ( iexist ) THEN
      OPEN (UNIT=Lu,FILE=ifile,STATUS='OLD',ERR=100)
      RETURN
   ENDIF
 100  RETURN 1
END SUBROUTINE nasopn