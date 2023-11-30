
SUBROUTINE nasopn(Lu,Dsn) !HIDESTARS (*,Lu,Dsn)
   IMPLICIT NONE
   INCLUDE 'NASNAMES.COM'
   CHARACTER*80 Dsn
   INTEGER Lu
   LOGICAL iexist
   CHARACTER*80 ifile
   INTEGER klen
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