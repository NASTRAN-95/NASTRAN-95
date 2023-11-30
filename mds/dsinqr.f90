
SUBROUTINE dsinqr(Dsn,Istat,Isize)
   IMPLICIT NONE
   CHARACTER*(*) Dsn
   INTEGER Isize , Istat
   LOGICAL avail
   INTEGER nrec
!        DSINQR DETERMINES THE EXISTANCE OF A FILE:
!            DSN   ( INPUT  )   FILE NAME
!            ISTAT ( OUTPUT )   =0, IF NOT EXIST; =1, IF EXIST
!            ISIZE ( OUTPUT )   = FILE SIZE IN GINO BLOCKS
!
   INQUIRE (FILE=Dsn,EXIST=avail,NEXTREC=nrec)
   Istat = 0
   IF ( avail ) Istat = 1
   Isize = nrec - 1
END SUBROUTINE dsinqr
