
SUBROUTINE dsinqr(Dsn,Istat,Isize)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   CHARACTER*(*) Dsn
   INTEGER Isize , Istat
!
! Local variable declarations
!
   LOGICAL avail
   INTEGER nrec
!
! End of declarations
!
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
