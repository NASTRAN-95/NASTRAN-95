!*==dsinqr.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsinqr(Dsn,Istat,Isize)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(*) :: Dsn
   INTEGER :: Istat
   INTEGER :: Isize
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: avail
   INTEGER :: nrec
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
