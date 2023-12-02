!*==dsread.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsread(Iunit,Buff,Len,Irec)
   USE i_dsiof
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Len
   INTEGER :: Iunit
   INTEGER , DIMENSION(Len) :: Buff
   INTEGER :: Irec
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iccerr , istat
!
! End of declarations rewritten by SPAG
!
   IF ( Irec>=0 ) THEN
!      PRINT *,' DSREAD,LEN,IREC,IUNIT=',LEN,IREC,IUNIT
      istat = 0
      READ (Iunit,REC=Irec,ERR=100,IOSTAT=istat) Buff
      IF ( istat==0 ) THEN
         numrea = numrea + 1
         RETURN
      ELSE
         ioerr = istat
         CALL dsmsg(101)
         CALL mesage(-61,0,0)
      ENDIF
   ENDIF
   WRITE (iwr,99001) Iunit , Irec , mdsnam(Iunit)
99001 FORMAT (//' ERROR IN DSREAD-BAD REC NO., UNIT=',I4,' REC=',I4,/,' FILE NAME=',A72)
   iccerr = 0
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
   numrea = numrea + 1
   RETURN
 100  WRITE (iwr,99002) Iunit , Irec , istat , mdsnam(Iunit)
99002 FORMAT (//', ERROR ENCOUNTERED IN DSREAD, UNIT=',I5,' RECORD=',I5,' STATUS=',I9,/' DSNAME=',A72)
   iccerr = istat
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
   numrea = numrea + 1
END SUBROUTINE dsread
