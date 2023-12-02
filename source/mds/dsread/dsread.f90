!*==dsread.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsread(Iunit,Buff,Len,Irec)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
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
         GOTO 99999
      ELSE
         ioerr = istat
         CALL dsmsg(101)
         CALL mesage(-61,0,0)
      ENDIF
   ENDIF
   WRITE (Iwr,99001) Iunit , Irec , mdsnam(Iunit)
99001 FORMAT (//' ERROR IN DSREAD-BAD REC NO., UNIT=',I4,' REC=',I4,/,' FILE NAME=',A72)
   iccerr = 0
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
   numrea = numrea + 1
   GOTO 99999
 100  WRITE (Iwr,99002) Iunit , Irec , istat , mdsnam(Iunit)
99002 FORMAT (//', ERROR ENCOUNTERED IN DSREAD, UNIT=',I5,' RECORD=',I5,' STATUS=',I9,/' DSNAME=',A72)
   iccerr = istat
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
   numrea = numrea + 1
99999 END SUBROUTINE dsread
