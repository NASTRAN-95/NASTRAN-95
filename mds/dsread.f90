
SUBROUTINE dsread(Iunit,Buff,Len,Irec)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! COMMON variable declarations
!
   INTEGER Iwr
   REAL Sysbuf
   COMMON /system/ Sysbuf , Iwr
!
! Dummy argument declarations
!
   INTEGER Irec , Iunit , Len
   INTEGER Buff(Len)
!
! Local variable declarations
!
   INTEGER iccerr , istat
!
! End of declarations
!
   IF ( Irec>=0 ) THEN
!      PRINT *,' DSREAD,LEN,IREC,IUNIT=',LEN,IREC,IUNIT
      istat = 0
      READ (Iunit,REC=Irec,ERR=100,IOSTAT=istat) Buff
      IF ( istat==0 ) THEN
         Numrea = Numrea + 1
         GOTO 99999
      ELSE
         Ioerr = istat
         CALL dsmsg(101)
         CALL mesage(-61,0,0)
      ENDIF
   ENDIF
   WRITE (Iwr,99001) Iunit , Irec , Mdsnam(Iunit)
99001 FORMAT (//' ERROR IN DSREAD-BAD REC NO., UNIT=',I4,' REC=',I4,/,' FILE NAME=',A72)
   iccerr = 0
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
   Numrea = Numrea + 1
   GOTO 99999
 100  WRITE (Iwr,99002) Iunit , Irec , istat , Mdsnam(Iunit)
99002 FORMAT (//', ERROR ENCOUNTERED IN DSREAD, UNIT=',I5,' RECORD=',I5,' STATUS=',I9,/' DSNAME=',A72)
   iccerr = istat
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
   Numrea = Numrea + 1
99999 END SUBROUTINE dsread
