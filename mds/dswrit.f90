
SUBROUTINE dswrit(Iunit,Buff,Len,Irec,Iccerr)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Iwr
   REAL Sysbuf
   COMMON /system/ Sysbuf , Iwr
   INTEGER Iccerr , Irec , Iunit , Len
   INTEGER Buff(Len)
   INTEGER istat
!      print *,' dswrit,len,IREC,UNIT=',len,irec,iunit
   IF ( Irec<=0 ) THEN
      WRITE (Iwr,99001) Iunit , Irec , Mdsnam(Iunit)
99001 FORMAT (//' ERROR IN DSWRIT, BAD RECORD NO., UNIT=',I4,' REC=',I5,/,' FILE NAME=',A80)
      Iccerr = istat
      CALL dsmsg(101)
      CALL mesage(-61,0,0)
   ELSE
      WRITE (Iunit,REC=Irec,IOSTAT=istat,ERR=100) Buff
      Iccerr = 0
      GOTO 200
   ENDIF
 100  WRITE (Iwr,99002) Iunit , Irec , istat , Mdsnam(Iunit)
99002 FORMAT (//', ERROR ENCOUNTERED IN DSWRCC, UNIT=',I5,' RECORD=',I5,' STATUS=',I9,/' DSNAME=',A80)
   Iccerr = istat
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
 200  Numwri = Numwri + 1
END SUBROUTINE dswrit
