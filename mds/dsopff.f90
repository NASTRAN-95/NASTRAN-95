
SUBROUTINE dsopff(Dsname,Iunit,Istatus)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER Iwr , Lqro , Mac(3)
   REAL Sysbuf
   COMMON /machin/ Mac , Lqro
   COMMON /system/ Sysbuf , Iwr
   CHARACTER*80 Dsname
   INTEGER Istatus , Iunit
   INTEGER iccerr , nbuff4
   nbuff4 = Nbuff*(mod(Lqro,100)/10)
   OPEN (UNIT=Iunit,FILE=Dsname,RECL=nbuff4,FORM='UNFORMATTED',ACCESS='DIRECT',IOSTAT=Istatus,ERR=100,STATUS='UNKNOWN')
   GOTO 99999
 100  WRITE (Iwr,99001) Iunit , Istatus , Dsname
99001 FORMAT (//,' FATAL ERROR IN DSOPFF, UNABLE TO OPEN UNIT=',I4,' IOSTAT=',I5,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
99999 RETURN
END SUBROUTINE dsopff