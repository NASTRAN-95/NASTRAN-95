!*==dsopff.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsopff(Dsname,Iunit,Istatus)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_MACHIN
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(80) :: Dsname
   INTEGER :: Iunit
   INTEGER :: Istatus
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iccerr , nbuff4
!
! End of declarations rewritten by SPAG
!
   nbuff4 = nbuff*(mod(Lqro,100)/10)
   OPEN (UNIT=Iunit,FILE=Dsname,RECL=nbuff4,FORM='UNFORMATTED',ACCESS='DIRECT',IOSTAT=Istatus,ERR=100,STATUS='UNKNOWN')
   GOTO 99999
 100  WRITE (Iwr,99001) Iunit , Istatus , Dsname
99001 FORMAT (//,' FATAL ERROR IN DSOPFF, UNABLE TO OPEN UNIT=',I4,' IOSTAT=',I5,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE dsopff
