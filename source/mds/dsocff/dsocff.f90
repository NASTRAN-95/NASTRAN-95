!*==dsocff.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsocff(Dsname,Iunit,Istatus)
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
!  OPEN AND CLOSE FILE IN ORDER TO DELETE SPACE
   OPEN (UNIT=Iunit,FILE=Dsname,IOSTAT=Istatus,ERR=100,STATUS='UNKNOWN')
 100  CLOSE (UNIT=Iunit,STATUS='DELETE',IOSTAT=Istatus,ERR=200)
! NOW, OPEN FILE AS NEW FOR NASTRAN
!      print *,' dsocff,nbuff=',nbuff
   nbuff4 = nbuff*(mod(Lqro,100)/10)
   OPEN (UNIT=Iunit,FILE=Dsname,RECL=nbuff4,STATUS='NEW',ACCESS='direct',FORM='unformatted',IOSTAT=Istatus,ERR=300)
   GOTO 99999
 200  WRITE (Iwr,99001) Iunit , Istatus , Dsname
99001 FORMAT (//,' FATAL ERROR IN DSOCFF, UNABLE TO CLOSE UNIT=',I4,' STATUS='I4,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
 300  WRITE (Iwr,99002) Iunit , Istatus , Dsname
99002 FORMAT (//,' FATAL ERROR IN DSOCFF, UNABLE TO OPEN UNIT=',I4,' STATUS=',I4,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE dsocff
