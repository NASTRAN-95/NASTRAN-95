!*==dsocff.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsocff(Dsname,Iunit,Istatus)
   USE i_dsiof
   USE c_machin
   USE c_system
   IMPLICIT NONE
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
   nbuff4 = nbuff*(mod(lqro,100)/10)
   OPEN (UNIT=Iunit,FILE=Dsname,RECL=nbuff4,STATUS='NEW',ACCESS='direct',FORM='unformatted',IOSTAT=Istatus,ERR=300)
   RETURN
 200  WRITE (iwr,99001) Iunit , Istatus , Dsname
99001 FORMAT (//,' FATAL ERROR IN DSOCFF, UNABLE TO CLOSE UNIT=',I4,' STATUS='I4,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
 300  WRITE (iwr,99002) Iunit , Istatus , Dsname
99002 FORMAT (//,' FATAL ERROR IN DSOCFF, UNABLE TO OPEN UNIT=',I4,' STATUS=',I4,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
END SUBROUTINE dsocff
