!*==dsopff.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsopff(Dsname,Iunit,Istatus)
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
   nbuff4 = nbuff*(mod(lqro,100)/10)
   OPEN (UNIT=Iunit,FILE=Dsname,RECL=nbuff4,FORM='UNFORMATTED',ACCESS='DIRECT',IOSTAT=Istatus,ERR=100,STATUS='UNKNOWN')
   RETURN
 100  WRITE (iwr,99001) Iunit , Istatus , Dsname
99001 FORMAT (//,' FATAL ERROR IN DSOPFF, UNABLE TO OPEN UNIT=',I4,' IOSTAT=',I5,/,' FILE NAME=',A80)
   iccerr = Istatus
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
END SUBROUTINE dsopff
