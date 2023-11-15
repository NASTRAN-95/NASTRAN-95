
SUBROUTINE clstab(File,Opt)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER File , Opt
!
! Local variable declarations
!
   INTEGER trlr(7)
!
! End of declarations
!
   DATA trlr/6*0 , 1/
!
   trlr(1) = File
   CALL close(trlr,Opt)
   CALL wrttrl(trlr)
END SUBROUTINE clstab
