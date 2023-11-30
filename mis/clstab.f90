
SUBROUTINE clstab(File,Opt)
   IMPLICIT NONE
   INTEGER File , Opt
   INTEGER trlr(7)
   DATA trlr/6*0 , 1/
!
   trlr(1) = File
   CALL close(trlr,Opt)
   CALL wrttrl(trlr)
END SUBROUTINE clstab