!*==bdat05.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat05
   IMPLICIT NONE
   USE C_BLANK
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa , gnew
   INTEGER :: flag , ifile , imsg
   EXTERNAL close , eof , locate , mesage , open
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE PROCESSES THE GNEW BULK DATA
!
   DATA gnew/1410 , 14/ , aaa/4HBDAT , 4H05  /
!
   ifile = Scr2
   CALL open(*200,Scr2,Z(Buf3),1)
   ifile = Geom4
   CALL locate(*100,Z(Buf1),gnew,flag)
   WRITE (Outt,99001) Ufm
99001 FORMAT (A23,' 6532, THE GNEW OPTION IS NOT CURRENTLY AVAILABLE.')
   Idry = -2
   RETURN
!
 100  CALL eof(Scbdat)
   CALL close(Scr2,1)
   RETURN
!
 200  imsg = -1
   CALL mesage(imsg,ifile,aaa)
END SUBROUTINE bdat05
