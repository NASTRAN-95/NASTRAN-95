!*==bdat05.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat05
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
   ifile = scr2
   CALL open(*200,scr2,z(buf3),1)
   ifile = geom4
   CALL locate(*100,z(buf1),gnew,flag)
   WRITE (outt,99001) ufm
99001 FORMAT (A23,' 6532, THE GNEW OPTION IS NOT CURRENTLY AVAILABLE.')
   idry = -2
   RETURN
!
 100  CALL eof(scbdat)
   CALL close(scr2,1)
   RETURN
!
 200  imsg = -1
   CALL mesage(imsg,ifile,aaa)
END SUBROUTINE bdat05
