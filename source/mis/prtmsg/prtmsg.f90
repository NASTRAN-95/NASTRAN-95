!*==prtmsg.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prtmsg
   USE c_output
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: blank
   INTEGER :: i , j
   INTEGER , SAVE :: inprew , msg
   EXTERNAL open , read , wrtmsg
!
! End of declarations rewritten by SPAG
!
!
!
   DATA inprew , msg , blank/0 , 101 , 4H    /
!
   CALL open(*99999,msg,buf,inprew)
   CALL read(*99999,*99999,msg,0,0,1,j)
   DO j = 4 , 6
      DO i = 1 , 32
         title(i,j) = blank
      ENDDO
   ENDDO
   CALL wrtmsg(msg)
99999 END SUBROUTINE prtmsg
