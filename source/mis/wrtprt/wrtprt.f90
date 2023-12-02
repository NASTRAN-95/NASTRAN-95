!*==wrtprt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wrtprt(File,List,Format,N)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   INTEGER :: File
   INTEGER , DIMENSION(1) :: List
   INTEGER , DIMENSION(N) :: Format
   EXTERNAL write
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!
   CALL write(File,List,List(1)+1,0)
   CALL write(File,N,1,0)
   CALL write(File,Format,N,0)
END SUBROUTINE wrtprt
