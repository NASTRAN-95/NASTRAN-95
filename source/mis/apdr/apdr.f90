!*==apdr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdr(File,Z,Core,In,Out,Wr,Buf,Type)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER , DIMENSION(1) :: Z
   INTEGER :: Core
   INTEGER :: In
   INTEGER :: Out
   INTEGER :: Wr
   INTEGER :: Buf
   INTEGER , DIMENSION(3) :: Type
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: flag
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL locate , mesage , read
!
! End of declarations rewritten by SPAG
!
!
   DATA name/4HAPD  , 4HR   /
!
   Wr = 0
   In = 0
   CALL locate(*200,Z(Buf),Type,flag)
   In = Out + 1
   CALL read(*300,*100,File,Z(In),Core,0,Wr)
!
   CALL mesage(-3,File,name)
   GOTO 300
 100  Out = In + Wr - 1
 200  Core = Core - Wr
   RETURN
 300  CALL mesage(-2,File,name)
   GOTO 200
END SUBROUTINE apdr
