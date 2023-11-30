
SUBROUTINE apdr(File,Z,Core,In,Out,Wr,Buf,Type)
   IMPLICIT NONE
   INTEGER Buf , Core , File , In , Out , Wr
   INTEGER Type(3) , Z(1)
   INTEGER flag , name(2)
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