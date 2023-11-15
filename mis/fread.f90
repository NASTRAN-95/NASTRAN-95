
SUBROUTINE fread(File,Block,N,Eor)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Eor , File , N
   REAL Block(1)
!
! Local variable declarations
!
   INTEGER k
   REAL subnam(2)
!
! End of declarations
!
!
   DATA subnam/4H FRE , 4HAD  /
!
   CALL read(*100,*200,File,Block,N,Eor,k)
   RETURN
 100  k = -2
   GOTO 300
 200  k = -3
 300  DO
      CALL mesage(k,File,subnam)
   ENDDO
END SUBROUTINE fread
