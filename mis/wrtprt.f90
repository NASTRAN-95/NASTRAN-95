
SUBROUTINE wrtprt(File,List,Format,N)
   IMPLICIT NONE
   INTEGER File , N
   INTEGER Format(N) , List(1)
!
!
   CALL write(File,List,List(1)+1,0)
   CALL write(File,N,1,0)
   CALL write(File,Format,N,0)
END SUBROUTINE wrtprt