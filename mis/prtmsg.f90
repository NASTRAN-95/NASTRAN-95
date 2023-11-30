
SUBROUTINE prtmsg
   IMPLICIT NONE
   REAL Buf(1) , Title(32,6)
   COMMON /output/ Title
   COMMON /zzzzzz/ Buf
   REAL blank
   INTEGER i , inprew , j , msg
!
!
   DATA inprew , msg , blank/0 , 101 , 4H    /
!
   CALL open(*99999,msg,Buf,inprew)
   CALL read(*99999,*99999,msg,0,0,1,j)
   DO j = 4 , 6
      DO i = 1 , 32
         Title(i,j) = blank
      ENDDO
   ENDDO
   CALL wrtmsg(msg)
99999 RETURN
END SUBROUTINE prtmsg
