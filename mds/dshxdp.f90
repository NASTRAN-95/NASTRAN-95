
SUBROUTINE dshxdp(Iarr,Len)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Len
   INTEGER Iarr(10000)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
   WRITE (6,99001) (Iarr(i),i=1,Len)
99001 FORMAT (200(8(1X,Z8),/))
END SUBROUTINE dshxdp
