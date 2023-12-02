!*==dshxdd.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dshxdd(Ii,Iarr,Len)
   USE c_dsbuff
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii
   INTEGER , DIMENSION(10000) :: Iarr
   INTEGER :: Len
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , k
!
! End of declarations rewritten by SPAG
!
   DO k = 1 , Len
      iibuff(k) = Iarr(k)
   ENDDO
   WRITE (6,99001) Ii , (iibuff(i),i=1,Len)
99001 FORMAT (I5,200(8(1X,Z8),/))
END SUBROUTINE dshxdd
