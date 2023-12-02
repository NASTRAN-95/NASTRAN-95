!*==dshxdd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dshxdd(Ii,Iarr,Len)
   IMPLICIT NONE
   USE C_DSBUFF
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
      Iibuff(k) = Iarr(k)
   ENDDO
   WRITE (6,99001) Ii , (Iibuff(i),i=1,Len)
99001 FORMAT (I5,200(8(1X,Z8),/))
END SUBROUTINE dshxdd
