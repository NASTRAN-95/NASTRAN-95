!*==permut.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE permut(Ia,Id,N,Isw)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ia
   INTEGER , DIMENSION(10) :: Id
   INTEGER :: N
   INTEGER :: Isw
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , i1 , is1 , j , k , l , n1
   INTEGER , DIMENSION(32) :: ib , ic
!
! End of declarations rewritten by SPAG
!
   DO i = 1 , N
      ic(i) = Ia(i)
      ib(i) = i
   ENDDO
   n1 = N - 1
   DO i = 1 , n1
      i1 = i + 1
      DO j = i1 , N
         IF ( ic(j)<ic(i) ) THEN
            is1 = ib(j)
            ib(j) = ib(i)
            ib(i) = is1
            is1 = ic(j)
            ic(j) = ic(i)
            ic(i) = is1
         ENDIF
      ENDDO
   ENDDO
   DO i = 1 , N
      IF ( ic(i)>=Isw ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   k = 1
   CALL spag_block_2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      DO j = i , N
         k = j - i + 1
         Id(k) = ib(j)
      ENDDO
      IF ( k==N ) RETURN
      k = k + 1
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      DO j = k , N
         l = j - k + 1
         Id(j) = ib(l)
      ENDDO
   END SUBROUTINE spag_block_2
END SUBROUTINE permut
