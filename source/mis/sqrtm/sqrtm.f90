!*==sqrtm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sqrtm(A,Ia,B,Ib)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: A
   INTEGER :: Ia
   REAL(REAL64) :: B
   INTEGER :: Ib
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: detsw
   INTEGER , DIMENSION(1) :: ipsw
!
! End of declarations rewritten by SPAG
!
!
!     SCALED ARITHMETIC ROUTINES--SQUARE ROOT
!
   A = B
   Ia = Ib
   IF ( mod(Ia,2)/=0 ) THEN
      Ia = Ia - 1
      A = A*10.0
   ENDIF
   Ia = Ia/2
   A = dsqrt(dmax1(A,0.D0))
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      RETURN
!
!     DCALE OF DETERMINANT BY FACTORS OF 10
!
      ENTRY detm6(detsw,ipsw)
      IF ( detsw(1)/=0.0D0 ) THEN
         DO WHILE ( dabs(detsw(1))>10.0D0 )
            detsw(1) = detsw(1)*0.1D0
            ipsw(1) = ipsw(1) + 1
         ENDDO
         DO WHILE ( dabs(detsw(1))<0.1D0 )
            detsw(1) = detsw(1)*10.0D0
            ipsw(1) = ipsw(1) - 1
         ENDDO
      ENDIF
      CALL spag_block_1
   END SUBROUTINE spag_block_1
END SUBROUTINE sqrtm
