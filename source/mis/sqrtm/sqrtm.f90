!*==sqrtm.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sqrtm(A,Ia,B,Ib)
   IMPLICIT NONE
   DOUBLE PRECISION A , B
   INTEGER Ia , Ib
   DOUBLE PRECISION Detsw(1)
   INTEGER Ipsw(1)
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
      ENTRY detm6(Detsw,Ipsw)
      IF ( Detsw(1)/=0.0D0 ) THEN
         DO WHILE ( dabs(Detsw(1))>10.0D0 )
            Detsw(1) = Detsw(1)*0.1D0
            Ipsw(1) = Ipsw(1) + 1
         ENDDO
         DO WHILE ( dabs(Detsw(1))<0.1D0 )
            Detsw(1) = Detsw(1)*10.0D0
            Ipsw(1) = Ipsw(1) - 1
         ENDDO
      ENDIF
      CALL spag_block_1
      RETURN
   END ENTRY detm6
END SUBROUTINE sqrtm
