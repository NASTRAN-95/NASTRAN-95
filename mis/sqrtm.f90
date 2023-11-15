
SUBROUTINE sqrtm(A,Ia,B,Ib)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   DOUBLE PRECISION A , B
   INTEGER Ia , Ib
   DOUBLE PRECISION Detsw(1)
   INTEGER Ipsw(1)
!
! End of declarations
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
 100  RETURN
!
!     DCALE OF DETERMINANT BY FACTORS OF 10
!
   ENTRY detm6(Detsw,Ipsw)
   IF ( Detsw(1)==0.0D0 ) GOTO 100
   DO WHILE ( dabs(Detsw(1))>10.0D0 )
      Detsw(1) = Detsw(1)*0.1D0
      Ipsw(1) = Ipsw(1) + 1
   ENDDO
   DO WHILE ( dabs(Detsw(1))<0.1D0 )
      Detsw(1) = Detsw(1)*10.0D0
      Ipsw(1) = Ipsw(1) - 1
   ENDDO
   GOTO 100
END SUBROUTINE sqrtm
