
SUBROUTINE stpbg(Bm,Gm,Ns,Bloc,D,Ca,Nsize)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ns
   REAL Bloc(1) , Bm(4,4,Ns) , Ca(1) , D(1) , Gm(4,3,Ns)
   INTEGER Nsize(1)
!
! Local variable declarations
!
   REAL e
   INTEGER i , j , n
!
! End of declarations
!
!     MAKES MATRICES BM AND GM FOR EACH STRIP
   DO n = 1 , Ns
      DO i = 1 , 4
         DO j = 1 , 4
            Bm(i,j,n) = 0.0
         ENDDO
      ENDDO
      DO i = 1 , 4
         DO j = 1 , 3
            Gm(i,j,n) = 0.0
         ENDDO
      ENDDO
      Bm(1,1,n) = Bloc(n)
      Bm(2,2,n) = -Bloc(n)*Bloc(n)
      Gm(1,1,n) = -1.0/Bloc(n)
      Gm(2,2,n) = 1.0
      IF ( Nsize(n)/=2 ) THEN
!         CONTROL SURFACE CASE
         e = Ca(n) + D(n) - 1.5*Bloc(n)
         Bm(3,3,n) = e*Bloc(n)
         Bm(3,4,n) = Bm(2,2,n)
         Gm(3,3,n) = 1.0
         Gm(4,3,n) = -e/Bloc(n)
      ENDIF
   ENDDO
END SUBROUTINE stpbg
