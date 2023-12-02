!*==mbgate.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbgate(Ntote,Dphite,N,Ywte,Q,Q1,Q2,Kte,Kte1,Kte2)
   IMPLICIT NONE
   USE C_MBOXA
   USE C_MBOXC
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   INTEGER :: Ntote
   COMPLEX , DIMENSION(3,N) :: Dphite
   REAL , DIMENSION(1) :: Ywte
   COMPLEX , DIMENSION(1) :: Q
   COMPLEX , DIMENSION(1) :: Q1
   COMPLEX , DIMENSION(1) :: Q2
   INTEGER , DIMENSION(1) :: Kte
   INTEGER , DIMENSION(1) :: Kte1
   INTEGER , DIMENSION(1) :: Kte2
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX :: dphi
   INTEGER :: isp , j
!
! End of declarations rewritten by SPAG
!
!
!     SUM ON TRAILING EDGE
!
   DO j = 1 , Ntote
      dphi = Dphite(1,j)*0.5*amin0(j,2)
      IF ( Cntrl1 .AND. Ywte(j)>=Y(7) .AND. Ywte(j)<=Y(11) ) THEN
         isp = Kte1(j)
         Q1(isp) = dphi
      ELSEIF ( Cntrl2 .AND. Ywte(j)>Y(11) .AND. Ywte(j)<=Y(12) ) THEN
         isp = Kte2(j)
         Q2(isp) = dphi
      ELSE
         isp = Kte(j)
         Q(isp) = dphi
      ENDIF
   ENDDO
END SUBROUTINE mbgate
