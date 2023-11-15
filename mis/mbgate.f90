
SUBROUTINE mbgate(Ntote,Dphite,N,Ywte,Q,Q1,Q2,Kte,Kte1,Kte2)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Cntrl1 , Cntrl2
   REAL Crank1 , Crank2 , X(12) , Y(12)
   INTEGER Njj
   COMMON /mboxa / X , Y
   COMMON /mboxc / Njj , Crank1 , Crank2 , Cntrl1 , Cntrl2
!
! Dummy argument declarations
!
   INTEGER N , Ntote
   COMPLEX Dphite(3,N) , Q(1) , Q1(1) , Q2(1)
   INTEGER Kte(1) , Kte1(1) , Kte2(1)
   REAL Ywte(1)
!
! Local variable declarations
!
   COMPLEX dphi
   INTEGER isp , j
!
! End of declarations
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
