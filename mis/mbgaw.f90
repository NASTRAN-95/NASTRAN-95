
SUBROUTINE mbgaw(Boxl,Dphi,Ws,Paw,Paf1,Paf2,Q,Q1,Q2,M,Kc,Kc1,Kc2)
   IMPLICIT NONE
   REAL Boxl , Paf1 , Paf2 , Paw
   COMPLEX Dphi , Ws
   INTEGER Kc , Kc1 , Kc2 , M
   COMPLEX Q(1) , Q1(1) , Q2(1)
!
!     MAIN PLANE BOXES
!     (NEW MSC METHOD USED)
!
   Ws = (-0.5*amin0(M,2)*Boxl)*Dphi
   IF ( Paw>=0.005 ) Q(Kc) = Paw*Ws
   IF ( Paf1>=0.005 ) Q1(Kc1) = Paf1*Ws
   IF ( Paf2>=0.005 ) Q2(Kc2) = Paf2*Ws
END SUBROUTINE mbgaw
