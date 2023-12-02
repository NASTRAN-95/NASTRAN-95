!*==mbgaw.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbgaw(Boxl,Dphi,Ws,Paw,Paf1,Paf2,Q,Q1,Q2,M,Kc,Kc1,Kc2)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Boxl
   COMPLEX :: Dphi
   COMPLEX :: Ws
   REAL :: Paw
   REAL :: Paf1
   REAL :: Paf2
   COMPLEX , DIMENSION(1) :: Q
   COMPLEX , DIMENSION(1) :: Q1
   COMPLEX , DIMENSION(1) :: Q2
   INTEGER :: M
   INTEGER :: Kc
   INTEGER :: Kc1
   INTEGER :: Kc2
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     MAIN PLANE BOXES
!     (NEW MSC METHOD USED)
!
   Ws = (-0.5*amin0(M,2)*Boxl)*Dphi
   IF ( Paw>=0.005 ) Q(Kc) = Paw*Ws
   IF ( Paf1>=0.005 ) Q1(Kc1) = Paf1*Ws
   IF ( Paf2>=0.005 ) Q2(Kc2) = Paf2*Ws
END SUBROUTINE mbgaw
