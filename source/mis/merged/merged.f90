!*==merged.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE merged(A11,A12,A21,A22,A,Rp,Cp,N1,N2)
   USE c_parmeg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A11
   INTEGER :: A12
   INTEGER :: A21
   INTEGER :: A22
   INTEGER :: A
   INTEGER :: Rp
   INTEGER :: Cp
   INTEGER :: N1
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iotyp
   INTEGER , DIMENSION(20) :: mcb , mcb1
   EXTERNAL korsz , merge , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!
   IF ( Rp/=0 ) THEN
!
      mcb(1) = Rp
      CALL rdtrl(mcb)
   ELSE
      mcb(1) = 0
      mcb(2) = 1
      mcb(3) = N1
      mcb(4) = 2
      mcb(5) = 1
   ENDIF
   IF ( Cp/=0 ) THEN
!
      mcb1(1) = Cp
      CALL rdtrl(mcb1)
   ELSE
      mcb1(1) = 0
      mcb1(2) = 1
      mcb1(3) = N2
      mcb1(4) = 2
      mcb1(5) = 1
   ENDIF
   nx = korsz(iz)
   rule = 0
   iotyp = 0
   mcba11(1) = A11
   IF ( A11/=0 ) THEN
      CALL rdtrl(mcba11)
      IF ( mcba11(1)<=0 ) mcba11(1) = 0
   ENDIF
!
   mcba21(1) = A21
   IF ( A21/=0 ) THEN
      CALL rdtrl(mcba21)
      IF ( mcba21(1)<=0 ) mcba21(1) = 0
   ENDIF
!
   mcba12(1) = A12
   IF ( A12/=0 ) THEN
      CALL rdtrl(mcba12)
      IF ( mcba12(1)<=0 ) mcba12(1) = 0
   ENDIF
!
   mcba22(1) = A22
   IF ( A22/=0 ) THEN
      CALL rdtrl(mcba22)
      IF ( mcba22(1)<=0 ) mcba22(1) = 0
   ENDIF
!
   mcba(1) = A
   mcba(2) = mcb(3)
   mcba(3) = mcb1(3)
   DO i = 1 , 28 , 7
      IF ( mcba11(i)/=0 ) iotyp = max0(iotyp,mcba11(i+4))
   ENDDO
   mcba(4) = 2
   mcba(5) = iotyp
   IF ( mcba(2)==mcba(3) ) mcba(4) = 1
   CALL merge(mcb,mcb1,iz)
   CALL wrttrl(mcba)
END SUBROUTINE merged
