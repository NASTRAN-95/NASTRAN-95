!*==ampb2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampb2(A,A11,A12,A21,A22,Rp,Cp,N1,N2)
   USE c_parmeg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: A11
   INTEGER :: A12
   INTEGER :: A21
   INTEGER :: A22
   INTEGER :: Rp
   INTEGER :: Cp
   INTEGER :: N1
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(20) :: mcb , mcb1
   EXTERNAL korsz , partn , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE IS A GENERAL DRIVER FOR PARTN
!
!
!
!-----------------------------------------------------------------------
!
   mcb(1) = Rp
   CALL rdtrl(mcb)
   mcb1(1) = Cp
   CALL rdtrl(mcb1)
   nx = korsz(iz)
   rule = 0
   mcba11(1) = A11
   IF ( A11/=0 ) THEN
      CALL rdtrl(mcba11)
      IF ( mcba11(1)<=0 ) mcba11(1) = 0
   ENDIF
   mcba21(1) = A21
   IF ( A21>0 ) THEN
      CALL rdtrl(mcba21)
      IF ( mcba21(1)<=0 ) mcba21(1) = 0
   ENDIF
   mcba12(1) = A12
   IF ( A12/=0 ) THEN
      CALL rdtrl(mcba12)
      IF ( mcba12(1)<=0 ) mcba12(1) = 0
   ENDIF
   mcba22(1) = A22
   IF ( A22/=0 ) THEN
      CALL rdtrl(mcba22)
      IF ( mcba22(1)<=0 ) mcba22(1) = 0
   ENDIF
   mcba(1) = A
   CALL rdtrl(mcba)
   mcba11(2) = mcba(2) - mcb(6)
   mcba11(3) = mcba(3) - mcb1(6)
   mcba12(2) = mcba(2) - mcba11(2)
   mcba12(3) = mcba11(3)
   mcba21(2) = mcba11(2)
   mcba21(3) = mcba(3) - mcba11(3)
   mcba22(2) = mcb(6)
   mcba22(3) = mcb1(6)
   mcba11(4) = 2
   mcba21(4) = 2
   mcba12(4) = 2
   mcba22(4) = 2
   mcba11(5) = mcba(5)
   mcba21(5) = mcba(5)
   mcba12(5) = mcba(5)
   mcba22(5) = mcba(5)
   CALL partn(mcb,mcb1,iz)
   IF ( mcba11(1)>0 ) CALL wrttrl(mcba11)
   IF ( mcba21(1)>0 ) CALL wrttrl(mcba21)
   IF ( mcba12(1)>0 ) CALL wrttrl(mcba12)
   IF ( mcba22(1)>0 ) CALL wrttrl(mcba22)
END SUBROUTINE ampb2
