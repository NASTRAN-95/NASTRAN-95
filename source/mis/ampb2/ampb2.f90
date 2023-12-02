!*==ampb2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampb2(A,A11,A12,A21,A22,Rp,Cp,N1,N2)
   IMPLICIT NONE
   USE C_PARMEG
   USE C_ZZZZZZ
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
   Nx = korsz(Iz)
   Rule = 0
   Mcba11(1) = A11
   IF ( A11/=0 ) THEN
      CALL rdtrl(Mcba11)
      IF ( Mcba11(1)<=0 ) Mcba11(1) = 0
   ENDIF
   Mcba21(1) = A21
   IF ( A21>0 ) THEN
      CALL rdtrl(Mcba21)
      IF ( Mcba21(1)<=0 ) Mcba21(1) = 0
   ENDIF
   Mcba12(1) = A12
   IF ( A12/=0 ) THEN
      CALL rdtrl(Mcba12)
      IF ( Mcba12(1)<=0 ) Mcba12(1) = 0
   ENDIF
   Mcba22(1) = A22
   IF ( A22/=0 ) THEN
      CALL rdtrl(Mcba22)
      IF ( Mcba22(1)<=0 ) Mcba22(1) = 0
   ENDIF
   Mcba(1) = A
   CALL rdtrl(Mcba)
   Mcba11(2) = Mcba(2) - mcb(6)
   Mcba11(3) = Mcba(3) - mcb1(6)
   Mcba12(2) = Mcba(2) - Mcba11(2)
   Mcba12(3) = Mcba11(3)
   Mcba21(2) = Mcba11(2)
   Mcba21(3) = Mcba(3) - Mcba11(3)
   Mcba22(2) = mcb(6)
   Mcba22(3) = mcb1(6)
   Mcba11(4) = 2
   Mcba21(4) = 2
   Mcba12(4) = 2
   Mcba22(4) = 2
   Mcba11(5) = Mcba(5)
   Mcba21(5) = Mcba(5)
   Mcba12(5) = Mcba(5)
   Mcba22(5) = Mcba(5)
   CALL partn(mcb,mcb1,Iz)
   IF ( Mcba11(1)>0 ) CALL wrttrl(Mcba11)
   IF ( Mcba21(1)>0 ) CALL wrttrl(Mcba21)
   IF ( Mcba12(1)>0 ) CALL wrttrl(Mcba12)
   IF ( Mcba22(1)>0 ) CALL wrttrl(Mcba22)
END SUBROUTINE ampb2
