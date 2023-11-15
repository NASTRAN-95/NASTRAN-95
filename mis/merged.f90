
SUBROUTINE merged(A11,A12,A21,A22,A,Rp,Cp,N1,N2)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iz(1) , Mcba(7) , Mcba11(7) , Mcba12(7) , Mcba21(7) , Mcba22(7) , Nx , Rule
   COMMON /parmeg/ Mcba , Mcba11 , Mcba21 , Mcba12 , Mcba22 , Nx , Rule
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   INTEGER A , A11 , A12 , A21 , A22 , Cp , N1 , N2 , Rp
!
! Local variable declarations
!
   INTEGER i , iotyp , mcb(20) , mcb1(20)
   INTEGER korsz
!
! End of declarations
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
   Nx = korsz(Iz)
   Rule = 0
   iotyp = 0
   Mcba11(1) = A11
   IF ( A11/=0 ) THEN
      CALL rdtrl(Mcba11)
      IF ( Mcba11(1)<=0 ) Mcba11(1) = 0
   ENDIF
!
   Mcba21(1) = A21
   IF ( A21/=0 ) THEN
      CALL rdtrl(Mcba21)
      IF ( Mcba21(1)<=0 ) Mcba21(1) = 0
   ENDIF
!
   Mcba12(1) = A12
   IF ( A12/=0 ) THEN
      CALL rdtrl(Mcba12)
      IF ( Mcba12(1)<=0 ) Mcba12(1) = 0
   ENDIF
!
   Mcba22(1) = A22
   IF ( A22/=0 ) THEN
      CALL rdtrl(Mcba22)
      IF ( Mcba22(1)<=0 ) Mcba22(1) = 0
   ENDIF
!
   Mcba(1) = A
   Mcba(2) = mcb(3)
   Mcba(3) = mcb1(3)
   DO i = 1 , 28 , 7
      IF ( Mcba11(i)/=0 ) iotyp = max0(iotyp,Mcba11(i+4))
   ENDDO
   Mcba(4) = 2
   Mcba(5) = iotyp
   IF ( Mcba(2)==Mcba(3) ) Mcba(4) = 1
   CALL merge(mcb,mcb1,Iz)
   CALL wrttrl(Mcba)
END SUBROUTINE merged
