
SUBROUTINE alg03(Lnct,L)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iprtc , Limit , Lq
   COMMON /ud3prt/ Iprtc
   COMMON /upage / Limit , Lq
!
! Dummy argument declarations
!
   INTEGER L , Lnct
!
! End of declarations
!
!
!
   Lnct = Lnct + L
   IF ( Lnct<=Limit ) RETURN
   Lnct = 1 + L
   IF ( Iprtc/=0 ) WRITE (Lq,99001)
99001 FORMAT (1H1)
END SUBROUTINE alg03
