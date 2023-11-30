
SUBROUTINE alg03(Lnct,L)
   IMPLICIT NONE
   INTEGER Iprtc , Limit , Lq
   COMMON /ud3prt/ Iprtc
   COMMON /upage / Limit , Lq
   INTEGER L , Lnct
!
!
   Lnct = Lnct + L
   IF ( Lnct<=Limit ) RETURN
   Lnct = 1 + L
   IF ( Iprtc/=0 ) WRITE (Lq,99001)
99001 FORMAT (1H1)
END SUBROUTINE alg03