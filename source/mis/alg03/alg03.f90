!*==alg03.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg03(Lnct,L)
   USE c_ud3prt
   USE c_upage
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lnct
   INTEGER :: L
!
! End of declarations rewritten by SPAG
!
!
!
   Lnct = Lnct + L
   IF ( Lnct<=limit ) RETURN
   Lnct = 1 + L
   IF ( iprtc/=0 ) WRITE (lq,99001)
99001 FORMAT (1H1)
END SUBROUTINE alg03
