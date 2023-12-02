!*==read6.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE read6(Irig,Gphia,Nr,Phia)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Irig
   INTEGER :: Gphia
   INTEGER :: Nr
   INTEGER :: Phia
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf1 , ibuf2 , ncol
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(3) :: z
   EXTERNAL close , gopen , korsz , makmcb , pack , rdtrl , skprec , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     ADDS GIVENS EIGENVECTORS TO RIGID BODY MODES ON PHIA
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
!
!
   ibuf1 = korsz(z) - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   mcb(1) = Gphia
   CALL rdtrl(mcb)
   ncol = mcb(2) - Nr
   Ii = 1
   Jj = mcb(3)
   It1 = mcb(5)
   It2 = mcb(5)
   It2u = mcb(5)
   CALL makmcb(mcb,Phia,Jj,mcb(4),It1)
   Incr1 = 1
   CALL gopen(Phia,z(ibuf1),1)
   IF ( Nr/=0 ) THEN
      file = Irig
      CALL gopen(Irig,z(ibuf2),0)
      z(1) = 0.0
      z(2) = 0.0
      DO i = 1 , Nr
         Iiu = 0
         CALL unpack(*20,Irig,z(3))
         Ii = Iiu
         Jj = Jju
         CALL pack(z(3),Phia,mcb)
         CYCLE
 20      Ii = 1
         Jj = 1
         CALL pack(z,Phia,mcb)
      ENDDO
      CALL close(Irig,1)
   ENDIF
   IF ( ncol>0 ) THEN
      CALL gopen(Gphia,z(ibuf2),0)
      file = Gphia
      Incr1u = 1
      z(1) = 0.0
      z(2) = 0.0
      CALL skprec(Gphia,Nr)
      DO i = 1 , ncol
         Iiu = 0
         CALL unpack(*40,Gphia,z(3))
         Ii = Iiu
         Jj = Jju
         CALL pack(z(3),Phia,mcb)
         CYCLE
 40      Ii = 1
         Jj = 1
         CALL pack(z,Phia,mcb)
      ENDDO
      CALL close(Gphia,1)
   ENDIF
   CALL close(Phia,1)
   CALL wrttrl(mcb)
END SUBROUTINE read6
