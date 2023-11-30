
SUBROUTINE read6(Irig,Gphia,Nr,Phia)
   IMPLICIT NONE
   INTEGER Ii , Iiu , Incr1 , Incr1u , It1 , It2 , It2u , Iz(1) , Jj , Jju , Sysbuf
   REAL Z(3)
   COMMON /packx / It1 , It2 , Ii , Jj , Incr1
   COMMON /system/ Sysbuf
   COMMON /unpakx/ It2u , Iiu , Jju , Incr1u
   COMMON /zzzzzz/ Iz
   INTEGER Gphia , Irig , Nr , Phia
   INTEGER file , i , ibuf1 , ibuf2 , mcb(7) , ncol
   INTEGER korsz
!
!     ADDS GIVENS EIGENVECTORS TO RIGID BODY MODES ON PHIA
!
   EQUIVALENCE (Iz(1),Z(1))
!
!
   ibuf1 = korsz(Z) - Sysbuf + 1
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
   CALL gopen(Phia,Z(ibuf1),1)
   IF ( Nr/=0 ) THEN
      file = Irig
      CALL gopen(Irig,Z(ibuf2),0)
      Z(1) = 0.0
      Z(2) = 0.0
      DO i = 1 , Nr
         Iiu = 0
         CALL unpack(*20,Irig,Z(3))
         Ii = Iiu
         Jj = Jju
         CALL pack(Z(3),Phia,mcb)
         CYCLE
 20      Ii = 1
         Jj = 1
         CALL pack(Z,Phia,mcb)
      ENDDO
      CALL close(Irig,1)
   ENDIF
   IF ( ncol>0 ) THEN
      CALL gopen(Gphia,Z(ibuf2),0)
      file = Gphia
      Incr1u = 1
      Z(1) = 0.0
      Z(2) = 0.0
      CALL skprec(Gphia,Nr)
      DO i = 1 , ncol
         Iiu = 0
         CALL unpack(*40,Gphia,Z(3))
         Ii = Iiu
         Jj = Jju
         CALL pack(Z(3),Phia,mcb)
         CYCLE
 40      Ii = 1
         Jj = 1
         CALL pack(Z,Phia,mcb)
      ENDDO
      CALL close(Gphia,1)
   ENDIF
   CALL close(Phia,1)
   CALL wrttrl(mcb)
END SUBROUTINE read6
