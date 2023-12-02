!*==ampc2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampc2(Inp,Outp,Scrf)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Inp
   INTEGER :: Outp
   INTEGER :: Scrf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , j , ncol , nri , nro , nrowis , nrowo , nterm
   INTEGER , DIMENSION(7) :: mcbi , mcbo
   EXTERNAL close , filswi , gopen , korsz , pack , rdtrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     THE PURPOSE OF THIS ROUTINE IS TO COPY SCR5 ONTO THE BOTTOM OF
!     OUTPUT
!
!
!
   mcbi(1) = Inp
   CALL rdtrl(mcbi)
   mcbo(1) = Outp
   CALL rdtrl(mcbo)
!
!     IS THIS THE FIRST ENTRY
!
   IF ( mcbo(2)/=0 ) THEN
!
!     MUST DO COPY
!
      CALL filswi(Outp,Scrf)
      ibuf1 = korsz(Iz) - Sysbuf + 1
      ibuf2 = ibuf1 - Sysbuf
      ibuf3 = ibuf2 - Sysbuf
      CALL gopen(Inp,Iz(ibuf1),0)
      CALL gopen(Scrf,Iz(ibuf2),0)
      CALL gopen(Outp,Iz(ibuf2),1)
      ncol = mcbi(2)
      nrowo = mcbi(3) + mcbo(3)
      It1 = mcbi(5)
      It2 = It1
      It3 = It1
      Incr = 1
      Incr1 = 1
      nterm = nrowo*Iword(It1)
      Ii = 1
      Jj = nrowo
      nrowis = mcbo(3)*Iword(It1) + 1
      Ii1 = 1
      nri = mcbi(3)
      nro = mcbo(3)
      mcbo(2) = 0
      mcbo(6) = 0
      mcbo(7) = 0
      mcbo(3) = nrowo
      DO i = 1 , ncol
         DO j = 1 , nterm
            Iz(j) = 0
         ENDDO
         Jj1 = nro
         CALL unpack(*20,Scrf,Iz)
 20      Jj1 = nri
         CALL unpack(*40,Inp,Iz(nrowis))
 40      CALL pack(Iz,Outp,mcbo)
      ENDDO
      CALL close(Scrf,1)
      CALL close(Inp,1)
      CALL close(Outp,1)
      CALL wrttrl(mcbo)
      RETURN
   ENDIF
!
!     SWITCH SCRATCH FILES
!
   CALL filswi(Inp,Outp)
   RETURN
END SUBROUTINE ampc2
