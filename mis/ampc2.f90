
SUBROUTINE ampc2(Inp,Outp,Scrf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Ii1 , Incr , Incr1 , Isk(2) , It1 , It2 , It3 , Iword(4) , Iz(1) , Jj , Jj1 , Sysbuf
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf
   COMMON /type  / Isk , Iword
   COMMON /unpakx/ It3 , Ii1 , Jj1 , Incr1
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   INTEGER Inp , Outp , Scrf
!
! Local variable declarations
!
   INTEGER i , ibuf1 , ibuf2 , ibuf3 , j , mcbi(7) , mcbo(7) , ncol , nri , nro , nrowis , nrowo , nterm
   INTEGER korsz
!
! End of declarations
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
      GOTO 99999
   ENDIF
!
!     SWITCH SCRATCH FILES
!
   CALL filswi(Inp,Outp)
   RETURN
99999 RETURN
END SUBROUTINE ampc2
