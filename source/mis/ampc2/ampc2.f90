!*==ampc2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampc2(Inp,Outp,Scrf)
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
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
      ibuf1 = korsz(iz) - sysbuf + 1
      ibuf2 = ibuf1 - sysbuf
      ibuf3 = ibuf2 - sysbuf
      CALL gopen(Inp,iz(ibuf1),0)
      CALL gopen(Scrf,iz(ibuf2),0)
      CALL gopen(Outp,iz(ibuf2),1)
      ncol = mcbi(2)
      nrowo = mcbi(3) + mcbo(3)
      it1 = mcbi(5)
      it2 = it1
      it3 = it1
      incr = 1
      incr1 = 1
      nterm = nrowo*iword(it1)
      ii = 1
      jj = nrowo
      nrowis = mcbo(3)*iword(it1) + 1
      ii1 = 1
      nri = mcbi(3)
      nro = mcbo(3)
      mcbo(2) = 0
      mcbo(6) = 0
      mcbo(7) = 0
      mcbo(3) = nrowo
      DO i = 1 , ncol
         DO j = 1 , nterm
            iz(j) = 0
         ENDDO
         jj1 = nro
         CALL unpack(*20,Scrf,iz)
 20      jj1 = nri
         CALL unpack(*40,Inp,iz(nrowis))
 40      CALL pack(iz,Outp,mcbo)
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
END SUBROUTINE ampc2
