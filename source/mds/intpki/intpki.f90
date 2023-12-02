!*==intpki.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE intpki(A,I,File,Block,Ieol)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER File , I , Ieol
   INTEGER A(4) , Block(15)
   INTEGER index , itypot , kk , num
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         name = File
         iretrn = 0
         I = Block(4)
         index = (Block(5)-1)*Block(14) + 1 + Block(7)*Block(11)
         itypot = Block(13)
         IF ( Block(2)/=itypot ) THEN
            CALL dsupkc(Block(2),itypot,ibase(index),A)
         ELSE
            num = nwrdel(itypot)
!DIR$ NOVECTOR
            DO kk = 1 , num
               A(kk) = ibase(index+kk-1)
!DIR$ VECTOR
            ENDDO
         ENDIF
         Block(4) = Block(4) + 1
         Block(7) = Block(7) + 1
         Block(10) = Block(4)
         IF ( Block(7)<Block(6) ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL endget(Block)
         CALL getstr(*20,Block)
 20      Block(7) = 0
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iretrn/=0 ) THEN
            Ieol = 1
         ELSE
            Ieol = 0
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE intpki
