!*==dmpy.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dmpy(Z,Zd)
USE C_DMPYX
USE C_NAMES
USE C_SYSTEM
USE C_UNPAKX
USE C_ZBLPKX
USE C_ZNTPKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Z
   REAL(REAL64) , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , j , k , kl , kr , ptype , qtype , rcc
   EXTERNAL bldpk , bldpkn , close , gopen , intpk , unpack , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DMPY WILL PRE OR POST MULTIPLY AN ARBITRARY MATRIX BY A DIAGONAL
!     MATRIX.
!
!     FILEA = MATRIX CONTROL BLOCK FOR DIAGONAL MATRIX.
!     FILEB = MATRIX CONTROL BLOCK FOR ARBITRARY MATRIX.
!     FILEC = MATRIX CONTROL BLOCK FOR PRODUCT MATRIX.
!     Z     = ADDRESS OF A BLOCK OF CORE FOR WORKING SPACE. ZD IS SAME
!             BLOCK.
!     NZ    = LENGTH OF THIS BLOCK.
!     FLAG .EQ. 0 FOR PRE-MULTIPLICATION BY DIAGONAL.
!     FLAG .NE. 0 FOR POST-MULTIPLICATION BY DIAGONAL.
!     SIGN .EQ. +1 FOR POSITIVE PRODUCT.
!     SIGN .EQ. -1 FOR NEGATIVE PRODUCT.
!
!
!
!
!     PERFORM GENERAL INITIALIZATION
!
         buf1 = Nz - Sysbuf + 1
         buf2 = buf1 - Sysbuf
         One = 1
         Incr = 1
         Filec(2) = 0
         Filec(6) = 0
         Filec(7) = 0
         Nx = Filea(3)
!
!     COMPUTE TYPE OF C MATRIX.
!     RCC = 1 FOR REAL, = 2 FOR COMPLEX
!     QTYPE = 2 FOR RDP, = 4 FOR CDP
!
         rcc = 0
         IF ( Filea(5)>2 .OR. Fileb(5)>2 ) rcc = 2
         qtype = rcc + 2
         IF ( rcc==0 ) rcc = 1
         Type = qtype*Sign
         ptype = Filec(5)
!
!     OPEN PRODUCT MATRIX AND WRITE HEADER RECORD.
!
         CALL gopen(Filec(1),Z(buf1),Wrtrew)
!
!     UNPACK DIAGONAL MATRIX IN CORE AND OPEN ARBITRARY MATRIX.
!
         CALL gopen(Filea(1),Z(buf2),Rdrew)
         CALL unpack(*40,Filea,Z)
         CALL close(Filea(1),Clsrew)
         CALL gopen(Fileb(1),Z(buf2),Rdrew)
!
!     PERFORM MATRIX MULTIPLICATION.
!
         j = 1
         spag_nextblock_1 = 2
      CASE (2)
         kr = (j-1)*rcc + 1
         CALL bldpk(qtype,ptype,Filec(1),0,0)
         CALL intpk(*20,Fileb(1),0,qtype,0)
         SPAG_Loop_1_1: DO
            CALL zntpki
            kl = (I-1)*rcc + 1
            k = kl
            IF ( Flag/=0 ) k = kr
            Xd(1) = Zd(k)*Ad(1)
            IF ( rcc/=1 ) THEN
               Xd(1) = Xd(1) - Zd(k+1)*Ad(2)
               Xd(2) = Zd(k)*Ad(2) + Zd(k+1)*Ad(1)
            ENDIF
            Ix = I
            CALL zblpki
            IF ( Eol/=0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
 20      CALL bldpkn(Filec(1),0,Filec)
         j = j + 1
         IF ( j<=Fileb(2) ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      SPAG_Loop_1_2: DO
!
!     CODE FOR NULL DIAGONAL MATRIX.
!
            CALL bldpkn(Filec(1),0,Filec)
            IF ( Filec(2)>=Fileb(2) ) EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 3
      CASE (3)
!
!     CLOSE FILES AND RETURN.
!
         CALL close(Filea(1),Clsrew)
         CALL close(Fileb(1),Clsrew)
         CALL close(Filec(1),Clsrew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dmpy
