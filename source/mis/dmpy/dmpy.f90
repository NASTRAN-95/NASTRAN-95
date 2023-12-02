!*==dmpy.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dmpy(Z,Zd)
   USE c_dmpyx
   USE c_names
   USE c_system
   USE c_unpakx
   USE c_zblpkx
   USE c_zntpkx
   USE iso_fortran_env
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
         buf1 = nz - sysbuf + 1
         buf2 = buf1 - sysbuf
         one = 1
         incr = 1
         filec(2) = 0
         filec(6) = 0
         filec(7) = 0
         nx = filea(3)
!
!     COMPUTE TYPE OF C MATRIX.
!     RCC = 1 FOR REAL, = 2 FOR COMPLEX
!     QTYPE = 2 FOR RDP, = 4 FOR CDP
!
         rcc = 0
         IF ( filea(5)>2 .OR. fileb(5)>2 ) rcc = 2
         qtype = rcc + 2
         IF ( rcc==0 ) rcc = 1
         type = qtype*sign
         ptype = filec(5)
!
!     OPEN PRODUCT MATRIX AND WRITE HEADER RECORD.
!
         CALL gopen(filec(1),Z(buf1),wrtrew)
!
!     UNPACK DIAGONAL MATRIX IN CORE AND OPEN ARBITRARY MATRIX.
!
         CALL gopen(filea(1),Z(buf2),rdrew)
         CALL unpack(*40,filea,Z)
         CALL close(filea(1),clsrew)
         CALL gopen(fileb(1),Z(buf2),rdrew)
!
!     PERFORM MATRIX MULTIPLICATION.
!
         j = 1
         spag_nextblock_1 = 2
      CASE (2)
         kr = (j-1)*rcc + 1
         CALL bldpk(qtype,ptype,filec(1),0,0)
         CALL intpk(*20,fileb(1),0,qtype,0)
         SPAG_Loop_1_1: DO
            CALL zntpki
            kl = (i-1)*rcc + 1
            k = kl
            IF ( flag/=0 ) k = kr
            xd(1) = Zd(k)*ad(1)
            IF ( rcc/=1 ) THEN
               xd(1) = xd(1) - Zd(k+1)*ad(2)
               xd(2) = Zd(k)*ad(2) + Zd(k+1)*ad(1)
            ENDIF
            ix = i
            CALL zblpki
            IF ( eol/=0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
 20      CALL bldpkn(filec(1),0,filec)
         j = j + 1
         IF ( j<=fileb(2) ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      SPAG_Loop_1_2: DO
!
!     CODE FOR NULL DIAGONAL MATRIX.
!
            CALL bldpkn(filec(1),0,filec)
            IF ( filec(2)>=fileb(2) ) EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 3
      CASE (3)
!
!     CLOSE FILES AND RETURN.
!
         CALL close(filea(1),clsrew)
         CALL close(fileb(1),clsrew)
         CALL close(filec(1),clsrew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dmpy
