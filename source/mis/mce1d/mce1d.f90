!*==mce1d.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1d
USE C_BLANK
USE C_SYSTEM
USE C_ZBLPKX
USE C_ZNTPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: bcd
   INTEGER :: k , n , n1 , ncol , ncol1 , nz
   INTEGER , DIMENSION(7) :: mcb1 , mcb2
   INTEGER , SAVE :: rdp
   REAL , DIMENSION(1) :: z
   EXTERNAL bldpk , bldpkn , close , gopen , intpk , korsz , makmcb , mesage , rdtrl , wrttrl , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MCE1D SOLVES FOR GM IN THE MATRIX EQUATION RM*GM = -RN
!     WHERE RM IS A DIAGONAL MATRIX.
!
   !>>>>EQUIVALENCE (Mcb(2),Ncol) , (Ad(1),A(1)) , (Mcb(5),Type) , (Zd(1),Z(1)) , (Bd(1),B(1)) , (mcb1(2),ncol1)
   DATA bcd , rdp/4HMCE1 , 4HD    , 2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN RM MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
!
         nz = korsz(z)
         n = nz - Sysbuf
         CALL gopen(Rm,z(n+1),0)
         Mcb(1) = Rm
         CALL rdtrl(Mcb)
!
!     FORM -RM
!
         ncol = Mcb(2)
         DO k = 1 , ncol
            CALL intpk(*20,Rm,0,rdp,0)
            CALL zntpki
            IF ( I/=k ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Zd(k) = -Ad(1)
         ENDDO
         CALL close(Rm,1)
!
!     OPEN RN MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
!
         CALL gopen(Rn,z(n+1),0)
         mcb1(1) = Rn
         CALL rdtrl(mcb1)
!
!     SET UP MATRIX CONTROL BLOCK BLOCK FOR GM
!
         CALL makmcb(mcb2,Gm,mcb1(3),mcb1(4),Ipr)
!
!     OPEN OUTPUT FILE FOR GM AND WRITE HEADER RECORD
!
         n1 = n - Sysbuf
         CALL gopen(Gm,z(n1+1),1)
!
!     FORM GM = -RM(-1)*RN
!
         ncol1 = mcb1(2)
         DO k = 1 , ncol1
            CALL bldpk(rdp,Ipr,Gm,0,0)
            CALL intpk(*10,Rn,0,rdp,0)
            SPAG_Loop_2_1: DO
               CALL zntpki
               J = I
               Bd(1) = Ad(1)/Zd(J)
               CALL zblpki
               IF ( Eol/=0 ) EXIT SPAG_Loop_2_1
            ENDDO SPAG_Loop_2_1
 10         CALL bldpkn(Gm,0,mcb2)
         ENDDO
!
!     CLOSE GM AND RM FILES AND WRITE TRAILER FOR GM
!
         CALL close(Gm,1)
         CALL close(Rn,1)
         CALL wrttrl(mcb2)
         RETURN
!
!     CALL MESSAGE WRITER IF FATAL ERROR DETECTED
!
 20      L = -5
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         L = -16
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(L,Rm,bcd)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mce1d
