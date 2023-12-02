!*==mce1d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mce1d
   USE c_blank
   USE c_system
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   USE iso_fortran_env
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
         n = nz - sysbuf
         CALL gopen(rm,z(n+1),0)
         mcb(1) = rm
         CALL rdtrl(mcb)
!
!     FORM -RM
!
         ncol = mcb(2)
         DO k = 1 , ncol
            CALL intpk(*20,rm,0,rdp,0)
            CALL zntpki
            IF ( i/=k ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            zd(k) = -ad(1)
         ENDDO
         CALL close(rm,1)
!
!     OPEN RN MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
!
         CALL gopen(rn,z(n+1),0)
         mcb1(1) = rn
         CALL rdtrl(mcb1)
!
!     SET UP MATRIX CONTROL BLOCK BLOCK FOR GM
!
         CALL makmcb(mcb2,gm,mcb1(3),mcb1(4),ipr)
!
!     OPEN OUTPUT FILE FOR GM AND WRITE HEADER RECORD
!
         n1 = n - sysbuf
         CALL gopen(gm,z(n1+1),1)
!
!     FORM GM = -RM(-1)*RN
!
         ncol1 = mcb1(2)
         DO k = 1 , ncol1
            CALL bldpk(rdp,ipr,gm,0,0)
            CALL intpk(*10,rn,0,rdp,0)
            SPAG_Loop_2_1: DO
               CALL zntpki
               j = i
               bd(1) = ad(1)/zd(j)
               CALL zblpki
               IF ( eol/=0 ) EXIT SPAG_Loop_2_1
            ENDDO SPAG_Loop_2_1
 10         CALL bldpkn(gm,0,mcb2)
         ENDDO
!
!     CLOSE GM AND RM FILES AND WRITE TRAILER FOR GM
!
         CALL close(gm,1)
         CALL close(rn,1)
         CALL wrttrl(mcb2)
         RETURN
!
!     CALL MESSAGE WRITER IF FATAL ERROR DETECTED
!
 20      l = -5
         spag_nextblock_1 = 3
      CASE (2)
         l = -16
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(l,rm,bcd)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mce1d
