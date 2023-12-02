!*==cyct2b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cyct2b(Input,Outpt,Ncol,Iz,Mcb)
   IMPLICIT NONE
   USE C_PACKX
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Outpt
   INTEGER :: Ncol
   INTEGER , DIMENSION(4) :: Iz
   INTEGER , DIMENSION(7) :: Mcb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , izero
   REAL , SAVE :: zero
   EXTERNAL pack , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THE PURPOSE OF THIS SUBROUTINE IS TO COPY NCOL COLUMNS FROM
!     INPUT TO OUTPUT USING CORE AT IZ -- MCB IS THE TRAILER
!
   !>>>>EQUIVALENCE (zero,izero)
   DATA zero/0.0/
!
!
   Ita = iabs(Itc)
   Itb = Ita
   Incr = Incr1
   DO i = 1 , Ncol
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            Iik = 0
            CALL unpack(*10,Input,Iz)
            Ii = Iik
            Jj = Jjk
            spag_nextblock_1 = 2
         CASE (2)
            CALL pack(Iz,Outpt,Mcb)
            CYCLE
!
!     NULL COLUMN
!
 10         Ii = 1
            Jj = 1
            Iz(1) = izero
            Iz(2) = izero
            Iz(3) = izero
            Iz(4) = izero
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
END SUBROUTINE cyct2b
