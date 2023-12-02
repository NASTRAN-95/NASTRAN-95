!*==feer2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer2(Iret)
USE C_FEERCX
USE C_FEERXX
USE C_NAMES
USE C_OPINV
USE C_SFACT
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iret
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   EXTERNAL korsz , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FEER2 INITIALIZES THEN CALLS  SDCOMP
!
!
!
         Iret = 0
!
         Filea(1) = Iflelm(1)
         Filel(1) = Iflvec(1)
         Fileu(1) = Sr3fle
         Isr1fl = Sr4fle
         Isr2fl = Sr5fle
         Isr3fl = Sr6fle
         Ichl = 0
         IF ( Ibk==1 .OR. Ifset==1 ) Ichl = 1
         Filea(2) = Ifkaa(2)
         Filea(3) = Ifkaa(3)
         Filea(4) = Ifkaa(4)
         Filea(5) = Prec
         Filea(6) = 0
         Filea(7) = 0
         Filel(5) = Prec
!
!     SYMMETRIC DECOMPOSITION
!
         Nz = korsz(Z)
         CALL sdcomp(*20,Z,Z,Z)
         spag_nextblock_1 = 2
      CASE (2)
         Filel(3) = Filel(2)
         Filel(4) = Lowtri
         CALL wrttrl(Filel)
         DO i = 1 , 7
            Mcblt(i) = Filel(i)
         ENDDO
         RETURN
!
 20      Iret = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE feer2
