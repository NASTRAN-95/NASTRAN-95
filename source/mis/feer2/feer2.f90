!*==feer2.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer2(Iret)
   USE c_feercx
   USE c_feerxx
   USE c_names
   USE c_opinv
   USE c_sfact
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
         filea(1) = iflelm(1)
         filel(1) = iflvec(1)
         fileu(1) = sr3fle
         isr1fl = sr4fle
         isr2fl = sr5fle
         isr3fl = sr6fle
         ichl = 0
         IF ( ibk==1 .OR. ifset==1 ) ichl = 1
         filea(2) = ifkaa(2)
         filea(3) = ifkaa(3)
         filea(4) = ifkaa(4)
         filea(5) = prec
         filea(6) = 0
         filea(7) = 0
         filel(5) = prec
!
!     SYMMETRIC DECOMPOSITION
!
         nz = korsz(z)
         CALL sdcomp(*20,z,z,z)
         spag_nextblock_1 = 2
      CASE (2)
         filel(3) = filel(2)
         filel(4) = lowtri
         CALL wrttrl(filel)
         DO i = 1 , 7
            mcblt(i) = filel(i)
         ENDDO
         RETURN
!
 20      Iret = 1
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE feer2
