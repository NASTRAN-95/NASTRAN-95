!*==scat.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scat(Kg,Ncon,Inv,Ii3,Norig)
   IMPLICIT NONE
   USE C_BANDB
   USE C_BANDS
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii3
   INTEGER , DIMENSION(1) :: Kg
   INTEGER :: Ncon
   INTEGER , DIMENSION(Ii3,2) :: Inv
   INTEGER , DIMENSION(1) :: Norig
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , loc , nold
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS ROUTINE USES SCATTER SORT TECHNIQUES FOR EACH GRID POINT
!     ENCOUNTERED TO DETERMINE WHETHER OR NOT THE POINT HAS BEEN SEEN
!     BEFORE.   IF NOT, INV, NORIG, AND NN ARE UPDATED.
!
!     INV(I,1) CONTAINS AN ORIGINAL GRID POINT NUMBER
!     INV(I,2) CONTAINS THE INTERNAL NUMBER ASSIGNED TO IT (BEFORE SORT)
!
!
   IF ( Ncon<1 ) RETURN
   DO i = 1 , Ncon
      nold = Kg(i)
      IF ( nold/=0 ) THEN
         loc = nold - 1
         SPAG_Loop_2_1: DO
            loc = mod(loc,Kmod) + 1
            IF ( Inv(loc,1)==0 ) THEN
               Inv(loc,1) = nold
               Nn = Nn + 1
               IF ( Nn>Maxgrd ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
               Norig(Nn) = nold
               Inv(loc,2) = Nn
               EXIT SPAG_Loop_2_1
            ELSEIF ( Inv(loc,1)==nold ) THEN
               EXIT SPAG_Loop_2_1
            ENDIF
         ENDDO SPAG_Loop_2_1
         Kg(i) = Inv(loc,2)
      ENDIF
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      Ngrid = -1
   END SUBROUTINE spag_block_1
END SUBROUTINE scat
