!*==degree.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE degree(Ig,Ideg,Jg)
   IMPLICIT NONE
   USE C_BANDS
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     SET UP THE IDEG ARRAY CONTAINING THE DEGREE OF EACH NODE STORED
!     IN THE IG ARRAY.
!     IDEG(I)=DEGREE OF NODE I
!
!     INTEGER          BUNPK
!
   DO i = 1 , Nn
      Ideg(i) = 0
      CALL bunpak(Ig,i,Mm,Jg)
      SPAG_Loop_2_1: DO j = 1 , Mm
!     IF (BUNPK(IG,I,J)) 100,100,50
         IF ( Jg(j)<=0 ) EXIT SPAG_Loop_2_1
         Ideg(i) = Ideg(i) + 1
      ENDDO SPAG_Loop_2_1
   ENDDO
END SUBROUTINE degree
