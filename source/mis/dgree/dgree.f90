!*==dgree.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dgree(Ndstk,Ndeg,Iold,Ibw1,Ipf1,Nu)
   IMPLICIT NONE
   USE C_BANDG
   USE C_BANDS
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ndstk
   INTEGER , DIMENSION(1) :: Ndeg
   INTEGER , DIMENSION(1) :: Iold
   INTEGER :: Ibw1
   INTEGER :: Ipf1
   INTEGER , DIMENSION(1) :: Nu
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , idif , irw , itst , j
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!     DGREE COMPUTES THE DEGREE OF EACH NODE IN NDSTK AND STORES
!     IT IN THE ARRAY NDEG.  THE BANDWIDTH AND PROFILE FOR THE ORIGINAL
!     OR INPUT RENUMBERING OF THE GRAPH IS COMPUTED ALSO.
!
!     COMPUTE MAXIMUM DEGREE MM AND STORE IN IDEG.
!
!     INTEGER          BUNPK
!
   Ibw1 = 0
   Ipf1 = 0
   Ideg = Mm
   Mm = 0
   DO i = 1 , N
      Ndeg(i) = 0
      irw = 0
      CALL bunpak(Ndstk,i,Ideg,Nu)
      SPAG_Loop_2_1: DO j = 1 , Ideg
         itst = Nu(j)
         IF ( itst<=0 ) EXIT SPAG_Loop_2_1
         Ndeg(i) = Ndeg(i) + 1
         idif = Iold(i) - Iold(itst)
         IF ( irw<idif ) irw = idif
         Mm = max0(Mm,j)
      ENDDO SPAG_Loop_2_1
      Ipf1 = Ipf1 + irw
      IF ( irw>Ibw1 ) Ibw1 = irw
   ENDDO
   Ideg = Mm
!
!     INCLUDE DIAGONAL TERMS IN BANDWIDTH AND PROFILE
   Ibw1 = Ibw1 + 1
   Ipf1 = Ipf1 + N
END SUBROUTINE dgree
