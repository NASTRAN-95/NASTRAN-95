
SUBROUTINE dgree(Ndstk,Ndeg,Iold,Ibw1,Ipf1,Nu)
   IMPLICIT NONE
   INTEGER Ideg , Idpth , Mm , N , Nn
   COMMON /bandg / N , Idpth , Ideg
   COMMON /bands / Nn , Mm
   INTEGER Ibw1 , Ipf1
   INTEGER Iold(1) , Ndeg(1) , Ndstk(1) , Nu(1)
   INTEGER i , idif , irw , itst , j
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
      DO j = 1 , Ideg
         itst = Nu(j)
         IF ( itst<=0 ) EXIT
         Ndeg(i) = Ndeg(i) + 1
         idif = Iold(i) - Iold(itst)
         IF ( irw<idif ) irw = idif
         Mm = max0(Mm,j)
      ENDDO
      Ipf1 = Ipf1 + irw
      IF ( irw>Ibw1 ) Ibw1 = irw
   ENDDO
   Ideg = Mm
!
!     INCLUDE DIAGONAL TERMS IN BANDWIDTH AND PROFILE
   Ibw1 = Ibw1 + 1
   Ipf1 = Ipf1 + N
END SUBROUTINE dgree