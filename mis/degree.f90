
SUBROUTINE degree(Ig,Ideg,Jg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Mm , Nn
   COMMON /bands / Nn , Mm
!
! Dummy argument declarations
!
   INTEGER Ideg(1) , Ig(1) , Jg(1)
!
! Local variable declarations
!
   INTEGER i , j
!
! End of declarations
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
      DO j = 1 , Mm
!     IF (BUNPK(IG,I,J)) 100,100,50
         IF ( Jg(j)<=0 ) EXIT
         Ideg(i) = Ideg(i) + 1
      ENDDO
   ENDDO
END SUBROUTINE degree
