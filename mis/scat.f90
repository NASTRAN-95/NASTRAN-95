
SUBROUTINE scat(Kg,Ncon,Inv,Ii3,Norig)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum3(3) , Dum3b(3)
   INTEGER Isys , Kmod , Maxdeg , Maxgrd , Ngrid , Nn , Nout
   COMMON /bandb / Dum3b , Ngrid
   COMMON /bands / Nn , Dum3 , Maxgrd , Maxdeg , Kmod
   COMMON /system/ Isys , Nout
!
! Dummy argument declarations
!
   INTEGER Ii3 , Ncon
   INTEGER Inv(Ii3,2) , Kg(1) , Norig(1)
!
! Local variable declarations
!
   INTEGER i , loc , nold
!
! End of declarations
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
         DO
            loc = mod(loc,Kmod) + 1
            IF ( Inv(loc,1)==0 ) THEN
               Inv(loc,1) = nold
               Nn = Nn + 1
               IF ( Nn>Maxgrd ) GOTO 100
               Norig(Nn) = nold
               Inv(loc,2) = Nn
               EXIT
            ELSEIF ( Inv(loc,1)==nold ) THEN
               EXIT
            ENDIF
         ENDDO
         Kg(i) = Inv(loc,2)
      ENDIF
   ENDDO
   RETURN
!
 100  Ngrid = -1
END SUBROUTINE scat
