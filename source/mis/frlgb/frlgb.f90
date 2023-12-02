!*==frlgb.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE frlgb(Pp,Usetd,Gmd,God,Multi,Single,Omit,Modal,Phidh,Pd,Ps,Ph,Scr1,Scr2,Scr3,Scr4)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_PATX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Pp
   INTEGER :: Usetd
   INTEGER :: Gmd
   INTEGER :: God
   INTEGER :: Multi
   INTEGER :: Single
   INTEGER :: Omit
   INTEGER :: Modal
   INTEGER :: Phidh
   INTEGER :: Pd
   INTEGER :: Ps
   INTEGER :: Ph
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: moda
   INTEGER :: pdbar , pf , pm , pn , pnbar , po
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE REDUCES LOADS FROM P SET TO D SET
!
!     ENTRY POINT - FRRD1B
!                   ======
!
!
   DATA moda/4HMODA/
!
!
   ENTRY frrd1b(Pp,Usetd,Gmd,God,Multi,Single,Omit,Modal,Phidh,Pd,Ps,Ph,Scr1,Scr2,Scr3,Scr4)
!     =============================================================
!
!     SET UP INITIAL VALUES
!
   Nz = korsz(Core)
   Uset = Usetd
   pnbar = Scr2
   pm = Scr3
   pn = Scr4
   pf = Scr2
   pdbar = Scr3
   po = Ph
!
!     REMOVE EACH TYPE OF CONSTRAINT
!
   IF ( Multi<0 ) THEN
!
!     NO M-S
!
      pn = Pp
   ELSE
!
!     REMOVE MULTIPOINT CONSTRAINTS
!
      IF ( Single<0 .AND. Omit<0 ) pn = Pd
      CALL calcv(Scr1,Up,Une,Um,Core(1))
      CALL ssg2a(Pp,pnbar,pm,Scr1)
      CALL ssg2b(Gmd,pm,pnbar,pn,1,1,1,Scr1)
   ENDIF
   IF ( Single<0 ) THEN
!
!     NO SINGLE POINT CONSTRAINTS
!
      pf = pn
   ELSE
!
!     REMOVE SINGLE POINT CONSTRAINTS
!
      IF ( Omit<0 ) pf = Pd
      CALL calcv(Scr1,Une,Ufe,Us,Core(1))
      CALL ssg2a(pn,pf,Ps,Scr1)
   ENDIF
   IF ( Omit<0 ) THEN
      Pd = pf
   ELSE
!
!     REMOVE OMITS
!
      CALL calcv(Scr1,Ufe,Ud,Uo,Core(1))
      CALL ssg2a(pf,pdbar,po,Scr1)
      CALL ssg2b(God,po,pdbar,Pd,1,1,1,Scr1)
   ENDIF
!
!     TRANSFORM TO MODAL COORDINATES
!
   IF ( Modal==moda ) CALL ssg2b(Phidh,Pd,0,Ph,1,1,1,Scr1)
END SUBROUTINE frlgb
