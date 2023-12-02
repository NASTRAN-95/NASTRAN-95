!*==gigtka.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gigtka(Multi,Single,Omit)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_GICOM
   USE C_PATX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: Multi
   LOGICAL :: Single
   LOGICAL :: Omit
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: gkf , gkn , gko , gks
   EXTERNAL calcv , korsz , ssg2a , ssg2b
!
! End of declarations rewritten by SPAG
!
!
!
!
!-----------------------------------------------------------------------
!
   Lc = korsz(Core)
   gkf = Gknb
   gks = Gkm
   gko = gks
   Uset1 = Useta
!
!     REDUCE TO N SET IF MULTI POINT CONSTRAINTS
!
   gkn = Gkg
   IF ( Multi ) THEN
      IF ( .NOT.Single .AND. .NOT.Omit ) gkn = Gka
      CALL calcv(Scr1,Ug,Un,Um,Core)
      CALL ssg2a(Gkg,Gknb,Gkm,Scr1)
      CALL ssg2b(Gm,Gkm,Gknb,gkn,1,1,1,Scr1)
   ENDIF
!
!     PARTITION INTO F SET IF SINGLE POINT CONSTRAINTS
!
   IF ( .NOT.Single ) THEN
!
!     REDUCE TO A SET IF OMITS
!
      gkf = gkn
   ELSE
      IF ( .NOT.Omit ) gkf = Gka
      CALL calcv(Scr1,Un,Uf,Us,Core)
      CALL ssg2a(gkn,gkf,0,Scr1)
   ENDIF
   IF ( Omit ) THEN
      CALL calcv(Scr1,Uf,Ua,Uo,Core)
      CALL ssg2a(gkf,Gkab,gko,Scr1)
      CALL ssg2b(Go,gko,Gkab,Gka,1,1,1,Scr1)
   ENDIF
END SUBROUTINE gigtka
