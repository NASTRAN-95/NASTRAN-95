!*==gigtka.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gigtka(Multi,Single,Omit)
   USE c_bitpos
   USE c_gicom
   USE c_patx
   USE c_zzzzzz
   IMPLICIT NONE
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
   lc = korsz(core)
   gkf = gknb
   gks = gkm
   gko = gks
   uset1 = useta
!
!     REDUCE TO N SET IF MULTI POINT CONSTRAINTS
!
   gkn = gkg
   IF ( Multi ) THEN
      IF ( .NOT.Single .AND. .NOT.Omit ) gkn = gka
      CALL calcv(scr1,ug,un,um,core)
      CALL ssg2a(gkg,gknb,gkm,scr1)
      CALL ssg2b(gm,gkm,gknb,gkn,1,1,1,scr1)
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
      IF ( .NOT.Omit ) gkf = gka
      CALL calcv(scr1,un,uf,us,core)
      CALL ssg2a(gkn,gkf,0,scr1)
   ENDIF
   IF ( Omit ) THEN
      CALL calcv(scr1,uf,ua,uo,core)
      CALL ssg2a(gkf,gkab,gko,scr1)
      CALL ssg2b(go,gko,gkab,gka,1,1,1,scr1)
   ENDIF
END SUBROUTINE gigtka
