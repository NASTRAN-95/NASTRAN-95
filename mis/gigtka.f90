
SUBROUTINE gigtka(Multi,Single,Omit)
   IMPLICIT NONE
   REAL Bgpt , Cstm , Eqaero , Gsize , Sila , Spline
   INTEGER Core(1) , Gka , Gkab , Gkg , Gkm , Gknb , Gm , Go , Ibc , Ksize , Lc , N , No , Ny , Scr1 , Ua , Uf , Ug , Ul , Um , Un ,&
         & Uo , Ur , Us , Usb , Uset1 , Useta , Usg
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /gicom / Spline , Useta , Cstm , Bgpt , Sila , Eqaero , Gm , Go , Gka , Ksize , Gsize , Scr1 , Gkg , Gknb , Gkm , Gkab
   COMMON /patx  / Lc , N , No , Ny , Uset1 , Ibc
   COMMON /zzzzzz/ Core
   LOGICAL Multi , Omit , Single
   INTEGER gkf , gkn , gko , gks
   INTEGER korsz
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