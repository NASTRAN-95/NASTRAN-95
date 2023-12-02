!*==gi.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gi
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_GICOM
   USE C_TWO
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) , SAVE :: ia
   LOGICAL , SAVE :: multi , omit , single
   EXTERNAL andf , giggks , gigtka , gigtkg , gipsst , rdtrl
!
! End of declarations rewritten by SPAG
!
!
   DATA single/.TRUE./ , multi/.TRUE./ , omit/.TRUE./
   DATA ia/7*0/
!
   Spline = 101
   Useta = 102
   Cstm = 103
   Bagpdt = 104
   Sila = 105
   Ecta = 106
   Gm = 107
   Go = 108
   Gtka = 201
   Ksize = Nk
   Gsize = Ng
   IF ( Gsize<=0 ) THEN
      ia(1) = Sila
      CALL rdtrl(ia)
      Gsize = ia(3)
   ENDIF
   Scr1 = 301
   Scr2 = 302
   Scr3 = 303
   Scr4 = 304
   Scr5 = 305
   CALL giggks
   ia(1) = Useta
   CALL rdtrl(ia)
   IF ( andf(ia(5),Two1(Um))==0 ) multi = .FALSE.
   IF ( andf(ia(5),Two1(Us))==0 ) single = .FALSE.
   IF ( andf(ia(5),Two1(Uo))==0 ) omit = .FALSE.
   IF ( .NOT.(multi .OR. single .OR. omit) ) Scr2 = Gtka
   CALL gigtkg
   CALL gipsst
   IF ( multi .OR. single .OR. omit ) CALL gigtka(multi,single,omit)
END SUBROUTINE gi
