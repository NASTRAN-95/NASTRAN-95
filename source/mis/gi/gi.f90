!*==gi.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gi
   USE c_bitpos
   USE c_blank
   USE c_gicom
   USE c_two
   IMPLICIT NONE
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
   spline = 101
   useta = 102
   cstm = 103
   bagpdt = 104
   sila = 105
   ecta = 106
   gm = 107
   go = 108
   gtka = 201
   ksize = nk
   gsize = ng
   IF ( gsize<=0 ) THEN
      ia(1) = sila
      CALL rdtrl(ia)
      gsize = ia(3)
   ENDIF
   scr1 = 301
   scr2 = 302
   scr3 = 303
   scr4 = 304
   scr5 = 305
   CALL giggks
   ia(1) = useta
   CALL rdtrl(ia)
   IF ( andf(ia(5),two1(um))==0 ) multi = .FALSE.
   IF ( andf(ia(5),two1(us))==0 ) single = .FALSE.
   IF ( andf(ia(5),two1(uo))==0 ) omit = .FALSE.
   IF ( .NOT.(multi .OR. single .OR. omit) ) scr2 = gtka
   CALL gigtkg
   CALL gipsst
   IF ( multi .OR. single .OR. omit ) CALL gigtka(multi,single,omit)
END SUBROUTINE gi
