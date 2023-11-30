
SUBROUTINE gi
   IMPLICIT NONE
   INTEGER Bagpdt , Cstm , Ecta , Gm , Go , Gsize , Gtka , Ksize , Ng , Nk , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sila , Spline ,     &
         & Two1(32) , Ua , Uf , Ug , Ul , Um , Un , Uo , Ur , Us , Usb , Useta , Usg
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /blank / Nk , Ng
   COMMON /gicom / Spline , Useta , Cstm , Bagpdt , Sila , Ecta , Gm , Go , Gtka , Ksize , Gsize , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /two   / Two1
   INTEGER andf
   INTEGER ia(7)
   LOGICAL multi , omit , single
   EXTERNAL andf
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