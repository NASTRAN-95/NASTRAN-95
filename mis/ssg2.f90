
SUBROUTINE ssg2
   IMPLICIT NONE
   REAL Core(1) , Dum54(54) , Two1(32) , Usb , Usg
   INTEGER Ibc , Iprec , Lc , N , N4 , No , Single , Ua , Uf , Ug , Ul , Um , Un , Uo , Ur , Us , Uset1
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /blank / Single
   COMMON /patx  / Lc , N , No , N4 , Uset1 , Ibc
   COMMON /system/ Dum54 , Iprec
   COMMON /two   / Two1
   COMMON /zzzzzz/ Core
   INTEGER andf , korsz
   INTEGER d , gm , go , ia(7) , kfs , multi , omit , pa , pabar , pf , pfbar , pg , pl , pm , pn , pnbar , po , pr , ps , pvect ,  &
         & qr , react , sr4 , uset , ys
   EXTERNAL andf
!
!     MULTI  = 0  IMPLIES NO MULTI-POINT CONSTRAINTS PN = PG
!
!     SINGLE = 0  IMPLIES NO SINGLE POINT CONSTRAINTS PF = PN
!
!     OMIT   = 0  IMPLIES NO OMITTED POINTS PA = PF
!
!     REACT  = 0  IMPLIES NO FREE BODY PROBLEM PL = PA
!
!
!
   DATA uset , gm , kfs , go , pnbar , pm , po , pn/101 , 102 , 104 , 105 , 302 , 303 , 202 , 204/
   DATA pfbar , pf , pabar , pa , ps , d , pl , pr/302 , 204 , 302 , 204 , 203 , 106 , 204 , 302/
   DATA qr , pvect , ys , pg , sr4/201 , 301 , 103 , 107 , 304/
!
!
   pnbar = 302
   pn = 204
   pr = 302
   pf = 204
   pa = 204
   Lc = korsz(Core)
!
!     DECIDE IF MULTI,SINGLE,OMIT,REACT ARE 1 OR ZERO
!
   ia(1) = uset
   Uset1 = uset
   CALL rdtrl(ia)
   multi = andf(ia(5),Two1(Um))
   Single = andf(ia(5),Two1(Us))
   omit = andf(ia(5),Two1(Uo))
   react = andf(ia(5),Two1(Ur))
   IF ( react>0 ) THEN
      IF ( multi<=0 .OR. Single/=0 .OR. omit/=0 ) THEN
         pf = 201
         pa = 303
      ELSE
         pnbar = 204
         pn = 302
         pr = 303
         GOTO 100
      ENDIF
   ENDIF
   IF ( multi==0 ) THEN
!
      pn = pg
      GOTO 200
   ENDIF
!
 100  CALL calcv(pvect,Ug,Un,Um,Core(1))
   CALL ssg2a(pg,pnbar,pm,pvect)
   CALL ssg2b(gm,pm,pnbar,pn,1,Iprec,1,sr4)
 200  IF ( Single/=0 ) THEN
      CALL calcv(pvect,Un,Uf,Us,Core(1))
      CALL ssg2a(pn,pfbar,ps,pvect)
      CALL ssg2b(kfs,ys,pfbar,pf,0,Iprec,0,sr4)
   ELSE
      pf = pn
   ENDIF
   IF ( omit/=0 ) THEN
!
      CALL calcv(pvect,Uf,Ua,Uo,Core(1))
      CALL ssg2a(pf,pabar,po,pvect)
      CALL ssg2b(go,po,pabar,pa,1,Iprec,1,sr4)
   ELSE
      pa = pf
   ENDIF
   IF ( react/=0 ) THEN
!
      CALL calcv(pvect,Ua,Ul,Ur,Core(1))
      CALL ssg2a(pa,pl,pr,pvect)
      CALL ssg2b(d,pl,pr,qr,1,Iprec,-1,sr4)
   ENDIF
END SUBROUTINE ssg2