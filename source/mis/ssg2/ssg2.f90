!*==ssg2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssg2
   USE c_bitpos
   USE c_blank
   USE c_patx
   USE c_system
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: d , gm , go , kfs , pa , pabar , pf , pfbar , pg , pl , pm , pn , pnbar , po , pr , ps , pvect , qr , sr4 ,    &
                   & uset , ys
   INTEGER , DIMENSION(7) :: ia
   INTEGER :: multi , omit , react
   EXTERNAL andf , calcv , korsz , rdtrl , ssg2a , ssg2b
!
! End of declarations rewritten by SPAG
!
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
   lc = korsz(core)
!
!     DECIDE IF MULTI,SINGLE,OMIT,REACT ARE 1 OR ZERO
!
   ia(1) = uset
   uset1 = uset
   CALL rdtrl(ia)
   multi = andf(ia(5),two1(um))
   single = andf(ia(5),two1(us))
   omit = andf(ia(5),two1(uo))
   react = andf(ia(5),two1(ur))
   IF ( react>0 ) THEN
      IF ( multi<=0 .OR. single/=0 .OR. omit/=0 ) THEN
         pf = 201
         pa = 303
      ELSE
         pnbar = 204
         pn = 302
         pr = 303
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   IF ( multi==0 ) THEN
!
      pn = pg
      CALL spag_block_2
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      CALL calcv(Pvect,ug,un,Um,core(1))
      CALL ssg2a(Pg,Pnbar,Pm,Pvect)
      CALL ssg2b(Gm,Pm,Pnbar,Pn,1,iprec,1,Sr4)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( Single/=0 ) THEN
         CALL calcv(Pvect,un,uf,Us,core(1))
         CALL ssg2a(Pn,Pfbar,Ps,Pvect)
         CALL ssg2b(Kfs,Ys,Pfbar,Pf,0,iprec,0,Sr4)
      ELSE
         Pf = Pn
      ENDIF
      IF ( Omit/=0 ) THEN
!
         CALL calcv(Pvect,uf,ua,Uo,core(1))
         CALL ssg2a(Pf,Pabar,Po,Pvect)
         CALL ssg2b(Go,Po,Pabar,Pa,1,iprec,1,Sr4)
      ELSE
         Pa = Pf
      ENDIF
      IF ( React/=0 ) THEN
!
         CALL calcv(Pvect,ua,ul,Ur,core(1))
         CALL ssg2a(Pa,Pl,Pr,Pvect)
         CALL ssg2b(D,Pl,Pr,Qr,1,iprec,-1,Sr4)
      ENDIF
   END SUBROUTINE spag_block_2
END SUBROUTINE ssg2
