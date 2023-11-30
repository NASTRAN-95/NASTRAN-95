
SUBROUTINE trlgb(Usetd,Ap,Gmd,God,Phidh,As,Ad,Ah,Iflag1,Scr1,Scr2,Scr3,Scr4)
   IMPLICIT NONE
   INTEGER Iprec , Iskip(54) , Iz(1) , N1 , N2 , N3 , Nz , Um , Uo , Us , Uset1
   REAL Two1(32) , Ua , Ud , Ue , Uf , Ufe , Ug , Ul , Un , Une , Up , Ur , Usb , Usg
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /patx  / Nz , N1 , N2 , N3 , Uset1
   COMMON /system/ Iskip , Iprec
   COMMON /two   / Two1
   COMMON /zzzzzz/ Iz
   INTEGER Ad , Ah , Ap , As , Gmd , God , Iflag1 , Phidh , Scr1 , Scr2 , Scr3 , Scr4 , Usetd
   INTEGER adbar , af , am , an , anbar , ao , mcb(7) , modal , multi , omit , prec , sign , single , trnsp
   INTEGER andf , korsz
   EXTERNAL andf
!
!     THE PURPOSE OF THIS ROUTINE IS TO REDUCE THE SCALE FACTOR MATRIX
!     AP TO  A TRANS FORMATION MATRIX  AS, AD, AH
!
!     INPUTS (5)
!         USETD
!         AP     SCALE MATRIX --P SIZE
!         GMD    M- SET TRASNFORMATION MATRIX
!         GOD    0- SET TRASNFORMATION MATRIX
!         PHIDH  H- SET TRASNFORMATION MATRIX
!
!     OUTPUTS(3)
!         AS     SCALE MATRIX --S SET
!         AD     SCALE MATRIX --D SET
!         AH     SCALE MATRIX --H SET
!
!     NOTE  IFLAG1 WILL BE SET  TO -1  IF  AP = AD (N0 M,S,O)
!
!
!
!
   anbar = Scr2
   am = Scr3
   an = Scr4
   af = Scr2
   adbar = Scr3
   ao = Scr4
!
!     SET FLAGS FOR PRESCENCE OF SETS
!
   mcb(1) = Usetd
   CALL rdtrl(mcb)
   Uset1 = Usetd
   multi = andf(mcb(5),Two1(Um))
   single = andf(mcb(5),Two1(Us))
   omit = andf(mcb(5),Two1(Uo))
   modal = 0
   mcb(1) = Phidh
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) modal = 1
   Nz = korsz(Iz)
   sign = 1
   trnsp = 1
   prec = Iprec
!
!     REMOVE EACH CONSTRAINT
!
   IF ( multi==0 ) THEN
!
!     NO MULTI-POINT CONSTRAINTS
!
      an = Ap
   ELSE
      IF ( single==0 .AND. omit==0 ) an = Ad
      CALL calcv(Scr1,Up,Une,Um,Iz)
      CALL ssg2a(Ap,anbar,am,Scr1)
      CALL ssg2b(Gmd,am,anbar,an,trnsp,prec,sign,Scr1)
   ENDIF
!
!     REMOVE SINGLES
!
   IF ( single==0 ) THEN
!
!     NO SINGLES
!
      af = an
   ELSE
      IF ( omit==0 ) af = Ad
      CALL calcv(Scr1,Une,Ufe,Us,Iz)
      CALL ssg2a(an,af,As,Scr1)
   ENDIF
   IF ( omit==0 ) THEN
!
!     NO OMITS
!
      Ad = af
   ELSE
!
!     REMOVE OMITS
!
      CALL calcv(Scr1,Ufe,Ud,Uo,Iz)
      IF ( af==ao ) ao = Scr2
      CALL ssg2a(af,adbar,ao,Scr1)
      CALL ssg2b(God,ao,adbar,Ad,trnsp,prec,sign,Scr1)
   ENDIF
!
!     REMOVE TO H SET
!
   IF ( modal==0 ) CALL ssg2b(Phidh,Ad,0,Ah,trnsp,prec,sign,Scr1)
   Iflag1 = multi + single + omit
   IF ( Iflag1==0 ) Iflag1 = -1
END SUBROUTINE trlgb