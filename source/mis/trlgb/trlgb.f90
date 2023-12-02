!*==trlgb.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trlgb(Usetd,Ap,Gmd,God,Phidh,As,Ad,Ah,Iflag1,Scr1,Scr2,Scr3,Scr4)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_PATX
   USE C_SYSTEM
   USE C_TWO
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Usetd
   INTEGER :: Ap
   INTEGER :: Gmd
   INTEGER :: God
   INTEGER :: Phidh
   INTEGER :: As
   INTEGER :: Ad
   INTEGER :: Ah
   INTEGER :: Iflag1
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: adbar , af , am , an , anbar , ao , modal , multi , omit , prec , sign , single , trnsp
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL andf , calcv , korsz , rdtrl , ssg2a , ssg2b
!
! End of declarations rewritten by SPAG
!
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
