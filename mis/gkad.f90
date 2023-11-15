
SUBROUTINE gkad
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER App(2) , Form(2) , Ib2pp , Ik2pp , Im2pp , Modacc , Multi , Nobgg , Nok4gg , Nokmgg , Noue , Omit , Single , Type(2)
   REAL G , Ua , Ud , Ue , Uf , Ufe , Ug , Ul , Um , Un , Une , Uo , Up , Ur , Us , Usb , Usg , W3 , W4
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /blank / Type , App , Form , G , W3 , W4 , Ik2pp , Im2pp , Ib2pp , Multi , Single , Omit , Noue , Nok4gg , Nobgg ,        &
                 & Nokmgg , Modacc
!
! Local variable declarations
!
   INTEGER b1dd , b2dd , b2pp , baa , bdd , blck(12) , forc , gm , gmd , go , god , iblock(11) , k1dd , k2dd , k2pp , k41dd , k4aa ,&
         & kaa , kdd , m1dd , m2dd , m2pp , maa , mcb(7) , mdd , modal , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , tran , usetd
   DOUBLE PRECISION block(5)
   REAL xnum
!
! End of declarations
!
!
!     GENERAL K ASSEMBLER DIRECT
!
!     INPUT = 10,  USETD,GM,GO,KAA,BAA,MAA,K4AA,K2PP,M2PP,B2PP
!     OUTPUT = 8,  KDD,BDD,MDD,GMD,GOD,K2DD,M2DD,B2DD
!     SCRATCHES = 6
!     PARAMETERS 3 BCD, 3 REAL, 11 INTERGER
!     - TYPE,APP,FORM, G,W3,W4, NOK2PP,MOM2PP,NOB2PP,MULTI,SINGLE,OMIT,
!       NOUE,NOK4GG,NOBGG,NOKMGG,MODACC
!
!
   EQUIVALENCE (iblock(1),blck(2)) , (block(1),blck(3))
   DATA usetd , gm , go , kaa , baa , maa , k4aa , k2pp , m2pp , b2pp/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 , 110/
   DATA kdd , bdd , mdd , gmd , god , k2dd , m2dd , b2dd/201 , 202 , 203 , 204 , 205 , 206 , 207 , 208/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , scr6/301 , 302 , 303 , 304 , 305 , 306/
   DATA forc , tran , modal/4HFORC , 4HTRAN , 4HMODA/
   DATA block(1) , block(2) , block(4) , block(5) , iblock(1) , iblock(7)/1.0D0 , 0.0D0 , 1.0D0 , 0.0D0 , 2 , 2/
   DATA xnum , mcb/1.0 , 7*0/ , iblock(6)/ - 1/
!
!
   kdd = 201
   bdd = 202
   mdd = 203
   k2dd = 206
   m2dd = 207
   b2dd = 208
   k1dd = 302
   m1dd = 303
   b1dd = 304
   k41dd = 305
   scr3 = 303
   scr4 = 304
   IF ( Noue<=0 ) THEN
!
!     NO E-S A = 1DD
!
      k1dd = kaa
      b1dd = baa
      m1dd = maa
      k41dd = k4aa
   ENDIF
   IF ( Type(1)==tran ) THEN
!
!     TRANSIENT ANALYSIS - SETUP FOR FINAL ADD
!
      IF ( Ik2pp<0 ) k1dd = kdd
      IF ( Im2pp<0 ) m1dd = mdd
      IF ( W3==0.0 ) THEN
         G = 0.0
         W3 = 1.0
      ENDIF
      IF ( W4==0.0 ) THEN
         W4 = 1.0
         xnum = 0.0
      ENDIF
   ELSE
!
!     COMPLEX EIGENVALUE OR FREQUENCY RESPONSE - SET UP FOR FINAL ADD
!
      IF ( Ib2pp<0 ) b1dd = bdd
      IF ( Im2pp<0 ) m1dd = mdd
   ENDIF
   IF ( App(1)/=forc ) THEN
!
!     DISPLACEMENT APPROACH - REDUCE P TO D
!
!     IF MODAL DO NOT MAKE KDD AND BDD
!
      IF ( Form(1)==modal ) THEN
         kdd = 0
         k1dd = 0
         bdd = 0
         b1dd = 0
      ENDIF
      IF ( Noue>=0 ) THEN
!
!     BUILD GMD AND GOD
!
!     M-S PRESENT
!
         IF ( Multi>=0 ) CALL gkad1a(usetd,gm,gmd,scr1,Ue,Un,Une)
!
!     0-S PRESENT
!
         IF ( Omit>=0 ) CALL gkad1a(usetd,go,god,scr1,Ue,Ua,Ud)
      ENDIF
!
      IF ( Multi>=0 .OR. Single>=0 .OR. Omit>=0 ) THEN
         IF ( Im2pp>=0 .OR. Ib2pp>=0 .OR. Ik2pp>=0 ) THEN
!
!     REDUCE 2PP-S TO 2DD-S
!
            CALL gkad1c(gmd,god,scr1,scr2,scr3,scr4,scr5,scr6,usetd)
            IF ( Ik2pp>=0 ) CALL gkad1d(k2pp,k2dd)
            IF ( Im2pp>=0 ) CALL gkad1d(m2pp,m2dd)
            IF ( Ib2pp>=0 ) CALL gkad1d(b2pp,b2dd)
         ENDIF
      ENDIF
      IF ( Form(1)==modal .AND. Modacc<0 ) GOTO 100
!
!     EXPAND AA-S TO DD SET
!
      IF ( Noue>=0 ) CALL gkad1b(usetd,maa,baa,k4aa,m1dd,b1dd,k41dd,Ua,Ue,Ud,scr1)
   ELSE
!
!     FORCE APPROACH P = D
!
      k2dd = k2pp
      b2dd = b2pp
      m2dd = m2pp
   ENDIF
   IF ( Type(1)==tran ) THEN
!
!     TRANSIENT ANALYSIS
!
      iblock(1) = 2
      iblock(7) = 2
      IF ( k1dd/=kdd .AND. Nokmgg>=0 ) CALL ssg2c(k1dd,k2dd,kdd,1,iblock(1))
      IF ( m1dd/=mdd .AND. Nokmgg>=0 ) CALL ssg2c(m1dd,m2dd,mdd,1,iblock(1))
      IF ( b1dd/=bdd ) THEN
         block(1) = G/W3
         block(4) = xnum/W4
         IF ( G/=0.0 .OR. xnum/=0.0 .OR. Nobgg>=0 .OR. Ib2pp>=0 ) THEN
            IF ( Nobgg<0 .AND. Ib2pp<0 ) scr3 = bdd
            CALL ssg2c(k1dd,k41dd,scr3,1,iblock(1))
            IF ( scr3/=bdd ) THEN
               block(1) = 1.0D0
               block(4) = 1.0D0
               CALL ssg2c(b1dd,b2dd,scr5,1,iblock(1))
               CALL ssg2c(scr5,scr3,bdd,1,iblock(1))
            ENDIF
         ENDIF
      ENDIF
   ELSE
!
!     FREQUENCY RESPONSE OR COMPLEX EIGENVALUE
!
      IF ( b1dd/=bdd .AND. Nobgg>=0 .AND. Form(1)/=modal ) CALL ssg2c(b1dd,b2dd,bdd,1,iblock(1))
      IF ( m1dd/=mdd .AND. Nokmgg>=0 ) CALL ssg2c(m1dd,m2dd,mdd,1,iblock(1))
      IF ( k1dd/=kdd .AND. Form(1)/=modal .AND. Nokmgg>=0 ) THEN
         iblock(1) = 4
         block(2) = G
         IF ( Nok4gg<0 ) scr4 = kdd
!
!     DETERMINE IF KDD IS REAL OR IMAGINARY  (COMPLEX EIGEN)
!
         mcb(1) = k2dd
         CALL rdtrl(mcb(1))
         IF ( G==0.0 .AND. Nok4gg<=0 .AND. mcb(5)<=2 ) THEN
            iblock(1) = 2
            iblock(7) = 2
         ENDIF
         CALL ssg2c(k1dd,k2dd,scr4,1,iblock)
         IF ( Nok4gg>=0 ) THEN
            block(1) = 0.0D0
            block(2) = 1.0D0
            CALL ssg2c(k41dd,scr4,kdd,1,iblock(1))
         ENDIF
      ENDIF
   ENDIF
 100  RETURN
END SUBROUTINE gkad
