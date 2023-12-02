!*==gkad.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gkad
   USE c_bitpos
   USE c_blank
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: b1dd , k1dd , k41dd , m1dd
   INTEGER , SAVE :: b2dd , b2pp , baa , bdd , forc , gm , gmd , go , god , k2dd , k2pp , k4aa , kaa , kdd , m2dd , m2pp , maa ,    &
                   & mdd , modal , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , tran , usetd
   REAL(REAL64) , DIMENSION(5) , SAVE :: block
   INTEGER , DIMENSION(11) , SAVE :: iblock
   INTEGER , DIMENSION(7) , SAVE :: mcb
   REAL , SAVE :: xnum
   EXTERNAL gkad1a , gkad1b , gkad1c , gkad1d , rdtrl , ssg2c
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (iblock(1),blck(2)) , (block(1),blck(3))
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
   IF ( noue<=0 ) THEN
!
!     NO E-S A = 1DD
!
      k1dd = kaa
      b1dd = baa
      m1dd = maa
      k41dd = k4aa
   ENDIF
   IF ( type(1)==tran ) THEN
!
!     TRANSIENT ANALYSIS - SETUP FOR FINAL ADD
!
      IF ( ik2pp<0 ) k1dd = kdd
      IF ( im2pp<0 ) m1dd = mdd
      IF ( w3==0.0 ) THEN
         g = 0.0
         w3 = 1.0
      ENDIF
      IF ( w4==0.0 ) THEN
         w4 = 1.0
         xnum = 0.0
      ENDIF
   ELSE
!
!     COMPLEX EIGENVALUE OR FREQUENCY RESPONSE - SET UP FOR FINAL ADD
!
      IF ( ib2pp<0 ) b1dd = bdd
      IF ( im2pp<0 ) m1dd = mdd
   ENDIF
   IF ( app(1)/=forc ) THEN
!
!     DISPLACEMENT APPROACH - REDUCE P TO D
!
!     IF MODAL DO NOT MAKE KDD AND BDD
!
      IF ( form(1)==modal ) THEN
         kdd = 0
         k1dd = 0
         bdd = 0
         b1dd = 0
      ENDIF
      IF ( noue>=0 ) THEN
!
!     BUILD GMD AND GOD
!
!     M-S PRESENT
!
         IF ( multi>=0 ) CALL gkad1a(usetd,gm,gmd,scr1,ue,un,une)
!
!     0-S PRESENT
!
         IF ( omit>=0 ) CALL gkad1a(usetd,go,god,scr1,ue,ua,ud)
      ENDIF
!
      IF ( multi>=0 .OR. single>=0 .OR. omit>=0 ) THEN
         IF ( im2pp>=0 .OR. ib2pp>=0 .OR. ik2pp>=0 ) THEN
!
!     REDUCE 2PP-S TO 2DD-S
!
            CALL gkad1c(gmd,god,scr1,scr2,scr3,scr4,scr5,scr6,usetd)
            IF ( ik2pp>=0 ) CALL gkad1d(k2pp,k2dd)
            IF ( im2pp>=0 ) CALL gkad1d(m2pp,m2dd)
            IF ( ib2pp>=0 ) CALL gkad1d(b2pp,b2dd)
         ENDIF
      ENDIF
      IF ( form(1)==modal .AND. modacc<0 ) RETURN
!
!     EXPAND AA-S TO DD SET
!
      IF ( noue>=0 ) CALL gkad1b(usetd,maa,baa,k4aa,m1dd,b1dd,k41dd,ua,ue,ud,scr1)
   ELSE
!
!     FORCE APPROACH P = D
!
      k2dd = k2pp
      b2dd = b2pp
      m2dd = m2pp
   ENDIF
   IF ( type(1)==tran ) THEN
!
!     TRANSIENT ANALYSIS
!
      iblock(1) = 2
      iblock(7) = 2
      IF ( k1dd/=kdd .AND. nokmgg>=0 ) CALL ssg2c(k1dd,k2dd,kdd,1,iblock(1))
      IF ( m1dd/=mdd .AND. nokmgg>=0 ) CALL ssg2c(m1dd,m2dd,mdd,1,iblock(1))
      IF ( b1dd/=bdd ) THEN
         block(1) = g/w3
         block(4) = xnum/w4
         IF ( g/=0.0 .OR. xnum/=0.0 .OR. nobgg>=0 .OR. ib2pp>=0 ) THEN
            IF ( nobgg<0 .AND. ib2pp<0 ) scr3 = bdd
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
      IF ( b1dd/=bdd .AND. nobgg>=0 .AND. form(1)/=modal ) CALL ssg2c(b1dd,b2dd,bdd,1,iblock(1))
      IF ( m1dd/=mdd .AND. nokmgg>=0 ) CALL ssg2c(m1dd,m2dd,mdd,1,iblock(1))
      IF ( k1dd/=kdd .AND. form(1)/=modal .AND. nokmgg>=0 ) THEN
         iblock(1) = 4
         block(2) = g
         IF ( nok4gg<0 ) scr4 = kdd
!
!     DETERMINE IF KDD IS REAL OR IMAGINARY  (COMPLEX EIGEN)
!
         mcb(1) = k2dd
         CALL rdtrl(mcb(1))
         IF ( g==0.0 .AND. nok4gg<=0 .AND. mcb(5)<=2 ) THEN
            iblock(1) = 2
            iblock(7) = 2
         ENDIF
         CALL ssg2c(k1dd,k2dd,scr4,1,iblock)
         IF ( nok4gg>=0 ) THEN
            block(1) = 0.0D0
            block(2) = 1.0D0
            CALL ssg2c(k41dd,scr4,kdd,1,iblock(1))
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE gkad
