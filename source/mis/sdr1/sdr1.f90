!*==sdr1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr1
   USE c_bitpos
   USE c_blank
   USE c_system
   USE c_two
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: dyna , reig
   INTEGER :: gm , go , imulti , iomt , ipvect , ireact , irfno , isav , iscr5 , iscr6 , isng , itran , iua , iuf , iuf1 , iug ,    &
            & ium , iun , iuo , iur , ius , kfs , kss , noue , pg , pgx , ps , qr , qsx , ua , uf , ug , ugvx , ulv , un , uoov ,   &
            & uset , ys
   INTEGER , DIMENSION(7) :: ia
   EXTERNAL andf , rdtrl , sdr1a , sdr1b , sdr1d , ssg2b
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Isys(25),Irfno)
   DATA dyna , reig/4HDYNA , 4HREIG/
!
   ium = 304
   iscr6 = 306
   iur = 0
   iuo = 304
   ipvect = 301
   ius = 304
   uset = 101
   pg = 102
   ulv = 103
   uoov = 104
   ys = 105
   go = 106
   gm = 107
   ps = 108
   kss = 110
   qr = 111
   ugvx = 201
   pgx = 202
   qsx = 203
   kfs = 109
   iua = 302
   iuf = 303
   iun = 302
   iug = 306
   iscr5 = 0
!
!     COPY PG ONTO PGX
!
   CALL sdr1a(pg,pgx)
!
!     SET FLAGS TO CONTROL LOGIC
!
   ia(1) = uset
   CALL rdtrl(ia(1))
   IF ( ia(1)<=0 ) RETURN
   iomt = andf(ia(5),two1(uo))
   noue = andf(ia(5),two1(ue))
   isng = andf(ia(5),two1(us))
   ireact = andf(ia(5),two1(ur))
   imulti = andf(ia(5),two1(um))
   itran = 1
!
!     TEST FOR DYNAMICS OR STATICS
!
   IF ( noue/=0 .OR. itype(1)==dyna ) THEN
!
!     DYNAMICS
!
      ug = up
      un = une
      uf = ufe
      ua = ud
      IF ( iheat/=0 ) itran = 0
      IF ( irfno==9 ) itran = 0
   ELSE
!
!     STATICS
!
      ua = ua1
      uf = uf1
      un = un1
      ug = ug1
   ENDIF
!
!     IF REAL EIGENVALUE,BUCKLING,OR DYNAMICS PROBLEM UR = 0
!
   IF ( itype(1)==dyna .OR. itype(1)==reig ) THEN
!
!     NO REACT
!
!
!     NON STATICS APPROACH
!
      iua = ulv
   ELSEIF ( ireact/=0 ) THEN
!
!     REACTIONS
!
      CALL sdr1b(ipvect,ulv,iur,iua,ua,ul,ur,uset,0,0)
      iscr5 = 305
      IF ( isng/=0 ) THEN
         CALL sdr1b(ipvect,qr,0,iscr5,uf,ur,ul,uset,0,0)
         iug = iua
      ELSE
!
!     REACTS BUT NO SINGLES - MAKE QG
!
         CALL sdr1b(ipvect,qr,0,iscr5,ug,ur,ul,uset,0,0)
         CALL sdr1a(iscr5,qsx)
      ENDIF
   ELSE
      iua = ulv
   ENDIF
   IF ( iomt/=0 ) THEN
!
!     OMITTED POINTS
!
      CALL ssg2b(go,iua,uoov,iuo,0,iprec,1,iscr6)
      CALL sdr1b(ipvect,iua,iuo,iuf,uf,ua,uo,uset,0,0)
      iug = iuf
   ELSE
!
!     NO OMITTED POINTS
!
      isav = iuf
      iuf = iua
      iun = isav
   ENDIF
   IF ( isng/=0 ) THEN
!
!     SINGLE POINT CONSTRAINTS
!
!
!     TEST FOR PRESENCE OF YS VECTOR
!
      ia(1) = ys
      CALL rdtrl(ia(1))
      IF ( ia(1)<0 .OR. ia(6)==0 ) THEN
!
!     NO YS VECTOR
!
         CALL sdr1b(ipvect,iuf,0,iun,un,uf,us,uset,0,0)
         ia(1) = qsx
         CALL rdtrl(ia(1))
         IF ( ia(1)>0 ) THEN
!
!     COMPUTE QS = KFS T*UF
!
            iuf1 = iuf
            IF ( itype(1)==dyna ) THEN
!
!     EXPAND  KFS TO  D SET
!
               IF ( noue/=0 ) THEN
                  CALL sdr1b(ipvect,kfs,0,ius,uf,uf1,ue,uset,0,0)
                  kfs = ius
               ENDIF
            ENDIF
!
!     IF TRANSIENT STRIP VELOCITY AND ACCERERATION FROM IUF
!
            CALL sdr1d(ps,iuf,qsx,itran)
            IF ( itran/=1 ) iuf1 = qsx
            CALL ssg2b(kfs,iuf1,ps,ipvect,1,iprec,2,iscr6)
            IF ( imulti/=0 .AND. ireact/=0 .AND. itype(1)/=dyna .AND. itype(1)/=reig ) THEN
               CALL sdr1b(ius,ipvect,iscr5,iuf,un,us,uf,uset,0,0)
               CALL sdr1b(ius,iuf,0,ipvect,ug,un,um,uset,0,0)
               CALL sdr1a(ipvect,qsx)
            ELSE
               CALL sdr1b(ius,ipvect,iscr5,iscr6,ug,us,uf,uset,0,0)
               CALL sdr1a(iscr6,qsx)
            ENDIF
         ENDIF
      ELSE
         CALL sdr1b(ipvect,iuf,ys,iun,un,uf,us,uset,1,ius)
!
!     IUS CONTAINS EXPANDED YS FROM SPC
!
!
!     IS QS REWUESTED
!
         ia(1) = qsx
         CALL rdtrl(ia(1))
         IF ( ia(1)>0 ) THEN
!
!     COMPUTE QS
!
            CALL ssg2b(kss,ius,ps,ipvect,0,iprec,2,iscr6)
            CALL ssg2b(kfs,iuf,ipvect,ius,1,iprec,1,iscr6)
            IF ( imulti/=0 .AND. ireact/=0 ) THEN
               CALL sdr1b(ipvect,ius,iscr5,iscr6,un,us,uf,uset,0,0)
               CALL sdr1b(ipvect,iscr6,0,ius,ug,un,um,uset,0,0)
               CALL sdr1a(ius,qsx)
            ELSE
               CALL sdr1b(ipvect,ius,iscr5,iscr6,ug,us,uf,uset,0,0)
               CALL sdr1a(iscr6,qsx)
            ENDIF
         ENDIF
      ENDIF
   ELSE
!
!     NO SINGLE POINT CONSTRAINTS
!
      iug = iun
      iun = iuf
   ENDIF
!
   IF ( imulti/=0 ) THEN
!
!     MULTI POINT CONSTRAINTS
!
      iug = iscr6
      CALL ssg2b(gm,iun,0,ium,0,iprec,1,iscr6)
      CALL sdr1b(ipvect,iun,ium,iug,ug,un,um,uset,0,0)
   ELSE
!
!     NO MULTI POINT CONSTRAINTS
!
      iug = iun
   ENDIF
   CALL sdr1a(iug,ugvx)
END SUBROUTINE sdr1
