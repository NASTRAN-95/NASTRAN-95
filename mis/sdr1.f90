
SUBROUTINE sdr1
   IMPLICIT NONE
   REAL Append , Two1(32) , Ul , Usb , Usg
   INTEGER Iheat , Iprec , Irfno , Isys(54) , Itype(2) , Ua1 , Ud , Ue , Uf1 , Ufe , Ug1 , Um , Un1 , Une , Uo , Up , Ur , Us
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua1 , Uf1 , Us , Un1 , Ug1 , Ue , Up , Une , Ufe , Ud
   COMMON /blank / Append , Itype
   COMMON /system/ Isys , Iprec , Iheat
   COMMON /two   / Two1
   INTEGER andf
   INTEGER dyna , gm , go , ia(7) , imulti , iomt , ipvect , ireact , isav , iscr5 , iscr6 , isng , itran , iua , iuf , iuf1 , iug ,&
         & ium , iun , iuo , iur , ius , kfs , kss , noue , pg , pgx , ps , qr , qsx , reig , ua , uf , ug , ugvx , ulv , un ,      &
         & uoov , uset , ys
   EXTERNAL andf
!
   EQUIVALENCE (Isys(25),Irfno)
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
   iomt = andf(ia(5),Two1(Uo))
   noue = andf(ia(5),Two1(Ue))
   isng = andf(ia(5),Two1(Us))
   ireact = andf(ia(5),Two1(Ur))
   imulti = andf(ia(5),Two1(Um))
   itran = 1
!
!     TEST FOR DYNAMICS OR STATICS
!
   IF ( noue/=0 .OR. Itype(1)==dyna ) THEN
!
!     DYNAMICS
!
      ug = Up
      un = Une
      uf = Ufe
      ua = Ud
      IF ( Iheat/=0 ) itran = 0
      IF ( Irfno==9 ) itran = 0
   ELSE
!
!     STATICS
!
      ua = Ua1
      uf = Uf1
      un = Un1
      ug = Ug1
   ENDIF
!
!     IF REAL EIGENVALUE,BUCKLING,OR DYNAMICS PROBLEM UR = 0
!
   IF ( Itype(1)==dyna .OR. Itype(1)==reig ) THEN
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
      CALL sdr1b(ipvect,ulv,iur,iua,ua,Ul,Ur,uset,0,0)
      iscr5 = 305
      IF ( isng/=0 ) THEN
         CALL sdr1b(ipvect,qr,0,iscr5,uf,Ur,Ul,uset,0,0)
         iug = iua
      ELSE
!
!     REACTS BUT NO SINGLES - MAKE QG
!
         CALL sdr1b(ipvect,qr,0,iscr5,ug,Ur,Ul,uset,0,0)
         CALL sdr1a(iscr5,qsx)
      ENDIF
   ELSE
      iua = ulv
   ENDIF
   IF ( iomt/=0 ) THEN
!
!     OMITTED POINTS
!
      CALL ssg2b(go,iua,uoov,iuo,0,Iprec,1,iscr6)
      CALL sdr1b(ipvect,iua,iuo,iuf,uf,ua,Uo,uset,0,0)
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
         CALL sdr1b(ipvect,iuf,0,iun,un,uf,Us,uset,0,0)
         ia(1) = qsx
         CALL rdtrl(ia(1))
         IF ( ia(1)>0 ) THEN
!
!     COMPUTE QS = KFS T*UF
!
            iuf1 = iuf
            IF ( Itype(1)==dyna ) THEN
!
!     EXPAND  KFS TO  D SET
!
               IF ( noue/=0 ) THEN
                  CALL sdr1b(ipvect,kfs,0,ius,uf,Uf1,Ue,uset,0,0)
                  kfs = ius
               ENDIF
            ENDIF
!
!     IF TRANSIENT STRIP VELOCITY AND ACCERERATION FROM IUF
!
            CALL sdr1d(ps,iuf,qsx,itran)
            IF ( itran/=1 ) iuf1 = qsx
            CALL ssg2b(kfs,iuf1,ps,ipvect,1,Iprec,2,iscr6)
            IF ( imulti/=0 .AND. ireact/=0 .AND. Itype(1)/=dyna .AND. Itype(1)/=reig ) THEN
               CALL sdr1b(ius,ipvect,iscr5,iuf,un,Us,uf,uset,0,0)
               CALL sdr1b(ius,iuf,0,ipvect,ug,un,Um,uset,0,0)
               CALL sdr1a(ipvect,qsx)
            ELSE
               CALL sdr1b(ius,ipvect,iscr5,iscr6,ug,Us,uf,uset,0,0)
               CALL sdr1a(iscr6,qsx)
            ENDIF
         ENDIF
      ELSE
         CALL sdr1b(ipvect,iuf,ys,iun,un,uf,Us,uset,1,ius)
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
            CALL ssg2b(kss,ius,ps,ipvect,0,Iprec,2,iscr6)
            CALL ssg2b(kfs,iuf,ipvect,ius,1,Iprec,1,iscr6)
            IF ( imulti/=0 .AND. ireact/=0 ) THEN
               CALL sdr1b(ipvect,ius,iscr5,iscr6,un,Us,uf,uset,0,0)
               CALL sdr1b(ipvect,iscr6,0,ius,ug,un,Um,uset,0,0)
               CALL sdr1a(ius,qsx)
            ELSE
               CALL sdr1b(ipvect,ius,iscr5,iscr6,ug,Us,uf,uset,0,0)
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
      CALL ssg2b(gm,iun,0,ium,0,Iprec,1,iscr6)
      CALL sdr1b(ipvect,iun,ium,iug,ug,un,Um,uset,0,0)
   ELSE
!
!     NO MULTI POINT CONSTRAINTS
!
      iug = iun
   ENDIF
   CALL sdr1a(iug,ugvx)
END SUBROUTINE sdr1
