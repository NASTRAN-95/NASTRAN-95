
SUBROUTINE apdb2(Ibuf1,Ibuf2,Next,Left,Nstns,Nlines,Xsign,Lcstm,Acstm,Nodex,Nodei,Isilc,Xyzb)
   IMPLICIT NONE
   REAL Ap(4) , Z(1)
   INTEGER Clsrew , Core(1) , Ibc(7) , Ii , Iprec , Itwo(32) , Iz(1) , Ksystm(54) , Lc , N , No , Ny , Rd , Rdrew , Ua , Uf , Ug ,  &
         & Ul , Um , Un , Uo , Ur , Us , Usb , Uset1 , Usg , Wrt , Wrtrew
   LOGICAL Debug
   COMMON /apdbug/ Debug
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /patx  / Lc , N , No , Ny , Uset1 , Ibc
   COMMON /system/ Ksystm , Iprec
   COMMON /two   / Itwo
   COMMON /zblpkx/ Ap , Ii
   COMMON /zzzzzz/ Core
   INTEGER Ibuf1 , Ibuf2 , Lcstm , Left , Next , Nlines , Nstns
   REAL Xsign
   REAL Acstm(1) , Xyzb(4,Nstns)
   INTEGER Isilc(1) , Nodei(1) , Nodex(1)
   INTEGER andf , korsz
   INTEGER gkab , gkf , gkm , gkn , gknb , gko , gks , gm , go , gsize , gtka , gtkg , icol , idata(7) , isil , itrl(7) , ncs ,     &
         & ndeg , nline , nloop , nn , nst , scr1 , scr2 , tgkg(7)
   LOGICAL multi , omit , single
   REAL rdata(7) , ta(3,3) , tbl(3) , tbla(3) , tblr(3) , tblt(3) , uset
   EXTERNAL andf
!
!     GENERATE GTKA TRANSFORMATION MATRIX FOR SWEPT TURBOPROP
!     BLADES (AERODYNAMIC THEORY NUMBER 7).
!
   !>>>>EQUIVALENCE (Z(1),Core(1))
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (idata(1),rdata(1))
   DATA single , multi , omit/.TRUE. , .TRUE. , .TRUE./
!
   uset = 102
   gm = 106
   go = 107
   gtka = 204
   scr1 = 301
   scr2 = 302
   gknb = 303
   gkm = 304
   gkab = 305
   itrl(1) = uset
   CALL rdtrl(itrl)
   gsize = itrl(3)
   IF ( andf(itrl(5),Itwo(Um))==0 ) multi = .FALSE.
   IF ( andf(itrl(5),Itwo(Us))==0 ) single = .FALSE.
   IF ( andf(itrl(5),Itwo(Uo))==0 ) omit = .FALSE.
   IF ( .NOT.(multi .OR. single .OR. omit) ) scr2 = gtka
   gtkg = scr2
!
!     OPEN SCR1 TO READ BLADE NODE DATA
!
!                         T
!     OPEN SCR2 TO WRITE G   MATRIX OF ORDER (GSIZE X KSIZE)
!                         KG
!
   CALL gopen(scr1,Z(Ibuf1),Rdrew)
   CALL gopen(gtkg,Z(Ibuf2),Wrtrew)
   tgkg(1) = gtkg
   tgkg(2) = 0
   tgkg(3) = gsize
   tgkg(4) = 2
   tgkg(5) = 1
   tgkg(6) = 0
   tgkg(7) = 0
!
!     SET-UP CALL TO TRANSS VIA PRETRS
!
   IF ( Lcstm>0 ) CALL pretrs(Acstm,Lcstm)
!
!     LOOP ON STREAMLINES
!
   DO nline = 1 , Nlines
!
!     READ STREAMLINE NODE DATA FROM SCR1
!
      DO nst = 1 , Nstns
         CALL fread(scr1,idata,7,0)
         IF ( Debug ) CALL bug1('SCR1 IDATA',10,idata,7)
         Nodex(nst) = idata(1)
         Nodei(nst) = idata(2)
         Isilc(nst) = idata(3)
         Xyzb(1,nst) = rdata(4)
         Xyzb(2,nst) = rdata(5)
         Xyzb(3,nst) = rdata(6)
         Xyzb(4,nst) = rdata(7)
      ENDDO
!
!     GENERATE BASIC TO LOCAL TRANSFORMATION MATRIX FOR THIS STREAMLINE
!
      CALL apdb2a(Nlines,nline,scr1,Nstns,Xsign,Xyzb(2,1),Xyzb(2,Nstns),tblt,tblr)
!
!     SET TRANSFORMATION TO TRANSLATION FIRST
!
      DO nn = 1 , 3
         tbl(nn) = tblt(nn)
      ENDDO
!
!     LOOP FOR TRANSLATION THEN ROTATION
!
      ndeg = 0
      DO nloop = 1 , 2
         IF ( Debug ) CALL bug1('MAT-TBL   ',18,tbl,3)
!
!     LOOP ON COMPUTING STATIONS
!
         DO ncs = 1 , Nstns
!
!     LOCATE GLOBAL TO BASIC TRANSFORMATION MATRIX
!
            rdata(1) = Xyzb(1,ncs)
            IF ( Lcstm==0 .OR. idata(1)==0 ) THEN
               tbla(1) = tbl(1)
               tbla(2) = tbl(2)
               tbla(3) = tbl(3)
            ELSE
               CALL transs(Xyzb(1,ncs),ta)
               CALL gmmats(tbl,1,3,0,ta,3,3,0,tbla)
            ENDIF
            IF ( Debug ) CALL bug1('MAT-TBLA  ',25,tbla,3)
!
!     COMPUTE LOCATION IN G-SET USING SIL
!     KODE = 1 FOR GRID POINT
!     KODE = 2 FOR SCALAR POINT (NOT ALLOWED, CHECK WAS MADE BY APDB)
!
            isil = Isilc(ncs)/10
            CALL bldpk(1,1,gtkg,0,0)
!
!     OUTPUT GKG(TRANSPOSE) = GTKG
!     II IS ROW POSITION
!
            DO icol = 1 , 3
               Ii = isil + ndeg
               Ap(1) = tbla(icol)
               IF ( Debug ) CALL bug1('ISIL      ',28,isil,1)
               IF ( Debug ) CALL bug1('MAT-AP    ',29,Ap,1)
               CALL zblpki
               isil = isil + 1
            ENDDO
            CALL bldpkn(gtkg,0,tgkg)
         ENDDO
!
!     CHANGE BASIC TO LOCAL TRANSFORMATION TO ROTATION
!
         DO nn = 1 , 3
            tbl(nn) = tblr(nn)
         ENDDO
         ndeg = 3
      ENDDO
   ENDDO
   CALL close(scr1,Clsrew)
   CALL close(gtkg,Clsrew)
   CALL wrttrl(tgkg)
!
!     CREATE GTKA MATRIX
!
   IF ( multi .OR. single .OR. omit ) THEN
      Lc = korsz(Core)
      gkf = gknb
      gks = gkm
      gko = gks
      Uset1 = uset
!
!     REDUCE TO N-SET IF MULTI POINT CONSTRAINTS
!
      gkn = gtkg
      IF ( multi ) THEN
         IF ( .NOT.single .AND. .NOT.omit ) gkn = gtka
         CALL calcv(scr1,Ug,Un,Um,Core)
         CALL ssg2a(gtkg,gknb,gkm,scr1)
         CALL ssg2b(gm,gkm,gknb,gkn,1,Iprec,1,scr1)
      ENDIF
!
!     PARTITION INTO F-SET IF SINGLE POINT CONSTRAINTS
!
      IF ( .NOT.single ) THEN
!
!     REDUCE TO A-SET IF OMITS
!
         gkf = gkn
      ELSE
         IF ( .NOT.omit ) gkf = gtka
         CALL calcv(scr1,Un,Uf,Us,Core)
         CALL ssg2a(gkn,gkf,0,scr1)
      ENDIF
      IF ( omit ) THEN
         CALL calcv(scr1,Uf,Ua,Uo,Core)
         CALL ssg2a(gkf,gkab,gko,scr1)
         CALL ssg2b(go,gko,gkab,gtka,1,Iprec,1,scr1)
      ENDIF
   ENDIF
END SUBROUTINE apdb2