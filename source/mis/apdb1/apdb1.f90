!*==apdb1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdb1(Ibuf1,Ibuf2,Next,Left,Nstns,Nlines,Xsign,Lcstm,Acstm,Nodex,Nodei,Isilc,Xyzb)
   IMPLICIT NONE
   USE C_APDBUG
   USE C_BITPOS
   USE C_NAMES
   USE C_PATX
   USE C_SYSTEM
   USE C_TWO
   USE C_ZBLPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nstns
   INTEGER :: Ibuf1
   INTEGER :: Ibuf2
   INTEGER :: Next
   INTEGER :: Left
   INTEGER :: Nlines
   REAL :: Xsign
   INTEGER :: Lcstm
   REAL , DIMENSION(1) :: Acstm
   INTEGER , DIMENSION(1) :: Nodex
   INTEGER , DIMENSION(1) :: Nodei
   INTEGER , DIMENSION(1) :: Isilc
   REAL , DIMENSION(4,Nstns) :: Xyzb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: gkab , gkf , gkm , gkn , gknb , gko , gks , gm , go , gsize , gtka , gtkg , icol , isil , ncs , nline , nst , scr1 ,  &
            & scr2
   INTEGER , DIMENSION(7) :: idata , itrl , tgkg
   LOGICAL , SAVE :: multi , omit , single
   REAL , DIMENSION(7) :: rdata
   REAL :: rl1 , rl2 , uset , xbmxa , ybmya , zbmza
   REAL , DIMENSION(3,3) :: ta
   REAL , DIMENSION(3) :: tbl , tbla
   REAL , DIMENSION(1) :: z
   EXTERNAL andf , bldpk , bldpkn , bug1 , calcv , close , fread , gmmats , gopen , korsz , pretrs , rdtrl , ssg2a , ssg2b ,        &
          & transs , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
!
!     GENERATE GTKA TRANSFORMATION MATRIX
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
   CALL gopen(scr1,z(Ibuf1),Rdrew)
   CALL gopen(gtkg,z(Ibuf2),Wrtrew)
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
      xbmxa = Xyzb(2,Nstns) - Xyzb(2,1)
      ybmya = Xyzb(3,Nstns) - Xyzb(3,1)
      zbmza = Xyzb(4,Nstns) - Xyzb(4,1)
      rl1 = sqrt(xbmxa*xbmxa+ybmya*ybmya+zbmza*zbmza)
      rl2 = sqrt(rl1*rl1-zbmza*zbmza)
      tbl(1) = -Xsign*(ybmya/rl2)
      tbl(2) = Xsign*(xbmxa/rl2)
      tbl(3) = 0.0
      IF ( Debug ) CALL bug1('MAT-TBL   ',15,tbl,3)
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
            Ii = isil
            Ap(1) = tbla(icol)
            IF ( Debug ) CALL bug1('ISIL      ',28,isil,1)
            IF ( Debug ) CALL bug1('MAT-AP    ',29,Ap,1)
            CALL zblpki
            isil = isil + 1
         ENDDO
         CALL bldpkn(gtkg,0,tgkg)
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
END SUBROUTINE apdb1
