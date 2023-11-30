
SUBROUTINE matgpr
   IMPLICIT NONE
   REAL A(4) , Filter , Head(96) , Raddeg
   INTEGER Core(1) , Ia(4) , Ibits(32) , Ichar(17) , Idum(2) , Ieol , Ieor , Iflflg , Ii , Iiset(2) , Inx(6) , Inx1(2) , Ipopt(2) , &
         & Kkset(2) , Label(96) , Line , Nlpp , Otpe , Sysbuf , Two1(32)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /bitpos/ Ibits , Ichar
   COMMON /blank / Iiset , Kkset , Ipopt , Filter , Iflflg
   COMMON /condas/ Idum , Raddeg
   COMMON /output/ Head , Label
   COMMON /system/ Sysbuf , Otpe , Inx , Nlpp , Inx1 , Line
   COMMON /two   / Two1
   COMMON /xmssg / Ufm , Uwm
   COMMON /zntpkx/ Ia , Ii , Ieol , Ieor
   COMMON /zzzzzz/ Core
   REAL amag , prbufx(5) , value , xxbuf(15)
   INTEGER andf , korsz
   INTEGER blank , comps(6) , exid , extra , gpl , head2(32) , hset , i , iallp , ibegn , ibuf , icmpx , icomp , ie , iend , ifin , &
         & iflag , ihset , iksil , im(7) , in , inlopt , inull , iout , ipb , ipb1 , ipbc , iprbf(4) , iset , isil , iuset , jc ,   &
         & jj , k , kk , kset , ksil , kuset , l , lcore , lgpl , loop , loops , lsil , luset , mask , mask1 , matrx , muset ,      &
         & name(2) , ncol , nline , null , prbuf(15) , prbufc(5) , scalar , sil , tycomp , uset
   EXTERNAL andf
!
!     DMAP FOR MATGPR MODULE
!
!     MATGPR    GPL,USET,SIL,KFS//C,N,F/C,N,S/C,N,PRTOPT/
!                                 C,N,FILTER/C,N,FLTRFLAG $
!
!     THIS MODULE ENHANCED BY P.R.PAMIDI/RPK CORPORATION, 3/1988
!
   !>>>>EQUIVALENCE (xxbuf(1),prbuf(1))
   !>>>>EQUIVALENCE (prbufc(1),prbufx(1))
   !>>>>EQUIVALENCE (Ia(1),A(1))
   DATA gpl , uset , sil , matrx/101 , 102 , 103 , 104/
   DATA scalar , comps , nline/4H S   , 4HT1   , 4HT2   , 4HT3   , 4HR1   , 4HR2   , 4HR3   , 15/
   DATA name/4HMATG , 4HPR  /
   DATA null/4HNULL/
   DATA blank , extra , hset/4H     , 4H E   , 4H H  /
   DATA ihset/4HH   /
   DATA iallp/4HALLP/
   DATA head2/4H     , 4HPOIN , 4HT    , 4H     , 4H   V , 4HALUE , 4H     , 4H POI , 4HNT   , 4H     , 4H     , 4HVALU , 4HE    ,  &
       &4H  P0 , 4HINT  , 4H     , 4H     , 4H VAL , 4HUE   , 4H   P , 4HOINT , 4H     , 4H     , 4H  VA , 4HLUE  , 4H     ,        &
      & 4H POI , 4HNT   , 4H     , 4H     , 4HVALU , 4HE   /
!
!
   iset = Iiset(1)
   kset = Kkset(1)
   inlopt = 0
   IF ( Ipopt(1)==null ) inlopt = 1
   IF ( Filter/=0.0 ) THEN
      iflag = 1
      IF ( Filter<0.0 ) iflag = 2
      IF ( Iflflg/=0 ) iflag = iflag + 2
   ENDIF
   im(1) = matrx
   CALL rdtrl(im(1))
   IF ( im(1)<0 ) GOTO 2000
!
!     CONVERT BCD TO BIT POSITION IN USET
!
   DO i = 1 , 32
      IF ( Ichar(i)==iset ) GOTO 100
   ENDDO
   IF ( iset/=ihset ) THEN
      WRITE (Otpe,99001) Uwm , Iiset
99001 FORMAT (A25,', UNKNOWN SET ',2A4,' SPECIFIED FOR THE FIRST PARA','METER OF THE MATGPR MODULE.  MODULE NOT EXECUTED.')
      RETURN
   ELSE
      iset = -1
      GOTO 200
   ENDIF
!
 100  iset = Ibits(i)
 200  DO i = 1 , 32
      IF ( Ichar(i)==kset ) GOTO 300
   ENDDO
   kset = iset
   GOTO 400
 300  kset = Ibits(i)
 400  lcore = korsz(Core) - Sysbuf
   ibuf = lcore + 1
   IF ( iset+kset==-2 ) THEN
!
!     NSET ONLY  NO GPL,USET,  ETC.
!
      lgpl = 0
      luset = 0
      lsil = 0
      iuset = 1
      isil = 1
      GOTO 800
   ELSE
      CALL gopen(gpl,Core(ibuf),0)
      CALL read(*2500,*500,gpl,Core(1),lcore,0,lgpl)
      CALL close(gpl,1)
      GOTO 2900
   ENDIF
 500  CALL close(gpl,1)
   lcore = lcore - lgpl
   CALL gopen(uset,Core(ibuf),0)
   iuset = lgpl + 1
   CALL read(*2700,*600,uset,Core(lgpl+1),lcore,0,luset)
   CALL close(uset,1)
   GOTO 2900
 600  CALL close(uset,1)
   lcore = lcore - luset
   CALL gopen(sil,Core(ibuf),0)
   isil = lgpl + luset + 1
   CALL read(*2800,*700,sil,Core(isil),lcore,0,lsil)
   CALL close(sil,1)
   GOTO 2900
 700  CALL close(sil,1)
   k = isil + lsil
   lcore = lcore - lsil - 1
   Core(k) = luset + 1
!
!     LOAD HEADER FOR PAGES
!
   lsil = lsil + 1
 800  DO i = 1 , 96
      Label(i) = blank
   ENDDO
   DO i = 1 , 32
      k = 32 + i
      Label(k) = head2(i)
   ENDDO
   ncol = im(2)
   CALL fname(matrx,Label(4))
   CALL gopen(matrx,Core(ibuf),0)
   ie = Ibits(12)
   inull = 0
   loop = 0
   icmpx = 1
   IF ( im(5)>2 ) icmpx = 3
   IF ( iset/=-1 ) mask = Two1(iset)
   IF ( kset/=-1 ) mask1 = Two1(kset)
   muset = 0
   jc = 0
   iksil = 1
   l = 1
   ASSIGN 1400 TO iout
   CALL page
!
!     START LOOP ON EACH COLUMN
!
 900  loop = loop + 1
   CALL intpk(*2100,matrx,0,icmpx,0)
   IF ( inull/=0 ) THEN
      ifin = loop - 1
      inull = 0
      GOTO 2200
   ENDIF
 1000 IF ( inlopt==1 ) THEN
      CALL fwdrec(*3000,matrx)
      GOTO 1900
   ENDIF
!
!     CHECK FOR HSET
!
 1100 DO WHILE ( iset/=-1 )
      IF ( muset==loop ) THEN
!
!     JC IS INDEX OF NON-ZERO IN G SET-- SOOK UP SIL
!
         DO WHILE ( iksil/=lsil+1 )
            kk = isil + iksil
            IF ( jc<Core(kk) ) THEN
               icomp = jc - Core(kk-1) + 1
               IF ( icomp/=1 ) THEN
                  tycomp = comps(icomp)
!
!     CHECK FOR SCALAR POINT
!
               ELSEIF ( Core(kk)-Core(kk-1)>1 ) THEN
                  tycomp = comps(icomp)
               ELSE
                  tycomp = scalar
!
!     CHECK FOR EXTRA
!
                  kk = lgpl + jc
                  IF ( andf(Core(kk),Two1(ie))/=0 ) tycomp = extra
               ENDIF
               exid = Core(iksil)
               iprbf(l+1) = tycomp
               iprbf(l) = exid
               GOTO 1300
            ELSE
               iksil = iksil + 1
            ENDIF
         ENDDO
         EXIT
      ELSE
         DO
            jc = jc + 1
            IF ( jc>luset ) GOTO 1200
            kk = lgpl + jc
            IF ( andf(Core(kk),mask)/=0 ) THEN
!
!     FOUND COLUMN IN USET
!
               muset = muset + 1
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!     COLUMN NOT IN USET
!
 1200 iprbf(l) = loop
   iprbf(l+1) = hset
 1300 GOTO iout
 1400 WRITE (Otpe,99002) loop , iprbf(1) , iprbf(2)
99002 FORMAT ('0COLUMN',I8,2H (,I8,1H-,A2,2H).)
   Line = Line + 2
   IF ( Line>=Nlpp ) CALL page
   jj = 0
   kuset = 0
   ksil = 1
   ipb = 1
   ipbc = 1
   iend = 0
 1500 DO WHILE ( Ieol==0 )
      CALL zntpki
!
!     CHECK FILTER
!
      IF ( Filter/=0.0 ) THEN
!
!     FILTER IS NON-ZERO
!
         value = A(1)
         IF ( icmpx==3 ) value = sqrt(A(1)*A(1)+A(2)*A(2))
         IF ( iflag==2 ) THEN
            IF ( abs(value)>abs(Filter) ) CYCLE
         ELSEIF ( iflag==3 ) THEN
            IF ( value<Filter .AND. value>0.0 ) CYCLE
         ELSEIF ( iflag==4 ) THEN
            IF ( value>Filter .AND. value<0.0 ) CYCLE
!
         ELSEIF ( abs(value)<Filter ) THEN
            CYCLE
         ENDIF
      ENDIF
!
!     CHECK FOR HSET
!
      IF ( kset/=-1 ) THEN
!
!     LOOK UP ROW IN USET
!
         DO WHILE ( kuset<=luset+1 )
            IF ( kuset==Ii ) THEN
!
!     JJ IS INDEX OF NON-ZERO IN G SET - NOW SEARCH SIL FOR JJ
!
               DO WHILE ( ksil/=lsil+1 )
                  kk = isil + ksil
                  IF ( jj<Core(kk) ) THEN
                     icomp = jj - Core(kk-1) + 1
                     IF ( icomp/=1 ) THEN
                        tycomp = comps(icomp)
                        exid = Core(ksil)
!
!     CHECK FOR SCALAR POINT
!
                     ELSEIF ( Core(kk)-Core(kk-1)>1 ) THEN
                        tycomp = comps(icomp)
                        exid = Core(ksil)
                     ELSE
                        tycomp = scalar
!
!     CHECK FOR EXTRA POINT
!
                        kk = lgpl + jj
!
!     EXTRA POINT
!
                        IF ( andf(Core(kk),Two1(ie))/=0 ) tycomp = extra
                        exid = Core(ksil)
                     ENDIF
                     GOTO 1600
                  ELSE
                     ksil = ksil + 1
                  ENDIF
               ENDDO
               GOTO 3000
            ELSE
               DO
                  jj = jj + 1
!
!     PROTECT AGINST NO BITPOS OR NO USET
!
                  IF ( jj>luset ) GOTO 1550
                  kk = lgpl + jj
                  IF ( andf(Core(kk),mask1)/=0 ) THEN
!
!     FOUND ELEMENT IN USET
!
                     kuset = kuset + 1
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         GOTO 2900
      ENDIF
!
!     H POINT
!
 1550 tycomp = hset
      exid = Ii
      GOTO 1600
   ENDDO
!
!     END OF COLUMN
!
   iend = 1
   IF ( ipb/=1 ) GOTO 1800
   GOTO 1900
 1600 IF ( ipb>=nline ) GOTO 1800
 1700 prbuf(ipb) = exid
   prbuf(ipb+1) = tycomp
   IF ( icmpx/=1 ) THEN
      IF ( Ipopt(1)==iallp ) THEN
         amag = sqrt(A(1)*A(1)+A(2)*A(2))
         IF ( amag/=0.0 ) THEN
            A(2) = atan2(A(2),A(1))*Raddeg
            IF ( A(2)<-0.00005 ) A(2) = A(2) + 360.0
            A(1) = amag
         ENDIF
      ENDIF
   ENDIF
   prbuf(ipb+2) = Ia(1)
   prbufc(ipbc) = Ia(2)
   ipbc = ipbc + 1
   ipb = ipb + 3
   GOTO 1500
 1800 ipb1 = ipb - 1
   ipbc = ipbc - 1
   WRITE (Otpe,99003) (prbuf(i),prbuf(i+1),xxbuf(i+2),i=1,ipb1,3)
99003 FORMAT (5X,5(1X,I8,1X,1A2,1X,1P,E12.5))
   Line = Line + 1
   IF ( icmpx/=1 ) THEN
      WRITE (Otpe,99004) (prbufx(i),i=1,ipbc)
99004 FORMAT (5X,5(13X,1P,E12.5))
      WRITE (Otpe,99005)
99005 FORMAT (1H )
      Line = Line + 2
   ENDIF
   ipbc = 1
   ipb = 1
   IF ( Line>=Nlpp ) CALL page
   IF ( iend/=1 ) GOTO 1700
 1900 IF ( loop/=ncol ) GOTO 900
   IF ( inull/=0 ) THEN
      ifin = loop
      GOTO 2200
   ELSE
      CALL close(matrx,1)
   ENDIF
 2000 RETURN
!
 2100 IF ( inull==0 ) THEN
      inull = 1
      ibegn = loop
   ENDIF
   GOTO 1900
 2200 loops = loop
   loop = ibegn
   ASSIGN 2300 TO iout
   GOTO 1100
 2300 l = 3
   loop = ifin
   ASSIGN 2400 TO iout
   GOTO 1100
 2400 ASSIGN 1400 TO iout
   l = 1
   loop = loops
   WRITE (Otpe,99006) ibegn , iprbf(1) , iprbf(2) , ifin , iprbf(3) , iprbf(4)
99006 FORMAT ('0COLUMNS',I8,2H (,I8,1H-,A2,6H) THRU,I8,2H (,I8,1H-,A2,11H) ARE NULL.)
   Line = Line + 2
   IF ( Line>=Nlpp ) CALL page
   IF ( ifin/=ncol ) GOTO 1000
   CALL close(matrx,1)
   GOTO 2000
 2500 in = gpl
 2600 CALL mesage(-2,in,name)
 2700 in = uset
   GOTO 2600
 2800 in = sil
   GOTO 2600
 2900 CALL mesage(8,0,name)
   CALL close(matrx,1)
   GOTO 2000
 3000 CALL mesage(7,0,name)
   CALL close(matrx,1)
   GOTO 2000
END SUBROUTINE matgpr