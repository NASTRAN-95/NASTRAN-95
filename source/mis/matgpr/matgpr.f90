!*==matgpr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE matgpr
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_CONDAS
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: a
   REAL :: amag , value
   INTEGER , SAVE :: blank , extra , gpl , hset , iallp , ihset , matrx , nline , null , scalar , sil , uset
   INTEGER , DIMENSION(6) , SAVE :: comps
   INTEGER :: exid , i , ibegn , ibuf , icmpx , icomp , ie , iend , ifin , iflag , iksil , in , inlopt , inull , iout , ipb , ipb1 ,&
            & ipbc , iset , isil , iuset , jc , jj , k , kk , kset , ksil , kuset , l , lcore , lgpl , loop , loops , lsil , luset ,&
            & mask , mask1 , muset , ncol , tycomp
   INTEGER , DIMENSION(32) , SAVE :: head2
   INTEGER , DIMENSION(7) :: im
   INTEGER , DIMENSION(4) :: iprbf
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(15) :: prbuf
   INTEGER , DIMENSION(5) :: prbufc
   REAL , DIMENSION(5) :: prbufx
   REAL , DIMENSION(15) :: xxbuf
   EXTERNAL andf , close , fname , fwdrec , gopen , intpk , korsz , mesage , page , rdtrl , read , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         IF ( im(1)<0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CONVERT BCD TO BIT POSITION IN USET
!
         DO i = 1 , 32
            IF ( Ichar(i)==iset ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         IF ( iset/=ihset ) THEN
            WRITE (Otpe,99001) Uwm , Iiset
99001       FORMAT (A25,', UNKNOWN SET ',2A4,' SPECIFIED FOR THE FIRST PARA','METER OF THE MATGPR MODULE.  MODULE NOT EXECUTED.')
            RETURN
         ELSE
            iset = -1
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (2)
!
         iset = Ibits(i)
         spag_nextblock_1 = 3
      CASE (3)
         DO i = 1 , 32
            IF ( Ichar(i)==kset ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         kset = iset
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         kset = Ibits(i)
         spag_nextblock_1 = 5
      CASE (5)
         lcore = korsz(Core) - Sysbuf
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
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL gopen(gpl,Core(ibuf),0)
            CALL read(*160,*20,gpl,Core(1),lcore,0,lgpl)
            CALL close(gpl,1)
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      CALL close(gpl,1)
         lcore = lcore - lgpl
         CALL gopen(uset,Core(ibuf),0)
         iuset = lgpl + 1
         CALL read(*180,*40,uset,Core(lgpl+1),lcore,0,luset)
         CALL close(uset,1)
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(uset,1)
         lcore = lcore - luset
         CALL gopen(sil,Core(ibuf),0)
         isil = lgpl + luset + 1
         CALL read(*200,*60,sil,Core(isil),lcore,0,lsil)
         CALL close(sil,1)
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(sil,1)
         k = isil + lsil
         lcore = lcore - lsil - 1
         Core(k) = luset + 1
!
!     LOAD HEADER FOR PAGES
!
         lsil = lsil + 1
         spag_nextblock_1 = 6
      CASE (6)
         DO i = 1 , 96
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
         ASSIGN 80 TO iout
         CALL page
         spag_nextblock_1 = 7
      CASE (7)
!
!     START LOOP ON EACH COLUMN
!
         loop = loop + 1
         CALL intpk(*100,matrx,0,icmpx,0)
         IF ( inull/=0 ) THEN
            ifin = loop - 1
            inull = 0
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         IF ( inlopt==1 ) THEN
            CALL fwdrec(*220,matrx)
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     CHECK FOR HSET
!
         SPAG_Loop_1_1: DO WHILE ( iset/=-1 )
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
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     iksil = iksil + 1
                  ENDIF
               ENDDO
               EXIT SPAG_Loop_1_1
            ELSE
               SPAG_Loop_2_2: DO
                  jc = jc + 1
                  IF ( jc>luset ) EXIT SPAG_Loop_1_1
                  kk = lgpl + jc
                  IF ( andf(Core(kk),mask)/=0 ) THEN
!
!     FOUND COLUMN IN USET
!
                     muset = muset + 1
                     EXIT SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     COLUMN NOT IN USET
!
         iprbf(l) = loop
         iprbf(l+1) = hset
         spag_nextblock_1 = 10
      CASE (10)
         GOTO iout
 80      WRITE (Otpe,99002) loop , iprbf(1) , iprbf(2)
99002    FORMAT ('0COLUMN',I8,2H (,I8,1H-,A2,2H).)
         Line = Line + 2
         IF ( Line>=Nlpp ) CALL page
         jj = 0
         kuset = 0
         ksil = 1
         ipb = 1
         ipbc = 1
         iend = 0
         spag_nextblock_1 = 11
      CASE (11)
         DO WHILE ( Ieol==0 )
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL zntpki
!
!     CHECK FILTER
!
                  IF ( Filter/=0.0 ) THEN
!
!     FILTER IS NON-ZERO
!
                     value = a(1)
                     IF ( icmpx==3 ) value = sqrt(a(1)*a(1)+a(2)*a(2))
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
                                 spag_nextblock_1 = 12
                                 CYCLE SPAG_DispatchLoop_1
                              ELSE
                                 ksil = ksil + 1
                              ENDIF
                           ENDDO
                           GOTO 220
                        ELSE
                           SPAG_Loop_3_3: DO
                              jj = jj + 1
!
!     PROTECT AGINST NO BITPOS OR NO USET
!
                              IF ( jj>luset ) THEN
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                              kk = lgpl + jj
                              IF ( andf(Core(kk),mask1)/=0 ) THEN
!
!     FOUND ELEMENT IN USET
!
                                 kuset = kuset + 1
                                 EXIT SPAG_Loop_3_3
                              ENDIF
                           ENDDO SPAG_Loop_3_3
                        ENDIF
                     ENDDO
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
!
!     H POINT
!
                  tycomp = hset
                  exid = Ii
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
!     END OF COLUMN
!
         iend = 1
         IF ( ipb==1 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         IF ( ipb>=nline ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         prbuf(ipb) = exid
         prbuf(ipb+1) = tycomp
         IF ( icmpx/=1 ) THEN
            IF ( Ipopt(1)==iallp ) THEN
               amag = sqrt(a(1)*a(1)+a(2)*a(2))
               IF ( amag/=0.0 ) THEN
                  a(2) = atan2(a(2),a(1))*Raddeg
                  IF ( a(2)<-0.00005 ) a(2) = a(2) + 360.0
                  a(1) = amag
               ENDIF
            ENDIF
         ENDIF
         prbuf(ipb+2) = Ia(1)
         prbufc(ipbc) = Ia(2)
         ipbc = ipbc + 1
         ipb = ipb + 3
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         ipb1 = ipb - 1
         ipbc = ipbc - 1
         WRITE (Otpe,99003) (prbuf(i),prbuf(i+1),xxbuf(i+2),i=1,ipb1,3)
99003    FORMAT (5X,5(1X,I8,1X,1A2,1X,1P,E12.5))
         Line = Line + 1
         IF ( icmpx/=1 ) THEN
            WRITE (Otpe,99004) (prbufx(i),i=1,ipbc)
99004       FORMAT (5X,5(13X,1P,E12.5))
            WRITE (Otpe,99005)
99005       FORMAT (1H )
            Line = Line + 2
         ENDIF
         ipbc = 1
         ipb = 1
         IF ( Line>=Nlpp ) CALL page
         IF ( iend/=1 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
         IF ( loop/=ncol ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( inull/=0 ) THEN
            ifin = loop
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL close(matrx,1)
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
         RETURN
!
 100     IF ( inull==0 ) THEN
            inull = 1
            ibegn = loop
         ENDIF
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
         loops = loop
         loop = ibegn
         ASSIGN 120 TO iout
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 120     l = 3
         loop = ifin
         ASSIGN 140 TO iout
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 140     ASSIGN 80 TO iout
         l = 1
         loop = loops
         WRITE (Otpe,99006) ibegn , iprbf(1) , iprbf(2) , ifin , iprbf(3) , iprbf(4)
99006    FORMAT ('0COLUMNS',I8,2H (,I8,1H-,A2,6H) THRU,I8,2H (,I8,1H-,A2,11H) ARE NULL.)
         Line = Line + 2
         IF ( Line>=Nlpp ) CALL page
         IF ( ifin/=ncol ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(matrx,1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 160     in = gpl
         spag_nextblock_1 = 18
      CASE (18)
         CALL mesage(-2,in,name)
 180     in = uset
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 200     in = sil
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
      CASE (19)
         CALL mesage(8,0,name)
         CALL close(matrx,1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 220     CALL mesage(7,0,name)
         CALL close(matrx,1)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE matgpr
