!*==cidck.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cidck(Z,Buf,Nopen)
!
!     BULK DATA CARD COORDINATE CHECK
!     THIS ROUTINE IS CALLED ONLY BY IFP, IN LINK1
!
!     WRITTEN BY G.CHAN/UNISYS   9/1989
!
!     LIST OF NASTRAN BULK DATA CARDS THAT REFERENCE COORDINATE CID -
!
!     BULK DATA      CID      NO. OF     GINO         LOCATE
!     CARD           FIELD    WORDS      FILE         INDEX
!     ----------   -------   -------   ---------   ------------
!     AXIF            1          1       AXIC         8815,88
!     BFIELD          1          2       GEOM1        3101,31
!     CEMLOOP*        13        13       GEOM3        3109,31
!     CONM2           3         13       GEOM2        1501,15
!     CORD1C          1          6       GEOM1        1701,17
!     CORD1R          1          6       GEOM1        1801,18
!     CORD1S          1          6       GEOM1        1901,19
!     CORD2C          1,4       13       GEOM1        2001,20
!     CORD2R          1,4       13       GEOM1        2101,21
!     CORD2S          1,4       13       GEOM1        2201,22
!     FORCE           3          7       GEOM3        4201,42
!     GEMLOOP*        3          -       GEOM3        3309,33
!     GRAV            2          6       GEOM3        4401,44
!     GRID/GRDSET     2,6        8       GEOM1        4501,45
!     GRIDB           3          5       GEOM1        8115,81
!     MDIPOLE*        2         10       GEOM3        3509,35
!     MOMENT          3          7       GEOM3        4801,48
!     PIHEX           3          7       EPT          7002,70
!     PLOAD4          9         12       GEOM3        6709,67
!     REMFLUX*        2          -       GEOM3        3409,34
!WKBR 2/95 SPR94015 RFORCE    3     8       GEOM3        5509,55
!     RFORCE          3          7       GEOM3        5509,55
!     SPCFLD*         2          -       GEOM3        3209,32
!
!     * THE CID'S ON THESE CARDS CURRENTLY MUST BE ZERO OR BLANK, AND
!       WERE CHECKED ALREADY IN IFS4P. THEREFORE THEY ARE NOT CHECKED
!       HERE.
!
   USE c_system
   USE c_two
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Buf
   INTEGER :: Nopen
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: axic , ept , geom1 , geom2 , geom3 , pcd
   INTEGER , DIMENSION(2) , SAVE :: axif , bfield , conm2 , cord , force , grav , grid , gridb , moment , name , pihex , pload4 ,   &
                                  & rforce
   CHARACTER(7) , SAVE :: caxif , cbfiel , cconm2 , ccord2 , cforce , cgrav , cgrid , cgridb , cmment , cpihex , cplod4 , crforc ,  &
                        & pcc
   CHARACTER(7) :: cc
   INTEGER :: cid , file , i , ib , ic , ie , ii , im , irtn , j , jrtn , k , krtn , l , m , ncord , nrid , nz , nzx , pvcid
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL andf , close , locate , mesage , page2 , preloc , rdtrl , read , sort
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA geom1 , geom2 , geom3 , ept/201 , 208 , 209 , 202/ , axic , name , pcd , pcc/215 , 4HCIDC , 2HK  , 0 , 'XXXX   '/
   DATA axif , caxif/8815 , 88 , 'AXIF   '/bfield , cbfiel/3101 , 31 , 'BFIELD '/ , conm2 , cconm2/1501 , 15 , 'CONM2  '/cord ,     &
      & ccord2/1601 , 16 , 'CORD2  '/ , force , cforce/4201 , 42 , 'FORCE  '/grav , cgrav/4401 , 44 , 'GRAV   '/ , grid ,           &
      & cgrid/4501 , 45 , 'GRID   '/gridb , cgridb/8115 , 81 , 'GRIDB  '/ , moment , cmment/4801 , 48 , 'MOMENT '/pihex ,           &
      & cpihex/7002 , 70 , 'PIHEX  '/ , pload4 , cplod4/6709 , 67 , 'PLOAD4 '/rforce , crforc/5509 , 55 , 'RFORCE '/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     OPEN GEOM1 AND SAVE ALL COORDINATE IDS IN Z(1) THRU Z(NCORD)
!     AND REFERENCED COORD ID IN Z(NRID) THRU Z(NOPEN). NOPEN IS
!     LENGTH OF THE AVAILABLE OPEN CORE.
!     SORT AND CHECK ID UNIQUENESS
!
         ncord = 1
         nrid = Nopen
         file = geom1
         CALL preloc(*99999,Buf,geom1)
         k = 6
         DO i = 1 , 6
            cord(1) = cord(1) + 100
            cord(2) = cord(2) + 1
            IF ( i==4 ) k = 13
            CALL locate(*20,Buf,cord(1),m)
            DO
               CALL read(*780,*20,geom1,Z(ncord),k,0,m)
               ncord = ncord + 1
               IF ( i>=4 .AND. Z(ncord+2)/=0 ) THEN
                  Z(nrid) = Z(ncord+2)
                  nrid = nrid - 1
               ENDIF
            ENDDO
 20      ENDDO
         ncord = ncord - 1
         nrid = nrid + 1
         IF ( ncord>1 ) THEN
            CALL sort(0,0,1,1,Z(1),ncord)
            j = 1
            DO i = 2 , ncord
               IF ( Z(i)/=Z(i-1) ) THEN
                  j = j + 1
                  Z(j) = Z(i)
               ELSE
                  CALL page2(-2)
                  WRITE (nout,99001) ufm , Z(i)
99001             FORMAT (A23,' 328, DUPLICATE COORDINATE ID',I9)
               ENDIF
            ENDDO
            ncord = j
         ENDIF
!
!     IF CORD2C/R/S CARDS ARE PRESENT, CHECK REFERENCE COORDINATE ID
!
         IF ( nrid<=Nopen ) THEN
            cc = ccord2
            SPAG_Loop_1_1: DO i = nrid , Nopen
               cid = Z(i)
               IF ( ncord>0 ) THEN
                  DO j = 1 , ncord
                     IF ( cid==Z(j) ) CYCLE SPAG_Loop_1_1
                  ENDDO
               ENDIF
               CALL page2(-2)
               WRITE (nout,99002) ufm , cid , cc
               abort = .TRUE.
            ENDDO SPAG_Loop_1_1
         ENDIF
!
!     DOUBLE THE COORDINATE ID ARRAY FOR 'CIRCULAR' SEARCH, AND MOVE
!     THE ARRAY TO HIGH END OF OPEN CORE, Z(II) THRU Z(NOPEN-1)
!
         ii = Nopen - 2*ncord - 1
         IF ( ncord/=0 ) THEN
            DO i = 1 , ncord
               Z(ii+i) = Z(i)
               Z(ii+i+ncord) = Z(i)
            ENDDO
         ENDIF
         nz = ii
         im = ii + ncord
         ii = ii + 1
         Z(Nopen) = -999
!
!     CHECK CID ON GRID CARDS
!
         cc = cgrid
         CALL locate(*100,Buf,grid(1),m)
         nzx = (nz/8)*8
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*780,*40,geom1,Z(1),nzx,0,m)
         m = nzx
         IF ( m<=0 ) GOTO 100
 40      pvcid = 0
         ASSIGN 60 TO irtn
         i = -6
 60      SPAG_Loop_1_2: DO
            i = i + 8
            IF ( i>m ) THEN
               pvcid = 0
               ASSIGN 80 TO irtn
               i = -2
               EXIT SPAG_Loop_1_2
            ELSE
               cid = Z(i)
               IF ( cid/=0 .AND. cid/=pvcid ) THEN
!
!
!     INTERNAL ROUTINE TO LOOK FOR CID MATCH
!     CID ARRAY (DOUBLE) IS AT HIGH END OF CORE, Z(II) THRU Z(NOPEN)
!
                  pvcid = cid
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
 80      SPAG_Loop_1_3: DO
            i = i + 8
            IF ( i>m ) THEN
               IF ( m/=nzx ) EXIT SPAG_Loop_1_3
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               cid = Z(i)
               IF ( cid/=0 .AND. cid/=pvcid ) THEN
                  pvcid = cid
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_3
!
!     CHECK GRIDB CARDS
!
 100     cc = cgridb
         CALL locate(*160,Buf,gridb(1),m)
         nzx = (nz/5)*5
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*780,*120,geom1,Z(1),nzx,0,m)
         m = nzx
         IF ( m<=0 ) GOTO 160
 120     pvcid = 0
         ASSIGN 140 TO irtn
         i = -2
 140     SPAG_Loop_1_4: DO
            i = i + 5
            IF ( i>m ) THEN
               IF ( m/=nzx ) EXIT SPAG_Loop_1_4
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               cid = Z(i)
               IF ( cid/=0 .AND. cid/=pvcid ) THEN
                  pvcid = cid
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_4
!
!     CHECK BFIELD CARDS
!
 160     cc = cbfiel
         CALL locate(*220,Buf,bfield(1),m)
         CALL read(*780,*180,geom1,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 180     pvcid = 0
         ASSIGN 200 TO irtn
         i = -1
 200     SPAG_Loop_1_5: DO
            i = i + 2
            IF ( i>m ) EXIT SPAG_Loop_1_5
            cid = Z(i)
            IF ( cid/=0 .AND. cid/=pvcid ) THEN
               pvcid = cid
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_5
!
!     END OF GEOM1 PROCESSING
!
 220     CALL close(geom1,1)
!
!
!     CHECK THE PRESENCE OF CONM2 CARDS IN GEOM2
!
         file = geom2
         k = conm2(2)
         ASSIGN 240 TO jrtn
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 240     IF ( k==0 ) GOTO 320
!
!     OPEN GEOM2, AND CHECK CONM2 CARDS
!
         cc = cconm2
         CALL preloc(*320,Buf,geom2)
         CALL locate(*300,Buf,conm2(1),m)
         nzx = (nz/13)*13
         spag_nextblock_1 = 4
      CASE (4)
         CALL read(*780,*260,geom2,Z(1),nzx,0,m)
         m = nzx
         IF ( m<=0 ) GOTO 300
 260     pvcid = 0
         ASSIGN 280 TO irtn
         i = -10
 280     SPAG_Loop_1_6: DO
            i = i + 13
            IF ( i>m ) THEN
               IF ( m/=nzx ) EXIT SPAG_Loop_1_6
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               cid = Z(i)
               IF ( cid/=0 .AND. cid/=pvcid ) THEN
                  pvcid = cid
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_6
!
 300     CALL close(geom2,1)
!
!
!     CHECK THE PRESENCE OF BULK DATA CARDS IN GEOM3
!     (FORCE, MOMENT, RFORCE, GRAV AND PLOAD4)
!
 320     file = geom3
         k = force(2)
         ASSIGN 340 TO jrtn
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 340     IF ( k/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = moment(2)
         ASSIGN 360 TO jrtn
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 360     IF ( k/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = rforce(2)
         ASSIGN 380 TO jrtn
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 380     IF ( k/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = grav(2)
         ASSIGN 400 TO jrtn
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 400     IF ( k/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = pload4(2)
         ASSIGN 420 TO jrtn
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 420     IF ( k==0 ) GOTO 580
         spag_nextblock_1 = 5
      CASE (5)
!
!     OPEN GEOM3, AND CHECK CID ON BULK DATA CARDS
!
         CALL preloc(*580,Buf,geom3)
         CALL locate(*440,Buf,force(1),m)
         cc = cforce
         ib = 3
         ic = 7
         ASSIGN 440 TO krtn
!
         CALL read(*780,*520,geom3,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 440     CALL locate(*460,Buf,moment(1),m)
         cc = cmment
         ib = 3
         ic = 7
         ASSIGN 460 TO krtn
         CALL read(*780,*520,geom3,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 460     CALL locate(*480,Buf,rforce(1),m)
         cc = crforc
         ib = 3
!WKBR 2/95 SPR94015      IC = 8
         ic = 7
         ASSIGN 480 TO krtn
         CALL read(*780,*520,geom3,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 480     CALL locate(*500,Buf,grav(1),m)
         cc = cgrav
         ib = 2
         ic = 6
         ASSIGN 500 TO krtn
         CALL read(*780,*520,geom3,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 500     CALL locate(*560,Buf,pload4(1),m)
         cc = cplod4
         ib = 9
         ic = 12
         ASSIGN 560 TO krtn
         CALL read(*780,*520,geom3,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 520     ASSIGN 540 TO irtn
         i = ib - ic
 540     DO
            i = i + ic
            IF ( i>m ) GOTO krtn
            cid = Z(i)
            IF ( cid/=0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
 560     CALL close(geom3,1)
!
!
!     CHECK THE PRESENCE OF PIHEX CARD IN EPT. IF PRESENT, OPEN EPT,
!     AND CHECK CID ON PIHEX CARDS
!
 580     file = ept
         k = pihex(2)
         ASSIGN 600 TO jrtn
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 600     IF ( k==0 ) GOTO 680
         CALL preloc(*680,Buf,ept)
         CALL locate(*660,Buf,pihex(1),m)
         CALL read(*780,*620,ept,Z(1),nz,1,m)
         j = -8
         CALL mesage(j,file,name)
         RETURN
 620     cc = cpihex
         ASSIGN 640 TO irtn
         i = -4
 640     SPAG_Loop_1_7: DO
            i = i + 7
            IF ( i>m ) EXIT SPAG_Loop_1_7
            cid = Z(i)
            IF ( cid/=0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_7
 660     CALL close(ept,1)
!
!
!     CHECK THE PRESENCE OF AXIF CARD IN AXIC. IF PRESENT, OPEN AXIC,
!     AND CHECK CID ON AXIF CARD. ONLY ONE AXIF CARD EXISTS
!
 680     file = axic
         k = axif(2)
         ASSIGN 700 TO jrtn
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 700     IF ( k==0 ) GOTO 760
         CALL preloc(*760,Buf,axic)
         CALL locate(*740,Buf,axif(1),m)
         CALL read(*780,*720,axic,cid,1,1,m)
 720     cc = caxif
         ASSIGN 740 TO irtn
         IF ( cid/=0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 740     CALL close(axic,1)
!
 760     RETURN
      CASE (6)
         IF ( cid/=Z(ii) ) THEN
            IF ( ncord>1 ) THEN
               ie = ii + ncord - 1
               DO j = ii , ie
                  IF ( cid==Z(j) ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            IF ( cc/=pcc .OR. cid/=pcd ) THEN
               CALL page2(-2)
               WRITE (nout,99002) ufm , cid , cc
               pcc = cc
               pcd = cid
               abort = .TRUE.
            ENDIF
         ENDIF
         spag_nextblock_1 = 8
      CASE (7)
         ii = j
         IF ( ii>im ) ii = ii - ncord
         spag_nextblock_1 = 8
      CASE (8)
         GOTO irtn
      CASE (9)
!
!
!     INTERNAL ROUTINE TO CHECK THE PRESENCE OF A PARTICULAR BULK DATA
!     CARD
!
         trl(1) = file
         CALL rdtrl(trl(1))
         spag_nextblock_1 = 10
      CASE (10)
         IF ( trl(1)<0 ) THEN
            k = 0
         ELSE
            j = (k-1)/16
            l = k - 16*j
            IF ( andf(trl(j+2),two(l+16))==0 ) k = 0
         ENDIF
         GOTO jrtn
!
!     ERRORS
!
 780     j = -2
         CALL mesage(j,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (A23,' 328, UNDEFINED COORDINATE',I9,' IS REFERENCED BY A ',A7,' CARD')
99999 END SUBROUTINE cidck
