
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
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Abort
   INTEGER Ibuf , Nout , Two(1)
   CHARACTER*23 Ufm
   COMMON /system/ Ibuf , Nout , Abort
   COMMON /two   / Two
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Nopen
   INTEGER Buf(1) , Z(1)
!
! Local variable declarations
!
   INTEGER andf
   INTEGER axic , axif(2) , bfield(2) , cid , conm2(2) , cord(2) , ept , file , force(2) , geom1 , geom2 , geom3 , grav(2) , grid(2)&
         & , gridb(2) , i , ib , ic , ie , ii , im , irtn , j , jrtn , k , krtn , l , m , moment(2) , name(2) , ncord , nrid , nz , &
         & nzx , pcd , pihex(2) , pload4(2) , pvcid , rforce(2) , trl(7)
   CHARACTER*7 caxif , cbfiel , cc , cconm2 , ccord2 , cforce , cgrav , cgrid , cgridb , cmment , cpihex , cplod4 , crforc , pcc
   EXTERNAL andf
!
! End of declarations
!
   DATA geom1 , geom2 , geom3 , ept/201 , 208 , 209 , 202/ , axic , name , pcd , pcc/215 , 4HCIDC , 2HK  , 0 , 'XXXX   '/
   DATA axif , caxif/8815 , 88 , 'AXIF   '/bfield , cbfiel/3101 , 31 , 'BFIELD '/ , conm2 , cconm2/1501 , 15 , 'CONM2  '/cord ,     &
      & ccord2/1601 , 16 , 'CORD2  '/ , force , cforce/4201 , 42 , 'FORCE  '/grav , cgrav/4401 , 44 , 'GRAV   '/ , grid ,           &
      & cgrid/4501 , 45 , 'GRID   '/gridb , cgridb/8115 , 81 , 'GRIDB  '/ , moment , cmment/4801 , 48 , 'MOMENT '/pihex ,           &
      & cpihex/7002 , 70 , 'PIHEX  '/ , pload4 , cplod4/6709 , 67 , 'PLOAD4 '/rforce , crforc/5509 , 55 , 'RFORCE '/
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
      CALL locate(*100,Buf,cord(1),m)
      DO
         CALL read(*4800,*100,geom1,Z(ncord),k,0,m)
         ncord = ncord + 1
         IF ( i>=4 .AND. Z(ncord+2)/=0 ) THEN
            Z(nrid) = Z(ncord+2)
            nrid = nrid - 1
         ENDIF
      ENDDO
 100  ENDDO
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
            WRITE (Nout,99001) Ufm , Z(i)
99001       FORMAT (A23,' 328, DUPLICATE COORDINATE ID',I9)
         ENDIF
      ENDDO
      ncord = j
   ENDIF
!
!     IF CORD2C/R/S CARDS ARE PRESENT, CHECK REFERENCE COORDINATE ID
!
   IF ( nrid<=Nopen ) THEN
      cc = ccord2
      DO i = nrid , Nopen
         cid = Z(i)
         IF ( ncord>0 ) THEN
            DO j = 1 , ncord
               IF ( cid==Z(j) ) GOTO 150
            ENDDO
         ENDIF
         CALL page2(-2)
         WRITE (Nout,99002) Ufm , cid , cc
         Abort = .TRUE.
 150  ENDDO
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
   CALL locate(*600,Buf,grid(1),m)
   nzx = (nz/8)*8
 200  CALL read(*4800,*300,geom1,Z(1),nzx,0,m)
   m = nzx
   IF ( m<=0 ) GOTO 600
 300  pvcid = 0
   ASSIGN 400 TO irtn
   i = -6
 400  DO
      i = i + 8
      IF ( i>m ) THEN
         pvcid = 0
         ASSIGN 500 TO irtn
         i = -2
         EXIT
      ELSE
         cid = Z(i)
         IF ( cid/=0 .AND. cid/=pvcid ) THEN
!
!
!     INTERNAL ROUTINE TO LOOK FOR CID MATCH
!     CID ARRAY (DOUBLE) IS AT HIGH END OF CORE, Z(II) THRU Z(NOPEN)
!
            pvcid = cid
            GOTO 4300
         ENDIF
      ENDIF
   ENDDO
 500  DO
      i = i + 8
      IF ( i>m ) THEN
         IF ( m/=nzx ) EXIT
         GOTO 200
      ELSE
         cid = Z(i)
         IF ( cid/=0 .AND. cid/=pvcid ) THEN
            pvcid = cid
            GOTO 4300
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK GRIDB CARDS
!
 600  cc = cgridb
   CALL locate(*1000,Buf,gridb(1),m)
   nzx = (nz/5)*5
 700  CALL read(*4800,*800,geom1,Z(1),nzx,0,m)
   m = nzx
   IF ( m<=0 ) GOTO 1000
 800  pvcid = 0
   ASSIGN 900 TO irtn
   i = -2
 900  DO
      i = i + 5
      IF ( i>m ) THEN
         IF ( m/=nzx ) EXIT
         GOTO 700
      ELSE
         cid = Z(i)
         IF ( cid/=0 .AND. cid/=pvcid ) THEN
            pvcid = cid
            GOTO 4300
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK BFIELD CARDS
!
 1000 cc = cbfiel
   CALL locate(*1300,Buf,bfield(1),m)
   CALL read(*4800,*1100,geom1,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 1100 pvcid = 0
   ASSIGN 1200 TO irtn
   i = -1
 1200 DO
      i = i + 2
      IF ( i>m ) EXIT
      cid = Z(i)
      IF ( cid/=0 .AND. cid/=pvcid ) THEN
         pvcid = cid
         GOTO 4300
      ENDIF
   ENDDO
!
!     END OF GEOM1 PROCESSING
!
 1300 CALL close(geom1,1)
!
!
!     CHECK THE PRESENCE OF CONM2 CARDS IN GEOM2
!
   file = geom2
   k = conm2(2)
   ASSIGN 1400 TO jrtn
   GOTO 4600
 1400 IF ( k==0 ) GOTO 1900
!
!     OPEN GEOM2, AND CHECK CONM2 CARDS
!
   cc = cconm2
   CALL preloc(*1900,Buf,geom2)
   CALL locate(*1800,Buf,conm2(1),m)
   nzx = (nz/13)*13
 1500 CALL read(*4800,*1600,geom2,Z(1),nzx,0,m)
   m = nzx
   IF ( m<=0 ) GOTO 1800
 1600 pvcid = 0
   ASSIGN 1700 TO irtn
   i = -10
 1700 DO
      i = i + 13
      IF ( i>m ) THEN
         IF ( m/=nzx ) EXIT
         GOTO 1500
      ELSE
         cid = Z(i)
         IF ( cid/=0 .AND. cid/=pvcid ) THEN
            pvcid = cid
            GOTO 4300
         ENDIF
      ENDIF
   ENDDO
!
 1800 CALL close(geom2,1)
!
!
!     CHECK THE PRESENCE OF BULK DATA CARDS IN GEOM3
!     (FORCE, MOMENT, RFORCE, GRAV AND PLOAD4)
!
 1900 file = geom3
   k = force(2)
   ASSIGN 2000 TO jrtn
   GOTO 4600
 2000 IF ( k/=0 ) GOTO 2500
   k = moment(2)
   ASSIGN 2100 TO jrtn
   GOTO 4700
 2100 IF ( k/=0 ) GOTO 2500
   k = rforce(2)
   ASSIGN 2200 TO jrtn
   GOTO 4700
 2200 IF ( k/=0 ) GOTO 2500
   k = grav(2)
   ASSIGN 2300 TO jrtn
   GOTO 4700
 2300 IF ( k/=0 ) GOTO 2500
   k = pload4(2)
   ASSIGN 2400 TO jrtn
   GOTO 4700
 2400 IF ( k==0 ) GOTO 3300
!
!     OPEN GEOM3, AND CHECK CID ON BULK DATA CARDS
!
 2500 CALL preloc(*3300,Buf,geom3)
   CALL locate(*2600,Buf,force(1),m)
   cc = cforce
   ib = 3
   ic = 7
   ASSIGN 2600 TO krtn
!
   CALL read(*4800,*3000,geom3,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 2600 CALL locate(*2700,Buf,moment(1),m)
   cc = cmment
   ib = 3
   ic = 7
   ASSIGN 2700 TO krtn
   CALL read(*4800,*3000,geom3,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 2700 CALL locate(*2800,Buf,rforce(1),m)
   cc = crforc
   ib = 3
!WKBR 2/95 SPR94015      IC = 8
   ic = 7
   ASSIGN 2800 TO krtn
   CALL read(*4800,*3000,geom3,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 2800 CALL locate(*2900,Buf,grav(1),m)
   cc = cgrav
   ib = 2
   ic = 6
   ASSIGN 2900 TO krtn
   CALL read(*4800,*3000,geom3,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 2900 CALL locate(*3200,Buf,pload4(1),m)
   cc = cplod4
   ib = 9
   ic = 12
   ASSIGN 3200 TO krtn
   CALL read(*4800,*3000,geom3,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 3000 ASSIGN 3100 TO irtn
   i = ib - ic
 3100 DO
      i = i + ic
      IF ( i>m ) GOTO krtn
      cid = Z(i)
      IF ( cid/=0 ) GOTO 4300
   ENDDO
!
 3200 CALL close(geom3,1)
!
!
!     CHECK THE PRESENCE OF PIHEX CARD IN EPT. IF PRESENT, OPEN EPT,
!     AND CHECK CID ON PIHEX CARDS
!
 3300 file = ept
   k = pihex(2)
   ASSIGN 3400 TO jrtn
   GOTO 4600
 3400 IF ( k==0 ) GOTO 3800
   CALL preloc(*3800,Buf,ept)
   CALL locate(*3700,Buf,pihex(1),m)
   CALL read(*4800,*3500,ept,Z(1),nz,1,m)
   j = -8
   CALL mesage(j,file,name)
   GOTO 99999
 3500 cc = cpihex
   ASSIGN 3600 TO irtn
   i = -4
 3600 DO
      i = i + 7
      IF ( i>m ) EXIT
      cid = Z(i)
      IF ( cid/=0 ) GOTO 4300
   ENDDO
 3700 CALL close(ept,1)
!
!
!     CHECK THE PRESENCE OF AXIF CARD IN AXIC. IF PRESENT, OPEN AXIC,
!     AND CHECK CID ON AXIF CARD. ONLY ONE AXIF CARD EXISTS
!
 3800 file = axic
   k = axif(2)
   ASSIGN 3900 TO jrtn
   GOTO 4600
 3900 IF ( k==0 ) GOTO 4200
   CALL preloc(*4200,Buf,axic)
   CALL locate(*4100,Buf,axif(1),m)
   CALL read(*4800,*4000,axic,cid,1,1,m)
 4000 cc = caxif
   ASSIGN 4100 TO irtn
   IF ( cid/=0 ) GOTO 4300
 4100 CALL close(axic,1)
!
 4200 RETURN
 4300 IF ( cid/=Z(ii) ) THEN
      IF ( ncord>1 ) THEN
         ie = ii + ncord - 1
         DO j = ii , ie
            IF ( cid==Z(j) ) GOTO 4400
         ENDDO
      ENDIF
      IF ( cc/=pcc .OR. cid/=pcd ) THEN
         CALL page2(-2)
         WRITE (Nout,99002) Ufm , cid , cc
         pcc = cc
         pcd = cid
         Abort = .TRUE.
      ENDIF
   ENDIF
   GOTO 4500
 4400 ii = j
   IF ( ii>im ) ii = ii - ncord
 4500 GOTO irtn
!
!
!     INTERNAL ROUTINE TO CHECK THE PRESENCE OF A PARTICULAR BULK DATA
!     CARD
!
 4600 trl(1) = file
   CALL rdtrl(trl(1))
 4700 IF ( trl(1)<0 ) THEN
      k = 0
   ELSE
      j = (k-1)/16
      l = k - 16*j
      IF ( andf(trl(j+2),Two(l+16))==0 ) k = 0
   ENDIF
   GOTO jrtn
!
!     ERRORS
!
 4800 j = -2
   CALL mesage(j,file,name)
99002 FORMAT (A23,' 328, UNDEFINED COORDINATE',I9,' IS REFERENCED BY A ',A7,' CARD')
99999 RETURN
END SUBROUTINE cidck
