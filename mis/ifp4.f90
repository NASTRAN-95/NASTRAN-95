
SUBROUTINE ifp4
   IMPLICIT NONE
   INTEGER Cls , Clsrew , Iaxif , Output , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   REAL Dum34(34) , Rz(4)
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Output , Nogo , Dum34 , Iaxif
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   REAL angle , bd , degrad , drho , g , r , rbuf(10) , rcard(10) , rhob , rj , rjl1 , rjp1 , temp , temp1 , temp2 , zj , zjl1 ,    &
      & zjp1 , ztemp , zz
   LOGICAL any , anygb , anygrd , bit , bit2 , end , g1eof , g2eof , g4eof , harms , mateof , press , set102
   INTEGER axic , axif(2) , bdylst(2) , bndfl(2) , buf(10) , buf1 , buf2 , buf3 , buf4 , buf5 , card(10) , cfluid(6) , cfsmas(2) ,  &
         & cord(8) , core , corsys , csf , dmiax(2) , dmig(2) , entrys , eor , file , flag , flsym(2) , freept(2) , fslst(2) ,      &
         & geom1 , geom2 , geom4 , grid(2) , gridb(2) , i , i2 , ibd , ibdyl , icore , icrq , id , idf , idfaft , idfpre , idlast , &
         & idrho , ier , ifslst , igridb , ii , ilist , impc , in , iname , index , iretrn , irhob , iring , ispc , ispnt , itemp , &
         & itwo31 , j , jgridb , jpoint , jsave , k , kid , l , last(10) , matpol , minus1 , mones(4) , mpc(2) , mpcadd(2) , msg1(2)&
         & , msg2(2) , n , nbdyl , ncord(4) , nfslst , ngridb , ni
   INTEGER korsz
   INTEGER nlist , nmpc , nn , nname , noeor , nosym , nring , nsize , nspc , nspnt , ntemp , nwords , point , prespt(2) , ringfl(2)&
         & , saveid(5) , scrt1 , scrt2 , seqgp(2) , space , spc(2) , spc1(2) , spcadd(2) , spoint(2) , subr(2) , trail(7) , type(2) &
         & , words
!
!     HYDROELASTIC PREFACE ROUTINE
!
!     THIS PREFACE MODULE OPERATES ON FLUID RELATED INPUT DATA WHICH
!     EXISTS AT THIS POINT IN THE FORM OF CARD IMAGES ON THE AXIC DATA
!     BLOCK.
!
!     7/12/73 NO AXIAL SYMMETRY FIRST FIVE WORDS OF BNDFL NO WRITTEN
!
!     THE FOLLOWING LIST GIVES THE CARD IMAGES IFP4 WILL LOOK FOR ON THE
!     AXIC DATA BLOCK, THE CARD IMAGES IFP4 WILL GENERATE OR MODIFY, AND
!     THE DATA BLOCKS ONTO WHICH THE GENERATED OR MODIFIED CARD IMAGES
!     WILL BE PLACED.
!
!     IFP4 INPUT         IFP4 OUTPUT        DATA BLOCK
!     CARD IMAGE         CARD IMAGE         EFFECTED
!     -----------        -----------        ----------
!       AXIF               -NONE-             -NONE-
!       BDYLIST            -DATA-             MATPOOL
!       CFLUID2            CFLUID2            GEOM2
!       CFLUID3            CFLUID3            GEOM2
!       CFLUID4            CFLUID4            GEOM2
!       FLSYM              -DATA-             MATPOOL
!       FREEPT             SPOINT             GEOM2
!                          MPC                GEOM4
!       FSLIST             CFSMASS            GEOM2
!                          SPC                GEOM4
!       GRIDB              GRID               GEOM1
!       PRESPT             SPOINT             GEOM2
!                          MPC                GEOM4
!       RINGFL             GRID               GEOM1
!                          SEQGP              GEOM1
!       DMIAX              DMIG               MATPOOL
!
!     SOME OF THE ABOVE OUTPUT CARD IMAGES ARE A FUNCTION OF SEVERAL
!     INPUT CARD IMAGES
!
   EQUIVALENCE (Z(1),Rz(1)) , (buf(1),rbuf(1)) , (card(1),rcard(1)) , (core,icore) , (rhob,irhob) , (bd,ibd)
   DATA axif/8815 , 88/
   DATA bdylst/8915 , 89/
   DATA cfluid/7815 , 78 , 7915 , 79 , 8015 , 80/
   DATA flsym/9115 , 91/
   DATA freept/9015 , 90/
   DATA fslst/8215 , 82/
   DATA gridb/8115 , 81/
   DATA prespt/8415 , 84/
   DATA ringfl/8315 , 83/
   DATA cfsmas/2508 , 25/
   DATA bndfl/9614 , 96/
   DATA mpc/4901 , 49/
   DATA spc/5501 , 55/
   DATA spc1/5481 , 58/
   DATA mpcadd/4891 , 60/
   DATA spcadd/5491 , 59/
   DATA spoint/5551 , 49/
   DATA grid/4501 , 45/
   DATA seqgp/5301 , 53/
   DATA dmiax/214 , 2/
   DATA dmig/114 , 1/
   DATA cord/1701 , 17 , 1901 , 19 , 2001 , 20 , 2201 , 22/
   DATA ncord/6 , 6 , 13 , 13/
   DATA mones/ - 1 , -1 , -1 , -1/
   DATA subr/4HIFP4 , 4H    /
   DATA degrad/1.7453292519943E-02/ , minus1/ - 1/
   DATA geom1 , geom2 , geom4/201 , 208 , 210/
   DATA axic , matpol , eor/215 , 214 , 1/
!
!     NOTE  SCRATCH2 IN IFP4 IS EQUIVALENCED TO THE -FORCE- DATA BLOCK
!
   DATA scrt1 , scrt2 , noeor/301 , 213 , 0/
   DATA msg1/4HIFP4 , 4HBEGN/ , msg2/4HIFP4 , 4HEND /
!
!     DEFINE CORE AND BUFFER POINTERS
!
   CALL conmsg(msg1,2,0)
   icore = korsz(Z)
   buf1 = icore - Sysbuf - 2
   buf2 = buf1 - Sysbuf - 2
   buf3 = buf2 - Sysbuf - 2
   buf4 = buf3 - Sysbuf - 2
   buf5 = buf4 - Sysbuf - 2
   icore = buf5 - 1
   icrq = 100 - icore
   IF ( icore<100 ) GOTO 7500
!
!     OPEN AXIC DATA BLOCK (IF NAME NOT IN FIST RETURN - NO MESSAGE)
!
   CALL preloc(*6900,Z(buf1),axic)
!
!     PICK UP AXIF CARD. (IF AXIF CARD NOT PRESENT - RETURN NO MESSAGE)
!
   CALL locate(*6900,Z(buf1),axif,flag)
   CALL read(*7000,*100,axic,Z(1),icore,eor,words)
   WRITE (Output,99001) Ufm
99001 FORMAT (A23,' 4031, INSUFFICIENT CORE TO READ DATA ON AXIF CARD.')
   WRITE (Output,99022) icore
!
!     FATAL ERROR NO MORE PROCESSING POSSIBLE
!
   Nogo = .TRUE.
   GOTO 6900
!
!     DATA OF AXIF CARD IS NOW STORED
!
 100  csf = Z(1)
   g = Rz(2)
   drho = Rz(3)
   j = 3
   idrho = Z(j)
   bd = Rz(4)
   nosym = Z(j+2)
   in = 6
   nn = words - 1
   ni = nn
   j = nn - in + 1
   harms = .FALSE.
   IF ( j>=1 ) harms = .TRUE.
   IF ( harms ) THEN
!
!     CONVERT USER INPUT LIST OF HARMONIC NUMBERS TO A LIST OF INDICES.
!
      IF ( j/=1 ) CALL sort(0,0,1,1,Z(in),j)
      ii = nn + 1
      ni = nn
      DO i = in , nn
         itemp = 2*Z(i)
         IF ( nosym/=0 ) THEN
            IF ( Z(i)/=0 ) THEN
               ni = ni + 1
               Z(ni) = itemp + 1
            ENDIF
         ENDIF
         ni = ni + 1
         Z(ni) = itemp + 2
      ENDDO
      n = ni - ii + 1
!
!     SET MAXIMUM HARMONIC+1 FOR USE BY SDR2C AND VDRB
!
      Iaxif = Z(nn) + 1
   ENDIF
!
!     BEGIN GEOM1 PROCESSING
!     **********************
!
!     OPEN GEOM1 AND FIND CORD1C, CORD1S, CORD2C, OR CORD2S CARD IMAGE
!     WITH COORDINATE SYSTEM ID = CSF OF AXIF CARD. THEN NOTE TYPE
!     (CYLINDRICAL OR SPHERICAL, 2 OR 3 RESPECTIVELY)
!
   file = geom1
!
!     BEFORE CALLING PRELOC ON GEOM1 CHECK FOR DATA ON GEOM1
!
   trail(1) = geom1
   CALL rdtrl(trail)
   DO i = 2 , 7
      IF ( trail(i)/=0 ) GOTO 200
   ENDDO
   GOTO 400
 200  CALL preloc(*7400,Z(buf2),geom1)
   DO i = 1 , 4
      i2 = 2*i
      CALL locate(*300,Z(buf2),cord(i2-1),flag)
      nsize = ncord(i)
      DO
         CALL read(*7200,*300,geom1,Z(ni+1),nsize,noeor,flag)
         IF ( Z(ni+1)==csf ) GOTO 500
      ENDDO
 300  ENDDO
!
!     FALL THROUGH LOOP IMPLIES COORDINATE SYSTEM WAS NOT FOUND
!
 400  Nogo = .TRUE.
   WRITE (Output,99002) Ufm , csf
99002 FORMAT (A23,' 4033, COORDINATE SYSTEM ID =',I20,' AS SPECIFIED ','ON AXIF CARD IS NOT PRESENT',/5X,' AMONG ANY OF CORD1C,',   &
             &' CORD1S, CORD2C, OR CORD2S CARD TYPES.',/5X,' CYLINDRICAL TYPE ASSUMED FOR CONTINUING DATA CHECK.')
   corsys = 2
   GOTO 600
 500  corsys = Z(ni+2)
 600  CALL close(geom1,Clsrew)
!
!     READ INTO CORE FROM AXIC ALL GRIDB CARD IMAGES (5 WORDS / IMAGE)
!
   anygb = .FALSE.
   igridb = ni + 1
   ngridb = ni
   CALL locate(*800,Z(buf1),gridb,flag)
   anygb = .TRUE.
   space = core - ni
   CALL read(*7000,*700,axic,Z(igridb),space,eor,nwords)
   Nogo = .TRUE.
   WRITE (Output,99003) Ufm
99003 FORMAT (A23,' 4034, INSUFFICIENT CORE TO HOLD GRIDB CARD IMAGES.')
   WRITE (Output,99022) space
   anygb = .FALSE.
   GOTO 800
 700  ngridb = ni + nwords
!
!     IF ANY GRIDB IMAGES ARE PRESENT A BOUNDARY LIST IS FORMED IN CORE.
!
 800  ibdyl = ngridb + 1
   nbdyl = ngridb
   IF ( .NOT.anygb ) GOTO 1400
   CALL locate(*1400,Z(buf1),bdylst,flag)
 900  CALL read(*7000,*1100,axic,rhob,1,noeor,flag)
   IF ( irhob==1 ) THEN
      IF ( idrho/=1 ) THEN
         rhob = drho
      ELSE
         Nogo = .TRUE.
         WRITE (Output,99004) Ufm
99004    FORMAT (A23,' 4035, THE FLUID DENSITY HAS NOT BEEN SPECIFIED ON ','A BDYLIST CARD AND',/5X,'THERE IS NO DEFAULT FLUID ',   &
                &'DENSITY SPECIFIED ON THE AXIF CARD.')
         rhob = 1.0
      ENDIF
   ENDIF
   end = .FALSE.
   idfpre = 0
   DO
      CALL read(*7000,*7100,axic,idf,1,noeor,flag)
      IF ( idf/=0 ) EXIT
      idfpre = -1
   ENDDO
 1000 CALL read(*7000,*7100,axic,idfaft,1,noeor,flag)
!
!     NOTE.......  ON INPUT   ID=0 IMPLIES AXIS
!                             ID=-1 IMPLIES END OF CARD
!
!
!     NOTE.......  ON OUTPUT  ID=0 IMPLIES UNDEFINED ID
!                             ID=-1 IMPLIES AXIS
!
   IF ( idfaft==-1 ) THEN
      idfaft = 0
      end = .TRUE.
   ELSE
      IF ( idfaft==0 ) idfaft = -1
   ENDIF
!
!     DO NOT PUT OUT ENTRY WHEN IDF = AXIS
!
   IF ( idf/=-1 ) THEN
      IF ( nbdyl+7<=core ) THEN
         Z(nbdyl+1) = idf
         Z(nbdyl+2) = 1
         Z(nbdyl+3) = 1
         Z(nbdyl+4) = 1
         Z(nbdyl+5) = idfpre
         Z(nbdyl+6) = idfaft
         Rz(nbdyl+7) = rhob
         nbdyl = nbdyl + 7
      ELSE
         WRITE (Output,99005) Ufm
99005    FORMAT (A23,' 4036, INSUFFICIENT CORE TO BUILD BOUNDARY LIST ','TABLE.')
         icrq = nbdyl + 7 - core
         GOTO 7500
      ENDIF
   ENDIF
!
!     ROTATE THE ID-S
!
   idfpre = idf
   idf = idfaft
   IF ( end ) GOTO 900
   GOTO 1000
!
!     SORT ENTRIES ON FIRST WORD OF EACH ENTRY.
!
 1100 CALL sort(0,0,7,1,Z(ibdyl),nbdyl-ibdyl+1)
   entrys = (nbdyl-ibdyl+1)/7
!
!     PASS THE RINGFL IMAGES INSERTING X1, X2, AND X3 IN THE APPROPRIATE
!     BDYLIST ENTRY.
!
   CALL locate(*1300,Z(buf1),ringfl,flag)
 1200 DO
      CALL read(*7000,*1300,axic,buf,4,noeor,flag)
      IF ( corsys==3 ) THEN
         IF ( rbuf(3)==0. ) THEN
            Nogo = .TRUE.
            WRITE (Output,99023) Ufm , buf(1)
         ENDIF
      ENDIF
      IF ( buf(corsys+1)/=0 ) THEN
         Nogo = .TRUE.
         IF ( corsys==3 ) THEN
            WRITE (Output,99006) Ufm , buf(1)
99006       FORMAT (A23,' 4043, COORDINATE SYSTEM IS SPHERICAL BUT RINGFL ','CARD ID =',I20,' HAS A NON-ZERO X3 VALUE.')
         ELSE
            WRITE (Output,99007) Ufm , buf(1)
99007       FORMAT (A23,'4042, COORDINATE SYSTEM IS CYLINDRICAL BUT RINGFL ','CARD ID =',I20,' HAS A NON-ZERO X2 VALUE.')
         ENDIF
      ENDIF
      CALL bisloc(*1200,buf(1),Z(ibdyl),7,entrys,jpoint)
      ntemp = ibdyl + jpoint - 1
      IF ( Z(ntemp+1)==1 ) THEN
!
!     CHECK TO GET RANGE OF BDYLIST HAVING THIS SAME ID.
!     THEN FILL IN X1, X2, AND X3 IN THOSE ENTRIES.
!
         nlist = ntemp
         DO
            ntemp = ntemp - 7
            IF ( ntemp<ibdyl ) EXIT
            IF ( Z(ntemp)/=Z(ntemp+7) ) EXIT
         ENDDO
         ilist = ntemp + 7
         ntemp = nlist
         DO
            ntemp = ntemp + 7
            IF ( ntemp>nbdyl ) EXIT
            IF ( Z(ntemp)/=Z(ntemp-7) ) EXIT
         ENDDO
         nlist = ntemp - 1
         DO i = ilist , nlist , 7
            Z(i+1) = buf(2)
            Z(i+2) = buf(3)
            Z(i+3) = buf(4)
         ENDDO
      ELSE
         Nogo = .TRUE.
         WRITE (Output,99024) Ufm , buf(1)
      ENDIF
   ENDDO
!
!     CHECK TO SEE THAT X1, X2, AND X3 WERE FOUND FOR ALL ENTRIES.
!
 1300 DO i = ibdyl , nbdyl , 7
      IF ( Z(i+1)==1 ) THEN
         Nogo = .TRUE.
         WRITE (Output,99008) Ufm , Z(i)
99008    FORMAT (A23,' 4040, ID =',I20,' APPEARS ON A BDYLIST CARD, BUT ','NO RINGFL CARD IS PRESENT WITH THE SAME ID.')
      ENDIF
   ENDDO
!
!     OPEN GEOM1, OPEN SCRATCH1, COPY HEADER REC FROM GEOM1 TO SCRATCH1
!
 1400 CALL ifp4c(geom1,scrt1,Z(buf2),Z(buf3),g1eof)
!
!     COPY ALL DATA UP TO FIRST GRID CARD IMAGE.
!
   CALL ifp4b(geom1,scrt1,any,Z(nbdyl+1),core-nbdyl,grid,g1eof)
   anygrd = any
   IF ( .NOT.anygb ) GOTO 2900
   IF ( nbdyl<ibdyl ) GOTO 2900
!
!     CREATE AND MERGE WITH GRIDS FROM GEOM1, GRIDS FROM GRIDB IMAGES.
!
   file = geom1
   IF ( any ) THEN
      CALL read(*7200,*1500,geom1,last,8,noeor,flag)
      CALL ifp4e(last(1))
   ENDIF
   GOTO 1600
 1500 any = .FALSE.
 1600 DO i = igridb , ngridb , 5
      card(1) = Z(i)
      CALL ifp4e(card(1))
      card(2) = csf
      kid = Z(i+4)
      CALL bisloc(*1650,kid,Z(ibdyl),7,entrys,point)
      ntemp = ibdyl + point - 1
      card(3) = Z(ntemp+1)
      card(4) = Z(ntemp+2)
      card(5) = Z(ntemp+3)
      card(corsys+2) = Z(i+1)
      card(6) = Z(i+2)
      card(7) = Z(i+3)
      card(8) = 0
!
!     MERGE CARD IN
!
      IF ( .NOT.any ) GOTO 1750
      DO WHILE ( last(1)<=card(1) )
         CALL write(scrt1,last,8,noeor)
         CALL read(*7200,*1700,geom1,last,8,noeor,flag)
         CALL ifp4e(last(1))
      ENDDO
      GOTO 1750
 1650 Nogo = .TRUE.
      WRITE (Output,99009) Ufm , Z(i) , Z(i+4)
99009 FORMAT (A23,' 4057, GRIDB CARD WITH ID =',I10,' HAS A REFERENCE ','IDF =',I10,/5X,'WHICH DOES NOT APPEAR IN A BOUNDARY LIST')
      CYCLE
 1700 any = .FALSE.
 1750 CALL write(scrt1,card,8,noeor)
   ENDDO
!
   IF ( any ) THEN
      DO
         CALL write(scrt1,last,8,noeor)
         CALL read(*7200,*1800,geom1,last,8,noeor,flag)
         CALL ifp4e(last(1))
      ENDDO
   ENDIF
!
!     FURTHER ALTERATIONS TO BOUNDARY LIST TABLE AT THIS TIME.
!     RADIAL LOCATION (RJ) AND VERTICAL LOCATION (ZJ)
!
 1800 nring = ngridb
   IF ( .NOT.harms ) GOTO 3400
   DO i = ibdyl , nbdyl , 7
      IF ( corsys==3 ) THEN
!
         angle = Rz(i+2)*degrad
         temp = Rz(i+1)
         Rz(i+1) = temp*sin(angle)
         Rz(i+2) = temp*cos(angle)
      ELSE
         Z(i+2) = Z(i+3)
      ENDIF
   ENDDO
!
!     LENGTH AND ASSOCIATED ANGLE COMPONENTS OF A CONICAL SECTION. L,C,S
!
   IF ( .NOT.(Nogo) ) THEN
      DO i = ibdyl , nbdyl , 7
         rj = Rz(i+1)
         zj = Rz(i+2)
!
!     FIND R   , Z     AND  R   , Z     (RJL1,ZJL1,RJP1,ZJP1)
!           J-1   J-1        J+1   J+1
!
         IF ( Z(i+4)<0 ) THEN
!
!     SECONDARY ID IS AXIS
!
            rjl1 = 0
            zjl1 = zj
         ELSEIF ( Z(i+4)==0 ) THEN
!
!     SECONDARY ID IS NOT AVAILABLE
!
            rjl1 = rj
            zjl1 = zj
         ELSE
!
!     FIND SECONDARY ID ENTRY
!
            kid = Z(i+4)
            CALL bisloc(*7600,kid,Z(ibdyl),7,entrys,point)
            ntemp = ibdyl + point - 1
            rjl1 = Rz(ntemp+1)
            zjl1 = Rz(ntemp+2)
         ENDIF
!
!     SECONDARY ID ON PLUS SIDE
!
         IF ( Z(i+5)<0 ) THEN
!
!     SECONDARY ID IS AXIS
!
            rjp1 = 0
            zjp1 = zj
         ELSEIF ( Z(i+5)==0 ) THEN
!
!     SECONDARY ID IS NOT AVAILABLE
!
            rjp1 = rj
            zjp1 = zj
         ELSE
!
!     FIND SECONDARY ID ENTRY
!
            kid = Z(i+5)
            CALL bisloc(*7600,kid,Z(ibdyl),7,entrys,point)
            ntemp = ibdyl + point - 1
            rjp1 = Rz(ntemp+1)
            zjp1 = Rz(ntemp+2)
         ENDIF
!
!     COMPUTE AND INSERT L,C,S.
!
         IF ( rj/=0.0 ) THEN
!
            temp1 = rjp1 - rj
            temp2 = 0.25/rj
            r = 0.5*(rjp1-rjl1+temp2*(temp1*temp1-(rjl1-rj)**2))
            zz = 0.5*(zjl1-zjp1+temp2*(temp1*(zj-zjp1)-(rj-rjl1)*(zjl1-zj)))
            Rz(i+3) = sqrt(r*r+zz*zz)
            IF ( Rz(i+3)/=0.0 ) THEN
!
               Rz(i+4) = zz/Rz(i+3)
               Rz(i+5) = r/Rz(i+3)
            ELSE
               Nogo = .TRUE.
               WRITE (Output,99010) Ufm , Z(i)
99010          FORMAT (A23,' 4045, THE BOUNDARY LIST ENTRY FOR ID =',I9,' HAS A ZERO CROSS-SECTION LENGTH.')
            ENDIF
         ELSE
            Nogo = .TRUE.
            WRITE (Output,99011) Ufm , Z(i)
99011       FORMAT (A23,' 4044, RINGFL CARD ID =',I20,' HAS SPECIFIED A ','ZERO RADIAL LOCATION.')
         ENDIF
      ENDDO
   ENDIF
!
!     SORT GRIDB IMAGES TO BE IN SORT ON RID AND PHI WITHIN EACH RID
!
   ntemp = ngridb - igridb + 1
   CALL sort(0,0,5,-2,Z(igridb),ntemp)
   CALL sort(0,0,5,-5,Z(igridb),ntemp)
!
!     THE BOUNDARY FLUID DATA IS ADDED TO THE MATPOOL DATA BLOCK AS 1
!     LOCATE RECORD CONTAINING THE FOLLOWING.
!
!     1-3   LOCATE CODE  9614,96,0
!     4     CDF
!     5     G
!     6     DRHO
!     7     BD
!     8     NOSYM
!     9     M
!     10    S1
!     11    S2
!     12    N = NUMBER OF INDICES FOLLOWING
!     12+1  THRU  12+N  THE INDICES
!     13+N TO THE EOR IS THE BOUNDARY FLUID DATA
!
!
   file = matpol
   iname = nbdyl + 1
   nname = nbdyl
   CALL ifp4c(matpol,scrt2,Z(buf4),Z(buf5),mateof)
   IF ( mateof ) GOTO 2700
!
!     IF ANY DMIAX CARDS ARE PRESENT THEN THEY ARE MERGED IN FRONT OF
!     DMIG CARDS IN THE DMIG RECORD.  FILE NAMES MAY NOT BE THE SAME ON
!     BOTH DMIG AND DMIAX CARDS.
!
   CALL ifp4f(dmiax(2),matpol,bit)
   CALL ifp4f(dmig(2),matpol,bit2)
!
!     LOCATE DMIAX CARDS, COPY THEM TO SCRT2 AS DMIG CARDS AND KEEP
!     LIST OF THEIR FILE NAMES.
!
   IF ( .NOT.bit .AND. .NOT.bit2 ) GOTO 2500
   CALL close(matpol,Clsrew)
   CALL preloc(*7400,Z(buf4),matpol)
!
!     WRITE DMIG HEADER.
!
   buf(1) = dmig(1)
   buf(2) = dmig(2)
   buf(3) = 120
   CALL write(scrt2,buf,3,noeor)
   IF ( .NOT.bit ) GOTO 2200
   CALL locate(*2200,Z(buf4),dmiax,flag)
   ASSIGN 2000 TO iretrn
!
!     READ 9 WORD HEADER
!
 1900 GOTO iretrn
 2000 CALL read(*7200,*2200,matpol,buf,9,noeor,flag)
!
!     SAVE NAME
!
   Z(iname) = buf(1)
   Z(iname+1) = buf(2)
   nname = nname + 2
   icrq = nname + 2 - icore
   IF ( icrq>0 ) GOTO 7500
   CALL write(scrt2,buf,9,noeor)
 2100 DO
!
!     COPY THE COLUMN DATA.  FIRST THE COLUMN INDEX.
!
      CALL read(*7200,*7300,matpol,buf,2,noeor,flag)
      CALL write(scrt2,buf,2,noeor)
      IF ( buf(1)<0 ) GOTO 1900
      DO
!
!     TERMS OF COLUMN
!
         CALL read(*7200,*7300,matpol,buf,2,noeor,flag)
         CALL write(scrt2,buf,2,noeor)
         IF ( buf(1)<0 ) EXIT
         CALL read(*7200,*7300,matpol,buf,1,noeor,flag)
         CALL write(scrt2,buf,1,noeor)
      ENDDO
   ENDDO
!
!     DMIAX-S ALL COPIED.  NOW COPY ANY DMIG-S.
!
 2200 IF ( .NOT.bit2 ) GOTO 2400
   CALL locate(*2400,Z(buf4),dmig,flag)
   ASSIGN 2300 TO iretrn
!
!     READ HEADER
!
 2300 CALL read(*7000,*2400,matpol,buf,9,noeor,flag)
!
!     CHECK THE NAME FOR BEING THE SAME AS ONE ON A DMIAX CARD
!
   DO i = iname , nname , 2
      IF ( buf(1)==Z(i) ) THEN
         IF ( buf(2)==Z(i+1) ) THEN
!
!     ERROR FOR NAME DOES MATCH THAT OF A DMIAX NAME
!
            Nogo = .TRUE.
            WRITE (Output,99012) Ufm , buf(1) , buf(2)
99012       FORMAT (A23,' 4062, DMIG BULK DATA CARD SPECIFIES DATA BLOCK ',2A4,' WHICH ALSO APPEARS ON A DMIAX CARD.')
         ENDIF
      ENDIF
   ENDDO
!
!     COPY THE COLUMN DATA
!
   CALL write(scrt2,buf,9,noeor)
   GOTO 2100
!
!     WRITE THE END OF RECORD FOR DMIG CARDS
!
 2400 CALL write(scrt2,0,0,eor)
!
!     TURN ON BIT FOR DMIG CARD TYPE
!
   CALL ifp4g(dmig(2),matpol)
   CALL rewind(matpol)
   CALL fwdrec(*7200,matpol)
 2500 DO
!
!     COPY EVERYTHING ON MATPOL TO SCRT2, EXCEPT FOR DMIG, DMIAX, AND
!     THE 2**31-1 RECORD.
!
      CALL read(*2700,*7300,matpol,buf,3,noeor,flag)
!     2147483647  = 2**31-1
      itwo31 = 2147483647
      IF ( buf(1)/=itwo31 .AND. (buf(1)/=dmig(1) .OR. buf(2)/=dmig(2)) .AND. (buf(1)/=dmiax(1) .OR. buf(2)/=dmiax(2)) ) THEN
         CALL read(*7200,*2600,matpol,Z(nbdyl+1),core-nbdyl,noeor,flag)
         CALL write(scrt2,Z(nbdyl+1),core-nbdyl,noeor)
      ELSE
         CALL fwdrec(*7200,matpol)
      ENDIF
   ENDDO
 2600 CALL write(scrt2,Z(nbdyl+1),flag,eor)
   GOTO 2500
 2700 mateof = .TRUE.
   CALL ifp4b(matpol,scrt2,any,Z(nbdyl+1),core-nbdyl,bndfl,mateof)
   card(1) = 0
   card(2) = 0
   card(3) = 0
   card(4) = n
   CALL locate(*2800,Z(buf1),flsym,flag)
   CALL read(*7000,*7100,axic,card,3,eor,flag)
 2800 CALL write(scrt2,Z(1),5,noeor)
   CALL write(scrt2,card,4,noeor)
   CALL write(scrt2,Z(ii),n,noeor)
!
!     OUTPUT ENTRIES TO MATPOOL DATA BLOCK.(TEMPORARILY ON SCRT2)
!
   jgridb = igridb
   jsave = 0
   DO i = ibdyl , nbdyl , 7
!
!     POSSIBILITY OF 2 FLUID ID-S HAVING SAME VALUE
!
      IF ( jsave/=0 ) jgridb = jsave
      jsave = 0
      IF ( Z(i)==Z(i+7) ) jsave = jgridb
!
!     IF RHO FOR A FLUID POINT IS ZERO WE DO NOT PUT OUT FLUID
!     DATA AND CONNECTED POINTS.
!
      IF ( Rz(i+6)/=0 ) CALL write(scrt2,Z(i),7,noeor)
!
!     APPEND GRIDB POINTS WITH THEIR ANGLES.
!
      DO WHILE ( jgridb<=ngridb )
         IF ( Z(jgridb+4)<Z(i) ) THEN
            jgridb = jgridb + 5
         ELSEIF ( Z(jgridb+4)==Z(i) ) THEN
!
!     APPEND THE POINT
!
            IF ( Rz(i+6)/=0 ) CALL write(scrt2,Z(jgridb),2,noeor)
            jgridb = jgridb + 5
         ELSE
            EXIT
         ENDIF
      ENDDO
!
!     COMPLETE THE ENTRY
!
      IF ( Rz(i+6)/=0 ) CALL write(scrt2,mones,2,noeor)
   ENDDO
!
!     COMPLETE RECORD.
!
   CALL write(scrt2,0,0,eor)
   CALL ifp4b(matpol,scrt2,any,Z(ngridb+1),core-ngridb,mones,mateof)
!
!  READ ALL RINGFL CARD IMAGES INTO CORE
!
 2900 IF ( .NOT.(anygb) ) THEN
      IF ( anygrd ) THEN
!
!     COPY GRID CARDS NOT COPIED AS A RESULT OF THE ABSENCE OF GRIDB
!     CARDS.
!
         file = geom1
         DO
            CALL read(*7200,*3000,geom1,card,8,noeor,flag)
            CALL write(scrt1,card,8,noeor)
         ENDDO
      ENDIF
   ENDIF
 3000 iring = ngridb + 1
   nring = ngridb
   CALL locate(*3200,Z(buf1),ringfl,flag)
   CALL read(*7000,*3100,axic,Z(iring),core-iring,noeor,flag)
   WRITE (Output,99013) Ufm
99013 FORMAT (A23,' 4047, INSUFFICIENT CORE TO HOLD RINGFL IMAGES.')
   icrq = core - iring
   WRITE (Output,99022) icrq
   Nogo = .TRUE.
   GOTO 6900
 3100 nring = iring + flag - 1
!
!     OUTPUT HARMONIC GRID CARDS.
!
 3200 IF ( nring>=iring ) THEN
!
!     SORT RINGFL IDS
!
      CALL sort(0,0,4,1,Z(iring),flag)
      card(2) = 0
      rcard(5) = 0.0
!
!     CARD(6) = -1 AS A FLAG TO TELL GP1 THIS IS A ONE DEGREE OF
!     FREEDOM POINT.
!
      card(6) = -1
      card(7) = 0
      card(8) = 0
      DO i = ii , ni
         index = Z(i)*500000
         DO k = iring , nring , 4
!
!     CALL IFP4E TO CHECK ID RANGE 1 TO 99999
!
            CALL ifp4e(Z(k))
            IF ( k/=iring ) THEN
               IF ( Z(k)==ztemp ) THEN
                  Nogo = .TRUE.
                  WRITE (Output,99024) Ufm , Z(k)
               ENDIF
            ENDIF
            ztemp = Z(k)
            card(1) = Z(k) + index
            IF ( corsys==3 ) THEN
               angle = Rz(k+2)*degrad
               rcard(3) = Rz(k+1)*sin(angle)
               rcard(4) = Rz(k+1)*cos(angle)
               IF ( rcard(3)==0.0 ) THEN
                  Nogo = .TRUE.
                  WRITE (Output,99023) Ufm , Z(k)
                  EXIT
               ENDIF
            ELSE
               card(3) = Z(k+1)
               card(4) = Z(k+3)
            ENDIF
            CALL write(scrt1,card,8,noeor)
         ENDDO
      ENDDO
   ENDIF
!
!     COMPLETE GRID CARD RECORD.
!
   CALL write(scrt1,0,0,eor)
!
!     CREATE AND OUTPUT SEQGP CARDS ONTO SCRT1.  COPY GEOM1 TO SCRT1 UP
!     TO AND INCLUDING SEQGP 3-WORD HEADER.
!
   IF ( nring<iring ) GOTO 3500
   CALL ifp4b(geom1,scrt1,any,Z(nring+1),core-nring,seqgp,g1eof)
!
!     COPY ALL SEQGP CARDS OVER ALSO (ID-S MUST BE OF CORRECT VALUE).
!
   file = geom1
   IF ( any ) THEN
      DO
         CALL read(*7200,*3300,geom1,card,2,noeor,flag)
         CALL ifp4e(card(1))
         CALL write(scrt1,card,2,noeor)
      ENDDO
   ENDIF
!
!     NOW OUTPUT SEQGP CARDS FOR HARMONICS OF EACH RINGFL.
!
 3300 DO i = ii , ni
      index = Z(i)*500000
      ntemp = Z(i) - 1
      DO k = iring , nring , 4
         card(1) = Z(k) + index
         card(2) = Z(k)*1000 + ntemp
         CALL write(scrt1,card,2,noeor)
      ENDDO
   ENDDO
 3400 CALL write(scrt1,0,0,eor)
!
!     COPY BALANCE OF GEOM1 TO SCRT1 (IF ANY MORE, WRAP UP, AND COPY
!     BACK)
!
 3500 CALL ifp4b(geom1,scrt1,any,Z(nring+1),core-nring,mones,g1eof)
!
!     IF THERE ARE NO HARMONICS THEN ONLY GRID CARDS ARE CREATED FROM
!     GRIDB CARDS.
!
!     IF (.NOT. HARMS) GO TO 2300
! === IF (.NOT. HARMS) SHOULD NOT GO TO 2300 HERE === G.CHAN/UNISYS 86
!
!     END OF GEOM1 PROCESSING
!
!     BEGIN GEOM2 PROCESSING
!     **********************
!
!     OPEN GEOM2, AND SCRT1. COPY HEADER FROM GEOM2 TO SCRT1.
!
   CALL ifp4c(geom2,scrt1,Z(buf2),Z(buf3),g2eof)
!
!     PROCESS CFLUID2, CFLUID3, AND CFLUID4 CARDS.
!
   DO i = 1 , 3
      i2 = 2*i
      CALL locate(*3700,Z(buf1),cfluid(i2-1),flag)
!
!     COPY DATA FROM GEOM2 TO SCRT1 UP TO POINT WHERE CFLUID CARDS GO
!     AND WRITE 3-WORD RECORD ID.
!
      CALL ifp4b(geom2,scrt1,any,Z(ni+1),core-ni,cfluid(2*i-1),g2eof)
 3550 CALL read(*7000,*3600,axic,card,i+4,noeor,flag)
      IF ( card(i+3)==1 ) THEN
         IF ( idrho==1 ) THEN
            Nogo = .TRUE.
            WRITE (Output,99014) Ufm , card(1)
99014       FORMAT (A23,' 4058, THE FLUID DENSITY HAS NOT BEEN SPECIFIED ON ','A CFLUID CARD WITH ID =',I10,/5X,                    &
                   &'AND THERE IS NO DEFAULT ON THE AXIF CARD.')
         ENDIF
         rcard(i+3) = drho
      ENDIF
      IF ( card(i+4)==1 ) THEN
         IF ( ibd==1 ) THEN
            Nogo = .TRUE.
            WRITE (Output,99015) Ufm , card(1)
99015       FORMAT (A23,' 4059, THE FLUID BULK MODULUS HAS NOT BEEN SPECIFIED',' ON A CFLUID CARD WITH ID =',I10,/5X,               &
                   &'AND THERE IS NO ','DEFAULT ON THE AXIF CARD.')
         ENDIF
         rcard(i+4) = bd
      ENDIF
!
!     OUTPUT N IMAGES.
!
      ntemp = i + 2
      DO k = 1 , ntemp
         saveid(k) = card(k)
      ENDDO
!
      DO k = ii , ni
         card(1) = saveid(1)*1000 + Z(k)
         index = 500000*Z(k)
         DO l = 2 , ntemp
            card(l) = saveid(l) + index
         ENDDO
         card(ntemp+3) = (Z(k)-1)/2
         CALL write(scrt1,card,ntemp+3,noeor)
      ENDDO
      GOTO 3550
!
!     END OF CFLUID DATA
!
 3600 CALL write(scrt1,0,0,eor)
 3700 ENDDO
!
!     CONSTRUCTION OF FSLIST TABLE IN CORE 3-WORDS/ENTRY
!
   ifslst = ni + 1
   nfslst = ni
   CALL locate(*4000,Z(buf1),fslst,flag)
 3800 CALL read(*7000,*3900,axic,rhob,1,noeor,flag)
   IF ( irhob==1 ) THEN
      IF ( idrho/=1 ) THEN
         rhob = drho
      ELSE
         Nogo = .TRUE.
         WRITE (Output,99016) Ufm
99016    FORMAT (A23,' 4048, THE FLUID DENSITY HAS NOT BEEN SPECIFIED ON ','AN FSLIST CARD AND',/5X,'THERE IS NO DEFAULT FLUID ',   &
                &'DENSITY SPECIFIED ON THE AXIF CARD.')
         rhob = 1.0
      ENDIF
   ENDIF
   CALL read(*7000,*7100,axic,idf,1,noeor,flag)
   IF ( idf==0 ) idf = -1
   DO
      CALL read(*7000,*7100,axic,idfaft,1,noeor,flag)
      IF ( idfaft==-1 ) idfaft = -2
      IF ( idfaft==0 ) idfaft = -1
      IF ( nfslst+3<=core ) THEN
         Z(nfslst+1) = idf
         Z(nfslst+2) = idfaft
         Rz(nfslst+3) = rhob
         nfslst = nfslst + 3
         IF ( idfaft==-2 ) GOTO 3800
         idf = idfaft
      ELSE
         WRITE (Output,99017) Ufm
99017    FORMAT (A23,' 4049, INSUFFICIENT CORE TO BUILD FREE SURFACE ','LIST TABLE.')
         icrq = nfslst + 3 - core
         WRITE (Output,99022) icrq
         Nogo = .TRUE.
         GOTO 6900
      ENDIF
   ENDDO
!
!     TABLE IS COMPLETE. COPY GEOM2 DATA TO SCRT1 UP TO CFSMASS RECORD
!     SLOT
!
 3900 IF ( nfslst>ifslst ) THEN
      CALL ifp4b(geom2,scrt1,any,Z(nfslst+1),core-nfslst,cfsmas,g2eof)
      entrys = (nfslst-ifslst+1)/3
      k = 0
      DO i = ifslst , nfslst , 3
         IF ( Z(i+1)/=-2 ) THEN
            k = k + 1000000
            rcard(4) = Rz(i+2)*g
            DO l = ii , ni
               index = 500000*Z(l)
               card(1) = k + Z(l)
               card(2) = Z(i) + index
               IF ( Z(i)<=0 ) card(2) = Z(i+1) + index
               card(3) = Z(i+1) + index
               IF ( Z(i+1)<=0 ) card(3) = Z(i) + index
               card(5) = (Z(l)-1)/2
               CALL write(scrt1,card,5,noeor)
            ENDDO
         ENDIF
      ENDDO
      CALL write(scrt1,0,0,eor)
   ELSE
      Nogo = .TRUE.
      WRITE (Output,99018) Ufm
99018 FORMAT (A23,' 4050, FSLIST CARD HAS INSUFFICIENT IDF DATA, OR ','FSLIST DATA MISSING.')
   ENDIF
!
!     BEGIN GEOM4 PROCESSING
!     **********************
!
!     OPEN GEOM4 AND SCRT2 AND COPY HEADER RECORD FROM GEOM4 TO SCRT2.
!
 4000 CALL ifp4c(geom4,scrt2,Z(buf4),Z(buf5),g4eof)
!
!     COPY ALL DATA ON GEOM4 TO SCRT2 UP TO AND INCLUDING 3-WORD RECORD
!     HEADER OF MPC-RECORD.
!
   CALL ifp4b(geom4,scrt2,any,Z(nfslst+1),core-nfslst,mpc,g4eof)
!
!     COPY ANY MPC IMAGES HAVING A SET ID .LT. 103 TO SCRT2. ERROR
!     MESSAGE IF ANY HAVE ID = 102.  MAINTAIN A LIST OF SETID-S LESS
!     THAN 102.
!
   impc = nfslst + 1
   nmpc = nfslst
   idlast = 0
   file = geom4
   set102 = .FALSE.
   IF ( any ) THEN
      DO
!
!     PICK UP SET ID
!
         CALL read(*7200,*4100,geom4,id,1,noeor,flag)
         IF ( id>102 ) GOTO 4200
         IF ( id==102 ) THEN
            Nogo = .TRUE.
            WRITE (Output,99019) Ufm
99019       FORMAT (A23,' 4051, AN MPC CARD HAS A SET ID SPECIFIED = 102. ',' SET 102 IS ILLEGAL WHEN FLUID DATA IS PRESENT.')
         ENDIF
         CALL write(scrt2,id,1,noeor)
!
!     ADD ID TO LIST IF NOT IN LIST
!
         IF ( id/=idlast ) THEN
            nmpc = nmpc + 1
            Z(nmpc) = id
         ENDIF
         DO
!
!     3 WORD GROUPS
!
            CALL read(*7200,*7300,geom4,card,3,noeor,flag)
            CALL write(scrt2,card,3,noeor)
            IF ( card(1)==-1 ) EXIT
         ENDDO
      ENDDO
   ENDIF
!
!     NOW POSITIONED TO OUTPUT MPC CARDS FOR SET 102
!
 4100 id = 0
!
!     IF G FROM AXIF CARD IS NON-ZERO FREEPT DATA IS NOW PROCESSED.
!
 4200 ispnt = nmpc + 1
   nspnt = nmpc
   press = .FALSE.
   IF ( g==0.0 ) GOTO 4700
!
!     IF THERE IS NO FREE SURFACE LIST, FREEPT CARDS ARE NOT USED.
!
   IF ( nfslst<ifslst ) GOTO 4700
   CALL sort(0,0,3,1,Z(ifslst),nfslst-ifslst+1)
   CALL locate(*4700,Z(buf1),freept,flag)
!
!     PICK UP A 3-WORD FREEPT OR PRESPT IMAGE (IDF,IDP,PHI)
!
 4300 CALL read(*7000,*4600,axic,card,3,noeor,flag)
!
!     START MPC CARD
!
   angle = rcard(3)*degrad
   idf = card(1)
   card(1) = 102
   card(3) = 0
   IF ( press ) THEN
      rcard(4) = -1.0
   ELSE
!
!     LOOK UP RHOB IN FSLIST TABLE
!
      CALL bisloc(*4400,idf,Z(ifslst),3,entrys,point)
      ntemp = ifslst + point + 1
      rcard(4) = -abs(Rz(ntemp)*g)
   ENDIF
   GOTO 4500
 4400 Nogo = .TRUE.
   WRITE (Output,99020) Ufm , idf
99020 FORMAT (A23,' 4052, IDF =',I10,' ON A FREEPT CARD DOES NOT ','APPEAR ON ANY FSLIST CARD.')
 4500 CALL write(scrt2,card,4,noeor)
   set102 = .TRUE.
!
!     ADD SPOINT TO CORE LIST
!
   IF ( nspnt+1<=core ) THEN
      nspnt = nspnt + 1
      Z(nspnt) = card(2)
      card(2) = 0
!
!     HARMONIC COEFFICIENT DATA
!
      DO i = ii , ni
         card(1) = 500000*Z(i) + idf
         nn = (Z(i)-1)/2
         IF ( mod(Z(i),2)==0 ) THEN
            rcard(3) = cos(float(nn)*angle)
         ELSE
            rcard(3) = sin(float(nn)*angle)
         ENDIF
         CALL write(scrt2,card,3,noeor)
      ENDDO
      CALL write(scrt2,mones,3,noeor)
      GOTO 4300
   ELSE
      WRITE (Output,99021) Ufm
99021 FORMAT (A23,' 4053, INSUFFICIENT CORE TO PERFORM OPERATIONS ','REQUIRED AS A RESULT OF FREEPT OR PRESPT DATA CARDS')
      icrq = nspnt + 1 - core
      WRITE (Output,99022) icrq
      Nogo = .TRUE.
      GOTO 6900
   ENDIF
!
!     CREATE MPC CARDS AND SPOINTS AS A RESULT OF PRESPT DATA.
!
 4600 IF ( press ) GOTO 4800
 4700 CALL locate(*4800,Z(buf1),prespt,flag)
   press = .TRUE.
   GOTO 4300
!
!     ANY SPOINTS IN CORE ARE AT THIS TIME OUTPUT TO GEOM2.
!
 4800 IF ( nspnt<ispnt ) GOTO 5100
!
!     COPY DATA FROM GEOM2 TO SCRT1 UP TO AND INCLUDING THE 3-WORD
!     RECORD HEADER FOR SPOINTS
!
   file = geom2
   CALL ifp4b(geom2,scrt1,any,Z(nspnt+1),core-nspnt,spoint,g2eof)
   IF ( .NOT.any ) GOTO 5000
   DO
      CALL read(*7200,*4900,geom2,Z(nspnt+1),core-nspnt,noeor,flag)
      CALL write(scrt1,Z(nspnt+1),core-nspnt,noeor)
   ENDDO
 4900 CALL write(scrt1,Z(nspnt+1),flag,noeor)
 5000 CALL write(scrt1,Z(ispnt),nspnt-ispnt+1,eor)
!
!     COPY BALANCE OF GEOM2 TO SCRT1,CLOSE THEM, AND SWITCH DESIGNATIONS
!
 5100 CALL ifp4b(geom2,scrt1,any,Z(nmpc+1),core-nmpc,-1,g2eof)
!
!     END OF GEOM2 PROCESSING
!     ***********************
!
!     COPY BALANCE OF MPC IMAGES ON GEOM4 TO SCRT2, COMPLETE LIST OF MPC
!     SETS.
!
   file = geom4
   IF ( id==0 ) GOTO 5300
 5200 IF ( id/=idlast ) THEN
!
!     ADD ID TO LIST
!
      idlast = id
      nmpc = nmpc + 1
      Z(nmpc) = id
   ENDIF
   CALL write(scrt2,id,1,noeor)
   DO
!
!     3-WORD GROUPS
!
      CALL read(*7200,*7300,geom4,card,3,noeor,flag)
      CALL write(scrt2,card,3,noeor)
      IF ( card(1)==-1 ) THEN
         CALL read(*7200,*5300,geom4,id,1,noeor,flag)
         GOTO 5200
      ENDIF
   ENDDO
 5300 CALL write(scrt2,0,0,eor)
   type(1) = mpcadd(1)
   type(2) = mpcadd(2)
!
!     GENERATION OF MPCADD OR SPCADD CARDS FROM USER ID-S.  FIRST
!     OUTPUT MANDATORY MPCADD OR SPCADD.
!
 5400 CALL ifp4f(type(2),geom4,bit)
   IF ( .NOT.set102 .AND. nmpc<impc .AND. .NOT.bit ) GOTO 5700
   CALL ifp4b(geom4,scrt2,any,Z(nmpc+1),core-nmpc,type,g4eof)
   IF ( set102 ) THEN
      card(1) = 200000000
      card(2) = 102
      card(3) = -1
      CALL write(scrt2,card,3,noeor)
   ENDIF
!
!     NOW FROM USER ID-S
!
   IF ( nmpc>=impc ) THEN
      DO i = impc , nmpc
         card(1) = Z(i) + 200000000
         card(2) = Z(i)
         nn = 3
         IF ( set102 ) THEN
            card(3) = 102
            nn = 4
         ENDIF
         card(nn) = -1
         CALL write(scrt2,card,nn,noeor)
      ENDDO
   ENDIF
!
!     IF USER MPCADD OR SPCADD CARDS ARE PRESENT, NOW CHANGE THEIR ID-S
!     AND ADD THE 102 SET IF IT EXISTS.
!
   IF ( .NOT.any ) GOTO 5600
 5500 CALL read(*7200,*5600,geom4,id,1,noeor,flag)
   id = id + 200000000
   CALL write(scrt2,id,1,noeor)
   IF ( set102 ) CALL write(scrt2,102,1,noeor)
   DO
      CALL read(*7200,*7300,geom4,id,1,noeor,flag)
      CALL write(scrt2,id,1,noeor)
      IF ( id==-1 ) GOTO 5500
   ENDDO
!
 5600 CALL write(scrt2,0,0,eor)
 5700 IF ( type(1)==spcadd(1) ) THEN
!
!     ALL PROCESSING COMPLETE ON GEOM4
!
      CALL ifp4b(geom4,scrt2,any,Z(1),core,mones,g4eof)
      GOTO 6900
   ELSE
!
!     START LIST OF SPC AND SPC1 ID-S
!
      ispc = nfslst + 1
      nspc = nfslst
      set102 = .FALSE.
      idlast = 0
!
!     CHECK BIT FOR SPC CARDS
!
      CALL ifp4f(spc(2),geom4,bit)
      IF ( .NOT.bit ) GOTO 5900
!
!     COPY GEOM4 TO SCRT2 UP TO SPC CARDS
!
      CALL ifp4b(geom4,scrt2,any,Z(ispc),core-ispc,spc,g4eof)
   ENDIF
   DO
!
!     COPY SPC IMAGES KEEPING LIST OF ID-S.
!
      CALL read(*7200,*5800,geom4,id,1,noeor,flag)
      IF ( id/=idlast ) THEN
         IF ( id/=102 ) THEN
            nspc = nspc + 1
            Z(nspc) = id
            idlast = id
         ELSE
            Nogo = .TRUE.
            WRITE (Output,99025) Ufm
         ENDIF
      ENDIF
      CALL write(scrt2,id,1,noeor)
      CALL read(*7200,*7300,geom4,card,3,noeor,flag)
      CALL write(scrt2,card,3,noeor)
   ENDDO
 5800 CALL write(scrt2,0,0,eor)
!
!     CHECK FOR ANY SPC1 IMAGES
!
 5900 CALL ifp4f(spc1(2),geom4,bit)
   IF ( .NOT.bit .AND. g/=0.0 ) GOTO 6800
!
!     COPY FROM GEOM4 TO SCRT2 UP TO SPC1 DATA.
!
   CALL ifp4b(geom4,scrt2,any,Z(nspc+1),core-nspc-2,spc1,g4eof)
!
!     COPY SPC1-S UP TO SETID .GE. 103.  SET 102 IS ILLEGAL FOR USER.
!
   IF ( .NOT.bit ) GOTO 6200
 6000 CALL read(*7200,*6200,geom4,id,1,noeor,flag)
   IF ( id>=102 ) THEN
      IF ( id/=102 ) GOTO 6300
      Nogo = .TRUE.
      WRITE (Output,99025) Ufm
   ENDIF
!
!     ADD ID TO LIST IF NOT YET IN LIST
!
   IF ( nspc>=ispc ) THEN
      DO i = ispc , nspc
         IF ( id==Z(i) ) GOTO 6100
      ENDDO
   ENDIF
!
!     ADD ID TO LIST
!
   nspc = nspc + 1
   Z(nspc) = id
 6100 CALL write(scrt2,id,1,noeor)
   CALL read(*7200,*7300,geom4,id,1,noeor,flag)
   CALL write(scrt2,id,1,noeor)
   DO
      CALL read(*7200,*7300,geom4,id,1,noeor,flag)
      CALL write(scrt2,id,1,noeor)
      IF ( id==-1 ) GOTO 6000
   ENDDO
!
!     IF G IS ZERO AND THERE ARE FSLST ENTRIES, GENERATE SPC1-S NOW.
!
 6200 id = 0
 6300 IF ( g==0.0 .AND. nfslst>=ifslst ) THEN
!
!     GENERATION OF HARMONIC SPC1-S
!
      DO i = ifslst , nfslst , 3
         IF ( Z(i)/=-1 ) THEN
            card(1) = 102
            card(2) = 0
            CALL write(scrt2,card,2,noeor)
            DO j = ii , ni
               CALL write(scrt2,Z(i)+500000*Z(j),1,noeor)
            ENDDO
            CALL write(scrt2,minus1,1,noeor)
         ENDIF
      ENDDO
      set102 = .TRUE.
   ENDIF
!
!     COMPLETE COPYING OF SPC1 CARDS TO SCRT2 WITH SETID-S .GE. 103
!
   IF ( id==0 ) GOTO 6700
!
!     ADD ID TO LIST IF NOT YET IN
!
   IF ( nspc<ispc ) GOTO 6500
 6400 DO i = ispc , nspc
      IF ( id==Z(i) ) GOTO 6600
   ENDDO
!
!     ID NOT IN LIST, THUS ADD IT.
!
 6500 nspc = nspc + 1
   Z(nspc) = id
!
!     CONTINUE COPYING DATA TO NEXT ID
!
 6600 CALL write(scrt2,id,1,noeor)
   DO
      CALL read(*7200,*7300,geom4,id,1,noeor,flag)
      CALL write(scrt2,id,1,noeor)
      IF ( id==-1 ) THEN
         CALL read(*7200,*6700,geom4,id,1,noeor,flag)
         GOTO 6400
      ENDIF
   ENDDO
!
!     END OF SPC1 CARD IMAGES.
!
 6700 CALL write(scrt2,0,0,eor)
!
!     SORT LIST OF SPC AND SPC1 ID-S
!
   CALL sort(0,0,1,1,Z(ispc),nspc-ispc+1)
!
!     SPCADD WORK (USE MPCADD LOGIC)
!
 6800 type(1) = spcadd(1)
   type(2) = spcadd(2)
   impc = ispc
   nmpc = nspc
   GOTO 5400
!
!     END OF GEOM4 PROCESSING
!     ***********************
!
!     AXIC FILE NOT IN FIST OR AXIF CARD IS MISSING, THUS DO NOTHING.
!
 6900 CALL close(axic,Clsrew)
   CALL conmsg(msg2,2,0)
   RETURN
!
!     END OF FILE ON AXIC
!
 7000 file = axic
   GOTO 7200
!
!     END OF RECORD ON AXIC
!
 7100 file = axic
   GOTO 7300
!
!     END OF FILE OR END OF RECORD ON -FILE-, OR FILE NOT IN FIST.
!
 7200 ier = -2
   GOTO 7700
 7300 ier = -3
   GOTO 7700
 7400 ier = -1
   GOTO 7700
 7500 ier = -8
   file = icrq
   GOTO 7700
 7600 ier = -37
 7700 CALL mesage(ier,file,subr)
99022 FORMAT (5X,'ADDITIONAL CORE NEEDED =',I8,' WORDS.')
99023 FORMAT (A23,' 5003, ZERO X2 VALUE ON RINGFL CARD WITH SPHERICAL ','COORDINATES.  FLUID POINT ID =',I10)
99024 FORMAT (A23,' 4038, RINGFL CARD HAS ID =',I20,' WHICH HAS BEEN ','USED.')
99025 FORMAT (A23,' 4055, SET ID = 102 MAY NOT BE USED FOR SPC CARDS ','WHEN USING THE HYDROELASTIC-FLUID ELEMENTS.')
END SUBROUTINE ifp4
