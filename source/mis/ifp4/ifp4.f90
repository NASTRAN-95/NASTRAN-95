!*==ifp4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp4
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: angle , bd , drho , g , r , rhob , rj , rjl1 , rjp1 , temp , temp1 , temp2 , zj , zjl1 , zjp1 , ztemp , zz
   LOGICAL :: any , anygb , anygrd , bit , bit2 , end , g1eof , g2eof , g4eof , harms , mateof , press , set102
   INTEGER , SAVE :: axic , eor , geom1 , geom2 , geom4 , matpol , minus1 , noeor , scrt1 , scrt2
   INTEGER , DIMENSION(2) , SAVE :: axif , bdylst , bndfl , cfsmas , dmiax , dmig , flsym , freept , fslst , grid , gridb , mpc ,   &
                                  & mpcadd , msg1 , msg2 , prespt , ringfl , seqgp , spc , spc1 , spcadd , spoint , subr
   INTEGER , DIMENSION(10) :: buf , card , last
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , core , corsys , csf , entrys , file , flag , i , i2 , ibd , ibdyl , icore , icrq , &
            & id , idf , idfaft , idfpre , idlast , idrho , ier , ifslst , igridb , ii , ilist , impc , in , iname , index ,        &
            & iretrn , irhob , iring , ispc , ispnt , itemp , itwo31 , j , jgridb , jpoint , jsave , k , kid , l , n , nbdyl ,      &
            & nfslst , ngridb , ni , nlist , nmpc , nn , nname , nosym , nring , nsize , nspc , nspnt , ntemp , nwords , point ,    &
            & space , words
   INTEGER , DIMENSION(6) , SAVE :: cfluid
   INTEGER , DIMENSION(8) , SAVE :: cord
   REAL , SAVE :: degrad
   INTEGER , DIMENSION(4) , SAVE :: mones , ncord
   REAL , DIMENSION(10) :: rbuf , rcard
   REAL , DIMENSION(4) :: rz
   INTEGER , DIMENSION(5) :: saveid
   INTEGER , DIMENSION(7) :: trail
   INTEGER , DIMENSION(2) :: type
   EXTERNAL bisloc , close , conmsg , fwdrec , ifp4b , ifp4c , ifp4e , ifp4f , ifp4g , korsz , locate , mesage , preloc , rdtrl ,   &
          & read , rewind , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Z(1),Rz(1)) , (buf(1),rbuf(1)) , (card(1),rcard(1)) , (core,icore) , (rhob,irhob) , (bd,ibd)
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINE CORE AND BUFFER POINTERS
!
         CALL conmsg(msg1,2,0)
         icore = korsz(z)
         buf1 = icore - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         buf4 = buf3 - sysbuf - 2
         buf5 = buf4 - sysbuf - 2
         icore = buf5 - 1
         icrq = 100 - icore
         IF ( icore<100 ) THEN
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OPEN AXIC DATA BLOCK (IF NAME NOT IN FIST RETURN - NO MESSAGE)
!
         CALL preloc(*720,z(buf1),axic)
!
!     PICK UP AXIF CARD. (IF AXIF CARD NOT PRESENT - RETURN NO MESSAGE)
!
         CALL locate(*720,z(buf1),axif,flag)
         CALL read(*740,*20,axic,z(1),icore,eor,words)
         WRITE (output,99001) ufm
99001    FORMAT (A23,' 4031, INSUFFICIENT CORE TO READ DATA ON AXIF CARD.')
         WRITE (output,99022) icore
!
!     FATAL ERROR NO MORE PROCESSING POSSIBLE
!
         nogo = .TRUE.
         GOTO 720
!
!     DATA OF AXIF CARD IS NOW STORED
!
 20      csf = z(1)
         g = rz(2)
         drho = rz(3)
         j = 3
         idrho = z(j)
         bd = rz(4)
         nosym = z(j+2)
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
            IF ( j/=1 ) CALL sort(0,0,1,1,z(in),j)
            ii = nn + 1
            ni = nn
            DO i = in , nn
               itemp = 2*z(i)
               IF ( nosym/=0 ) THEN
                  IF ( z(i)/=0 ) THEN
                     ni = ni + 1
                     z(ni) = itemp + 1
                  ENDIF
               ENDIF
               ni = ni + 1
               z(ni) = itemp + 2
            ENDDO
            n = ni - ii + 1
!
!     SET MAXIMUM HARMONIC+1 FOR USE BY SDR2C AND VDRB
!
            iaxif = z(nn) + 1
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
            IF ( trail(i)/=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (2)
         CALL preloc(*820,z(buf2),geom1)
         DO i = 1 , 4
            i2 = 2*i
            CALL locate(*40,z(buf2),cord(i2-1),flag)
            nsize = ncord(i)
            DO
               CALL read(*780,*40,geom1,z(ni+1),nsize,noeor,flag)
               IF ( z(ni+1)==csf ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
 40      ENDDO
         spag_nextblock_1 = 3
      CASE (3)
!
!     FALL THROUGH LOOP IMPLIES COORDINATE SYSTEM WAS NOT FOUND
!
         nogo = .TRUE.
         WRITE (output,99002) ufm , csf
99002    FORMAT (A23,' 4033, COORDINATE SYSTEM ID =',I20,' AS SPECIFIED ','ON AXIF CARD IS NOT PRESENT',/5X,' AMONG ANY OF CORD1C,',&
                &' CORD1S, CORD2C, OR CORD2S CARD TYPES.',/5X,' CYLINDRICAL TYPE ASSUMED FOR CONTINUING DATA CHECK.')
         corsys = 2
         spag_nextblock_1 = 5
      CASE (4)
         corsys = z(ni+2)
         spag_nextblock_1 = 5
      CASE (5)
         CALL close(geom1,clsrew)
!
!     READ INTO CORE FROM AXIC ALL GRIDB CARD IMAGES (5 WORDS / IMAGE)
!
         anygb = .FALSE.
         igridb = ni + 1
         ngridb = ni
         CALL locate(*80,z(buf1),gridb,flag)
         anygb = .TRUE.
         space = core - ni
         CALL read(*740,*60,axic,z(igridb),space,eor,nwords)
         nogo = .TRUE.
         WRITE (output,99003) ufm
99003    FORMAT (A23,' 4034, INSUFFICIENT CORE TO HOLD GRIDB CARD IMAGES.')
         WRITE (output,99022) space
         anygb = .FALSE.
         GOTO 80
 60      ngridb = ni + nwords
!
!     IF ANY GRIDB IMAGES ARE PRESENT A BOUNDARY LIST IS FORMED IN CORE.
!
 80      ibdyl = ngridb + 1
         nbdyl = ngridb
         IF ( .NOT.anygb ) GOTO 160
         CALL locate(*160,z(buf1),bdylst,flag)
         spag_nextblock_1 = 6
      CASE (6)
         CALL read(*740,*100,axic,rhob,1,noeor,flag)
         IF ( irhob==1 ) THEN
            IF ( idrho/=1 ) THEN
               rhob = drho
            ELSE
               nogo = .TRUE.
               WRITE (output,99004) ufm
99004          FORMAT (A23,' 4035, THE FLUID DENSITY HAS NOT BEEN SPECIFIED ON ','A BDYLIST CARD AND',/5X,                          &
                      &'THERE IS NO DEFAULT FLUID ','DENSITY SPECIFIED ON THE AXIF CARD.')
               rhob = 1.0
            ENDIF
         ENDIF
         end = .FALSE.
         idfpre = 0
         SPAG_Loop_1_1: DO
            CALL read(*740,*760,axic,idf,1,noeor,flag)
            IF ( idf/=0 ) EXIT SPAG_Loop_1_1
            idfpre = -1
         ENDDO SPAG_Loop_1_1
         DO
            CALL read(*740,*760,axic,idfaft,1,noeor,flag)
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
                  z(nbdyl+1) = idf
                  z(nbdyl+2) = 1
                  z(nbdyl+3) = 1
                  z(nbdyl+4) = 1
                  z(nbdyl+5) = idfpre
                  z(nbdyl+6) = idfaft
                  rz(nbdyl+7) = rhob
                  nbdyl = nbdyl + 7
               ELSE
                  WRITE (output,99005) ufm
99005             FORMAT (A23,' 4036, INSUFFICIENT CORE TO BUILD BOUNDARY LIST ','TABLE.')
                  icrq = nbdyl + 7 - core
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     ROTATE THE ID-S
!
            idfpre = idf
            idf = idfaft
            IF ( end ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     SORT ENTRIES ON FIRST WORD OF EACH ENTRY.
!
 100     CALL sort(0,0,7,1,z(ibdyl),nbdyl-ibdyl+1)
         entrys = (nbdyl-ibdyl+1)/7
!
!     PASS THE RINGFL IMAGES INSERTING X1, X2, AND X3 IN THE APPROPRIATE
!     BDYLIST ENTRY.
!
         CALL locate(*140,z(buf1),ringfl,flag)
 120     DO
            CALL read(*740,*140,axic,buf,4,noeor,flag)
            IF ( corsys==3 ) THEN
               IF ( rbuf(3)==0. ) THEN
                  nogo = .TRUE.
                  WRITE (output,99023) ufm , buf(1)
               ENDIF
            ENDIF
            IF ( buf(corsys+1)/=0 ) THEN
               nogo = .TRUE.
               IF ( corsys==3 ) THEN
                  WRITE (output,99006) ufm , buf(1)
99006             FORMAT (A23,' 4043, COORDINATE SYSTEM IS SPHERICAL BUT RINGFL ','CARD ID =',I20,' HAS A NON-ZERO X3 VALUE.')
               ELSE
                  WRITE (output,99007) ufm , buf(1)
99007             FORMAT (A23,'4042, COORDINATE SYSTEM IS CYLINDRICAL BUT RINGFL ','CARD ID =',I20,' HAS A NON-ZERO X2 VALUE.')
               ENDIF
            ENDIF
            CALL bisloc(*120,buf(1),z(ibdyl),7,entrys,jpoint)
            ntemp = ibdyl + jpoint - 1
            IF ( z(ntemp+1)==1 ) THEN
!
!     CHECK TO GET RANGE OF BDYLIST HAVING THIS SAME ID.
!     THEN FILL IN X1, X2, AND X3 IN THOSE ENTRIES.
!
               nlist = ntemp
               SPAG_Loop_2_2: DO
                  ntemp = ntemp - 7
                  IF ( ntemp<ibdyl ) EXIT SPAG_Loop_2_2
                  IF ( z(ntemp)/=z(ntemp+7) ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
               ilist = ntemp + 7
               ntemp = nlist
               SPAG_Loop_2_3: DO
                  ntemp = ntemp + 7
                  IF ( ntemp>nbdyl ) EXIT SPAG_Loop_2_3
                  IF ( z(ntemp)/=z(ntemp-7) ) EXIT SPAG_Loop_2_3
               ENDDO SPAG_Loop_2_3
               nlist = ntemp - 1
               DO i = ilist , nlist , 7
                  z(i+1) = buf(2)
                  z(i+2) = buf(3)
                  z(i+3) = buf(4)
               ENDDO
            ELSE
               nogo = .TRUE.
               WRITE (output,99024) ufm , buf(1)
            ENDIF
         ENDDO
!
!     CHECK TO SEE THAT X1, X2, AND X3 WERE FOUND FOR ALL ENTRIES.
!
 140     DO i = ibdyl , nbdyl , 7
            IF ( z(i+1)==1 ) THEN
               nogo = .TRUE.
               WRITE (output,99008) ufm , z(i)
99008          FORMAT (A23,' 4040, ID =',I20,' APPEARS ON A BDYLIST CARD, BUT ','NO RINGFL CARD IS PRESENT WITH THE SAME ID.')
            ENDIF
         ENDDO
!
!     OPEN GEOM1, OPEN SCRATCH1, COPY HEADER REC FROM GEOM1 TO SCRATCH1
!
 160     CALL ifp4c(geom1,scrt1,z(buf2),z(buf3),g1eof)
!
!     COPY ALL DATA UP TO FIRST GRID CARD IMAGE.
!
         CALL ifp4b(geom1,scrt1,any,z(nbdyl+1),core-nbdyl,grid,g1eof)
         anygrd = any
         IF ( .NOT.anygb ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nbdyl<ibdyl ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CREATE AND MERGE WITH GRIDS FROM GEOM1, GRIDS FROM GRIDB IMAGES.
!
         file = geom1
         IF ( any ) THEN
            CALL read(*780,*180,geom1,last,8,noeor,flag)
            CALL ifp4e(last(1))
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 180     any = .FALSE.
         spag_nextblock_1 = 7
      CASE (7)
         DO i = igridb , ngridb , 5
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  card(1) = z(i)
                  CALL ifp4e(card(1))
                  card(2) = csf
                  kid = z(i+4)
                  CALL bisloc(*182,kid,z(ibdyl),7,entrys,point)
                  ntemp = ibdyl + point - 1
                  card(3) = z(ntemp+1)
                  card(4) = z(ntemp+2)
                  card(5) = z(ntemp+3)
                  card(corsys+2) = z(i+1)
                  card(6) = z(i+2)
                  card(7) = z(i+3)
                  card(8) = 0
!
!     MERGE CARD IN
!
                  IF ( any ) THEN
                     DO WHILE ( last(1)<=card(1) )
                        CALL write(scrt1,last,8,noeor)
                        CALL read(*780,*184,geom1,last,8,noeor,flag)
                        CALL ifp4e(last(1))
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 182              nogo = .TRUE.
                  WRITE (output,99009) ufm , z(i) , z(i+4)
99009             FORMAT (A23,' 4057, GRIDB CARD WITH ID =',I10,' HAS A REFERENCE ','IDF =',I10,/5X,                                &
                         &'WHICH DOES NOT APPEAR IN A BOUNDARY LIST')
                  CYCLE
 184              any = .FALSE.
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL write(scrt1,card,8,noeor)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
         IF ( any ) THEN
            DO
               CALL write(scrt1,last,8,noeor)
               CALL read(*780,*200,geom1,last,8,noeor,flag)
               CALL ifp4e(last(1))
            ENDDO
         ENDIF
!
!     FURTHER ALTERATIONS TO BOUNDARY LIST TABLE AT THIS TIME.
!     RADIAL LOCATION (RJ) AND VERTICAL LOCATION (ZJ)
!
 200     nring = ngridb
         IF ( .NOT.harms ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = ibdyl , nbdyl , 7
            IF ( corsys==3 ) THEN
!
               angle = rz(i+2)*degrad
               temp = rz(i+1)
               rz(i+1) = temp*sin(angle)
               rz(i+2) = temp*cos(angle)
            ELSE
               z(i+2) = z(i+3)
            ENDIF
         ENDDO
!
!     LENGTH AND ASSOCIATED ANGLE COMPONENTS OF A CONICAL SECTION. L,C,S
!
         IF ( .NOT.(nogo) ) THEN
            DO i = ibdyl , nbdyl , 7
               rj = rz(i+1)
               zj = rz(i+2)
!
!     FIND R   , Z     AND  R   , Z     (RJL1,ZJL1,RJP1,ZJP1)
!           J-1   J-1        J+1   J+1
!
               IF ( z(i+4)<0 ) THEN
!
!     SECONDARY ID IS AXIS
!
                  rjl1 = 0
                  zjl1 = zj
               ELSEIF ( z(i+4)==0 ) THEN
!
!     SECONDARY ID IS NOT AVAILABLE
!
                  rjl1 = rj
                  zjl1 = zj
               ELSE
!
!     FIND SECONDARY ID ENTRY
!
                  kid = z(i+4)
                  CALL bisloc(*840,kid,z(ibdyl),7,entrys,point)
                  ntemp = ibdyl + point - 1
                  rjl1 = rz(ntemp+1)
                  zjl1 = rz(ntemp+2)
               ENDIF
!
!     SECONDARY ID ON PLUS SIDE
!
               IF ( z(i+5)<0 ) THEN
!
!     SECONDARY ID IS AXIS
!
                  rjp1 = 0
                  zjp1 = zj
               ELSEIF ( z(i+5)==0 ) THEN
!
!     SECONDARY ID IS NOT AVAILABLE
!
                  rjp1 = rj
                  zjp1 = zj
               ELSE
!
!     FIND SECONDARY ID ENTRY
!
                  kid = z(i+5)
                  CALL bisloc(*840,kid,z(ibdyl),7,entrys,point)
                  ntemp = ibdyl + point - 1
                  rjp1 = rz(ntemp+1)
                  zjp1 = rz(ntemp+2)
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
                  rz(i+3) = sqrt(r*r+zz*zz)
                  IF ( rz(i+3)/=0.0 ) THEN
!
                     rz(i+4) = zz/rz(i+3)
                     rz(i+5) = r/rz(i+3)
                  ELSE
                     nogo = .TRUE.
                     WRITE (output,99010) ufm , z(i)
99010                FORMAT (A23,' 4045, THE BOUNDARY LIST ENTRY FOR ID =',I9,' HAS A ZERO CROSS-SECTION LENGTH.')
                  ENDIF
               ELSE
                  nogo = .TRUE.
                  WRITE (output,99011) ufm , z(i)
99011             FORMAT (A23,' 4044, RINGFL CARD ID =',I20,' HAS SPECIFIED A ','ZERO RADIAL LOCATION.')
               ENDIF
            ENDDO
         ENDIF
!
!     SORT GRIDB IMAGES TO BE IN SORT ON RID AND PHI WITHIN EACH RID
!
         ntemp = ngridb - igridb + 1
         CALL sort(0,0,5,-2,z(igridb),ntemp)
         CALL sort(0,0,5,-5,z(igridb),ntemp)
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
         CALL ifp4c(matpol,scrt2,z(buf4),z(buf5),mateof)
         IF ( mateof ) GOTO 320
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
         IF ( .NOT.bit .AND. .NOT.bit2 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(matpol,clsrew)
         CALL preloc(*820,z(buf4),matpol)
!
!     WRITE DMIG HEADER.
!
         buf(1) = dmig(1)
         buf(2) = dmig(2)
         buf(3) = 120
         CALL write(scrt2,buf,3,noeor)
         IF ( .NOT.bit ) GOTO 240
         CALL locate(*240,z(buf4),dmiax,flag)
         ASSIGN 220 TO iretrn
         spag_nextblock_1 = 8
      CASE (8)
!
!     READ 9 WORD HEADER
!
         GOTO iretrn
 220     CALL read(*780,*240,matpol,buf,9,noeor,flag)
!
!     SAVE NAME
!
         z(iname) = buf(1)
         z(iname+1) = buf(2)
         nname = nname + 2
         icrq = nname + 2 - icore
         IF ( icrq>0 ) THEN
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL write(scrt2,buf,9,noeor)
         spag_nextblock_1 = 9
      CASE (9)
         DO
!
!     COPY THE COLUMN DATA.  FIRST THE COLUMN INDEX.
!
            CALL read(*780,*800,matpol,buf,2,noeor,flag)
            CALL write(scrt2,buf,2,noeor)
            IF ( buf(1)<0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            SPAG_Loop_2_4: DO
!
!     TERMS OF COLUMN
!
               CALL read(*780,*800,matpol,buf,2,noeor,flag)
               CALL write(scrt2,buf,2,noeor)
               IF ( buf(1)<0 ) EXIT SPAG_Loop_2_4
               CALL read(*780,*800,matpol,buf,1,noeor,flag)
               CALL write(scrt2,buf,1,noeor)
            ENDDO SPAG_Loop_2_4
         ENDDO
!
!     DMIAX-S ALL COPIED.  NOW COPY ANY DMIG-S.
!
 240     IF ( .NOT.bit2 ) GOTO 280
         CALL locate(*280,z(buf4),dmig,flag)
         ASSIGN 260 TO iretrn
!
!     READ HEADER
!
 260     CALL read(*740,*280,matpol,buf,9,noeor,flag)
!
!     CHECK THE NAME FOR BEING THE SAME AS ONE ON A DMIAX CARD
!
         DO i = iname , nname , 2
            IF ( buf(1)==z(i) ) THEN
               IF ( buf(2)==z(i+1) ) THEN
!
!     ERROR FOR NAME DOES MATCH THAT OF A DMIAX NAME
!
                  nogo = .TRUE.
                  WRITE (output,99012) ufm , buf(1) , buf(2)
99012             FORMAT (A23,' 4062, DMIG BULK DATA CARD SPECIFIES DATA BLOCK ',2A4,' WHICH ALSO APPEARS ON A DMIAX CARD.')
               ENDIF
            ENDIF
         ENDDO
!
!     COPY THE COLUMN DATA
!
         CALL write(scrt2,buf,9,noeor)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     WRITE THE END OF RECORD FOR DMIG CARDS
!
 280     CALL write(scrt2,0,0,eor)
!
!     TURN ON BIT FOR DMIG CARD TYPE
!
         CALL ifp4g(dmig(2),matpol)
         CALL rewind(matpol)
         CALL fwdrec(*780,matpol)
         spag_nextblock_1 = 10
      CASE (10)
         DO
!
!     COPY EVERYTHING ON MATPOL TO SCRT2, EXCEPT FOR DMIG, DMIAX, AND
!     THE 2**31-1 RECORD.
!
            CALL read(*320,*800,matpol,buf,3,noeor,flag)
!     2147483647  = 2**31-1
            itwo31 = 2147483647
            IF ( buf(1)/=itwo31 .AND. (buf(1)/=dmig(1) .OR. buf(2)/=dmig(2)) .AND. (buf(1)/=dmiax(1) .OR. buf(2)/=dmiax(2)) ) THEN
               CALL read(*780,*300,matpol,z(nbdyl+1),core-nbdyl,noeor,flag)
               CALL write(scrt2,z(nbdyl+1),core-nbdyl,noeor)
            ELSE
               CALL fwdrec(*780,matpol)
            ENDIF
         ENDDO
 300     CALL write(scrt2,z(nbdyl+1),flag,eor)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 320     mateof = .TRUE.
         CALL ifp4b(matpol,scrt2,any,z(nbdyl+1),core-nbdyl,bndfl,mateof)
         card(1) = 0
         card(2) = 0
         card(3) = 0
         card(4) = n
         CALL locate(*340,z(buf1),flsym,flag)
         CALL read(*740,*760,axic,card,3,eor,flag)
 340     CALL write(scrt2,z(1),5,noeor)
         CALL write(scrt2,card,4,noeor)
         CALL write(scrt2,z(ii),n,noeor)
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
            IF ( z(i)==z(i+7) ) jsave = jgridb
!
!     IF RHO FOR A FLUID POINT IS ZERO WE DO NOT PUT OUT FLUID
!     DATA AND CONNECTED POINTS.
!
            IF ( rz(i+6)/=0 ) CALL write(scrt2,z(i),7,noeor)
!
!     APPEND GRIDB POINTS WITH THEIR ANGLES.
!
            SPAG_Loop_2_5: DO WHILE ( jgridb<=ngridb )
               IF ( z(jgridb+4)<z(i) ) THEN
                  jgridb = jgridb + 5
               ELSEIF ( z(jgridb+4)==z(i) ) THEN
!
!     APPEND THE POINT
!
                  IF ( rz(i+6)/=0 ) CALL write(scrt2,z(jgridb),2,noeor)
                  jgridb = jgridb + 5
               ELSE
                  EXIT SPAG_Loop_2_5
               ENDIF
            ENDDO SPAG_Loop_2_5
!
!     COMPLETE THE ENTRY
!
            IF ( rz(i+6)/=0 ) CALL write(scrt2,mones,2,noeor)
         ENDDO
!
!     COMPLETE RECORD.
!
         CALL write(scrt2,0,0,eor)
         CALL ifp4b(matpol,scrt2,any,z(ngridb+1),core-ngridb,mones,mateof)
         spag_nextblock_1 = 11
      CASE (11)
!
!  READ ALL RINGFL CARD IMAGES INTO CORE
!
         IF ( .NOT.(anygb) ) THEN
            IF ( anygrd ) THEN
!
!     COPY GRID CARDS NOT COPIED AS A RESULT OF THE ABSENCE OF GRIDB
!     CARDS.
!
               file = geom1
               DO
                  CALL read(*780,*360,geom1,card,8,noeor,flag)
                  CALL write(scrt1,card,8,noeor)
               ENDDO
            ENDIF
         ENDIF
 360     iring = ngridb + 1
         nring = ngridb
         CALL locate(*400,z(buf1),ringfl,flag)
         CALL read(*740,*380,axic,z(iring),core-iring,noeor,flag)
         WRITE (output,99013) ufm
99013    FORMAT (A23,' 4047, INSUFFICIENT CORE TO HOLD RINGFL IMAGES.')
         icrq = core - iring
         WRITE (output,99022) icrq
         nogo = .TRUE.
         GOTO 720
 380     nring = iring + flag - 1
!
!     OUTPUT HARMONIC GRID CARDS.
!
 400     IF ( nring>=iring ) THEN
!
!     SORT RINGFL IDS
!
            CALL sort(0,0,4,1,z(iring),flag)
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
               index = z(i)*500000
               SPAG_Loop_2_6: DO k = iring , nring , 4
!
!     CALL IFP4E TO CHECK ID RANGE 1 TO 99999
!
                  CALL ifp4e(z(k))
                  IF ( k/=iring ) THEN
                     IF ( z(k)==ztemp ) THEN
                        nogo = .TRUE.
                        WRITE (output,99024) ufm , z(k)
                     ENDIF
                  ENDIF
                  ztemp = z(k)
                  card(1) = z(k) + index
                  IF ( corsys==3 ) THEN
                     angle = rz(k+2)*degrad
                     rcard(3) = rz(k+1)*sin(angle)
                     rcard(4) = rz(k+1)*cos(angle)
                     IF ( rcard(3)==0.0 ) THEN
                        nogo = .TRUE.
                        WRITE (output,99023) ufm , z(k)
                        EXIT SPAG_Loop_2_6
                     ENDIF
                  ELSE
                     card(3) = z(k+1)
                     card(4) = z(k+3)
                  ENDIF
                  CALL write(scrt1,card,8,noeor)
               ENDDO SPAG_Loop_2_6
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
         IF ( nring<iring ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL ifp4b(geom1,scrt1,any,z(nring+1),core-nring,seqgp,g1eof)
!
!     COPY ALL SEQGP CARDS OVER ALSO (ID-S MUST BE OF CORRECT VALUE).
!
         file = geom1
         IF ( any ) THEN
            DO
               CALL read(*780,*420,geom1,card,2,noeor,flag)
               CALL ifp4e(card(1))
               CALL write(scrt1,card,2,noeor)
            ENDDO
         ENDIF
!
!     NOW OUTPUT SEQGP CARDS FOR HARMONICS OF EACH RINGFL.
!
 420     DO i = ii , ni
            index = z(i)*500000
            ntemp = z(i) - 1
            DO k = iring , nring , 4
               card(1) = z(k) + index
               card(2) = z(k)*1000 + ntemp
               CALL write(scrt1,card,2,noeor)
            ENDDO
         ENDDO
         spag_nextblock_1 = 12
      CASE (12)
         CALL write(scrt1,0,0,eor)
         spag_nextblock_1 = 13
      CASE (13)
!
!     COPY BALANCE OF GEOM1 TO SCRT1 (IF ANY MORE, WRAP UP, AND COPY
!     BACK)
!
         CALL ifp4b(geom1,scrt1,any,z(nring+1),core-nring,mones,g1eof)
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
         CALL ifp4c(geom2,scrt1,z(buf2),z(buf3),g2eof)
!
!     PROCESS CFLUID2, CFLUID3, AND CFLUID4 CARDS.
!
         DO i = 1 , 3
            i2 = 2*i
            CALL locate(*440,z(buf1),cfluid(i2-1),flag)
!
!     COPY DATA FROM GEOM2 TO SCRT1 UP TO POINT WHERE CFLUID CARDS GO
!     AND WRITE 3-WORD RECORD ID.
!
            CALL ifp4b(geom2,scrt1,any,z(ni+1),core-ni,cfluid(2*i-1),g2eof)
            DO
               CALL read(*740,*430,axic,card,i+4,noeor,flag)
               IF ( card(i+3)==1 ) THEN
                  IF ( idrho==1 ) THEN
                     nogo = .TRUE.
                     WRITE (output,99014) ufm , card(1)
99014                FORMAT (A23,' 4058, THE FLUID DENSITY HAS NOT BEEN SPECIFIED ON ','A CFLUID CARD WITH ID =',I10,/5X,           &
                            &'AND THERE IS NO DEFAULT ON THE AXIF CARD.')
                  ENDIF
                  rcard(i+3) = drho
               ENDIF
               IF ( card(i+4)==1 ) THEN
                  IF ( ibd==1 ) THEN
                     nogo = .TRUE.
                     WRITE (output,99015) ufm , card(1)
99015                FORMAT (A23,' 4059, THE FLUID BULK MODULUS HAS NOT BEEN SPECIFIED',' ON A CFLUID CARD WITH ID =',I10,/5X,      &
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
                  card(1) = saveid(1)*1000 + z(k)
                  index = 500000*z(k)
                  DO l = 2 , ntemp
                     card(l) = saveid(l) + index
                  ENDDO
                  card(ntemp+3) = (z(k)-1)/2
                  CALL write(scrt1,card,ntemp+3,noeor)
               ENDDO
            ENDDO
!
!     END OF CFLUID DATA
!
 430        CALL write(scrt1,0,0,eor)
 440     ENDDO
!
!     CONSTRUCTION OF FSLIST TABLE IN CORE 3-WORDS/ENTRY
!
         ifslst = ni + 1
         nfslst = ni
         CALL locate(*480,z(buf1),fslst,flag)
         SPAG_Loop_1_7: DO
            CALL read(*740,*460,axic,rhob,1,noeor,flag)
            IF ( irhob==1 ) THEN
               IF ( idrho/=1 ) THEN
                  rhob = drho
               ELSE
                  nogo = .TRUE.
                  WRITE (output,99016) ufm
99016             FORMAT (A23,' 4048, THE FLUID DENSITY HAS NOT BEEN SPECIFIED ON ','AN FSLIST CARD AND',/5X,                       &
                         &'THERE IS NO DEFAULT FLUID ','DENSITY SPECIFIED ON THE AXIF CARD.')
                  rhob = 1.0
               ENDIF
            ENDIF
            CALL read(*740,*760,axic,idf,1,noeor,flag)
            IF ( idf==0 ) idf = -1
            DO
               CALL read(*740,*760,axic,idfaft,1,noeor,flag)
               IF ( idfaft==-1 ) idfaft = -2
               IF ( idfaft==0 ) idfaft = -1
               IF ( nfslst+3<=core ) THEN
                  z(nfslst+1) = idf
                  z(nfslst+2) = idfaft
                  rz(nfslst+3) = rhob
                  nfslst = nfslst + 3
                  IF ( idfaft==-2 ) CYCLE SPAG_Loop_1_7
                  idf = idfaft
               ELSE
                  WRITE (output,99017) ufm
99017             FORMAT (A23,' 4049, INSUFFICIENT CORE TO BUILD FREE SURFACE ','LIST TABLE.')
                  icrq = nfslst + 3 - core
                  WRITE (output,99022) icrq
                  nogo = .TRUE.
                  GOTO 720
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
!
!     TABLE IS COMPLETE. COPY GEOM2 DATA TO SCRT1 UP TO CFSMASS RECORD
!     SLOT
!
 460     IF ( nfslst>ifslst ) THEN
            CALL ifp4b(geom2,scrt1,any,z(nfslst+1),core-nfslst,cfsmas,g2eof)
            entrys = (nfslst-ifslst+1)/3
            k = 0
            DO i = ifslst , nfslst , 3
               IF ( z(i+1)/=-2 ) THEN
                  k = k + 1000000
                  rcard(4) = rz(i+2)*g
                  DO l = ii , ni
                     index = 500000*z(l)
                     card(1) = k + z(l)
                     card(2) = z(i) + index
                     IF ( z(i)<=0 ) card(2) = z(i+1) + index
                     card(3) = z(i+1) + index
                     IF ( z(i+1)<=0 ) card(3) = z(i) + index
                     card(5) = (z(l)-1)/2
                     CALL write(scrt1,card,5,noeor)
                  ENDDO
               ENDIF
            ENDDO
            CALL write(scrt1,0,0,eor)
         ELSE
            nogo = .TRUE.
            WRITE (output,99018) ufm
99018       FORMAT (A23,' 4050, FSLIST CARD HAS INSUFFICIENT IDF DATA, OR ','FSLIST DATA MISSING.')
         ENDIF
!
!     BEGIN GEOM4 PROCESSING
!     **********************
!
!     OPEN GEOM4 AND SCRT2 AND COPY HEADER RECORD FROM GEOM4 TO SCRT2.
!
 480     CALL ifp4c(geom4,scrt2,z(buf4),z(buf5),g4eof)
!
!     COPY ALL DATA ON GEOM4 TO SCRT2 UP TO AND INCLUDING 3-WORD RECORD
!     HEADER OF MPC-RECORD.
!
         CALL ifp4b(geom4,scrt2,any,z(nfslst+1),core-nfslst,mpc,g4eof)
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
               CALL read(*780,*500,geom4,id,1,noeor,flag)
               IF ( id>102 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( id==102 ) THEN
                  nogo = .TRUE.
                  WRITE (output,99019) ufm
99019             FORMAT (A23,' 4051, AN MPC CARD HAS A SET ID SPECIFIED = 102. ',' SET 102 IS ILLEGAL WHEN FLUID DATA IS PRESENT.')
               ENDIF
               CALL write(scrt2,id,1,noeor)
!
!     ADD ID TO LIST IF NOT IN LIST
!
               IF ( id/=idlast ) THEN
                  nmpc = nmpc + 1
                  z(nmpc) = id
               ENDIF
               SPAG_Loop_2_8: DO
!
!     3 WORD GROUPS
!
                  CALL read(*780,*800,geom4,card,3,noeor,flag)
                  CALL write(scrt2,card,3,noeor)
                  IF ( card(1)==-1 ) EXIT SPAG_Loop_2_8
               ENDDO SPAG_Loop_2_8
            ENDDO
         ENDIF
!
!     NOW POSITIONED TO OUTPUT MPC CARDS FOR SET 102
!
 500     id = 0
         spag_nextblock_1 = 14
      CASE (14)
!
!     IF G FROM AXIF CARD IS NON-ZERO FREEPT DATA IS NOW PROCESSED.
!
         ispnt = nmpc + 1
         nspnt = nmpc
         press = .FALSE.
         IF ( g==0.0 ) GOTO 560
!
!     IF THERE IS NO FREE SURFACE LIST, FREEPT CARDS ARE NOT USED.
!
         IF ( nfslst<ifslst ) GOTO 560
         CALL sort(0,0,3,1,z(ifslst),nfslst-ifslst+1)
         CALL locate(*560,z(buf1),freept,flag)
         spag_nextblock_1 = 15
      CASE (15)
!
!     PICK UP A 3-WORD FREEPT OR PRESPT IMAGE (IDF,IDP,PHI)
!
         CALL read(*740,*540,axic,card,3,noeor,flag)
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
            CALL bisloc(*520,idf,z(ifslst),3,entrys,point)
            ntemp = ifslst + point + 1
            rcard(4) = -abs(rz(ntemp)*g)
         ENDIF
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 520     nogo = .TRUE.
         WRITE (output,99020) ufm , idf
99020    FORMAT (A23,' 4052, IDF =',I10,' ON A FREEPT CARD DOES NOT ','APPEAR ON ANY FSLIST CARD.')
         spag_nextblock_1 = 16
      CASE (16)
         CALL write(scrt2,card,4,noeor)
         set102 = .TRUE.
!
!     ADD SPOINT TO CORE LIST
!
         IF ( nspnt+1<=core ) THEN
            nspnt = nspnt + 1
            z(nspnt) = card(2)
            card(2) = 0
!
!     HARMONIC COEFFICIENT DATA
!
            DO i = ii , ni
               card(1) = 500000*z(i) + idf
               nn = (z(i)-1)/2
               IF ( mod(z(i),2)==0 ) THEN
                  rcard(3) = cos(float(nn)*angle)
               ELSE
                  rcard(3) = sin(float(nn)*angle)
               ENDIF
               CALL write(scrt2,card,3,noeor)
            ENDDO
            CALL write(scrt2,mones,3,noeor)
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSE
            WRITE (output,99021) ufm
99021       FORMAT (A23,' 4053, INSUFFICIENT CORE TO PERFORM OPERATIONS ','REQUIRED AS A RESULT OF FREEPT OR PRESPT DATA CARDS')
            icrq = nspnt + 1 - core
            WRITE (output,99022) icrq
            nogo = .TRUE.
            GOTO 720
         ENDIF
!
!     CREATE MPC CARDS AND SPOINTS AS A RESULT OF PRESPT DATA.
!
 540     IF ( press ) GOTO 580
 560     CALL locate(*580,z(buf1),prespt,flag)
         press = .TRUE.
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
!
!     ANY SPOINTS IN CORE ARE AT THIS TIME OUTPUT TO GEOM2.
!
 580     IF ( nspnt<ispnt ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COPY DATA FROM GEOM2 TO SCRT1 UP TO AND INCLUDING THE 3-WORD
!     RECORD HEADER FOR SPOINTS
!
         file = geom2
         CALL ifp4b(geom2,scrt1,any,z(nspnt+1),core-nspnt,spoint,g2eof)
         IF ( .NOT.any ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            CALL read(*780,*600,geom2,z(nspnt+1),core-nspnt,noeor,flag)
            CALL write(scrt1,z(nspnt+1),core-nspnt,noeor)
         ENDDO
 600     CALL write(scrt1,z(nspnt+1),flag,noeor)
         spag_nextblock_1 = 17
      CASE (17)
         CALL write(scrt1,z(ispnt),nspnt-ispnt+1,eor)
         spag_nextblock_1 = 18
      CASE (18)
!
!     COPY BALANCE OF GEOM2 TO SCRT1,CLOSE THEM, AND SWITCH DESIGNATIONS
!
         CALL ifp4b(geom2,scrt1,any,z(nmpc+1),core-nmpc,-1,g2eof)
!
!     END OF GEOM2 PROCESSING
!     ***********************
!
!     COPY BALANCE OF MPC IMAGES ON GEOM4 TO SCRT2, COMPLETE LIST OF MPC
!     SETS.
!
         file = geom4
         IF ( id/=0 ) THEN
            SPAG_Loop_1_9: DO
               IF ( id/=idlast ) THEN
!
!     ADD ID TO LIST
!
                  idlast = id
                  nmpc = nmpc + 1
                  z(nmpc) = id
               ENDIF
               CALL write(scrt2,id,1,noeor)
               DO
!
!     3-WORD GROUPS
!
                  CALL read(*780,*800,geom4,card,3,noeor,flag)
                  CALL write(scrt2,card,3,noeor)
                  IF ( card(1)==-1 ) THEN
                     CALL read(*780,*620,geom4,id,1,noeor,flag)
                     CYCLE SPAG_Loop_1_9
                  ENDIF
               ENDDO
               EXIT SPAG_Loop_1_9
            ENDDO SPAG_Loop_1_9
         ENDIF
 620     CALL write(scrt2,0,0,eor)
         type(1) = mpcadd(1)
         type(2) = mpcadd(2)
         spag_nextblock_1 = 19
      CASE (19)
!
!     GENERATION OF MPCADD OR SPCADD CARDS FROM USER ID-S.  FIRST
!     OUTPUT MANDATORY MPCADD OR SPCADD.
!
         CALL ifp4f(type(2),geom4,bit)
         IF ( .NOT.set102 .AND. nmpc<impc .AND. .NOT.bit ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL ifp4b(geom4,scrt2,any,z(nmpc+1),core-nmpc,type,g4eof)
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
               card(1) = z(i) + 200000000
               card(2) = z(i)
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
         IF ( any ) THEN
            SPAG_Loop_1_10: DO
               CALL read(*780,*640,geom4,id,1,noeor,flag)
               id = id + 200000000
               CALL write(scrt2,id,1,noeor)
               IF ( set102 ) CALL write(scrt2,102,1,noeor)
               DO
                  CALL read(*780,*800,geom4,id,1,noeor,flag)
                  CALL write(scrt2,id,1,noeor)
                  IF ( id==-1 ) CYCLE SPAG_Loop_1_10
               ENDDO
               EXIT SPAG_Loop_1_10
            ENDDO SPAG_Loop_1_10
         ENDIF
!
 640     CALL write(scrt2,0,0,eor)
         spag_nextblock_1 = 20
      CASE (20)
         IF ( type(1)==spcadd(1) ) THEN
!
!     ALL PROCESSING COMPLETE ON GEOM4
!
            CALL ifp4b(geom4,scrt2,any,z(1),core,mones,g4eof)
            GOTO 720
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
            IF ( .NOT.bit ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COPY GEOM4 TO SCRT2 UP TO SPC CARDS
!
            CALL ifp4b(geom4,scrt2,any,z(ispc),core-ispc,spc,g4eof)
         ENDIF
         DO
!
!     COPY SPC IMAGES KEEPING LIST OF ID-S.
!
            CALL read(*780,*660,geom4,id,1,noeor,flag)
            IF ( id/=idlast ) THEN
               IF ( id/=102 ) THEN
                  nspc = nspc + 1
                  z(nspc) = id
                  idlast = id
               ELSE
                  nogo = .TRUE.
                  WRITE (output,99025) ufm
               ENDIF
            ENDIF
            CALL write(scrt2,id,1,noeor)
            CALL read(*780,*800,geom4,card,3,noeor,flag)
            CALL write(scrt2,card,3,noeor)
         ENDDO
 660     CALL write(scrt2,0,0,eor)
         spag_nextblock_1 = 21
      CASE (21)
!
!     CHECK FOR ANY SPC1 IMAGES
!
         CALL ifp4f(spc1(2),geom4,bit)
         IF ( .NOT.bit .AND. g/=0.0 ) THEN
            spag_nextblock_1 = 28
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COPY FROM GEOM4 TO SCRT2 UP TO SPC1 DATA.
!
         CALL ifp4b(geom4,scrt2,any,z(nspc+1),core-nspc-2,spc1,g4eof)
!
!     COPY SPC1-S UP TO SETID .GE. 103.  SET 102 IS ILLEGAL FOR USER.
!
         IF ( .NOT.bit ) GOTO 680
         spag_nextblock_1 = 22
      CASE (22)
         CALL read(*780,*680,geom4,id,1,noeor,flag)
         IF ( id>=102 ) THEN
            IF ( id/=102 ) THEN
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nogo = .TRUE.
            WRITE (output,99025) ufm
         ENDIF
!
!     ADD ID TO LIST IF NOT YET IN LIST
!
         IF ( nspc>=ispc ) THEN
            DO i = ispc , nspc
               IF ( id==z(i) ) THEN
                  spag_nextblock_1 = 23
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     ADD ID TO LIST
!
         nspc = nspc + 1
         z(nspc) = id
         spag_nextblock_1 = 23
      CASE (23)
         CALL write(scrt2,id,1,noeor)
         CALL read(*780,*800,geom4,id,1,noeor,flag)
         CALL write(scrt2,id,1,noeor)
         DO
            CALL read(*780,*800,geom4,id,1,noeor,flag)
            CALL write(scrt2,id,1,noeor)
            IF ( id==-1 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     IF G IS ZERO AND THERE ARE FSLST ENTRIES, GENERATE SPC1-S NOW.
!
 680     id = 0
         spag_nextblock_1 = 24
      CASE (24)
         IF ( g==0.0 .AND. nfslst>=ifslst ) THEN
!
!     GENERATION OF HARMONIC SPC1-S
!
            DO i = ifslst , nfslst , 3
               IF ( z(i)/=-1 ) THEN
                  card(1) = 102
                  card(2) = 0
                  CALL write(scrt2,card,2,noeor)
                  DO j = ii , ni
                     CALL write(scrt2,z(i)+500000*z(j),1,noeor)
                  ENDDO
                  CALL write(scrt2,minus1,1,noeor)
               ENDIF
            ENDDO
            set102 = .TRUE.
         ENDIF
!
!     COMPLETE COPYING OF SPC1 CARDS TO SCRT2 WITH SETID-S .GE. 103
!
         IF ( id==0 ) GOTO 700
!
!     ADD ID TO LIST IF NOT YET IN
!
         IF ( nspc<ispc ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 25
      CASE (25)
         DO i = ispc , nspc
            IF ( id==z(i) ) THEN
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 26
      CASE (26)
!
!     ID NOT IN LIST, THUS ADD IT.
!
         nspc = nspc + 1
         z(nspc) = id
         spag_nextblock_1 = 27
      CASE (27)
!
!     CONTINUE COPYING DATA TO NEXT ID
!
         CALL write(scrt2,id,1,noeor)
         DO
            CALL read(*780,*800,geom4,id,1,noeor,flag)
            CALL write(scrt2,id,1,noeor)
            IF ( id==-1 ) THEN
               CALL read(*780,*700,geom4,id,1,noeor,flag)
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     END OF SPC1 CARD IMAGES.
!
 700     CALL write(scrt2,0,0,eor)
!
!     SORT LIST OF SPC AND SPC1 ID-S
!
         CALL sort(0,0,1,1,z(ispc),nspc-ispc+1)
         spag_nextblock_1 = 28
      CASE (28)
!
!     SPCADD WORK (USE MPCADD LOGIC)
!
         type(1) = spcadd(1)
         type(2) = spcadd(2)
         impc = ispc
         nmpc = nspc
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
!
!     END OF GEOM4 PROCESSING
!     ***********************
!
!     AXIC FILE NOT IN FIST OR AXIF CARD IS MISSING, THUS DO NOTHING.
!
 720     CALL close(axic,clsrew)
         CALL conmsg(msg2,2,0)
         RETURN
!
!     END OF FILE ON AXIC
!
 740     file = axic
         GOTO 780
!
!     END OF RECORD ON AXIC
!
 760     file = axic
         GOTO 800
!
!     END OF FILE OR END OF RECORD ON -FILE-, OR FILE NOT IN FIST.
!
 780     ier = -2
         spag_nextblock_1 = 30
         CYCLE SPAG_DispatchLoop_1
 800     ier = -3
         spag_nextblock_1 = 30
         CYCLE SPAG_DispatchLoop_1
 820     ier = -1
         spag_nextblock_1 = 30
      CASE (29)
         ier = -8
         file = icrq
         spag_nextblock_1 = 30
         CYCLE SPAG_DispatchLoop_1
 840     ier = -37
         spag_nextblock_1 = 30
      CASE (30)
         CALL mesage(ier,file,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (5X,'ADDITIONAL CORE NEEDED =',I8,' WORDS.')
99023 FORMAT (A23,' 5003, ZERO X2 VALUE ON RINGFL CARD WITH SPHERICAL ','COORDINATES.  FLUID POINT ID =',I10)
99024 FORMAT (A23,' 4038, RINGFL CARD HAS ID =',I20,' WHICH HAS BEEN ','USED.')
99025 FORMAT (A23,' 4055, SET ID = 102 MAY NOT BE USED FOR SPC CARDS ','WHEN USING THE HYDROELASTIC-FLUID ELEMENTS.')
END SUBROUTINE ifp4
