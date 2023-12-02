!*==ifp5.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp5
   IMPLICIT NONE
   USE C_CONDAS
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alpha , bd , beta , bl1 , bp1 , fi , kf , l1 , l1l2 , l2 , l3 , lc , le , rbar , rho , rhod , term , twopi , wbar , wd
   LOGICAL :: any , g1eof , g2eof , plotel
   INTEGER , SAVE :: axic , eor , geom1 , geom2 , noeor , scrt1 , scrt2
   INTEGER , DIMENSION(2) , SAVE :: axslot , celas2 , grid , gridf , grids , msg1 , msg2 , plotls , slbdy , subr
   INTEGER , DIMENSION(24) :: buf
   INTEGER :: buf1 , buf2 , buf3 , buf4 , core , entrys , file , flag , i , ibase , icrq , ide , idf , idg , idgf , idgs , ids ,    &
            & idsl1 , idsp1 , ier , igf , igridf , igrids , igs , is1 , is3 , islbdy , j , jj , jpoint , k , k4 , k6 , kid , m ,    &
            & md , n , ngridf , ngrids , nlines , nslbdy , ntemp , output , sysbuf , words
   INTEGER , DIMENSION(10) :: card
   INTEGER , DIMENSION(6) , SAVE :: caxif
   INTEGER , DIMENSION(4) , SAVE :: cslot
   REAL , DIMENSION(24) :: rbuf
   REAL , DIMENSION(10) :: rcard
   REAL , DIMENSION(3) :: rr , ww , zz
   REAL , DIMENSION(4) :: rz
   EXTERNAL bisloc , close , conmsg , ifp4b , ifp4c , ifp4f , ifp5a , korsz , locate , mesage , open , preloc , read , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     ACOUSTIC CAVITY PREFACE ROUTINE
!
!     THIS PREFACE MODULE OPERATES ON ACOUSTIC-CAVITY-ANALYSIS DATA
!     CARDS WHICH AT THIS POINT ARE IN THE FORM OF IFP-OUTPUT IMAGES ON
!     THE AXIC DATA BLOCK.
!
!     THE FOLLOWING LIST GIVES THE CARD IMAGES IFP5 WILL LOOK FOR ON THE
!     AXIC OR GEOM2 DATA BLOCKS,  THE CARD IMAGES IFP5 WILL GENERATE OR
!     MODIFY, AND THE DATA BLOCKS ONTO WHICH THE GENERATED OR MODIFIED
!     CARD IMAGES WILL BE PLACED.
!
!      IFP5 INPUT         IFP5 OUTPUT        DATA BLOCK
!      CARD IMAGE         CARD IMAGE         EFFECTED
!     ------------       -----------        ----------
!      AXSLOT/AXIC        -NONE-             -NONE-
!      CAXIF2/GEOM2       PLOTEL             GEOM2
!      CAXIF3/GEOM2       PLOTEL             GEOM2
!      CSLOT3/GEOM2       PLOTEL             GEOM2
!      CSLOT4/GEOM2       PLOTEL             GEOM2
!      CAXIF4/GEOM2       PLOTEL             GEOM2
!      GRIDF/AXIC         GRID               GEOM1
!      GRIDS/AXIC         GRID               GEOM1
!      SLBDY/AXIC         CELAS2             GEOM2
!
!     SOME OF THE ABOVE OUTPUT DATA CARDS ARE A FUNCTION OF MORE THAN
!     ONE INPUT DATA CARDS
!
   !>>>>EQUIVALENCE (Consts(2),Twopi) , (Ksystm(1),Sysbuf) , (Ksystm(2),Output) , (Z(1),Rz(1)) , (buf(1),rbuf(1)) , (card(1),rcard(1))
   DATA axslot/1115 , 11/
   DATA slbdy/1415 , 14/
   DATA caxif/2108 , 21 , 2208 , 22 , 2308 , 23/
   DATA celas2/701 , 7/
   DATA cslot/4408 , 44 , 4508 , 45/
   DATA grid/4501 , 45/
   DATA grids/1315 , 13/
   DATA gridf/1215 , 12/
   DATA plotls/5201 , 52/
   DATA subr/4HIFP5 , 4H    /
   DATA eor , noeor/1 , 0/
!
!     NOTE...  SCRATCH2 IN IFP5 AS IN IFP4 IS EQUIVALENCED TO THE
!     -FORCE- DATA BLOCK.
!
   DATA axic , geom1 , geom2 , scrt1 , scrt2/215 , 201 , 208 , 301 , 213/
   DATA msg1/4HIFP5 , 4HBEGN/ , msg2/4HIFP5 , 4HEND /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINE CORE AND BUFFER POINTERS
!
         CALL conmsg(msg1,2,0)
         core = korsz(Z)
         buf1 = core - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         buf4 = buf3 - sysbuf - 2
         core = buf4 - 1
         icrq = 100 - core
         IF ( core<100 ) THEN
!
!     INSUFFICIENT CORE
!
            ier = -8
            file = icrq
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ELSE
            plotel = .FALSE.
!
!     OPEN AXIC DATA BLOCK. (IF NAME IS NOT IN FIST RETURN - NO MESSAGE)
!
            CALL preloc(*240,Z(buf1),axic)
!
!     PICK UP THE AXSLOT CARD AND SAVE THE VALUES ON IT.
!     RHOD, BD, N, WD, MD (FATAL ERROR IF NOT PRESSENT)
!
            CALL locate(*20,Z(buf1),axslot,flag)
            CALL read(*260,*40,axic,Z(1),6,eor,words)
         ENDIF
 20      CALL ifp5a(1)
         WRITE (output,99001)
99001    FORMAT (' AXSLOT DATA CARD IS NOT PRESENT OR IS INCORRECT.')
!
!     SET VALUES FOR CONTINUING DATA CHECK
!
         rhod = 0.0
         bd = 0.0
         n = 0
         wd = 1.0
         md = 0
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      IF ( words/=5 ) GOTO 20
         rhod = rz(1)
         bd = rz(2)
         j = 3
         n = Z(j)
         wd = rz(4)
         md = Z(j+2)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ GRIDS DATA CARDS INTO CORE FROM AXIC DATA BLOCK.
!
         igrids = 1
         ngrids = igrids - 1
         CALL locate(*80,Z(buf1),grids,flag)
         CALL read(*260,*60,axic,Z(igrids),core,eor,words)
         CALL ifp5a(2)
         WRITE (output,99002)
99002    FORMAT (49H INSUFFICIENT CORE TO HOLD ALL GRIDS CARD IMAGES.)
         WRITE (output,99011) core
         GOTO 80
 60      ngrids = ngrids + words
!
!     READ GRIDF DATA CARDS INTO CORE FROM AXIC DATA BLOCK.
!
 80      igridf = ngrids + 1
         ngridf = igridf - 1
         CALL locate(*120,Z(buf1),gridf,flag)
         CALL read(*260,*100,axic,Z(igridf),core-ngrids,eor,words)
         CALL ifp5a(3)
         WRITE (output,99003)
99003    FORMAT (49H INSUFFICIENT CORE TO HOLD ALL GRIDF CARD IMAGES.)
         icrq = core - ngrids
         WRITE (output,99011) icrq
         GOTO 120
 100     ngridf = ngridf + words
!
!     INSERT DEFAULT SLOT WIDTH INTO ANY GRIDS IMAGE HAVING NONE
!     SPECIFIED BY THE USER.
!
 120     IF ( ngrids>=igrids ) THEN
            DO i = igrids , ngrids , 5
               IF ( Z(i+3)==1 ) rz(i+3) = wd
            ENDDO
!
!     CREATE A GRIDF CARD FOR EACH GRIDS DATA CARD THAT HAS A NON-ZERO
!     IDF
!
            DO i = igrids , ngrids , 5
               IF ( Z(i+4)>0 ) THEN
                  ngridf = ngridf + 3
                  IF ( ngridf>core ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Z(ngridf-2) = Z(i+4)
                  Z(ngridf-1) = Z(i+1)
                  Z(ngridf) = Z(i+2)
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         CALL ifp5a(4)
         WRITE (output,99004)
99004    FORMAT (' INSUFFICIENT CORE TO HOLD ALL GRIDF CARD IMAGES BEING ',                                                         &
                &'CREATED INTERNALLY DUE TO GRIDS CARDS SPECIFYING AN IDF.')
         icrq = ngridf - core
         WRITE (output,99011) icrq
         ngridf = ngridf - 3
         spag_nextblock_1 = 4
      CASE (4)
!
!     SORT THE GRIDF CARDS ON THEIR ID.
!
         IF ( ngridf>igridf ) CALL sort(0,0,3,1,Z(igridf),ngridf-igridf+1)
!
!     OPEN GEOM1 AND SCRATCH1, COPY HEADER REC FROM GEOM1 TO SCRATCH1.
!
         CALL ifp4c(geom1,scrt1,Z(buf2),Z(buf3),g1eof)
!
!     COPY ALL DATA FROM GEOM1 TO SCRATCH1 UP TO FIRST GRID CARD.
!
         CALL ifp4b(geom1,scrt1,any,Z(ngridf+1),core-ngridf,grid,g1eof)
         file = geom1
!
!     CREATE GRID CARDS FROM GRIDS AND GRIDF CARDS.
!     MERGE THESE INTO EXISTING GRID CARDS CHECKING FOR DUPLICATE ID-S.
!
         igf = igridf
         idgf = 0
         igs = igrids
         idgs = 0
         IF ( igf<ngridf ) idgf = Z(igf)
         IF ( igs<ngrids ) idgs = Z(igs)
         card(2) = 0
         card(6) = -1
         card(7) = 0
         card(8) = 0
!
!     READ A GRID CARD INTO BUF.
!
         IF ( .NOT.any ) GOTO 140
         spag_nextblock_1 = 5
      CASE (5)
         CALL read(*300,*140,geom1,buf,8,noeor,words)
         idg = buf(1)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 140     idg = 0
         spag_nextblock_1 = 6
      CASE (6)
!
!     DETERMINE WHETHER GRID, GRIDF, OR GRIDS CARD IS TO OUTPUT NEXT.
!
         IF ( idg<=0 ) THEN
            IF ( idgf<=0 ) THEN
               IF ( idgs>0 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     ALL GRID CARDS HAVE BEEN OUPTUT, WRITE EOR.
!
               CALL write(scrt1,0,0,eor)
!
!     COPY BALANCE OF GEOM1 TO SCRT1, WRAP UP AND COPY BACK.
!
               CALL ifp4b(geom1,scrt1,any,Z(igridf),core-igridf,-1,g1eof)
!
!     SLBDY CARD IMAGES ARE NOW PROCESSED AND A BOUNDARY TABLE IS FORMED
!     IN CORE.  EACH ENTRY IN THE TABLE CONTAINS,
!
!              IDS , IDS   , IDS   , RHO, M
!                 I     I-1     I+1
!
!     IDS    = -1 IF IDS  IS THE FIRST ID ON SLBDY CARD.
!        I-1            I
!
!     IDS    = -1 IF IDS  IS THE LAST ID ON SLBDY CARD.
!        I+1            I
!
               islbdy = ngrids + 1
               nslbdy = islbdy - 1
               CALL locate(*160,Z(buf1),slbdy,flag)
               SPAG_Loop_1_2: DO
                  CALL read(*260,*160,axic,buf,2,noeor,words)
                  rho = rbuf(1)
                  m = buf(2)
                  idsl1 = -1
                  CALL read(*260,*280,axic,ids,1,noeor,words)
                  SPAG_Loop_2_1: DO
                     CALL read(*260,*280,axic,idsp1,1,noeor,words)
!
!     PLACE 5 WORD ENTRY INTO CORE
!
                     nslbdy = nslbdy + 5
                     IF ( nslbdy>core ) THEN
!
!     OUT OF CORE
!
                        CALL ifp5a(5)
                        WRITE (output,99005)
99005                   FORMAT (' INSUFFICIENT CORE TO CONSTRUCT ENTIRE BOUNDARY TABLE ','FOR SLBDY CARDS PRESENT.')
                        icrq = nslbdy - core
                        WRITE (output,99011) icrq
                        nslbdy = nslbdy - 5
!
!     SKIP BALANCE OF SLBDY DATA.
!
                        CALL read(*260,*160,axic,buf,1,eor,words)
                        EXIT SPAG_Loop_2_1
                     ELSE
                        Z(nslbdy-4) = ids
                        Z(nslbdy-3) = idsl1
                        Z(nslbdy-2) = idsp1
                        rz(nslbdy-1) = rho
                        Z(nslbdy) = m
                        idsl1 = ids
                        ids = idsp1
                        IF ( idsp1+1==0 ) CYCLE SPAG_Loop_1_2
                     ENDIF
                  ENDDO SPAG_Loop_2_1
                  GOTO 160
               ENDDO SPAG_Loop_1_2
            ELSE
               IF ( idgs<=0 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( idgf<idgs ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( idgf/=idgs ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ELSEIF ( idgf<=0 ) THEN
            IF ( idgs<=0 ) THEN
!
!     OUTPUT GRID CARD AND READ ANOTHER
!
               CALL write(scrt1,buf,8,noeor)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( idg<idgs ) THEN
               CALL write(scrt1,buf,8,noeor)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( idg/=idgs ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( idg<idgf ) THEN
            IF ( idgs<=0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( idg<idgs ) THEN
               CALL write(scrt1,buf,8,noeor)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( idg/=idgs ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( idg/=idgf ) THEN
            IF ( idgs<=0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( idgf<idgs ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( idgf/=idgs ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     ERROR - DUPLICATE ID-S ENCOUNTERED
!
         CALL ifp5a(10)
         WRITE (output,99006) idg , idgs , idgf
99006    FORMAT (' ONE OF THE FOLLOWING NON-ZERO IDENTIFICATION NUMBERS ','APPEARS ON SOME COMBINATION',/,' OF GRID, GRIDS, OR ',   &
                &'GRIDF BULK DATA CARDS.',3(6H   ID=,I12))
         IF ( idg==idgf ) THEN
            CALL write(scrt1,buf,8,noeor)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( idg/=idgs ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(scrt1,buf,8,noeor)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (7)
!
!     OUTPUT A GRID FROM GRIDF CARD.
!
         card(1) = idgf
         rcard(3) = rz(igf+1)
         rcard(4) = rz(igf+2)
         rcard(5) = 0.0
         igf = igf + 3
         IF ( igf>ngridf ) idgf = 0
         IF ( idgf/=0 ) idgf = Z(igf)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!     OUTPUT A GRID FROM GRIDS CARD.
!
         card(1) = idgs
         rcard(3) = rz(igs+1)
         rcard(4) = rz(igs+2)
         rcard(5) = rz(igs+3)
         igs = igs + 5
         IF ( igs>ngrids ) idgs = 0
         IF ( idgs/=0 ) idgs = Z(igs)
         spag_nextblock_1 = 9
      CASE (9)
         CALL write(scrt1,card,8,noeor)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     SORT BOUNDARY TABLE ON IDS . (FIRST WORD OF EACH ENTRY)
!                               I
!
 160     IF ( nslbdy>islbdy ) CALL sort(0,0,5,1,Z(islbdy),nslbdy-islbdy+1)
!/////
!     CALL BUG (10H BOUNDRY      ,440,Z(ISLBDY),NSLBDY-ISLBDY+1)
!
!     OPEN GEOM2, OPEN SCRATCH2, COPY HEADER REC FROM GEOM2 TO SCRATCH2.
!
         file = geom2
         CALL ifp4c(geom2,scrt2,Z(buf2),Z(buf3),g2eof)
!
!     OPEN SCRATCH1, FOR TEMPORARY OUTPUT OF PLOTEL IMAGES CREATED FROM
!     CAXIF2, CAXIF3, CAXIF4, CSLOT3, AND CSLOT4 CARDS.
!
         file = scrt1
         CALL open(*320,scrt1,Z(buf4),Wrtrew)
!
!     CREATE PLOTEL IMAGES FROM CAXIF2, CAXIF3, AND CAXIF4 AT THIS TIME
!
         file = geom2
         DO i = 1 , 3
            ibase = (i-1)*1000000
            IF ( i==3 ) ibase = 4000000
            k = 2*i - 1
            k4 = i + 5
!
!     CHECK TRAILER TO SEE IF CAXIF(I+1) EXISTS
!
            CALL ifp4f(caxif(k+1),geom2,any)
            IF ( .NOT.any ) CYCLE
!
!     COPY ALL DATA FROM GEOM2 TO SCRATCH2 UP TO FIRST CAXIF(I+1) IMAGE.
!
            CALL ifp4b(geom2,scrt2,any,Z(nslbdy+1),core-nslbdy,caxif(k),g2eof)
            IF ( .NOT.any ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO
!
!     COPY EACH IMAGE TO SCRATCH2 AND CREATE PLOTELS AT SAME TIME.
!
               CALL read(*300,*170,geom2,buf,k4,noeor,words)
               CALL write(scrt2,buf,k4,noeor)
               nlines = i + 1
               IF ( i==1 ) nlines = 1
               DO j = 1 , nlines
                  card(1) = buf(1) + ibase + j*1000000
                  card(2) = buf(j+1)
                  jj = j + 1
                  IF ( jj>nlines .AND. nlines/=1 ) jj = 1
                  card(3) = buf(jj+1)
                  CALL write(scrt1,card,3,noeor)
               ENDDO
               plotel = .TRUE.
            ENDDO
!
!     END OF RECORD HIT ON GEOM2.  COMPLETE RECORD ON SCRATCH2
!
 170        CALL write(scrt2,0,0,eor)
         ENDDO
!
!     COPY ALL DATA FROM GEOM2 TO SCRATCH2 UP TO FIRST CELAS2 CARD
!     IMAGE.
!
         CALL ifp4b(geom2,scrt2,any,Z(nslbdy+1),core-nslbdy,celas2,g2eof)
!
!     COPY ANY CELAS2 DATA CARDS, MAKE SURE ALL ID ARE LESS THAN
!     10000001.
!
         IF ( any ) THEN
            DO
               CALL read(*300,*180,geom2,buf,8,noeor,words)
               IF ( buf(1)>=10000001 ) THEN
                  CALL ifp5a(6)
                  WRITE (output,99007) buf(1)
99007             FORMAT (' CELAS2 DATA CARD HAS ID =',I14,', WHICH IS GREATER THAN 10000000,',/,' AND 10000000 IS THE ',           &
                         &'LIMIT FOR CELAS2 ID WITH ACOUSTIC ANALYSIS DATA CARDS PRESENT')
               ENDIF
               CALL write(scrt2,buf,8,noeor)
            ENDDO
         ENDIF
!
!     OUTPUT THREE CELAS2 IMAGES FOR EACH ENTRY IN THE BOUNDARY TABLE.
!
 180     IF ( nslbdy>=islbdy ) THEN
            entrys = (ngrids-igrids+1)/5
!/////
!     CALL BUG(10H BOUNDRY      ,540,Z(ISLBDY),NSLBDY-ISLBDY+1)
!     CALL BUG(10H GRIDS        ,540,Z(IGRIDS),NGRIDS-IGRIDS+1)
            ide = 10000000
            SPAG_Loop_1_3: DO i = islbdy , nslbdy , 5
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
!
!     FIND  R, Z, W FOR IDS , IDS   , IDS    RESPECTIVELY.
!                          I     I-1     I+1
!
                     k = 0
                     is1 = i
                     is3 = i + 2
                     DO j = is1 , is3
                        k = k + 1
                        IF ( Z(j)<0 ) THEN
!
!     IDS = -1
!
                           rr(k) = rr(1)
                           zz(k) = zz(1)
                           ww(k) = ww(1)
                           CYCLE
                        ELSEIF ( Z(j)/=0 ) THEN
                           IF ( entrys>0 ) THEN
                              kid = Z(j)
                              CALL bisloc(*182,kid,Z(igrids),5,entrys,jpoint)
                              ntemp = igrids + jpoint
!
!     NTEMP NOW POINTS TO THE SECOND WORD OF THE GRIDS ENTRY HAVING
!     THE ID SPECIFIED BY Z(J).  (1ST,2ND,OR 3RD ID IN SLBDY ENTRY)
!
!
!     NO CELAS2 CARDS ARE GENERATED IF GRIDS FOR IDS  HAS NO IDF.
!                                                   I
!
                              IF ( k==1 ) idf = Z(ntemp+3)
                              IF ( k==1 .AND. idf<=0 ) CYCLE SPAG_Loop_1_3
                              rr(k) = rz(ntemp)
                              zz(k) = rz(ntemp+1)
                              ww(k) = rz(ntemp+2)
                              CYCLE
                           ENDIF
                        ENDIF
!
!     IDS COULD NOT BE FOUND IN GRIDS ENTRYS.
!
 182                    CALL ifp5a(7)
                        WRITE (output,99008) Z(j)
99008                   FORMAT (11H SLBDY ID =,I12,' DOES NOT APPEAR ON ANY GRIDS DATA CARD.')
                        rr(k) = 0.0
                        zz(k) = 0.0
                        ww(k) = 0.0
                     ENDDO
!
!     COMPUTE GEOMETRY AND OTHER DATA.
!
                     l1 = sqrt((zz(3)-zz(1))**2+(rr(3)-rr(1))**2)
                     l2 = sqrt((zz(2)-zz(1))**2+(rr(2)-rr(1))**2)
                     l3 = sqrt((zz(3)-zz(2))**2+(rr(3)-rr(2))**2)/2.0
!
                     l1l2 = (l1+l2)*4.0
                     IF ( l1l2>0 ) THEN
!
!     COMPUTE W-BAR AND R-BAR
!
                        wbar = (l1*ww(3)+l2*ww(2))/l1l2 + 0.75*ww(1)
                        rbar = (l1*rr(3)+l2*rr(2))/l1l2 + 0.75*rr(1)
!
                        IF ( wbar/=0 ) THEN
                           IF ( rbar/=0 ) THEN
                              IF ( Z(i+4)/=0 ) THEN
!
!     COMPUTE BETA,LC
!
                                 beta = (twopi*rbar)/(float(Z(i+4))*wbar)
                                 IF ( beta>1.0 ) THEN
                                    bl1 = beta - 1.0
                                    bp1 = beta + 1.0
                                    lc = wbar/twopi
                                    lc = lc*((beta+1.0/beta)*alog(bp1/bl1)+2.0*alog(bp1*bl1/(4.0*beta)))
                                    term = 0.01*wbar
                                    le = amax1(lc,term)
                                    IF ( le/=0 ) THEN
                                       IF ( rz(i+3)/=0 ) THEN
!
!     FIND F  = M, IF N=0 OR N=M/2  OTHERWISE F  = M/2
!           I                                  I
!
                                         IF ( n==0 .OR. 2*n==Z(i+4) ) THEN
                                         fi = Z(i+4)
                                         ELSE
                                         fi = float(Z(i+4))/2.0
                                         ENDIF
                                         kf = (wbar*l3*fi)/(rz(i+3)*le)
                                         spag_nextblock_2 = 2
                                         CYCLE SPAG_DispatchLoop_2
                                       ELSE
                                         CALL ifp5a(9)
                                         WRITE (output,99009) Z(i)
99009                                    FORMAT (' RHO AS SPECIFIED ON SLBDY OR AXSLOT CARD IS 0.0 FOR ID',' =',I12)
                                         CYCLE
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ELSE
!
!     M = 0, THUS K  = 0.0
!                  F
!
                                 kf = 0.0
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
!
!     ERROR, ZERO OR NEGATIVE LENGTH
!
                     CALL ifp5a(8)
                     WRITE (output,99010) Z(i) , Z(i+1) , Z(i+2)
99010                FORMAT (' ONE OR MORE OF THE FOLLOWING ID-S NOT EQUAL TO -1 HAVE',' INCORRECT OR NO GEOMETRY DATA',            &
                            &/3(10X,4HID =,I10))
                     CYCLE
                  CASE (2)
!
!                           N WBAR
!                      SIN( ------ )
!                           2 RBAR
!     COMPUTE  ALPHA = --------------
!                           N WBAR
!                         ( ------ )
!                           2 RBAR
!
                     term = (float(n)*wbar)/(2.0*rbar)
                     IF ( term/=0 ) THEN
                        alpha = sin(term)/term
                     ELSE
                        alpha = 1.0
                     ENDIF
!
!  OUTPUT THE 3 CELAS2 CARDS
!
                     buf(1) = ide + 1
                     rbuf(2) = kf*(1.0-alpha)
                     buf(3) = Z(i)
                     buf(4) = 0
                     buf(5) = 1
                     buf(6) = 0
                     buf(7) = 0
                     buf(8) = 0
                     buf(9) = ide + 2
                     rbuf(10) = kf*alpha
                     buf(11) = Z(i)
                     buf(12) = idf
                     buf(13) = 1
                     buf(14) = 1
                     buf(15) = 0
                     buf(16) = 0
                     buf(17) = ide + 3
                     rbuf(18) = kf*alpha*(alpha-1.0)
                     buf(19) = idf
                     buf(20) = 0
                     buf(21) = 1
                     buf(22) = 0
                     buf(23) = 0
                     buf(24) = 0
                     CALL write(scrt2,buf,24,noeor)
                     ide = ide + 3
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO SPAG_Loop_1_3
         ENDIF
!
!     COMPLETE THE CELAS2 RECORD.
!
         CALL write(scrt2,0,0,eor)
!
!     CREATE PLOTEL IMAGES FROM CSLOT3, AND CSLOT4 AT THIS TIME IF ANY
!
         DO i = 1 , 2
            ibase = 3000000*i + 5000000
            k = 2*i - 1
            k6 = i + 7
!
!     CHECK TRAILER BIT TO SEE IF CSLOT(I+2) EXISTS.
!
            CALL ifp4f(cslot(k+1),geom2,any)
            IF ( .NOT.any ) CYCLE
!
!     COPY ALL DATA FROM GEOM2 TO SCRATCH2 UP TO FIRST CSLOT(I+2) IMAGE.
!
            CALL ifp4b(geom2,scrt2,any,Z(igrids),core-igrids,cslot(k),g2eof)
            IF ( .NOT.any ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO
!
!     COPY EACH IMAGE TO SCRATCH2 AND CREATE PLOTELS AT SAME TIME.
!
               CALL read(*300,*190,geom2,buf,k6,noeor,words)
               CALL write(scrt2,buf,k6,noeor)
               nlines = i + 2
               DO j = 1 , nlines
                  card(1) = buf(1) + ibase + j*1000000
                  card(2) = buf(j+1)
                  jj = j + 1
                  IF ( jj>nlines ) jj = 1
                  card(3) = buf(jj+1)
                  CALL write(scrt1,card,3,noeor)
               ENDDO
               plotel = .TRUE.
            ENDDO
!
!     END OF RECORD ON GEOM2.  COMPLETE RECORD ON SCRATCH2.
!
 190        CALL write(scrt2,0,0,eor)
         ENDDO
!
!     APPEND PLOTELS ON SCRATCH1 TO ANY PLOTELS ON GEOM2.
!     MAKE SURE ALL PLOTEL ID-S ARE .LE. 1000000
!     /// ID CHECK NOT IN YET.
!     POSITION TO PLOTELS ON GEOM2 IF ANY ARE ON SCRATCH1
!
         IF ( .NOT.plotel ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL ifp4b(geom2,scrt2,any,Z(igrids),core-igrids,plotls,g2eof)
         IF ( .NOT.any ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
!
!     BLAST COPY PLOTELS FROM GEOM2 TO SCRATCH2
!
            CALL read(*300,*200,geom2,Z(igrids),core-igrids,noeor,words)
            CALL write(scrt2,Z(igrids),core-igrids,noeor)
         ENDDO
 200     CALL write(scrt2,Z(igrids),words,noeor)
         spag_nextblock_1 = 10
      CASE (10)
!
!     CLOSE AND OPEN SCRATCH1 CONTAINING GENERATED PLOTEL IMAGES.
!
         file = scrt1
         CALL close(scrt1,Clsrew)
         CALL open(*320,scrt1,Z(buf4),Rdrew)
         DO
!
!     BLAST COPY PLOTELS FROM SCRATCH1 TO SCRATCH2.
!
            CALL read(*220,*220,scrt1,Z(igrids),core-igrids,noeor,words)
            CALL write(scrt2,Z(igrids),core-igrids,noeor)
         ENDDO
 220     CALL write(scrt2,Z(igrids),words,eor)
         spag_nextblock_1 = 11
      CASE (11)
         CALL close(scrt1,Clsrew)
!
!     ALL PROCESSING OF GEOM2 IS COMPLETE SO COPY BALANCE OF GEOM2 TO
!     SCRATCH2, WRAP UP, AND COPY BACK.
!
         CALL ifp4b(geom2,scrt2,any,Z(igrids),core-igrids,-1,g2eof)
!
!     ALL PROCESSING COMPLETE.
!
 240     CALL close(axic,Clsrew)
         CALL conmsg(msg2,2,0)
         RETURN
!
!     END OF FILE ON AXIC.
!
 260     file = axic
         GOTO 300
!
!     END OF RECORD ON AXIC
!
 280     file = axic
         ier = -3
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE OR END OF RECORD ON -FILE-.
!
 300     ier = -2
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
!
!     FILE NOT IN FIST
!
 320     ier = -1
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
!
!     BISLOC EXIT
!
         ier = -37
         spag_nextblock_1 = 13
      CASE (13)
!
         CALL mesage(ier,file,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99011 FORMAT (5X,24HADDITIONAL CORE NEEDED =,I8,7H WORDS.)
END SUBROUTINE ifp5
