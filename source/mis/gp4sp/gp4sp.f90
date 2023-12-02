!*==gp4sp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp4sp(Ibuf1,Ibuf2,Ibuf3)
   USE c_blank
   USE c_gp4fil
   USE c_gp4spx
   USE c_output
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ibuf1
   INTEGER :: Ibuf2
   INTEGER :: Ibuf3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eqexin , gpst , ieqexn , ierror , iscr2 , ncard , ogpst , scr2
   INTEGER :: i , ibase , icore , idum , ifile , iflag , igpid , ii , iii , iloop , ims , index , initl , iok , iold , iordr ,      &
            & isil , ispc , ist , istart , ityp , j , jj , k , kk , logic , mskin , mskms , mskxx , multi , neqexn , npts , num
   INTEGER , DIMENSION(9) :: iexcld , indxms , iponts , jponts
   INTEGER , DIMENSION(2) , SAVE :: isubnm
   INTEGER , DIMENSION(8) :: iword
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(10) :: ogpst1
   EXTERNAL andf , close , complf , fread , fwdrec , gopen , lshift , makmcb , mesage , open , orf , page1 , rdtrl , read , skprec ,&
          & sort , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     ROUTINE TO LOOK AT GPST TO ELIMINATE SINGULARITIES
!
!
   DATA eqexin , gpst , ogpst , scr2/103 , 107 , 205 , 302/
   DATA isubnm/4HGP4S , 4HP   /
   DATA ncard , ierror/2*0/
   DATA iscr2 , ieqexn/2* - 1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         index = iabs(iautsp)
         IF ( index>2 ) THEN
            ierror = 1
            WRITE (ioutpt,99001) uwm
99001       FORMAT (A25,' 2439, ILLEGAL VALUE INPUT FOR PARAMETER AUTOSPC - ','SINGULARITY PROCESSING SKIPPED IN MODULE GP4')
            GOTO 40
         ELSE
            IF ( index==2 .AND. omit1>0 ) index = 3
            mskms = orf(mskum,mskus)
            IF ( iautsp/=0 ) THEN
               multi = 0
               IF ( mpcf1/=-1 ) THEN
                  mskin = lshift(1,12)
                  mskxx = complf(mskin)
                  CALL gopen(irgt,iz(Ibuf1),0)
                  itypot = 1
                  iiii = 1
                  jjjj = 1
                  incr = 1
                  DO i = 1 , luset
                     CALL unpack(*2,irgt,idum)
                     IF ( andf(iz(i),mskms)==0 ) THEN
                        multi = 1
                        iz(i) = orf(iz(i),mskin)
                     ENDIF
 2                ENDDO
                  CALL close(irgt,1)
               ENDIF
            ENDIF
            CALL open(*80,gpst,iz(Ibuf1),0)
            ifile = gpst
            CALL fwdrec(*100,gpst)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
!
            CALL read(*40,*40,gpst,iordr,1,0,iflag)
            initl = iordr
            ims = 0
            ispc = 0
            DO i = 1 , 9
               indxms(i) = 0
               iexcld(i) = 0
               iponts(i) = 0
               jponts(i) = 0
            ENDDO
            CALL fread(gpst,npts,1,0)
            CALL fread(gpst,iponts,npts,0)
            ibase = iponts(1)
!
!     SET VARIOUS FLAGS FOR THE SINGULARITIES
!
            DO i = 1 , npts
               ii = iponts(i)
               j = iz(ii)
               IF ( andf(j,mskms)/=0 ) indxms(i) = 1
               IF ( iautsp/=0 ) THEN
                  IF ( andf(j,mskuo)/=0 .AND. andf(j,mskul)/=0 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( andf(j,mskuo)/=0 .AND. andf(j,mskur)/=0 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( andf(j,mskur)/=0 .AND. andf(j,mskul)/=0 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( andf(j,mskur)/=0 ) iexcld(i) = 1
                  IF ( multi/=0 .AND. indxms(i)==0 ) THEN
                     IF ( andf(j,mskin)/=0 ) iexcld(i) = 1
                  ENDIF
                  IF ( index>=3 ) THEN
                     IF ( andf(j,mskul)/=0 ) iexcld(i) = 1
                  ENDIF
               ENDIF
            ENDDO
!
!     DETERMINE THE ORDER OF SINGULARITY
!
            IF ( iordr<2 ) THEN
!
!     FIRST ORDER SINGULARITY
!
               DO i = 1 , npts
                  IF ( indxms(i)/=0 ) CYCLE SPAG_Loop_1_1
               ENDDO
               IF ( iautsp/=0 ) THEN
                  SPAG_Loop_2_2: DO i = 1 , npts
                     IF ( iexcld(i)==0 ) THEN
                        ii = iponts(i)
                        iz(ii) = msksng
                        nauto = nauto + 1
                        ispc = ispc + 1
                        jponts(ispc) = ii
                        EXIT SPAG_Loop_2_2
                     ENDIF
                  ENDDO SPAG_Loop_2_2
               ENDIF
               EXIT SPAG_Loop_1_1
            ELSEIF ( iordr==2 ) THEN
!
!     SECOND ORDER SINGULARITY
!
               iloop = 1
               SPAG_Loop_2_3: DO
                  DO i = 1 , npts , 2
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           ii = iponts(i)
                           IF ( ii/=0 ) THEN
                              IF ( indxms(i)/=0 ) THEN
                                 ims = ims + 1
                              ELSE
                                 IF ( iloop==1 ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                                 IF ( iexcld(i)/=0 ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                                 iz(ii) = msksng
                                 nauto = nauto + 1
                                 ispc = ispc + 1
                                 jponts(ispc) = ii
                              ENDIF
                              iordr = 1
                              DO iii = 1 , npts
                                 IF ( iponts(iii)==ii ) iponts(iii) = 0
                              ENDDO
                              ii = 0
                           ENDIF
                           spag_nextblock_2 = 2
                        CASE (2)
                           jj = iponts(i+1)
                           IF ( jj/=0 ) THEN
                              IF ( indxms(i+1)/=0 ) THEN
                                 ims = ims + 1
                              ELSE
                                 IF ( iloop==1 ) CYCLE
                                 IF ( iexcld(i+1)/=0 ) CYCLE
                                 iz(jj) = msksng
                                 nauto = nauto + 1
                                 ispc = ispc + 1
                                 jponts(ispc) = jj
                              ENDIF
                           ENDIF
                           IF ( ii/=0 ) THEN
                              iordr = 1
                              DO iii = 1 , npts
                                 IF ( iponts(iii)==jj ) iponts(iii) = 0
                              ENDDO
                           ELSE
                              logic = 160
                              IF ( ispc+ims<2 ) THEN
                                 spag_nextblock_1 = 14
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( ispc==0 ) EXIT SPAG_Loop_2_3
                              spag_nextblock_1 = 8
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
                  ENDDO
                  IF ( iautsp==0 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( iloop==2 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  iloop = 2
               ENDDO SPAG_Loop_2_3
            ELSE
!
!     THIRD ORDER SINGULARITY
!
               iok = 0
               DO i = 1 , npts
                  IF ( indxms(i)/=0 ) THEN
                     ims = ims + 1
                  ELSEIF ( iautsp==0 ) THEN
                     iok = 1
                     CYCLE
                  ELSEIF ( iexcld(i)/=0 ) THEN
                     iok = 1
                     CYCLE
                  ELSE
                     ii = iponts(i)
                     iz(ii) = msksng
                     nauto = nauto + 1
                     ispc = ispc + 1
                     jponts(ispc) = ii
                  ENDIF
                  iordr = iordr - 1
                  iponts(i) = 0
               ENDDO
               IF ( iok==1 ) THEN
                  iok = 0
                  DO i = 1 , npts
                     IF ( iponts(i)/=0 ) THEN
                        iok = iok + 1
                        iponts(iok) = iponts(i)
                        IF ( iok/=i ) iponts(i) = 0
                     ENDIF
                  ENDDO
                  npts = iok
                  EXIT SPAG_Loop_1_1
               ELSE
                  logic = 200
                  IF ( ispc+ims/=3 ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( ispc/=0 ) EXIT SPAG_Loop_1_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
!
!
         logic = 100
         IF ( ispc>initl ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ieqexn==1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ieqexn = 1
!
!     BRING IN EQEXIN
!
         CALL gopen(eqexin,iz(Ibuf2),0)
         CALL skprec(eqexin,1)
         mcb(1) = eqexin
         CALL rdtrl(mcb)
         icore = luset + 2*(mcb(2)+1) - Ibuf2
         IF ( icore<0 ) THEN
            ifile = eqexin
            CALL read(*100,*20,eqexin,iz(luset+1),Ibuf2-luset,0,neqexn)
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 20      CALL close(eqexin,1)
         CALL sort(0,0,2,2,iz(luset+1),neqexn)
         iz(luset+neqexn+1) = 0
         iz(luset+neqexn+2) = 10*(luset+1)
         neqexn = neqexn + 2
!
!     LOOK UP SIL IN EQEXIN
!
         istart = 2
         spag_nextblock_1 = 4
      CASE (4)
         kk = ibase
         DO i = istart , neqexn , 2
            k = luset + i
            isil = iz(k)/10
            IF ( kk<isil ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         logic = 110
         spag_nextblock_1 = 14
      CASE (5)
!
!     PICK UP POINT ID AND TYPE (GRID OR SCALAR) FROM EQEXIN
!
         igpid = iz(k-3)
         isil = iz(k-2)/10
         ityp = iz(k-2) - 10*isil
         istart = i - 2
         IF ( ityp/=1 ) THEN
!
!     SCALAR POINT
!
            iordr = 0
            npts = 0
         ENDIF
!
!
         IF ( ispc/=0 ) THEN
            logic = 120
            IF ( ityp==2 .AND. ispc>1 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ityp==2 ) jponts(1) = 0
            IF ( iscr2/=1 ) THEN
               iscr2 = 1
               iword(1) = spcset
               IF ( iword(1)<=0 ) iword(1) = 1
!
!     INITIALIZE SCR2
!
               CALL gopen(scr2,iz(Ibuf3),1)
            ENDIF
!
!     WRITE AUTOMATICALLY GENERATED SPC1 DATA ON SCR2
!
            DO i = 1 , ispc
               IF ( ityp/=2 ) THEN
                  IF ( jponts(i)>0 ) THEN
                     jponts(i) = jponts(i) - isil + 1
                  ELSE
                     logic = 130
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               CALL write(scr2,jponts(i),1,0)
               CALL write(scr2,igpid,1,0)
            ENDDO
            IF ( ispc+ims>=initl ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!
         IF ( iogpst/=1 ) THEN
            iogpst = 1
!
!     INITIALIZE OGPST
!
            CALL gopen(ogpst,iz(Ibuf2),1)
            ogpst1(1) = 0
            ogpst1(2) = 8
            ogpst1(3) = spcset
            ogpst1(4) = mpcset
            ogpst1(10) = 12
            CALL write(ogpst,ogpst1,10,0)
            CALL write(ogpst,iz,40,0)
            CALL write(ogpst,head(1),96,1)
         ENDIF
!
!     PUT OUT ERROR RECORDS ON OGPST
!
         CALL write(ogpst,igpid,1,0)
         CALL write(ogpst,ityp,1,0)
         CALL write(ogpst,iordr,1,0)
         iordr = iordr + 1
         IF ( iordr/=1 ) THEN
            DO i = 1 , npts
               IF ( iponts(i)>0 ) THEN
                  iponts(i) = iponts(i) - isil + 1
               ELSE
                  logic = 140
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            logic = 150
            IF ( iordr==1 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( iordr==2 ) THEN
!
!     FIRST ORDER OUTPUT
!
               iponts(4) = iponts(2)
               iponts(7) = iponts(3)
               iponts(2) = 0
               iponts(3) = 0
               iponts(5) = 0
               iponts(6) = 0
               iponts(8) = 0
               iponts(9) = 0
            ELSEIF ( iordr==3 ) THEN
!
!     SECOND ORDER OUTPUT
!
               iponts(8) = iponts(6)
               iponts(7) = iponts(5)
               iponts(5) = iponts(4)
               iponts(4) = iponts(3)
               iponts(3) = 0
               iponts(6) = 0
               iponts(9) = 0
            ELSEIF ( iordr/=4 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     THIRD ORDER OUTPUT
!
            CALL write(ogpst,iponts,9,0)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     SCALAR
!
         DO i = 1 , 9
            iponts(i) = 0
         ENDDO
         CALL write(ogpst,iponts,9,0)
         spag_nextblock_1 = 2
      CASE (7)
         IF ( iordr/=1 ) THEN
            IF ( iordr==2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            logic = 170
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         iok = 0
         DO i = 1 , npts
            IF ( iponts(i)/=0 ) THEN
               iok = iok + 1
               iponts(iok) = iponts(i)
               IF ( iok/=i ) iponts(i) = 0
               IF ( i/=npts ) THEN
                  ii = i + 1
                  DO j = ii , npts
                     IF ( iponts(j)/=0 ) THEN
                        IF ( iponts(j)==iponts(iok) ) iponts(j) = 0
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         npts = iok
         IF ( npts==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         logic = 180
         IF ( npts>2 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         logic = 190
         IF ( iponts(1)==iponts(2) ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 40      CALL close(gpst,1)
         IF ( iogpst==1 ) THEN
            CALL close(ogpst,1)
            IF ( ierror==0 ) THEN
               CALL makmcb(ogpst1,ogpst,0,0,0)
               ogpst1(2) = 8
               CALL wrttrl(ogpst1)
            ENDIF
         ENDIF
         IF ( iautsp==0 ) GOTO 80
         IF ( nauto>0 ) THEN
            logic = 220
            IF ( iscr2/=1 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(scr2,0,0,1)
            CALL close(scr2,1)
            IF ( ierror/=0 ) GOTO 80
            IF ( iogpst/=1 ) WRITE (ioutpt,99002) uim
!
99002       FORMAT (A29,' 2435, AT USER''S REQUEST, ALL POTENTIAL ','SINGULARITIES HAVE BEEN REMOVED BY THE',/5X,                   &
                   &'APPLICATION OF SINGLE POINT CONSTRAINTS.  REFER TO PRINT',                                                     &
                   &'OUT OF AUTOMATICALLY GENERATED SPC1 CARDS FOR DETAILS.')
            IF ( iogpst==1 ) WRITE (ioutpt,99003) uim
99003       FORMAT (A29,' 2436, AT USER''S REQUEST, ONE OR MORE POTENTIAL ','SINGULARITIES HAVE BEEN REMOVED BY THE',/5X,           &
                   &'APPLICATION OF SINGLE POINT CONSTRAINTS.  REFER TO PRINT',                                                     &
                   &'OUT OF AUTOMATICALLY GENERATED SPC1 CARDS FOR DETAILS.')
            IF ( iogpst==1 .AND. index<3 ) WRITE (ioutpt,99004) uwm
99004       FORMAT (A25,' 2437, ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT',' BEEN REMOVED',/5X,                                  &
                   &'BECAUSE OF THG PRESENCE OF SUPORT ','CARDS AND/OR MULTIPOINT CONSTRAINTS OR RIGID ELEMENTS.',/5X,              &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
            IF ( iogpst==1 .AND. index==3 ) WRITE (ioutpt,99005) uwm
99005       FORMAT (A25,' 2437, ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT',' BEEN REMOVED',/5X,                                  &
                   &'BECAUSE OF THG PRESENCE OF SUPORT ','CARDS AND/OR MULTIPOINT CONSTRAINTS OR RIGID ELEMENTS',/5X,               &
                   &'OR BECAUSE THE SINGULARITIES ARE NOT PART OF THE ','OMIT SET (O-SET) DEGREES OF FREEDOM.',/5X,                 &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
!
!     PRINT OUT AND, IF REQUESTED, PUNCH OUT
!     AUTOMATICALLY GENERATED SPC DATA CARDS
!
            CALL gopen(scr2,iz(Ibuf3),0)
            ifile = scr2
            CALL read(*100,*60,scr2,iz(luset+1),Ibuf3-luset,0,iflag)
            icore = luset + 2*nauto - Ibuf3
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
            logic = 210
            IF ( iscr2==1 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( iogpst==1 .AND. index<3 ) WRITE (ioutpt,99006) uwm
99006       FORMAT (A25,' 2437A, IN SPITE OF THE USER''S REQUEST, NONE OF ','THE POTENTIAL SINGULARITIES HAS BEEN REMOVED',/5X,     &
                   &'BECAUSE OF THG PRESENCE OF SUPORT CARDS AND/OR MULTI','POINT CONSTRAINTS OR RIGID ELEMENTS.',/5X,              &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
            IF ( iogpst==1 .AND. index==3 ) WRITE (ioutpt,99007) uwm
99007       FORMAT (A25,' 2437A, IN SPITE OF THE USER''S REQUEST, NONE OF ','THE POTENTIAL SINGULARITIES HAS BEEN REMOVED',/5X,     &
                   &'BECAUSE OF THG PRESENCE OF SUPORT CARDS AND/OR MULTI','POINT CONSTRAINTS OR RIGID ELEMENTS',/5X,'OR BECAUSE ', &
                   &'THE SINGULARITIES ARE NOT PART OF THE OMIT SET (O-SET) ','DEGREES OF FREEDOM.',/5X,                            &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
            GOTO 80
         ENDIF
 60      logic = 230
         IF ( iflag/=2*nauto ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sort(0,0,2,1,iz(luset+1),iflag)
         i = luset + 1
         iold = -1
         ist = i
         j = 0
         spag_nextblock_1 = 9
      CASE (9)
         SPAG_Loop_1_4: DO WHILE ( i<=luset+iflag )
            IF ( iold>=0 .AND. iz(i)/=iold ) EXIT SPAG_Loop_1_4
            iold = iz(i)
            j = j + 2
            i = i + 2
         ENDDO SPAG_Loop_1_4
         CALL sort(0,0,2,-2,iz(ist),j)
         IF ( i>luset+iflag ) THEN
!
            i = luset + 1
            iold = -1
            CALL page1
            WRITE (ioutpt,99012)
            line = line + 6
         ELSE
            iold = iz(i)
            ist = i
            j = 0
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         ii = 2
         SPAG_Loop_1_5: DO j = 1 , 6
            IF ( i>luset+iflag ) EXIT SPAG_Loop_1_5
            IF ( iold>=0 .AND. iz(i)/=iold ) EXIT SPAG_Loop_1_5
            iold = iz(i)
            iword(ii+1) = iz(i+1)
            ii = ii + 1
            i = i + 2
         ENDDO SPAG_Loop_1_5
         iword(2) = iold
         IF ( line>nlpp ) THEN
            CALL page1
            WRITE (ioutpt,99012)
            line = line + 6
         ENDIF
         ncard = ncard + 1
         WRITE (ioutpt,99008) ncard , (iword(j),j=1,ii)
99008    FORMAT (15X,I5,'-',8X,'SPC1    ',8I8)
         line = line + 1
         IF ( iautsp<0 ) WRITE (ipunch,99009) (iword(j),j=1,ii)
99009    FORMAT ('SPC1    ',8I8)
         IF ( i>luset+iflag ) THEN
            CALL close(scr2,1)
         ELSE
            iold = iz(i)
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 80      IF ( iautsp==0 .OR. multi==0 ) RETURN
         DO i = 1 , luset
            iz(i) = andf(iz(i),mskxx)
         ENDDO
         RETURN
!
!     ERROR MESSAGES
!
 100     num = -2
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(num,ifile,isubnm)
         spag_nextblock_1 = 12
      CASE (12)
         num = -8
         ifile = icore
         spag_nextblock_1 = 11
      CASE (13)
         ierror = 2
         WRITE (ioutpt,99010) uwm
99010    FORMAT (A25,' 2440, SINGULARITY PROCESSING SKIPPED IN MODULE GP4',' BECAUSE OF INCONSISTENT SET DEFINITION')
         GOTO 40
      CASE (14)
         WRITE (ioutpt,99011) sfm , logic
99011    FORMAT (A25,' 2438, LOGIC ERROR NO.',I4,' IN SUBROUTINE GP4SP IN MODULE GP4')
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99012 FORMAT (//32X,'A U T O M A T I C A L L Y   ','G E N E R A T E D   ','S P C 1   C A R D S',/,16X,'CARD ',8X,/,16X,'COUNT',8X,  &
             &'---1--- +++2+++ ---3--- +++4+++ ---5--- ','+++6+++ ---7--- +++8+++ ---9--- +++10+++',/)
!
END SUBROUTINE gp4sp
