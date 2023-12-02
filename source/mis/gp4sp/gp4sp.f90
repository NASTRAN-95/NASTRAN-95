!*==gp4sp.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp4sp(Ibuf1,Ibuf2,Ibuf3)
   IMPLICIT NONE
   USE C_BLANK
   USE C_GP4FIL
   USE C_GP4SPX
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
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
         index = iabs(Iautsp)
         IF ( index>2 ) THEN
            ierror = 1
            WRITE (Ioutpt,99001) Uwm
99001       FORMAT (A25,' 2439, ILLEGAL VALUE INPUT FOR PARAMETER AUTOSPC - ','SINGULARITY PROCESSING SKIPPED IN MODULE GP4')
            GOTO 40
         ELSE
            IF ( index==2 .AND. Omit1>0 ) index = 3
            mskms = orf(Mskum,Mskus)
            IF ( Iautsp/=0 ) THEN
               multi = 0
               IF ( Mpcf1/=-1 ) THEN
                  mskin = lshift(1,12)
                  mskxx = complf(mskin)
                  CALL gopen(Irgt,Iz(Ibuf1),0)
                  Itypot = 1
                  Iiii = 1
                  Jjjj = 1
                  Incr = 1
                  DO i = 1 , Luset
                     CALL unpack(*2,Irgt,idum)
                     IF ( andf(Iz(i),mskms)==0 ) THEN
                        multi = 1
                        Iz(i) = orf(Iz(i),mskin)
                     ENDIF
 2                ENDDO
                  CALL close(Irgt,1)
               ENDIF
            ENDIF
            CALL open(*80,gpst,Iz(Ibuf1),0)
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
               j = Iz(ii)
               IF ( andf(j,mskms)/=0 ) indxms(i) = 1
               IF ( Iautsp/=0 ) THEN
                  IF ( andf(j,Mskuo)/=0 .AND. andf(j,Mskul)/=0 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( andf(j,Mskuo)/=0 .AND. andf(j,Mskur)/=0 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( andf(j,Mskur)/=0 .AND. andf(j,Mskul)/=0 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( andf(j,Mskur)/=0 ) iexcld(i) = 1
                  IF ( multi/=0 .AND. indxms(i)==0 ) THEN
                     IF ( andf(j,mskin)/=0 ) iexcld(i) = 1
                  ENDIF
                  IF ( index>=3 ) THEN
                     IF ( andf(j,Mskul)/=0 ) iexcld(i) = 1
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
               IF ( Iautsp/=0 ) THEN
                  SPAG_Loop_2_2: DO i = 1 , npts
                     IF ( iexcld(i)==0 ) THEN
                        ii = iponts(i)
                        Iz(ii) = Msksng
                        Nauto = Nauto + 1
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
                                 Iz(ii) = Msksng
                                 Nauto = Nauto + 1
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
                                 Iz(jj) = Msksng
                                 Nauto = Nauto + 1
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
                  IF ( Iautsp==0 ) THEN
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
                  ELSEIF ( Iautsp==0 ) THEN
                     iok = 1
                     CYCLE
                  ELSEIF ( iexcld(i)/=0 ) THEN
                     iok = 1
                     CYCLE
                  ELSE
                     ii = iponts(i)
                     Iz(ii) = Msksng
                     Nauto = Nauto + 1
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
         CALL gopen(eqexin,Iz(Ibuf2),0)
         CALL skprec(eqexin,1)
         mcb(1) = eqexin
         CALL rdtrl(mcb)
         icore = Luset + 2*(mcb(2)+1) - Ibuf2
         IF ( icore<0 ) THEN
            ifile = eqexin
            CALL read(*100,*20,eqexin,Iz(Luset+1),Ibuf2-Luset,0,neqexn)
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 20      CALL close(eqexin,1)
         CALL sort(0,0,2,2,Iz(Luset+1),neqexn)
         Iz(Luset+neqexn+1) = 0
         Iz(Luset+neqexn+2) = 10*(Luset+1)
         neqexn = neqexn + 2
!
!     LOOK UP SIL IN EQEXIN
!
         istart = 2
         spag_nextblock_1 = 4
      CASE (4)
         kk = ibase
         DO i = istart , neqexn , 2
            k = Luset + i
            isil = Iz(k)/10
            IF ( kk<isil ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         logic = 110
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     PICK UP POINT ID AND TYPE (GRID OR SCALAR) FROM EQEXIN
!
         igpid = Iz(k-3)
         isil = Iz(k-2)/10
         ityp = Iz(k-2) - 10*isil
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
               iword(1) = Spcset
               IF ( iword(1)<=0 ) iword(1) = 1
!
!     INITIALIZE SCR2
!
               CALL gopen(scr2,Iz(Ibuf3),1)
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
         IF ( Iogpst/=1 ) THEN
            Iogpst = 1
!
!     INITIALIZE OGPST
!
            CALL gopen(ogpst,Iz(Ibuf2),1)
            ogpst1(1) = 0
            ogpst1(2) = 8
            ogpst1(3) = Spcset
            ogpst1(4) = Mpcset
            ogpst1(10) = 12
            CALL write(ogpst,ogpst1,10,0)
            CALL write(ogpst,Iz,40,0)
            CALL write(ogpst,Head(1),96,1)
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
         CYCLE SPAG_DispatchLoop_1
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
         IF ( Iogpst==1 ) THEN
            CALL close(ogpst,1)
            IF ( ierror==0 ) THEN
               CALL makmcb(ogpst1,ogpst,0,0,0)
               ogpst1(2) = 8
               CALL wrttrl(ogpst1)
            ENDIF
         ENDIF
         IF ( Iautsp==0 ) GOTO 80
         IF ( Nauto>0 ) THEN
            logic = 220
            IF ( iscr2/=1 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(scr2,0,0,1)
            CALL close(scr2,1)
            IF ( ierror/=0 ) GOTO 80
            IF ( Iogpst/=1 ) WRITE (Ioutpt,99002) Uim
!
99002       FORMAT (A29,' 2435, AT USER''S REQUEST, ALL POTENTIAL ','SINGULARITIES HAVE BEEN REMOVED BY THE',/5X,                   &
                   &'APPLICATION OF SINGLE POINT CONSTRAINTS.  REFER TO PRINT',                                                     &
                   &'OUT OF AUTOMATICALLY GENERATED SPC1 CARDS FOR DETAILS.')
            IF ( Iogpst==1 ) WRITE (Ioutpt,99003) Uim
99003       FORMAT (A29,' 2436, AT USER''S REQUEST, ONE OR MORE POTENTIAL ','SINGULARITIES HAVE BEEN REMOVED BY THE',/5X,           &
                   &'APPLICATION OF SINGLE POINT CONSTRAINTS.  REFER TO PRINT',                                                     &
                   &'OUT OF AUTOMATICALLY GENERATED SPC1 CARDS FOR DETAILS.')
            IF ( Iogpst==1 .AND. index<3 ) WRITE (Ioutpt,99004) Uwm
99004       FORMAT (A25,' 2437, ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT',' BEEN REMOVED',/5X,                                  &
                   &'BECAUSE OF THG PRESENCE OF SUPORT ','CARDS AND/OR MULTIPOINT CONSTRAINTS OR RIGID ELEMENTS.',/5X,              &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
            IF ( Iogpst==1 .AND. index==3 ) WRITE (Ioutpt,99005) Uwm
99005       FORMAT (A25,' 2437, ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT',' BEEN REMOVED',/5X,                                  &
                   &'BECAUSE OF THG PRESENCE OF SUPORT ','CARDS AND/OR MULTIPOINT CONSTRAINTS OR RIGID ELEMENTS',/5X,               &
                   &'OR BECAUSE THE SINGULARITIES ARE NOT PART OF THE ','OMIT SET (O-SET) DEGREES OF FREEDOM.',/5X,                 &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
!
!     PRINT OUT AND, IF REQUESTED, PUNCH OUT
!     AUTOMATICALLY GENERATED SPC DATA CARDS
!
            CALL gopen(scr2,Iz(Ibuf3),0)
            ifile = scr2
            CALL read(*100,*60,scr2,Iz(Luset+1),Ibuf3-Luset,0,iflag)
            icore = Luset + 2*Nauto - Ibuf3
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
            logic = 210
            IF ( iscr2==1 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Iogpst==1 .AND. index<3 ) WRITE (Ioutpt,99006) Uwm
99006       FORMAT (A25,' 2437A, IN SPITE OF THE USER''S REQUEST, NONE OF ','THE POTENTIAL SINGULARITIES HAS BEEN REMOVED',/5X,     &
                   &'BECAUSE OF THG PRESENCE OF SUPORT CARDS AND/OR MULTI','POINT CONSTRAINTS OR RIGID ELEMENTS.',/5X,              &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
            IF ( Iogpst==1 .AND. index==3 ) WRITE (Ioutpt,99007) Uwm
99007       FORMAT (A25,' 2437A, IN SPITE OF THE USER''S REQUEST, NONE OF ','THE POTENTIAL SINGULARITIES HAS BEEN REMOVED',/5X,     &
                   &'BECAUSE OF THG PRESENCE OF SUPORT CARDS AND/OR MULTI','POINT CONSTRAINTS OR RIGID ELEMENTS',/5X,'OR BECAUSE ', &
                   &'THE SINGULARITIES ARE NOT PART OF THE OMIT SET (O-SET) ','DEGREES OF FREEDOM.',/5X,                            &
                   &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
            GOTO 80
         ENDIF
 60      logic = 230
         IF ( iflag/=2*Nauto ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sort(0,0,2,1,Iz(Luset+1),iflag)
         i = Luset + 1
         iold = -1
         ist = i
         j = 0
         spag_nextblock_1 = 9
      CASE (9)
         SPAG_Loop_1_4: DO WHILE ( i<=Luset+iflag )
            IF ( iold>=0 .AND. Iz(i)/=iold ) EXIT SPAG_Loop_1_4
            iold = Iz(i)
            j = j + 2
            i = i + 2
         ENDDO SPAG_Loop_1_4
         CALL sort(0,0,2,-2,Iz(ist),j)
         IF ( i>Luset+iflag ) THEN
!
            i = Luset + 1
            iold = -1
            CALL page1
            WRITE (Ioutpt,99012)
            Line = Line + 6
         ELSE
            iold = Iz(i)
            ist = i
            j = 0
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         ii = 2
         SPAG_Loop_1_5: DO j = 1 , 6
            IF ( i>Luset+iflag ) EXIT SPAG_Loop_1_5
            IF ( iold>=0 .AND. Iz(i)/=iold ) EXIT SPAG_Loop_1_5
            iold = Iz(i)
            iword(ii+1) = Iz(i+1)
            ii = ii + 1
            i = i + 2
         ENDDO SPAG_Loop_1_5
         iword(2) = iold
         IF ( Line>Nlpp ) THEN
            CALL page1
            WRITE (Ioutpt,99012)
            Line = Line + 6
         ENDIF
         ncard = ncard + 1
         WRITE (Ioutpt,99008) ncard , (iword(j),j=1,ii)
99008    FORMAT (15X,I5,'-',8X,'SPC1    ',8I8)
         Line = Line + 1
         IF ( Iautsp<0 ) WRITE (Ipunch,99009) (iword(j),j=1,ii)
99009    FORMAT ('SPC1    ',8I8)
         IF ( i>Luset+iflag ) THEN
            CALL close(scr2,1)
         ELSE
            iold = Iz(i)
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 80      IF ( Iautsp==0 .OR. multi==0 ) RETURN
         DO i = 1 , Luset
            Iz(i) = andf(Iz(i),mskxx)
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
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         ierror = 2
         WRITE (Ioutpt,99010) Uwm
99010    FORMAT (A25,' 2440, SINGULARITY PROCESSING SKIPPED IN MODULE GP4',' BECAUSE OF INCONSISTENT SET DEFINITION')
         GOTO 40
      CASE (14)
         WRITE (Ioutpt,99011) Sfm , logic
99011    FORMAT (A25,' 2438, LOGIC ERROR NO.',I4,' IN SUBROUTINE GP4SP IN MODULE GP4')
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99012 FORMAT (//32X,'A U T O M A T I C A L L Y   ','G E N E R A T E D   ','S P C 1   C A R D S',/,16X,'CARD ',8X,/,16X,'COUNT',8X,  &
             &'---1--- +++2+++ ---3--- +++4+++ ---5--- ','+++6+++ ---7--- +++8+++ ---9--- +++10+++',/)
!
END SUBROUTINE gp4sp
