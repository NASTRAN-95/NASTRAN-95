!*==ema1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ema1
   USE c_blank
   USE c_gpta1
   USE c_ma1xx
   USE c_names
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(20) :: block
   INTEGER , DIMENSION(10) :: buf
   INTEGER :: buf1 , buf2 , buf3 , dof , dofg , i , icgvec , icode , idict , igrid , ii , iloc , ilook0 , imat , ipvt , isil0 , j , &
            & jj , k , kerr , kfact , kk , lcore , ll , luset , mach , maxdct , maxnbr , maxvec , minnbr , n , nbrcon , nbrgrd ,    &
            & nbrsil , ncgvec , ngrid , nmat , nout , npvt , nsca , ntrmec , nwdcgv , nwdcol , nwddct , nwdect , nwdmat , oldcod ,  &
            & opcls , openr , openw , prec , silnbr , sysbuf
   INTEGER , SAVE :: ect , gpect , kdict , kelem , kgg , large , lpcb , nhema1 , scr1 , scr2 , sil
   REAL :: eps , factor
   INTEGER :: even
   LOGICAL :: last
   INTEGER , DIMENSION(7) :: mcb , mcbkgg , trlsil
   INTEGER , DIMENSION(32) :: scalas
   INTEGER , DIMENSION(2) , SAVE :: subnam
   INTEGER , DIMENSION(3) :: tt
   REAL , DIMENSION(4) :: xs
   INTEGER , DIMENSION(1) :: zi
   REAL , DIMENSION(1) :: zs
   EXTERNAL bckrec , bldpk , bldpki , bldpkn , cdcbug , close , decode , ectloc , ema1d , ema1s , filpos , fread , gopen , gperr ,  &
          & korsz , makmcb , mesage , pack , rdtrl , read , skprec , sort , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     EMA1 ASSEMBLES A STRUCTURAL MATRIX FOR THE MODEL FROM EACH OF
!     THE INDIVIDUAL ELEMENT STRUCTURAL MATRICES.
!
!     EMA1   GPECT,KDICT,KELEM,SIL,ECT / KGG / C,N,NOK4/ C,N,WTMASS
!
!     NOK4 .NE. -1 MEANS MULTIPLY BY DAMPING FACTOR (GE)
!     ABS(WTMASS-1.0) .GT. 1.E-6 MEANS MULTIPLY BY WTMASS
!
!     EMA1 USES 2 SCRATCH FILES
!
   !>>>>EQUIVALENCE (System(1),Sysbuf) , (System(2),Nout) , (trlsil(2),nbrsil) , (trlsil(3),luset) , (System(22),Mach) ,                 &
!>>>>    & (Zd(1),Zs(1),Zi(1)) , (Xd(1),Xs(1))
!
!     DEFINITION OF INPUT DATA BLOCKS
!
   DATA gpect , kdict , kelem , sil , ect/101 , 102 , 103 , 104 , 105/
!
!     DEFINITION OF OUTPUT DATA BLOCKS
!
   DATA kgg/201/
!
!     DEFINITION OF SCRATCH FILES
!
   DATA scr1 , scr2/301 , 302/
!
!     MISCELANEOUS DATA
!
   DATA subnam/4HEMA1 , 4H    / , nhema1/4HEMA1/ , large/2147483647/ , lpcb/8/
!
!     DATA    TERMS /  1, 0, 9, 0, 0, 18 /,
!    1        SCL   /  1, 1, 0           /
!
!     STATEMENT FUNCTION
!
   even(n) = 2*((n+1)/2)
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERALIZATION
!
         lcore = korsz(zd)
         trlsil(1) = sil
         CALL rdtrl(trlsil)
         WRITE (nout,99002) (trlsil(i),i=1,7)
         isil0 = lcore - nbrsil - 1
         lcore = isil0
         buf1 = lcore - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf(1) = kelem
         CALL rdtrl(buf)
         WRITE (nout,99002) (buf(i),i=1,7)
         prec = buf(2)
         CALL makmcb(mcbkgg,kgg,luset,6,prec)
         openw = wrtrew
         openr = rdrew
         last = .FALSE.
         silnbr = 0
         opcls = cls
         maxdct = 0
         maxvec = 0
         oldcod = 0
!
!     SET SWITCH FOR MULTIPLICATION BY DAMPING AND/OR WEIGHT MASS FACTOR
!
         eps = abs(wtmass-1.0)
         IF ( eps<1.E-6 .AND. nok4<0 ) ASSIGN 100 TO kfact
         IF ( eps<1.E-6 .AND. nok4>=0 ) ASSIGN 120 TO kfact
         IF ( eps>=1.E-6 .AND. nok4<0 ) ASSIGN 140 TO kfact
         IF ( eps>=1.E-6 .AND. nok4>=0 ) ASSIGN 160 TO kfact
!
!     READ THE CONTENTS OF THE SIL DATA BLOCK INTO CORE
!
         CALL gopen(sil,zi(buf1),rdrew)
         CALL fread(sil,zi(isil0+1),nbrsil,1)
         CALL close(sil,clsrew)
         zi(isil0+nbrsil+1) = luset + 1
         CALL cdcbug(nhema1,100,zi(isil0+1),nbrsil+1)
!
!     READ THE KDICT AND ECT DATA BLOCKS. WRITE A MODIFIED KDICT ON SCR2
!     WHICH INCLUDES THE INTERNAL GRID NUMBERS FOR EACH ELEMENT.
!     THE FORMAT FOR EACH RECORD ON SCR2 IS...
!     3-WORD RECORD HEADER
!        1  ELEMENT TYPE
!        2  NBR OF WORDS PER ENTRY( N )
!        3  NBR OF GRID POINTS PER ENTRY
!     N-WORD ELEMENT ENTRY
!        1  ELEMENT ID( INTERNAL NUMBER )
!        2  FORM OF COLUMN PARTITIONS( 1=RECT, 2=DIAG )
!        3  NUMBER OF TERMS PER COLUMN PARTITION
!        4  SCALAR CODE DEFINING DOF PER GRID POINT
!        5  GE
!        6  INTERNAL INDEX OF 1ST GRID POINT
!        7  GINO ADDRESS OF 1ST COLUMN PARTITION
!       ...
!       N-1 INTERNAL INDEX OF LAST GRID POINT
!        N  GINO ADDRESS OF LAST COLUMN PARTITION
!
!     NOTE...
!     GRID POINTS ARE IN SORT BY INTERNAL INDEX. ZERO INDICATES
!     MISSING GRID POINT. ANY ZERO-S ARE LAST IN LIST.
!
         CALL gopen(kdict,zi(buf1),rdrew)
         CALL gopen(ect,zi(buf2),rdrew)
         CALL gopen(scr2,zi(buf3),wrtrew)
 20      CALL read(*60,*20,kdict,buf(4),3,0,j)
         CALL cdcbug(nhema1,111,buf(4),3)
         DO
            CALL ectloc(*200,ect,buf,i)
            CALL cdcbug(nhema1,112,buf,3)
            IF ( elem(i+2)==buf(4) ) THEN
               buf(5) = buf(5) + buf(6)
               CALL write(scr2,buf(4),3,0)
               igrid = elem(i+12)
               nbrgrd = elem(i+9)
               nwdect = elem(i+5)
               idict = nwdect + 1
               nwddct = buf(5) - buf(6)
               ngrid = igrid + nbrgrd - 1
               maxdct = max0(maxdct,buf(5))
               IF ( nbrgrd/=buf(6) ) THEN
                  kerr = 114
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  DO
                     CALL read(*40,*40,ect,zi,nwdect,0,j)
                     CALL cdcbug(nhema1,115,zi,nwdect)
                     CALL fread(kdict,zi(idict),nwddct,0)
                     CALL cdcbug(nhema1,116,zi(idict),nwddct)
                     DO j = igrid , ngrid
                        IF ( zi(j)==0 ) zi(j) = large
                     ENDDO
                     CALL sort(0,0,1,1,zi(igrid),nbrgrd)
                     DO j = igrid , ngrid
                        IF ( zi(j)==large ) zi(j) = 0
                     ENDDO
                     CALL cdcbug(nhema1,118,zi(igrid),nbrgrd)
                     CALL write(scr2,zi(idict),nwddct-nbrgrd,0)
                     iloc = idict + nwddct - nbrgrd
                     DO j = 1 , nbrgrd
                        CALL write(scr2,zi(igrid+j-1),1,0)
                        CALL write(scr2,zi(iloc+j-1),1,0)
                     ENDDO
                     maxvec = max0(maxvec,zi(idict+2)*prec)
                  ENDDO
               ENDIF
            ELSE
               CALL skprec(ect,1)
            ENDIF
         ENDDO
 40      CALL skprec(kdict,1)
         CALL write(scr2,0,0,1)
         GOTO 20
 60      CALL close(kdict,clsrew)
         CALL close(ect,clsrew)
         CALL close(scr2,clsrew)
         tt(1) = maxdct
         tt(2) = maxvec
         CALL cdcbug(nhema1,125,tt,2)
!
!     READ GPECT AND PREPARE THE SCR1 DATA BLOCK. FOR EACH GRID/SCALAR
!     POINT, TWO RECORDS ARE WRITTEN. THE 1ST CONTAINS 6 WORDS...
!       1  INTERNAL INDEX OF GRID/SCALAR POINT
!       2  DOF OF POINT (1=SCALAR, 6=GRID)
!       3  DOF OF EACH CONNECTED POINT (0 IF NO CONNECTED POINTS)
!       4  NUMBER OF CONNECTED POINTS
!       5  INDEX OF  1ST CONNECTED POINT
!       6  INDEX OF LAST CONNECTED POINT
!
!     THE 2ND RECORD IS A PACKED COLUMN WHICH CONTAINS A NON-ZERO TERM
!     FOR EACH CONNECTED POINT.
!
         typin1 = 1
         typou1 = 1
         incr1 = 1
         incr2 = 1
         CALL makmcb(mcb,scr1,nbrsil,1,1)
         ilook0 = nbrsil + 1
         IF ( ilook0+luset+1>=buf3 ) CALL mesage(-8,0,subnam)
         DO i = 1 , nbrsil
            j = zi(isil0+i)
            zi(ilook0+j) = i
         ENDDO
         CALL cdcbug(nhema1,131,zi(ilook0+1),luset)
         CALL gopen(gpect,zi(buf1),rdrew)
         CALL gopen(scr1,zi(buf2),wrtrew)
         DO ii = 1 , nbrsil
            nbrcon = buf(4)
            minnbr = buf(5)
            maxnbr = buf(6)
            IF ( ii==1 ) THEN
               nbrcon = nbrsil
               minnbr = 1
               maxnbr = nbrsil
            ENDIF
            CALL fread(gpect,buf,2,0)
            buf(1) = ii
            buf(3) = 0
            buf(4) = 0
            buf(5) = large
            buf(6) = 0
            IF ( nbrcon/=0 ) THEN
               DO i = minnbr , maxnbr
                  zi(i) = 0
               ENDDO
            ENDIF
            DO
               CALL read(*70,*70,gpect,tt,3,0,i)
               CALL cdcbug(nhema1,134,tt,3)
               nbrgrd = iabs(tt(1)) - 2
               DO i = 1 , nbrgrd
                  CALL fread(gpect,silnbr,1,0)
                  j = zi(ilook0+silnbr)
                  IF ( zs(j)==0 ) THEN
                     buf(3) = max0(buf(3),zi(isil0+j+1)-zi(isil0+j))
                     buf(4) = buf(4) + 1
                     buf(5) = min0(buf(5),j)
                     buf(6) = max0(buf(6),j)
                     zs(j) = 1.0
                  ENDIF
               ENDDO
            ENDDO
 70         CALL write(scr1,buf,6,1)
            CALL cdcbug(nhema1,138,buf,6)
            IF ( buf(4)==0 ) THEN
!
!     HERE IF PIVOT HAS NO CONNECTED POINTS
!
               mcb(2) = mcb(2) + 1
            ELSE
!
!     PACK COLUMN FOR POINT WITH CONNECTED POINTS
!
               ii1 = buf(5)
               jj1 = buf(6)
               CALL cdcbug(nhema1,139,zi(ii1),jj1-ii1+1)
               CALL pack(zs(ii1),scr1,mcb)
            ENDIF
!
!     CLOSE FILES
!
         ENDDO
         CALL close(gpect,clsrew)
         CALL close(scr1,clsrew)
         CALL wrttrl(mcb)
!
!     ALLOCATE STORAGE FOR MAXIMUM COLUMN OF ELEMENT MATRIX
!     AND MAXIMUM ENTRY FROM MODIFIED KDICT( SCR2 )
!
         idict = maxvec + 1
         igrid = idict + 5
         ipvt = idict + maxdct
         lcore = even(buf2) - 1
         spag_nextblock_1 = 2
      CASE (2)
!
!
!     BEGIN A PASS BY OPENING SCR1 AND SETTING ALLOCATION POINTERS
!
!
         CALL gopen(scr1,zi(buf1),openr)
         ii = ipvt
         jj = lcore
         SPAG_Loop_1_1: DO
!
!     BEGIN A PIVOT ALLOCATION BY READING PIVOT CONTROL BLOCK FROM SCR1
!
            tt(1) = ii
            tt(2) = jj
            CALL cdcbug(nhema1,160,tt,2)
            IF ( ii+lpcb>=jj ) EXIT SPAG_Loop_1_1
            CALL fread(scr1,zi(ii),6,1)
            silnbr = zi(ii)
            zi(ii+6) = 0
            zi(ii+7) = 0
            IF ( zi(ii+3)/=0 ) THEN
!
!     ATTEMPT TO ALLOCATE SPACE FOR CONNECTED GRID VECTOR
!     AND FOR MATRICES CONNECTED TO THE PIVOT
!
               nwdcgv = zi(ii+5) - zi(ii+4) + 1
               nwdmat = prec*zi(ii+1)*zi(ii+2)*zi(ii+3)
               IF ( ii+lpcb>=jj-nwdcgv-nwdmat ) THEN
!
!     HERE IF CURRENT PIVOT CANNOT BE ALLOCATED -- MAKE SURE AT LEAST
!     ONE PIVOT HAS BEEN ALLOCATED.
!
                  CALL bckrec(scr1)
                  EXIT SPAG_Loop_1_1
               ELSE
                  imat = jj - nwdmat
                  zi(ii+6) = imat - even(nwdcgv)
                  zi(ii+7) = imat
                  jj = zi(ii+6)
                  nmat = imat + nwdmat - 1
                  DO i = imat , nmat
                     zs(i) = 0
                  ENDDO
                  icgvec = jj
                  ncgvec = icgvec + nwdcgv - 1
!
!     UNPACK CONNECTED GRID VECTOR. CONVERT NON-ZERO POSITIONS TO
!     RELATIVE POINTERS (IN PRECISION OF PROBLEM) TO THE CORRESPONDING
!     1ST TERM OF THE ELEMENT MATRIX
!
                  ii2 = zi(ii+4)
                  jj2 = zi(ii+5)
                  ntrmec = zi(ii+2)
                  kk = 1
                  typin2 = 1
                  CALL unpack(*220,scr1,zs(icgvec))
                  DO i = icgvec , ncgvec
                     IF ( zi(i)/=0 ) THEN
                        zi(i) = kk
                        kk = kk + ntrmec
                     ENDIF
                  ENDDO
                  CALL cdcbug(nhema1,174,zi(ii),8)
                  CALL cdcbug(nhema1,175,zi(icgvec),nwdcgv)
                  IF ( kk-1/=zi(ii+2)*zi(ii+3) ) THEN
                     kerr = 174
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
!
!     TEST FOR LAST PIVOT. IF NOT, TRY TO ALLOCATE ANOTHER PIVOT
!
            IF ( silnbr==nbrsil ) THEN
!
!     HERE WHEN LAST PIVOT HAS BEEN READ AND ALLOCATED
!
               last = .TRUE.
               opcls = clsrew
               npvt = ii
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ii = ii + lpcb
            ENDIF
         ENDDO SPAG_Loop_1_1
         IF ( ii==ipvt ) CALL mesage(-8,0,subnam)
         npvt = ii - lpcb
         spag_nextblock_1 = 3
      CASE (3)
!
!
!     CLOSE SCR1, OPEN SCR2 AND KELEM. PREPARE TO ASSEMBLE
!     STRUCTURAL MATRIX FOR THOSE PIVOTS CURRENTLY ALLOCATED.
!
!
         CALL close(scr1,opcls)
         CALL gopen(scr2,zi(buf1),rdrew)
         CALL gopen(kelem,zi(buf2),rdrew)
!
!     READ HEADER FOR CURRENT ELEMENT TYPE FROM SCR2
!
 80      CALL read(*180,*80,scr2,tt,3,0,i)
         CALL cdcbug(nhema1,230,tt,3)
         nwddct = tt(2)
         ngrid = igrid + 2*(tt(3)-1)
         spag_nextblock_1 = 4
      CASE (4)
         DO
!
!     READ AN ELEMENT DEFINITION. IF ANY GRID POINT IS IN CURRENT
!     ALLOCATION, PREPARE TO PROCESS IT.
!
            CALL read(*80,*80,scr2,zi(idict),nwddct,0,i)
            CALL cdcbug(nhema1,240,zi(idict),nwddct)
            DO i = igrid , ngrid , 2
               IF ( zi(i)>=zi(ipvt) .AND. zi(i)<=zi(npvt) ) GOTO kfact
            ENDDO
         ENDDO
 100     factor = 1.0
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     factor = zs(idict+4)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     factor = wtmass
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 160     factor = wtmass*zs(idict+4)
         spag_nextblock_1 = 5
      CASE (5)
!
!     DECODE RELATIVE COLUMN NUMBERS
!
         IF ( oldcod/=zi(idict+3) ) THEN
            icode = zi(idict+3)
            CALL decode(icode,scalas,nsca)
            oldcod = zi(idict+3)
         ENDIF
!
!     READ EACH COLUMN OF THE ELEMENT MATRIX.
!     ADD IT TO THE STRUCTURAL MATRIX.
!
         nwdcol = prec*zi(idict+2)
         IF ( zi(idict+1)==2 ) nwdcol = prec
         SPAG_Loop_1_2: DO
            ii = ipvt + (zi(i)-zi(ipvt))*lpcb
            tt(1) = i
            tt(2) = zi(i)
            tt(3) = nsca
            CALL cdcbug(nhema1,252,tt,3)
            CALL filpos(kelem,zi(i+1))
            icgvec = zi(ii+6)
            imat = zi(ii+7)
            DO j = 1 , nsca
               CALL fread(kelem,zi,nwdcol,0)
               CALL cdcbug(nhema1,254,zi,nwdcol)
               IF ( prec==1 ) CALL ema1s(j,nsca,scalas,zi(ii),zi(idict),zi(icgvec),zi(imat),zi,factor)
               IF ( prec==2 ) CALL ema1d(j,nsca,scalas,zi(ii),zi(idict),zi(icgvec),zi(imat),zi,factor)
            ENDDO
            DO WHILE ( i/=ngrid )
               i = i + 2
               IF ( zi(i)>=zi(ipvt) .AND. zi(i)<=zi(npvt) ) CYCLE SPAG_Loop_1_2
            ENDDO
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDDO SPAG_Loop_1_2
!
!     ALL COLUMNS OF STRUCTURAL MATRIX NOW ALLOCATED ARE COMPLETE.
!     OPEN KGG AND PACK COLUMNS.
!
 180     CALL close(scr2,clsrew)
         CALL close(kelem,clsrew)
         CALL gopen(kgg,zi(buf1),openw)
         DO ii = ipvt , npvt , lpcb
            dof = zi(ii+1)
            dofg = zi(ii+2)
            nbrcon = zi(ii+3)
            icgvec = zi(ii+6)
            imat = zi(ii+7)
            ii1 = zi(ii+4)
            ii2 = zi(ii+5)
            kk = imat
            CALL cdcbug(nhema1,260,zi(imat),((ii2-ii1+1)*(dof*dofg)))
!
!     PACK COLUMNS WITH BLDPK
!
            DO jj = 1 , dof
               CALL bldpk(prec,prec,kgg,block,1)
               IF ( nbrcon/=0 ) THEN
                  i = icgvec
                  DO j = ii1 , ii2
                     IF ( zi(i)/=0 ) THEN
                        k = zi(isil0+j)
                        n = k + min0(dofg,zi(isil0+j+1)-zi(isil0+j)) - 1
                        ll = kk
                        DO silnbr = k , n
                           CALL bldpki(zs(ll),silnbr,kgg,block)
                           ll = ll + prec
                        ENDDO
                        kk = kk + dofg*prec
                     ENDIF
                     i = i + 1
                  ENDDO
               ENDIF
               CALL bldpkn(kgg,block,mcbkgg)
            ENDDO
         ENDDO
         CALL close(kgg,opcls)
!
!     TEST FOR COMPLETION OF LAST PASS
!
         IF ( last ) THEN
!
!     KGG NOW COMPLETE -- WRITE ITS TRAILER.
!
            CALL wrttrl(mcbkgg)
            RETURN
         ELSE
            openr = rd
            openw = wrt
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FATAL ERRORS
!
 200     kerr = 112
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 220     kerr = 172
         spag_nextblock_1 = 6
      CASE (6)
!
!     PROCESS LOGIC ERROR
!
         WRITE (nout,99001) sfm , kerr
99001    FORMAT (A25,' 3102, EMA1 LOGIC ERROR',I4)
         IF ( mach==2 .OR. mach==5 .OR. mach==21 ) kerr = -kerr
         CALL gperr(subnam,kerr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (1H ,7I10)
END SUBROUTINE ema1
