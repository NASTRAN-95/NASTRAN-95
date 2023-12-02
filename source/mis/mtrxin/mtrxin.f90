!*==mtrxin.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtrxin
!
!     TWO CAPABILITIES -
!
!     (1) TO PROVIDE DIRECT INPUT MATRICES CAPABILITY, IN DYNAMIC RIGID
!         FORMATS, AND
!     (2) TO CONVERT DMIG TYPE MATRICES TO NASTRAN MATRIX FORMAT.
!
!     REVISED  1/90 BY G.CHAN/UNISYS
!     NO INTEGER ROW AND COLUMN PACKING FOR 32-BIT WORD MACHINE, AND
!     REAL AND COMPLEX DMIG MATRIX GENERATION FROM DMIG INPUT CARDS
!     WITH DOUBLE PRECISION DATA
!
USE C_BLANK
USE C_MACHIN
USE C_NAMES
USE C_SADDX
USE C_SETUP
USE C_SYSTEM
USE C_TYPE
USE C_XMSSG
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: alpha , beta
   INTEGER , DIMENSION(81) , SAVE :: block
   INTEGER , DIMENSION(20) :: buf
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , bufx , col , file , file1 , file2 , file3 , flag , i , i10 , i11 , i12 , i45 , i7 ,&
            & icol , imtrx , iprc , iptr , iqq , isw , j , j1 , jn , jsw , k , khi , klo , kn , mask16 , n , ncol , neqex , nmtrx , &
            & nn , nodmig , nogo , nomat1 , nomat2 , nomat3 , nompoo , nwd , nwd1 , prec , ret , tfset , typalp , typbet
   REAL(REAL64) , DIMENSION(2) :: bufd , xd
   INTEGER , DIMENSION(3) :: bufi , filei
   REAL , DIMENSION(13) :: bufr
   INTEGER , SAVE :: casecc , eqex , imat1 , imat2 , imat3 , itf , mpool , nfiles , scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 ,&
                   & tfpool
   INTEGER , DIMENSION(13) :: db
   INTEGER , DIMENSION(2) , SAVE :: dmig , nam
   INTEGER , DIMENSION(7) :: filea , fileb , filec
   INTEGER , DIMENSION(50) , SAVE :: mcb
   LOGICAL :: pack
   EXTERNAL andf , bldpk , bldpki , bldpkn , close , fname , fread , fwdrec , gopen , korsz , locate , lshift , mesage , open ,     &
          & orf , preloc , rdtrl , read , rshift , sadd , skprec , sorti , sorti2 , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (buf(1),bufr(1)) , (X(1),Xd(1)) , (filei(1),file1) , (filei(2),file2) , (bufi(1),buf3) , (bufi(2),buf4) ,            &
!>>>>    & (Nomat(1),Nomat1) , (Nomat(2),Nomat2) , (filei(3),file3) , (bufi(3),buf5) , (Nomat(3),Nomat3) , (bufd(1),db(13)) ,            &
!>>>>    & (buf(1),db(2))
   !>>>>EQUIVALENCE (Mcbs(1),Filea(1)) , (Mcbs(8),Typalp) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Fileb(1)) , (Mcbs(20),Typbet) ,               &
!>>>>    & (Mcbs(21),Beta(1)) , (Mcbs(61),Filec(1))
   DATA mcb/201 , 9*0 , 202 , 9*0 , 203 , 29*0/ , casecc , mpool , eqex , tfpool/101 , 102 , 103 , 105/ , scr1 , scr2 , scr3 ,      &
      & scr4 , scr5 , scr6 , scr7/301 , 302 , 303 , 304 , 305 , 306 , 307/ , block/81*0/ , nam/4HMTRX , 4HIN  / , dmig/114 , 1/ ,   &
      & nfiles/21/ , imat1 , imat2 , imat3 , itf/139 , 141 , 143 , 15/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERAL INITIALIZATION
!
         buf1 = korsz(Z) - Sysbuf - 2
         buf2 = buf1 - Sysbuf
         buf3 = buf2 - Sysbuf
         buf4 = buf3 - Sysbuf
         buf5 = buf4 - Sysbuf
         nomat1 = -1
         nomat2 = -1
         nomat3 = -1
         mask16 = Jhalf
!
!     IF MACHINE IS MORE THEN 32 BITS PER WORD, WE USE PACKING LOGIC
!     OTHERWISE, WE DO NOT PACK ROW AND COLUMN INDICES INTO ONE WORD
!
         pack = .FALSE.
         IF ( Ihalf>16 ) pack = .TRUE.
         DO i = 1 , nfiles , 10
            j1 = i + 1
            jn = i + 9
            DO j = j1 , jn
               mcb(j) = 0
            ENDDO
         ENDDO
         tfset = 0
         typalp = 1
         alpha(1) = 1.0
         typbet = 1
         beta(1) = 1.0
         nogo = 0
!
!     OPEN MPOOL. IF PURGED, SET FLAG.
!
         file = mpool
         nompoo = 0
         nodmig = 0
         CALL preloc(*20,Z(buf1),mpool)
         nompoo = 1
         CALL locate(*20,Z(buf1),dmig,flag)
         nodmig = 1
!
!     READ CASE CONTROL RECORD.
!     SET NAMES OF REQUESTED MATRICES.
!     IF CASE CONTROL IS PURGED, SET NAMES OF REQUESTED MATRICES EQUAL
!     NAMES OF OUTPUT DATA BLOCKS.
!
 20      file = casecc
         CALL open(*60,casecc,Z(buf2),Rdrew)
         CALL fwdrec(*260,casecc)
         CALL read(*220,*40,casecc,Z,buf2,1,flag)
         CALL mesage(-8,0,nam)
 40      CALL close(casecc,Clsrew)
         tfset = Z(itf)
         IF ( Z(imat1)/=0 ) THEN
            nomat1 = 1
            mcb(8) = Z(imat1)
            mcb(9) = Z(imat1+1)
         ENDIF
         IF ( Z(imat2)/=0 ) THEN
            nomat2 = 1
            mcb(18) = Z(imat2)
            mcb(19) = Z(imat2+1)
         ENDIF
         IF ( Z(imat3)/=0 ) THEN
            nomat3 = 1
            mcb(28) = Z(imat3)
            mcb(29) = Z(imat3+1)
         ENDIF
!
!     IF TRANSFER FUNCTION MATRICES EXIST, BUILD THEM IN MATRIX FORMAT.
!     WRITE THEM ON 201,202,203 IF NO DMIG MATRICES TO ADD, OTHERWISE,
!     WRITE THEM ON SCR5,SCR6,SCR7.
!     IF NO TRANSFER FUNCTION MATRICES AND NO DMIG MATRICES, EXIT.
!
         IF ( nomat1+nomat2+nomat3==-3 ) THEN
            nodmig = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( nodmig/=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            GOTO 200
         ENDIF
 60      DO i = 1 , 21 , 10
            mcb(31) = mcb(i)
            CALL rdtrl(mcb(31))
            IF ( mcb(31)==mcb(i) ) CALL fname(mcb(31),mcb(i+7))
         ENDDO
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         IF ( nodmig==0 .AND. tfset==0 ) GOTO 220
         IF ( tfset==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file1 = scr5
!
!     TEST FOR PURGED OUTPUT DATA BLOCKS.
!
         DO i = 1 , 21 , 10
            CALL rdtrl(mcb(i))
         ENDDO
         IF ( nomat1==-1 ) file1 = mcb(1)
         file2 = scr7
         IF ( nomat3==-1 ) file2 = mcb(21)
         file3 = scr6
         IF ( nomat2==-1 ) file3 = mcb(11)
         nomat1 = 1
         nomat2 = 1
         nomat3 = 1
!
!     OPEN TFPOOL AND POSITION TO REQUESTED SET.
!     IF SET NOT IN TFPOOL, QUEUE MESSAGE AND TURN ON NOGO FLAG.
!
         file = tfpool
         CALL open(*80,tfpool,Z(buf2),Rdrew)
         DO
            CALL fwdrec(*80,tfpool)
            CALL read(*80,*80,tfpool,buf,1,0,flag)
            IF ( buf(1)==tfset ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 80      nogo = 1
         buf(1) = tfset
         buf(2) = 0
         CALL mesage(30,74,buf)
         IF ( dmig(1)/=tfset ) THEN
            CALL close(tfpool,Clsrew)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     OPEN OUTPUT FILES. WRITE HEADER RECORDS.
!
         DO i = 1 , 3
!
!     CHECK FOR PURGED OUTPUT DATA BLOCKS.
!
            IF ( filei(i)>0 ) THEN
               file = filei(i)
               bufx = bufi(i)
               CALL gopen(file,Z(bufx),Wrtrew)
            ELSE
               Nomat(i) = -1
            ENDIF
         ENDDO
!
!     PACK MATRICES ONTO OUTPUT FILES.
!
         ncol = Luset
         icol = 1
         jsw = 0
         isw = 0
         i45 = 5
         IF ( pack ) i45 = 4
         i12 = i45 - 3
         spag_nextblock_1 = 4
      CASE (4)
         DO i = 1 , 3
            IF ( filei(i)>0 ) CALL bldpk(1,1,filei(i),block(20*i-19),1)
         ENDDO
         IF ( isw/=0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( jsw/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*260,*100,tfpool,buf,i45,0,flag)
         isw = 1
         col = buf(1)
         Row = buf(2)
         IF ( pack ) THEN
            col = rshift(buf(1),Ihalf)
            Row = andf(buf(1),mask16)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( col<=icol ) THEN
            DO i = 1 , 3
               IF ( filei(i)>0 ) CALL bldpki(buf(i+i12),Row,filei(i),block(20*i-19))
            ENDDO
            isw = 0
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         DO i = 1 , 3
            IF ( filei(i)>0 ) CALL bldpkn(filei(i),block(20*i-19),block(7*i+54))
         ENDDO
         icol = icol + 1
         IF ( icol<=ncol ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CLOSE FILES AND WRITE TRAILERS. IF NO DMIG MATRICES, RETURN
!
         CALL close(tfpool,Clsrew)
         DO i = 1 , 3
            IF ( filei(i)>0 ) THEN
               i7 = 7*i
               block(i7+54) = filei(i)
               block(i7+56) = ncol
               block(i7+57) = 1
               block(i7+58) = 1
               CALL close(filei(i),1)
               CALL wrttrl(block(i7+54))
            ENDIF
         ENDDO
         IF ( nodmig==0 ) GOTO 220
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 100     jsw = 1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!     READ EQUIVALENCE TABLE INTO CORE
!
         file = eqex
         CALL gopen(eqex,Z(buf2),0)
         CALL skprec(eqex,1)
         CALL read(*260,*120,eqex,Z,buf2,1,neqex)
         CALL mesage(-8,0,nam)
 120     CALL close(eqex,Clsrew)
         kn = neqex/2
         nn = neqex - 1
         DO i = 1 , nn , 2
            Z(i+1) = Z(i+1)/10
         ENDDO
         spag_nextblock_1 = 9
      CASE (9)
         SPAG_Loop_1_1: DO
!
!     READ MATRIX HEADER INFORMATION.
!     LOOK UP MATRIX NAME IN NAME LIST. IF ABSENT, SKIP MATRIX.
!
            CALL read(*260,*200,mpool,buf,9,0,flag)
!
!     BUF(5)= INPUT MATRIX TYPE, BUF(6)= OUTOUT MATRIX TYPE
!     BUF(1) AND BUF(2) ARE MATRIX NAME FROM DMIG CARDS.
!
            k = buf(6)
            prec = Prc(k)
            k = buf(5)
            iprc = mod(k,2)
            nwd = Nwds(k)
            nwd1 = nwd + 1
            i11 = 11
            IF ( .NOT.(pack) ) THEN
               i11 = 10
               nwd1 = nwd + 2
            ENDIF
            i10 = i11 - 1
            DO i = 1 , nfiles , 10
               IF ( mcb(i+7)==buf(1) .AND. mcb(i+8)==buf(2) ) EXIT SPAG_Loop_1_1
            ENDDO
            DO
               CALL fread(mpool,buf,2,0)
               IF ( buf(1)==-1 ) CYCLE SPAG_Loop_1_1
               SPAG_Loop_3_2: DO
                  CALL fread(mpool,buf,2,0)
                  IF ( buf(1)==-1 ) EXIT SPAG_Loop_3_2
                  CALL fread(mpool,buf,-nwd,0)
               ENDDO SPAG_Loop_3_2
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     OPEN SCRATCH FILE. SET POINTERS.
!
         iptr = i
         file = mcb(iptr)
         mcb(iptr+2) = Luset
         mcb(iptr+3) = buf(4)
         mcb(iptr+4) = buf(6)
         mcb(iptr+9) = 1
         iqq = (iptr-1)/10
         Nomat(iqq+1) = +1
         isw = 0
         imtrx = neqex + 1
         i = imtrx
!
!
         CALL open(*240,scr1,Z(buf2),Wrtrew)
         spag_nextblock_1 = 10
      CASE (10)
!
!     READ COLUMN GRID AND COMPONENT, AND CHECK DUPLICATE.
!     CONVERT GRID AND COMPONENT TO SIL NO.
!
!     REMOVE DUPLICATE CHECK ADDED HERE IN 91 VERSION. CHECKING SHOULD
!     BE DONE EARLY IN IFP MODULE, AND NOT HERE. REMOVED ALSO NOGOX AND
!     ITS ASSOCIATED LINES.   3/93
!
         CALL fread(mpool,buf(10),2,0)
         IF ( buf(10)==-1 ) THEN
!
!     SORT MATRIX.
!
            IF ( isw==0 ) THEN
               n = i - imtrx
               nmtrx = i - nwd1
               CALL close(scr1,Clsrew)
               IF ( pack ) CALL sorti(0,0,nwd1,1,Z(imtrx),n)
               IF ( .NOT.pack ) CALL sorti2(0,0,nwd1,1,Z(imtrx),n)
            ELSE
               CALL write(scr1,0,0,1)
               CALL close(scr1,Clsrew)
               CALL open(*240,scr1,Z(buf2),Rdrew)
               Ifile(1) = scr2
               Ifile(2) = scr3
               Ifile(3) = scr4
               IF ( pack ) CALL sorti(scr1,0,nwd1,1,Z(imtrx),buf2-imtrx)
               IF ( .NOT.pack ) CALL sorti2(scr1,0,nwd1,1,Z(imtrx),buf2-imtrx)
               CALL close(scr1,Clsrew)
            ENDIF
!
!     OPEN OUTPUT FILE. WRITE HEADER RECORD
!     IF SORTED MATRIX NOT IN CORE, OPEN FILE WITH MATRIX.
!
            IF ( tfset/=0 ) file = scr1
            CALL open(*240,file,Z(buf2),Wrtrew)
            CALL fname(file,buf(19))
            CALL write(file,buf(19),2,1)
            IF ( isw/=0 ) CALL open(*240,Ifile(6),Z(buf3),Rdrew)
!
!     PACK MATRIX ONTO OUTPUT FILE.
!
            ncol = Luset
            j = imtrx
            icol = 1
            jsw = 0
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!
            ASSIGN 140 TO ret
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 140     col = Z(2*k)
         IF ( buf(11)/=0 ) col = col + buf(11) - 1
         IF ( pack ) col = lshift(col,Ihalf)
         spag_nextblock_1 = 11
      CASE (11)
!
!     READ A COLUMN OF THE MATRIX.
!     STORE IN CORE OR ON SCRATCH FILE IF TOO BIG FOR CORE.
!
         CALL fread(mpool,buf(10),2,0)
         IF ( buf(10)==-1 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 160 TO ret
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 160     Row = Z(2*k)
         IF ( buf(11)/=0 ) Row = Row + buf(11) - 1
         buf(11) = Row
         buf(10) = col
         IF ( pack ) buf(11) = Row + col
         CALL fread(mpool,buf(12),nwd,0)
         IF ( isw/=0 ) THEN
            CALL write(scr1,buf(i11),nwd1,0)
         ELSEIF ( i+nwd1<buf2 ) THEN
            DO j = 1 , nwd1
               Z(i) = buf(j+i10)
               i = i + 1
            ENDDO
         ELSE
            isw = 1
            CALL write(scr1,Z(imtrx),i-imtrx,0)
            CALL write(scr1,buf(i11),nwd1,0)
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         CALL bldpk(buf(6),buf(6),file,0,0)
         IF ( jsw/=0 ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         IF ( j>nmtrx ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( isw==0 ) THEN
            DO k = 1 , nwd1
               buf(k+i10) = Z(j)
               j = j + 1
            ENDDO
         ELSE
            CALL read(*260,*180,Ifile(6),buf(i11),nwd1,0,flag)
         ENDIF
         col = buf(10)
         Row = buf(11)
         IF ( pack ) THEN
            col = rshift(buf(11),Ihalf)
            Row = andf(buf(11),mask16)
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
         IF ( col>icol ) THEN
            jsw = 1
         ELSE
            jsw = 0
            IF ( prec==2 ) THEN
               IF ( iprc==0 ) THEN
                  xd(1) = bufd(1)
                  xd(2) = bufd(2)
               ELSE
                  xd(1) = bufr(12)
                  xd(2) = bufr(13)
               ENDIF
            ELSEIF ( iprc==0 ) THEN
               X(1) = bufd(1)
               X(2) = bufd(2)
            ELSE
               X(1) = bufr(12)
               X(2) = bufr(13)
            ENDIF
            CALL zblpki
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
         CALL bldpkn(file,0,mcb(iptr))
         icol = icol + 1
         IF ( icol<=ncol ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL close(file,Clsrew)
         IF ( isw/=0 ) CALL close(Ifile(6),Clsrew)
         CALL wrttrl(mcb(iptr))
!
!     IF TRANSFER FUNCTION MATRICES ARE TO BE ADDED, CALL MATRIX ADD
!     ROUTINE THEN RETURN TO READ NEXT MATRIX IN THE MATRIX POOL.
!
         IF ( tfset/=0 ) THEN
            j = 2
            IF ( iptr==1 ) j = 1
            IF ( iptr==11 ) j = 3
            DO i = 1 , 7
               k = iptr + i - 1
               filea(i) = mcb(k)
               k = 7*j + i
               fileb(i) = block(k+53)
               filec(i) = 0
            ENDDO
            filea(1) = scr1
            filec(1) = mcb(iptr)
            filec(2) = ncol
            filec(3) = ncol
            filec(4) = filea(4)
            filec(5) = filea(5)
            Nz = buf1 - imtrx
            Nomats = 2
            k = orf(imtrx,1)
            CALL sadd(Z(k),Z(k))
            CALL wrttrl(filec)
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 180     j = nmtrx + 1
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
!
!     TEST FOR ALL REQUESTED MATRICES FOUND.
!
 200     DO i = 1 , nfiles , 10
            IF ( mcb(i+7)/=0 .AND. mcb(i+9)==0 ) THEN
               CALL mesage(30,70,mcb(i+7))
               nogo = 1
            ENDIF
         ENDDO
 220     IF ( nompoo/=0 ) CALL close(mpool,Clsrew)
         IF ( nogo/=0 ) CALL mesage(-61,0,nam)
         RETURN
!
!     FATAL ERRORS
!
 240     n = -1
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 260     n = -2
         spag_nextblock_1 = 16
      CASE (16)
         CALL mesage(n,file,nam)
         spag_nextblock_1 = 17
      CASE (17)
!
!     BINARY SEARCH ROUTINE
!
         klo = 1
         khi = kn
         SPAG_Loop_1_4: DO
            k = (klo+khi+1)/2
            SPAG_Loop_2_3: DO
               IF ( buf(10)<Z(2*k-1) ) THEN
                  khi = k
               ELSEIF ( buf(10)==Z(2*k-1) ) THEN
                  EXIT SPAG_Loop_2_3
               ELSE
                  klo = k
               ENDIF
               IF ( khi-klo<1 ) THEN
                  nogo = 1
                  buf(11) = 0
                  EXIT SPAG_Loop_2_3
               ELSEIF ( khi-klo==1 ) THEN
                  IF ( k==klo ) THEN
                     k = khi
                  ELSE
                     k = klo
                  ENDIF
                  klo = khi
               ELSE
                  CYCLE SPAG_Loop_1_4
               ENDIF
            ENDDO SPAG_Loop_2_3
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
         GOTO ret
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mtrxin
