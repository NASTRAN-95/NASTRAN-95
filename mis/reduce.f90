
SUBROUTINE reduce
!
!     REDUCE BUILDS THE FOLLOWING DATA BLOCKS
!
!     1.  PVX  -  THE REDUCTION PARTITIONING VECTOR
!     2.  USX  -  THE USET EQUIVALENT VECTOR
!     3.  INX  -  THE REDUCTION TRANSFORMATION IDENTITY PARTITION
!
!     THE FOLLOWING BULK DATA CARDS ARE READ
!
!     1.  BDYC
!     2.  BDYS
!     3.  BDYS1
!
   IMPLICIT NONE
   INTEGER Dry , Idate(3) , Ihead(96) , Iiierr , Iinam(2) , Incr , Irow , Ititl(96) , Line , Nlpp , Nrow , Outt , Pora , Step ,     &
         & Sysbuf , Tpow(32) , Typin , Typout , X1(6) , X2(2) , X3(2) , Z(1)
   REAL Rz(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Step , Dry , Pora
   COMMON /cmbfnd/ Iinam , Iiierr
   COMMON /output/ Ititl , Ihead
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /system/ Sysbuf , Outt , X1 , Nlpp , X2 , Line , X3 , Idate
   COMMON /two   / Tpow
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER andf , korsz , lshift , orf , rshift
   INTEGER aray(6) , bdyc(2) , bdys(2) , bdys1(2) , bset , buf1 , buf2 , buf3 , casecc , cset(6) , flag , geom4 , i , i1 , i2 , i3 ,&
         & iadd , ib1 , ib2 , ib3 , iba , ibad , ibf , ibits(6) , ibo , icode , icomp , id , idhid , ierr , ifile , ifin , ihd(96) ,&
         & ii , iinc , ijk(6) , ik , iloc , imsg , index , inx , inxt , ipnew , ipo , ipset(6) , isid(100) , isil , isilm1 , ist ,  &
         & it , itest , itype , j , jj , jjj , jjj1 , jp , kf1 , kf2 , kf3 , kid , ks1 , ks2 , ks3 , listn(32) , listo(32) , litm , &
         & loap , loc , lods , mcb(7) , mnem(4) , modnam(2) , n , namnew(2) , namold(14) , ncsred , ncsub , ndof , nent
   LOGICAL bad , fset , inbset , lonly
   INTEGER ngrp , nhbgss , nhcstm , nheqss , nhplts , nipnew , nnew , nnn , no , nout , nrec , nrsid , nset , nsid , nw , nwbs ,    &
         & nwds , nwdscc , nz , nzwd , papp , prtopt , pvx , score , scr1 , scr2 , usx
   EXTERNAL andf , lshift , orf , rshift
   EQUIVALENCE (Rz(1),Z(1))
   DATA ihd/4H     , 8*4H**** , 4H S U , 4H B S , 4H T R , 4H U C , 4H T U , 4H R E , 4H     , 4HM O  , 4HD U  , 4HL E  , 4H   R ,  &
       &4H E D , 4H U C , 4H E * , 9*4H**** , 64*4H    /
   DATA nheqss , nhbgss , nhcstm , nhplts/4HEQSS , 4HBGSS , 4HCSTM , 4HPLTS/
   DATA modnam/4HREDU , 4HCE  /
   DATA papp , lods , loap/4HPAPP , 4HLODS , 4HLOAP/
!     --------------------
!     CODES TO LOCATE BULK DATA
!     --------------------
   DATA bdyc/910 , 9/ , bdys/1210 , 12/ , bdys1/1310 , 13/
!     --------------------
!     CASE CONTROL MNEMONICS
!     --------------------
   DATA mnem/4HNAMA , 4HNAMB , 4HBOUN , 4HOUTP/
!     --------------------
!     GINO FILES FOR DATA BLOCKS AND SCRATCH
!     --------------------
   DATA casecc/101/ , geom4/102/
   DATA pvx/201/ , usx/202/ , inx/203/
   DATA scr1/301/ , scr2/302/ , i3/3/
!
!
!     I.  COMPUTE OPEN CORE AND DEFINE GINO AND SOF BUFFERS
!     *****************************************************
!
   IF ( Dry==-2 ) RETURN
   iba = 128
   ibo = 4
   ibf = 64
   nzwd = korsz(Z(1))
   IF ( nzwd<=0 ) CALL mesage(-8,0,modnam)
!
   lonly = .FALSE.
   buf1 = nzwd - Sysbuf - 2
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   ib1 = buf3 - Sysbuf
   ib2 = ib1 - Sysbuf
   ib3 = ib2 - Sysbuf
!
!     SCORE IS STARTING ADDRESS OF OPEN CORE AND NZ THE LENGTH
!
   score = 1
   nz = ib3 - 1
!
!     INITIALIZE ACTIVITY ON THE SOF
!
   litm = lods
   IF ( Pora==papp ) litm = loap
   CALL sofopn(Z(ib1),Z(ib2),Z(ib3))
   DO i = 1 , 96
      Ihead(i) = ihd(i)
   ENDDO
!
!     II.  PROCESS THE CASE CONTROL DATA BLOCK ( CASECC )
!     ***************************************************
!
   DO i = 1 , 14
      namold(i) = 0
   ENDDO
   ifile = casecc
   CALL open(*3000,casecc,Z(buf2),0)
   prtopt = 0
   nrec = Step
   IF ( nrec/=0 ) THEN
      DO i = 1 , nrec
         CALL fwdrec(*3100,casecc)
      ENDDO
   ENDIF
!
!     BEGIN READING CASECC
!
   inbset = .FALSE.
   CALL read(*3100,*3200,casecc,Z(1),2,0,nnn)
   nwdscc = Z(i3-1)
   DO i = 1 , nwdscc , 3
      CALL read(*3100,*3200,casecc,Z(1),3,0,nnn)
!
!     CHECK FOR CASE CONTROL MNEMONICS
!
      DO j = 1 , 4
         IF ( Z(1)==mnem(j) ) GOTO 50
      ENDDO
      CYCLE
 50   IF ( j==2 ) THEN
         namnew(1) = Z(i3-1)
         namnew(2) = Z(i3)
      ELSEIF ( j==3 ) THEN
         inbset = .TRUE.
         bset = Z(i3)
      ELSEIF ( j==4 ) THEN
         prtopt = orf(prtopt,Z(i3))
      ELSE
         namold(1) = Z(i3-1)
         namold(2) = Z(i3)
      ENDIF
   ENDDO
   IF ( Dry==0 ) prtopt = 0
   IF ( andf(prtopt,1)==1 ) THEN
      CALL page1
      WRITE (Outt,99001) (namold(i),i=1,2) , namnew , bset , (namold(i),i=1,2)
99001 FORMAT (//41X,'S U M M A R Y    O F    C U R R E N T    P R O ','B L E M',//43X,'NAME OF PSEUDOSTRUCTURE TO BE REDUCED    - ',&
            & 2A4,//43X,'NAME GIVEN TO RESULTANT PSEUDOSTRUCTURE  - ',2A4,//43X,'BOUNDARY SET IDENTIFICATION NUMBER       - ',I8,   &
            & //43X,'NAMES OF COMPONENT SUBSTRUCTURES CONTAINED IN ',2A4/)
   ENDIF
   CALL close(casecc,1)
!
!     CHECK FOR ALLOWABILITY OF INPUT
!
   bad = .FALSE.
   CALL sfetch(namold,nheqss,3,itest)
   IF ( itest==4 ) THEN
!
!     IF NO ERRORS, CONTINUE PROCESSING
!
!
      WRITE (Outt,99002) Ufm , (namold(i),i=1,2)
99002 FORMAT (A23,' 6601, REQUEST TO REDUCE PSEUDOSTRUCTURE ',2A4,' INVALID. DOES NOT EXIST ON THE SOF.')
      bad = .TRUE.
   ENDIF
   CALL sfetch(namnew,nheqss,3,itest)
   IF ( itest/=4 .AND. Dry/=0 ) THEN
      CALL sfetch(namnew,litm,3,itest)
      IF ( itest/=3 ) THEN
         WRITE (Outt,99003) Ufm , (namnew(i),i=1,2)
99003    FORMAT (A23,' 6602, THE NAME ',2A4,' CAN NOT BE USED FOR THE ','REDUCED PSEUDOSTRUCTURE. IT ALREADY EXISTS ON THE SOF.')
         bad = .TRUE.
      ELSE
         lonly = .TRUE.
         GOTO 100
      ENDIF
   ELSEIF ( itest==4 .AND. Dry==0 ) THEN
      WRITE (Outt,99004) Ufm , namnew
99004 FORMAT (A23,' 6613, FOR RUN=GO, THE REDUCED SUBSTRUCTURE ',2A4,' MUST ALREADY EXIST.')
      bad = .TRUE.
   ENDIF
   IF ( .NOT.inbset ) THEN
      WRITE (Outt,99005) Ufm
99005 FORMAT (A23,' 6603, A BOUNDARY SET MUST BE SPECIFIED FOR A ','REDUCE OPERATION.')
      bad = .TRUE.
   ENDIF
   IF ( bad ) THEN
!
      WRITE (Outt,99006) Ufm
99006 FORMAT (A23,' 6535, MODULE REDUCE TERMINATING DUE TO ABOVE ','SUBSTRUCTURE CONTROL ERRORS.')
      GOTO 2900
   ENDIF
!
!     READ FIRST GROUP OF EQSS FOR THE STRUCTURE BEING REDUCED,
!     PLACE THE NAMES OF THE COMPONENT SUBSTRUCTURES INTO THE
!     FIRST NWDS WORDS OF OPEN CORE.
!
 100  ks1 = score
   CALL sfetch(namold,nheqss,1,itest)
   CALL suread(Z(ks1),-1,nout,itest)
!
!     NCSUB IS THE NUMBER OF COMPONENT SUBSTRUCTURES
!     NIPOLD IS THE NUMBER OF IP S IN THE STRUCTURE BEING REDUCED
!
   ncsub = Z(ks1+2)
   nout = nout - 4
   DO i = 1 , nout
      ii = i - 1
      Z(ks1+ii) = Z(ks1+4+ii)
   ENDDO
   nwds = nout
   score = ks1 + nwds
   kf1 = score - 1
   nz = nz - nwds
   IF ( andf(prtopt,1)==1 ) THEN
      WRITE (Outt,99007) (Z(jj),jj=ks1,kf1)
99007 FORMAT (48X,2A4,4X,2A4,4X,2A4,4X,2A4)
   ENDIF
!
!     III. READ BOUNDARY SET ( BDYC ) BULK DATA INTO OPEN CORE FOR
!     THE REQUESTED SET ( BSET ) FROM THE GEOM4 INPUT DATA BLOCK.
!     ************************************************************
!
   ks2 = score
   ifile = geom4
   CALL preloc(*3000,Z(buf1),geom4)
   CALL locate(*300,Z(buf1),bdyc,flag)
   DO
      CALL read(*3100,*300,geom4,id,1,0,nnn)
      IF ( id==bset ) THEN
!
!     CORRECT BOUNDARY SET HAS BEEN FOUND, STORE DATA IN SECOND NWBS WOR
!     OF OPEN CORE.
!
         nwbs = 0
         DO
            bad = .FALSE.
            CALL read(*3100,*3200,geom4,Z(ks2+nwbs),3,0,nnn)
            IF ( Z(ks2+nwbs+2)==-1 ) THEN
               score = ks2 + nwbs
               kf2 = score - 1
               nz = nz - nwbs
!
!     SORT ON SET ID
!
               CALL sort(0,0,3,3,Z(ks2),nwbs)
               IF ( andf(rshift(prtopt,1),1)/=1 ) GOTO 400
               ii = 0
               GOTO 200
            ELSE
!
!     MUST CHECK THAT THE SUBSTRUCTURE IS A PHASE1 BASIC SUBSTRUCTURE
!     AND THAT IT IS A COMPONENT OF THE STRUCTURE BEING REDUCED.
!
!     CHECK FOR COMPONENT
!
               DO i = 1 , nwds , 2
                  ii = i - 1
                  IF ( Z(ks1+ii)==Z(ks2+nwbs) .AND. Z(ks1+ii+1)==Z(ks2+nwbs+1) ) GOTO 105
               ENDDO
!
!     NOT A COMPONENT
!
               WRITE (Outt,99008) Ufm , Z(ks2+nwbs) , Z(ks2+nwbs+1)
99008          FORMAT (A23,' 6604, A BOUNDARY SET HAS BEEN SPECIFIED FOR ',2A4,', BUT IT IS NOT A COMPONENT OF THE',/31X,           &
                      &'PSEUDOSTRUC','TURE BEING REDUCED. THE BOUNDARY SET WILL BE IGNORED.')
               bad = .TRUE.
!
 105           IF ( .NOT.(bad) ) nwbs = nwbs + 3
            ENDIF
         ENDDO
      ELSE
         DO
            CALL read(*3100,*3200,geom4,aray,3,0,nnn)
            IF ( aray(3)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
 200  CALL page1
   WRITE (Outt,99009) bset
99009 FORMAT (//44X,'SUMMARY OF COMBINED BOUNDARY SET NUMBER',I9,//55X,'BASIC',11X,'BOUNDARY',/52X,'SUBSTRUCTURE',8X,'SET ID',/56X, &
             &'NAME',12X,'NUMBER',/)
   Line = Line + 7
   DO
      Line = Line + 1
      IF ( Line>Nlpp ) GOTO 200
      WRITE (Outt,99010) Z(ks2+ii) , Z(ks2+ii+1) , Z(ks2+ii+2)
99010 FORMAT (54X,2A4,9X,I8)
      ii = ii + 3
      IF ( ii>nwbs-3 ) GOTO 400
   ENDDO
!WKBR 8/94 ALPHA-VMS  490 WRITE (OUTT,493) IFM,BSET
 300  WRITE (Outt,99011) Ufm , bset
99011 FORMAT (A23,' 6606, BOUNDARY SET ,I8,61H SPECIFIED IN CASE ','CONTROL HAS NOT BEEN DEFINED BY BULK DATA.')
   GOTO 2700
!
!     IV. READ BDYS BULK DATA PROCESSING ONLY THE SET ID S REFERENCED ON
!     THE BDYC CARD.  IF DATA DOES NOT EXIST, GO TO BDYS1 PROCESSING SEC
!     ******************************************************************
!
 400  j = 0
   ierr = 0
   CALL locate(*700,Z(buf1),bdys,flag)
 500  CALL read(*3100,*800,geom4,idhid,1,0,nnn)
!
!     CHECK REQUESTED ID
!
   DO i = ks2 , kf2 , 3
      IF ( idhid==Z(i+2) ) GOTO 600
   ENDDO
   DO
      CALL read(*3100,*3200,geom4,aray,2,0,nnn)
      IF ( aray(1)==-1 .OR. aray(2)==-1 ) GOTO 500
   ENDDO
 600  DO
      CALL read(*3100,*3200,geom4,aray,2,0,nnn)
      IF ( aray(1)==-1 .AND. aray(2)==-1 ) GOTO 500
      Z(score+j) = idhid
      Z(score+j+1) = aray(1)
      Z(score+j+2) = aray(2)
      j = j + 3
   ENDDO
 700  ierr = ierr + 1
!
!     V. READ BDYS1 BULK DATA AND MERGE WITH BDYS IN OPEN CORE.
!     *********************************************************
!
 800  CALL locate(*1100,Z(buf1),bdys1,flag)
 900  CALL read(*3100,*1200,geom4,aray(1),2,0,nnn)
!
!     CHECK ID
!
   DO i = ks2 , kf2 , 3
      IF ( aray(1)==Z(i+2) ) GOTO 1000
   ENDDO
   DO
      CALL read(*3100,*3200,geom4,aray(3),1,0,nnn)
      IF ( aray(3)==-1 ) GOTO 900
   ENDDO
 1000 DO
      CALL read(*3100,*3200,geom4,aray(3),1,0,nnn)
      IF ( aray(3)==-1 ) GOTO 900
      Z(score+j) = aray(1)
      Z(score+j+1) = aray(3)
      Z(score+j+2) = aray(2)
      j = j + 3
   ENDDO
 1100 ierr = ierr + 1
 1200 CALL close(geom4,1)
   IF ( ierr/=2 ) THEN
!
!     SORT COMPLETE BOUNDARY SET DATA ON SET ID IN OPEN CORE
!
      CALL sort(0,0,3,1,Z(score),j)
!
!     TRANSLATE COMPONENT NUMBER TO BIT PATTERN
!
      it = score + j - 1
      DO i = score , it , 3
         CALL encode(Z(i+2))
      ENDDO
      IF ( andf(rshift(prtopt,2),1)/=1 ) GOTO 1400
      iinc = 0
   ELSE
      WRITE (Outt,99012) Ufm , bset
99012 FORMAT (A23,' 6607, NO BDYS OR BDYS1 BULK DATA HAS BEEN INPUT TO',' DEFINE BOUNDARY SET',I8)
      GOTO 2700
   ENDIF
 1300 CALL page1
   WRITE (Outt,99013)
99013 FORMAT (1H0,46X,44HTABLE OF GRID POINTS COMPOSING BOUNDARY SETS,//52X,8HBOUNDARY,/52X,34H SET ID      GRID POINT       DOF ,  &
            & /52X,34H NUMBER      ID  NUMBER       CODE,/)
   Line = Line + 7
   DO
      Line = Line + 1
      IF ( Line>Nlpp ) GOTO 1300
      icode = Z(score+iinc+2)
      CALL bitpat(icode,ibits)
      WRITE (Outt,99014) Z(score+iinc) , Z(score+iinc+1) , ibits(1) , ibits(2)
99014 FORMAT (52X,I8,6X,I8,7X,A4,A2)
      iinc = iinc + 3
      IF ( iinc>j-3 ) EXIT
   ENDDO
!
!     WRITE BOUNDARY SET DATA ON TO FILE SCR1, ONE LOGICAL RECORD FOR EA
!     SET ID.
!
 1400 CALL open(*3000,scr1,Z(buf2),1)
   ist = score + 3
   ifin = score + j - 1
   n = 1
   nsid = 1
   isid(1) = Z(score)
   CALL write(scr1,Z(score+1),2,0)
   DO i = ist , ifin , 3
      IF ( Z(i)/=isid(n) ) THEN
         n = n + 1
         nsid = nsid + 1
         isid(n) = Z(i)
         CALL write(scr1,aray,0,1)
      ENDIF
      CALL write(scr1,Z(i+1),2,0)
   ENDDO
   CALL write(scr1,aray,0,1)
   CALL close(scr1,1)
!
!
!     SCR1 NOW CONTAINS BOUNDARY SET DATA FOR ALL GRID POINTS
!
!     CHECK THAT ALL REQUESTED SID S HAVE BEEN FOUND
!
   nrsid = nwbs/3
   j = 0
   DO i = ks2 , kf2 , 3
      Z(score+j) = Z(i+2)
      j = j + 1
   ENDDO
   DO i = 1 , nrsid
      ii = i - 1
      DO j = 1 , nsid
         IF ( isid(j)==Z(score+ii) ) GOTO 1450
      ENDDO
      CYCLE
 1450 Z(score+ii) = 0
   ENDDO
   ibad = 0
   DO i = 1 , nrsid
      ii = i - 1
      IF ( Z(score+ii)/=0 ) THEN
         index = (i-1)*3
         WRITE (Outt,99015) Ufm , Z(ks2+index+2) , Z(ks2+index) , Z(ks2+index+1)
99015    FORMAT (A23,' 6608, THE REQUEST FOR BOUNDARY SET ',I8,' SUBSTRUCTURE ',2A4,' WAS NOT DEFINED.')
         ibad = 1
      ENDIF
   ENDDO
   IF ( ibad==1 ) GOTO 2800
!
!     VI. PROCESS THE EQSS FROM THE SOF FOR EACH COMPONENT SUBSTRUCTURE.
!     ******************************************************************
!
   CALL open(*3000,scr1,Z(buf3),0)
   CALL open(*3000,scr2,Z(buf2),1)
   CALL sfetch(namold,nheqss,1,itest)
   ngrp = 1
   CALL sjump(ngrp)
!
!     READ AND PROCESS EQSS
!
   bad = .FALSE.
   DO i = 1 , ncsub
      ii = 2*(i-1)
      CALL suread(Z(score),-1,nout,itest)
      IF ( andf(rshift(prtopt,3),1)==1 ) CALL cmiwrt(1,namold,Z(ks1+ii),score,nout,Z,Z)
!
!     FIND A BOUNDARY SET FOR THE COMPONENT
!
      inxt = 1
      fset = .FALSE.
 1500 DO j = inxt , nwbs , 3
         jj = j - 1
         IF ( Z(ks2+jj)==Z(ks1+ii) .AND. Z(ks2+jj+1)==Z(ks1+ii+1) ) GOTO 1550
      ENDDO
      IF ( fset ) THEN
!
!     WRITE IP AND CB ON SCR2
!
         i1 = score
         i2 = i1 + nout - 1
         ii = -1
         DO j = i1 , i2 , 3
            ii = ii + 1
            aray(1) = andf(Z(j+2),Z(ist+ii))
            IF ( aray(1)/=0 ) CALL write(scr2,Z(j+1),1,0)
            IF ( aray(1)/=0 ) CALL write(scr2,aray(1),1,0)
         ENDDO
         CALL write(scr2,aray(1),0,1)
      ELSE
!
!     NO BOUNDARY SET FOR COMPONENT - IMPLIES ENTIRE SUBSTRUCTURE WILL B
!     REDUCED - POSSSIBLE ERROR.
!
         IF ( nout/=0 ) WRITE (Outt,99016) Uim , Z(ks1+ii) , Z(ks1+ii+1) , (namold(j),j=1,2)
99016    FORMAT (A29,' 6609, NO BOUNDARY SET HAS BEEN SPECIFIED FOR ','COMPONENT ',2A4,' OF PSEUDOSTRUCTURE ',2A4,/35X,             &
                &'ALL DEGREES OF FREEDOM WILL BE REDUCED.')
         CALL write(scr2,aray(1),0,1)
      ENDIF
      CYCLE
!
!     COMPONENT HAS A BOUNDARY SET, CALL EQSCOD TO ACCOUNT FOR POSSIBLE
!     MULTIPLE IP NUMBERS.
!
 1550 IF ( .NOT.(fset) ) THEN
         CALL eqscod(score,nout,Z)
!
!     DEFINE ARRAY TO CB - DEGREES OF FREEDOM RETAINED AS BOUNDARY POINT
!
         ist = score + nout
         ifin = ist + nout/3 - 1
         DO j = ist , ifin
            Z(j) = 0
         ENDDO
      ENDIF
!
!     LOCATE BOUNDARY SET ON SCR1
!
      inxt = jj + 4
      fset = .TRUE.
      nset = Z(ks2+jj+2)
      DO j = 1 , nsid
         IF ( nset==isid(j) ) EXIT
      ENDDO
      nrec = j - 1
      IF ( nrec/=0 ) THEN
         DO jj = 1 , nrec
            CALL fwdrec(*3100,scr1)
         ENDDO
      ENDIF
!
!     READ BOUNDARY DATA AND UPDATE CB
!
 1600 CALL read(*3100,*1700,scr1,aray,2,0,nnn)
!
!     LOCATE GRID ID IN EQSS AND SETS OF VALUES IF THE GRID IS MULTIPLY
!
      IF ( nout/=0 ) THEN
         CALL gridip(aray(1),score,nout,ipset,cset,no,Z,loc)
         IF ( Iiierr/=1 ) GOTO 1650
      ENDIF
      bad = .TRUE.
      WRITE (Outt,99017) Ufm , aray(1) , nset , Z(ks1+ii) , Z(ks1+ii+1)
99017 FORMAT (A23,' 6611, GRID POINT',I9,' SPECIFIED IN BOUNDARY SET',I9,' FOR SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
 1650 iadd = loc
      IF ( no>1 ) THEN
         icomp = 0
         DO j = 1 , no
            cset(j) = cset(j) - lshift(rshift(cset(j),26),26)
            icomp = orf(icomp,cset(j))
         ENDDO
      ELSE
         icomp = Z(iadd+2) - lshift(rshift(Z(iadd+2),26),26)
      ENDIF
!
!     CHECK THAT THE RETAINED DOF ARE A SUBSET OF THE ORIGINAL.
!
      IF ( andf(aray(2),icomp)/=aray(2) .AND. Iiierr/=1 ) THEN
         WRITE (Outt,99018) Uwm , aray(1) , Z(ks1+ii) , Z(ks1+ii+1)
99018    FORMAT (A25,' 6610, DEGREES OF FREEDOM AT GRID POINT',I9,' COMPONENT SUBSTRUCTURE ',2A4,/31X,'INCLUDED IN A ',             &
                &'BOUNDARY SET DO NOT EXIST. REQUEST WILL BE IGNORED.')
         aray(2) = aray(2) - (orf(aray(2),icomp)-icomp)
      ENDIF
!
!     UPDATE CB ARRAY
!
      IF ( no>1 ) THEN
         nent = (iadd-score)/3
         DO j = 1 , no
            Z(ist+nent+j-1) = orf(Z(ist+nent+j-1),aray(2))
         ENDDO
      ELSE
         nent = (iadd-score)/3
         Z(ist+nent) = orf(Z(ist+nent),aray(2))
      ENDIF
      GOTO 1600
!
!     BOUNDARY SET COMPLETE, IS THERE ANOTHER
!
 1700 CALL rewind(scr1)
      GOTO 1500
   ENDDO
   CALL close(scr1,1)
   CALL close(scr2,1)
   IF ( bad ) GOTO 2800
!
!     VII. PROCESS MASTER SIL LIST AND ALLOCATE SPACE FOR CNEW
!     ********************************************************
!
   j = 0
   DO
      CALL suread(Z(score+j),2,nout,itest)
      IF ( itest==3 ) THEN
         nw = j - 3
         DO i = 1 , nw , 3
            jj = i - 1
            Z(score+jj+2) = 0
         ENDDO
         CALL open(*3000,scr2,Z(buf2),0)
         EXIT
      ELSE
         j = j + 3
      ENDIF
   ENDDO
 1800 DO
      CALL read(*1900,*1800,scr2,aray,2,0,nnn)
      iloc = 3*aray(1) - 3
!
!     READ NEXT COMPONENT
!
      Z(score+iloc+2) = orf(Z(score+iloc+2),aray(2))
   ENDDO
!
!     PROCESSING COMPLETE
!
 1900 CALL close(scr2,1)
   ks3 = score
   score = score + nw
   kf3 = score - 1
!
!     VIII. DEFINE PARTITIONING VECTORS PVX AND USX
!     *********************************************
!
   CALL gopen(pvx,Z(buf2),1)
!
!     GENERATE PVX DATA BLOCK IN CORE
!
   jjj = 0
   DO i = 1 , nw , 3
      icode = Z(ks3+i)
      CALL decode(icode,listo,Nrow)
      DO j = 1 , Nrow
         Rz(score+jjj+j-1) = 0.0
      ENDDO
      icode = Z(ks3+i+1)
      CALL decode(icode,listn,nnew)
      DO j = 1 , Nrow
         listo(j) = listo(j) + 1
      ENDDO
      IF ( nnew/=0 ) THEN
         DO j = 1 , nnew
            listn(j) = listn(j) + 1
         ENDDO
!
!     FIND DOF THAT REMAIN AT GIVEN IP
!
         DO j = 1 , nnew
            DO jj = 1 , Nrow
               IF ( listn(j)==listo(jj) ) GOTO 1910
            ENDDO
            CYCLE
 1910       ijk(j) = jj
         ENDDO
         DO j = 1 , nnew
            ik = ijk(j)
            Rz(score+jjj+ik-1) = 1.0
         ENDDO
      ENDIF
      jjj = jjj + Nrow
   ENDDO
!
!     SET PARAMETERS AND CALL PACK
!
   mcb(1) = pvx
   mcb(2) = 0
   mcb(3) = jjj
   mcb(4) = 2
   mcb(5) = 1
   mcb(6) = 0
   mcb(7) = 0
   Typin = 1
   Typout = 1
   Incr = 1
   Irow = 1
   Nrow = jjj
   CALL pack(Rz(score),pvx,mcb)
   CALL wrttrl(mcb)
   CALL close(pvx,1)
   IF ( lonly ) GOTO 2400
!
!     PROCESS USX USET EQUIVALENT
!
   CALL open(*3000,usx,Z(buf2),1)
   CALL fname(usx,aray)
   CALL write(usx,aray,2,0)
   CALL write(usx,0.0,1,0)
   CALL write(usx,0.0,1,1)
   mcb(1) = usx
   mcb(2) = 0
   mcb(3) = jjj
   mcb(4) = 0
   mcb(5) = iba + ibo + ibf
   mcb(6) = 0
   mcb(7) = 0
   DO j = 1 , jjj
      jj = j - 1
!WKBDB 8/94 ALPHA-VMS
!      IF (RZ(SCORE+JJ) .EQ. 0.0) Z(SCORE+JJ) = IBF + IBO
!      IF (RZ(SCORE+JJ) .EQ. 1.0) Z(SCORE+JJ) = IBF + IBA
!WKBDE 8/94 ALPHA-VMS
!WKBNB 8/94 ALPHA-VMS
      IF ( Rz(score+jj)/=0.0 ) THEN
         IF ( Rz(score+jj)==1.0 ) Z(score+jj) = ibf + iba
      ELSE
         Z(score+jj) = ibf + ibo
      ENDIF
!WKBNE 8/94 ALPHA-VMS
   ENDDO
   CALL write(usx,Z(score),jjj,1)
   CALL wrttrl(mcb)
   CALL close(usx,1)
!
!     IX. PROCESS THE SOF FOR THE REDUCED STRUCTURE
!     *********************************************
!
!
!     PROCESS THE EQSS FOR EACH COMPONENT SUBSTRUCTURE
!
   CALL open(*3000,scr1,Z(buf1),1)
   CALL sfetch(namold,nheqss,1,itest)
!
!     UPDATE (SIL,C) REPLACING SIL WITH IPNEW
!
   ipnew = 1
   DO i = ks3 , kf3 , 3
      IF ( Z(i+2)/=0 ) THEN
         Z(i) = ipnew
         ipnew = ipnew + 1
      ELSE
         Z(i) = 0
      ENDIF
   ENDDO
   nipnew = ipnew - 1
   ngrp = 1
   CALL sjump(ngrp)
   DO j = 1 , ncsub
      CALL suread(Z(score),-1,nout,itest)
!
!     WRITE EQSS ENTRY ON SCR1 IF THE OLD IP NUMBER STILL EXISTS IN THE
!     REDUCED STRUCTURE, ALSO UPDATE DOF CODE.
!
      IF ( nout/=0 ) THEN
         DO i = 1 , nout , 3
            ii = i - 1
            ipo = Z(score+ii+1)
            iadd = ks3 + (ipo-1)*3
            IF ( Z(iadd)/=0 ) THEN
               aray(1) = Z(score+ii)
               aray(2) = Z(iadd)
               aray(3) = Z(iadd+2)
               CALL write(scr1,aray,3,0)
            ENDIF
         ENDDO
      ENDIF
      CALL write(scr1,0,0,1)
   ENDDO
!
!     GENERATE NEW MASTER (SIL,C) LIST
!
   isil = 1
   DO i = ks3 , kf3 , 3
      IF ( Z(i)/=0 ) THEN
         icode = Z(i+2)
         CALL decode(icode,listn,ndof)
         aray(1) = isil
         aray(2) = Z(i+2)
         CALL write(scr1,aray,2,0)
         isil = isil + ndof
      ENDIF
   ENDDO
   CALL write(scr1,aray,0,1)
   CALL close(scr1,1)
   IF ( Dry==0 ) GOTO 2500
!
!     WRITE FIRST GROUP OF EQSS
!
   CALL open(*3000,scr1,Z(buf1),0)
   CALL setlvl(namnew,1,namold,itest,28)
   IF ( itest==8 ) THEN
!
      WRITE (Outt,99019) Ufm
99019 FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
      GOTO 2800
   ELSE
      itest = 3
      CALL sfetch(namnew,nheqss,2,itest)
      itest = 1
      CALL suwrt(namnew,2,itest)
      itest = 1
      CALL suwrt(ncsub,1,itest)
      itest = 1
      CALL suwrt(nipnew,1,itest)
      DO i = ks1 , kf1 , 2
         itest = 1
         CALL suwrt(Z(i),2,itest)
      ENDDO
      itest = 2
      CALL suwrt(Z(i),0,itest)
   ENDIF
 2000 CALL read(*2200,*2100,scr1,Z(score),nz,0,nnn)
   imsg = -8
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
 2100 CALL suwrt(Z(score),nnn,2)
   GOTO 2000
 2200 itest = 3
   CALL suwrt(aray,0,itest)
   CALL close(scr1,1)
!
!     WRITE BGSS FILE
!
   CALL sfetch(namold,nhbgss,1,itest)
   ngrp = 1
   CALL sjump(ngrp)
   CALL suread(Z(score),-1,nout,itest)
   j = 0
!
!     THE CID S THAT BELONG TO POINTS THAT ARE COMPLETELY REDUCED
!     WILL BE ACCUMULATED IN BUF3.
!
   jjj1 = 2
   DO i = 1 , nout , 4
      ii = i - 1
      IF ( Z(ks3+jjj1)==0 ) THEN
         Z(score+ii) = -1*Tpow(2)
      ELSEIF ( Z(score+ii)/=0 ) THEN
         Z(buf3+j) = Z(score+ii)
         j = j + 1
      ENDIF
      jjj1 = jjj1 + 3
   ENDDO
   ncsred = j
   itest = 3
   CALL sfetch(namnew,nhbgss,2,itest)
   itest = 1
   CALL suwrt(namnew,2,itest)
   itest = 2
   CALL suwrt(nipnew,1,itest)
   DO i = 1 , nout , 4
      ii = i - 1
      IF ( Z(score+ii)/=-Tpow(2) ) THEN
         itest = 1
         CALL suwrt(Z(score+ii),4,itest)
      ENDIF
   ENDDO
   itest = 2
   CALL suwrt(aray,0,itest)
   itest = 3
   CALL suwrt(aray,0,itest)
!
!     PROCESS THE CSTM FILES
!
   IF ( ncsred/=0 ) THEN
      CALL sfetch(namold,nhcstm,1,itest)
      IF ( itest==3 ) GOTO 2400
      ngrp = 1
      CALL sjump(ngrp)
!
!     SORT THE DELETED CID S
!
      CALL sort(0,0,1,1,Z(buf3),ncsred)
!
!     READ ALL RETAINED CSTM DATA INTO OPEN CORE
!
      j = 0
   ELSE
      CALL sfetch(namold,nhcstm,1,itest)
      IF ( itest/=3 ) THEN
         CALL suread(Z(score),-2,nout,itest)
         Z(score) = namnew(1)
         Z(score+1) = namnew(2)
         itest = 3
         CALL sfetch(namnew,nhcstm,2,itest)
         itest = 3
         CALL suwrt(Z(score),nout,itest)
      ENDIF
      GOTO 2400
   ENDIF
 2300 DO
      CALL suread(Z(score+j),14,nout,itest)
      IF ( itest==2 ) THEN
         itest = 3
         CALL sfetch(namnew,nhcstm,2,itest)
         itest = 2
         CALL suwrt(namnew,2,itest)
         itest = 2
         CALL suwrt(Z(score),j,itest)
         itest = 3
         CALL suwrt(aray,0,itest)
         EXIT
      ELSEIF ( Z(score+j)/=0 ) THEN
         kid = Z(score+j)
         CALL bisloc(*2300,kid,Z(buf3),1,ncsred,jp)
         j = j + 14
      ENDIF
   ENDDO
!
!     PROCESS LODS ITEM
!
 2400 CALL sfetch(namold,litm,1,itest)
   IF ( itest/=3 ) THEN
      CALL suread(Z(score),-2,nout,itest)
      Z(score) = namnew(1)
      Z(score+1) = namnew(2)
      itest = 3
      CALL sfetch(namnew,litm,2,itest)
      itest = 3
      CALL suwrt(Z(score),nout,itest)
   ENDIF
   IF ( .NOT.(lonly) ) THEN
!
!     PROCESS PLTS ITEM
!
      CALL sfetch(namold,nhplts,1,itest)
      IF ( itest/=3 ) THEN
         CALL suread(Z(score),-1,nout,itest)
         Z(score) = namnew(1)
         Z(score+1) = namnew(2)
         itest = 3
         CALL sfetch(namnew,nhplts,2,itest)
         itest = 2
         CALL suwrt(Z(score),nout,itest)
         itest = 3
         CALL suwrt(Z(score),0,itest)
      ENDIF
!
!     PROCESS OUTPUT REQUESTS
!
      IF ( andf(rshift(prtopt,4),1)==1 ) THEN
!
!     WRITE EQSS FOR NEW STRUCTURE
!
         CALL sfetch(namnew,nheqss,1,itest)
         CALL suread(Z(score),4,nout,itest)
         CALL suread(Z(score),-1,nout,itest)
         ist = score + nout
         DO i = 1 , ncsub
            CALL suread(Z(ist),-1,nout,itest)
            iadd = score + 2*(i-1)
            CALL cmiwrt(1,namnew,Z(iadd),ist,nout,Z,Z)
         ENDDO
         CALL suread(Z(ist),-1,nout,itest)
         CALL cmiwrt(8,namnew,0,ist,nout,Z,Z)
      ENDIF
      IF ( andf(rshift(prtopt,5),1)==1 ) THEN
!
!     WRITE NEW BGSS
!
         CALL sfetch(namnew,nhbgss,1,itest)
         ngrp = 1
         CALL sjump(ngrp)
         ist = score
         CALL suread(Z(ist),-1,nout,itest)
         CALL cmiwrt(2,namnew,namnew,ist,nout,Z,Z)
      ENDIF
      IF ( andf(rshift(prtopt,6),1)==1 ) THEN
!
!     WRITE CSTM ITEM
!
         CALL sfetch(namnew,nhcstm,1,itest)
         IF ( itest/=3 ) THEN
            ngrp = 1
            CALL sjump(ngrp)
            ist = score
            CALL suread(Z(ist),-1,nout,itest)
            CALL cmiwrt(3,namnew,namnew,ist,nout,Z,Z)
         ENDIF
      ENDIF
      IF ( andf(rshift(prtopt,7),1)==1 ) THEN
!
!     WRITE PLTS ITEM
!
         CALL sfetch(namnew,nhplts,1,itest)
         IF ( itest/=3 ) THEN
            ist = score
            CALL suread(Z(ist),3,nout,itest)
            CALL suread(Z(ist),-1,nout,itest)
            CALL cmiwrt(4,namnew,namnew,ist,nout,Z,Z)
         ENDIF
      ENDIF
   ENDIF
   IF ( andf(rshift(prtopt,8),1)==1 ) THEN
!
!     WRITE LODS ITEM
!
      CALL sfetch(namnew,lods,1,itest)
      IF ( itest/=3 ) THEN
         CALL suread(Z(score),4,nout,itest)
         CALL suread(Z(score),-1,nout,itest)
         ist = score + nout
         itype = 5
         IF ( litm==loap ) itype = 7
         DO i = 1 , ncsub
            iadd = score + 2*(i-1)
            CALL suread(Z(ist),-1,nout,itest)
            CALL cmiwrt(itype,namnew,Z(iadd),ist,nout,Z,Z)
            itype = 6
         ENDDO
      ENDIF
   ENDIF
   IF ( lonly ) GOTO 2600
!
!     X. GENERATE THE INX OUTPUT DATA BLOCK
!     *************************************
!
 2500 CALL gopen(inx,Z(buf2),1)
   mcb(1) = inx
   mcb(2) = 0
   mcb(3) = isil - 1
   mcb(4) = 1
   mcb(5) = 1
   mcb(6) = 0
   mcb(7) = 0
   Typin = 1
   Typout = 1
   Incr = 1
   isilm1 = isil - 1
   DO i = 1 , isilm1
      Irow = i
      Nrow = i
      CALL pack(1.0,inx,mcb)
   ENDDO
   CALL wrttrl(mcb)
   CALL close(inx,1)
 2600 CALL sofcls
   RETURN
!
 2700 WRITE (Outt,99020) Ufm
99020 FORMAT (A23,' 6536, MODULE REDUCE TERMINATING DUE TO ABOVE ','ERRORS IN BULK DATA.')
   CALL close(geom4,1)
   GOTO 2900
!
 2800 WRITE (Outt,99021) Ufm
99021 FORMAT (A23,' 6537, MODULE REDUCE TERMINATING DUE TO ABOVE ','ERRORS.')
 2900 Dry = -2
   CALL sofcls
   RETURN
 3000 imsg = -1
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
 3100 imsg = -2
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
 3200 imsg = -3
   CALL mesage(imsg,ifile,modnam)
99999 RETURN
END SUBROUTINE reduce
