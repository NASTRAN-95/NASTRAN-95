!*==reduce.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE C_BLANK
   USE C_CMBFND
   USE C_OUTPUT
   USE C_PACKX
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: aray , cset , ibits , ijk , ipset
   LOGICAL :: bad , fset , inbset , lonly
   INTEGER , DIMENSION(2) , SAVE :: bdyc , bdys , bdys1 , modnam
   INTEGER :: bset , buf1 , buf2 , buf3 , flag , i , i1 , i2 , iadd , ib1 , ib2 , ib3 , iba , ibad , ibf , ibo , icode , icomp ,    &
            & id , idhid , ierr , ifile , ifin , ii , iinc , ik , iloc , imsg , index , inxt , ipnew , ipo , isil , isilm1 , ist ,  &
            & it , itest , itype , j , jj , jjj , jjj1 , jp , kf1 , kf2 , kf3 , kid , ks1 , ks2 , ks3 , litm , loc , n , ncsred ,   &
            & ncsub , ndof , nent , ngrp , nipnew , nnew , nnn , no , nout , nrec , nrsid , nset , nsid , nw , nwbs , nwds ,        &
            & nwdscc , nz , nzwd , prtopt , score
   INTEGER , SAVE :: casecc , geom4 , i3 , inx , loap , lods , nhbgss , nhcstm , nheqss , nhplts , papp , pvx , scr1 , scr2 , usx
   INTEGER , DIMENSION(96) , SAVE :: ihd
   INTEGER , DIMENSION(100) :: isid
   INTEGER , DIMENSION(32) :: listn , listo
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(4) , SAVE :: mnem
   INTEGER , DIMENSION(2) :: namnew
   INTEGER , DIMENSION(14) :: namold
   REAL , DIMENSION(1) :: rz
   EXTERNAL andf , bisloc , bitpat , close , cmiwrt , decode , encode , eqscod , fname , fwdrec , gopen , gridip , korsz , locate , &
          & lshift , mesage , open , orf , pack , page1 , preloc , read , rewind , rshift , setlvl , sfetch , sjump , sofcls ,      &
          & sofopn , sort , suread , suwrt , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
   INTEGER :: spag_nextblock_5
   !>>>>EQUIVALENCE (Rz(1),Z(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL open(*220,casecc,Z(buf2),0)
         prtopt = 0
         nrec = Step
         IF ( nrec/=0 ) THEN
            DO i = 1 , nrec
               CALL fwdrec(*240,casecc)
            ENDDO
         ENDIF
!
!     BEGIN READING CASECC
!
         inbset = .FALSE.
         CALL read(*240,*260,casecc,Z(1),2,0,nnn)
         nwdscc = Z(i3-1)
         DO i = 1 , nwdscc , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL read(*240,*260,casecc,Z(1),3,0,nnn)
!
!     CHECK FOR CASE CONTROL MNEMONICS
!
                  DO j = 1 , 4
                     IF ( Z(1)==mnem(j) ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  CYCLE
               CASE (2)
                  IF ( j==2 ) THEN
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
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         IF ( Dry==0 ) prtopt = 0
         IF ( andf(prtopt,1)==1 ) THEN
            CALL page1
            WRITE (Outt,99001) (namold(i),i=1,2) , namnew , bset , (namold(i),i=1,2)
99001       FORMAT (//41X,'S U M M A R Y    O F    C U R R E N T    P R O ','B L E M',//43X,                                        &
                   &'NAME OF PSEUDOSTRUCTURE TO BE REDUCED    - ',2A4,//43X,'NAME GIVEN TO RESULTANT PSEUDOSTRUCTURE  - ',2A4,//43X,&
                   &'BOUNDARY SET IDENTIFICATION NUMBER       - ',I8,//43X,'NAMES OF COMPONENT SUBSTRUCTURES CONTAINED IN ',2A4/)
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
99002       FORMAT (A23,' 6601, REQUEST TO REDUCE PSEUDOSTRUCTURE ',2A4,' INVALID. DOES NOT EXIST ON THE SOF.')
            bad = .TRUE.
         ENDIF
         CALL sfetch(namnew,nheqss,3,itest)
         IF ( itest/=4 .AND. Dry/=0 ) THEN
            CALL sfetch(namnew,litm,3,itest)
            IF ( itest/=3 ) THEN
               WRITE (Outt,99003) Ufm , (namnew(i),i=1,2)
99003          FORMAT (A23,' 6602, THE NAME ',2A4,' CAN NOT BE USED FOR THE ',                                                      &
                      &'REDUCED PSEUDOSTRUCTURE. IT ALREADY EXISTS ON THE SOF.')
               bad = .TRUE.
            ELSE
               lonly = .TRUE.
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( itest==4 .AND. Dry==0 ) THEN
            WRITE (Outt,99004) Ufm , namnew
99004       FORMAT (A23,' 6613, FOR RUN=GO, THE REDUCED SUBSTRUCTURE ',2A4,' MUST ALREADY EXIST.')
            bad = .TRUE.
         ENDIF
         IF ( .NOT.inbset ) THEN
            WRITE (Outt,99005) Ufm
99005       FORMAT (A23,' 6603, A BOUNDARY SET MUST BE SPECIFIED FOR A ','REDUCE OPERATION.')
            bad = .TRUE.
         ENDIF
         IF ( bad ) THEN
!
            WRITE (Outt,99006) Ufm
99006       FORMAT (A23,' 6535, MODULE REDUCE TERMINATING DUE TO ABOVE ','SUBSTRUCTURE CONTROL ERRORS.')
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ FIRST GROUP OF EQSS FOR THE STRUCTURE BEING REDUCED,
!     PLACE THE NAMES OF THE COMPONENT SUBSTRUCTURES INTO THE
!     FIRST NWDS WORDS OF OPEN CORE.
!
         ks1 = score
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
99007       FORMAT (48X,2A4,4X,2A4,4X,2A4,4X,2A4)
         ENDIF
!
!     III. READ BOUNDARY SET ( BDYC ) BULK DATA INTO OPEN CORE FOR
!     THE REQUESTED SET ( BSET ) FROM THE GEOM4 INPUT DATA BLOCK.
!     ************************************************************
!
         ks2 = score
         ifile = geom4
         CALL preloc(*220,Z(buf1),geom4)
         CALL locate(*20,Z(buf1),bdyc,flag)
         SPAG_Loop_1_1: DO
            CALL read(*240,*20,geom4,id,1,0,nnn)
            IF ( id==bset ) THEN
!
!     CORRECT BOUNDARY SET HAS BEEN FOUND, STORE DATA IN SECOND NWBS WOR
!     OF OPEN CORE.
!
               nwbs = 0
               DO
                  bad = .FALSE.
                  CALL read(*240,*260,geom4,Z(ks2+nwbs),3,0,nnn)
                  IF ( Z(ks2+nwbs+2)==-1 ) THEN
                     score = ks2 + nwbs
                     kf2 = score - 1
                     nz = nz - nwbs
!
!     SORT ON SET ID
!
                     CALL sort(0,0,3,3,Z(ks2),nwbs)
                     IF ( andf(rshift(prtopt,1),1)/=1 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     ii = 0
                     EXIT SPAG_Loop_1_1
                  ELSE
!
!     MUST CHECK THAT THE SUBSTRUCTURE IS A PHASE1 BASIC SUBSTRUCTURE
!     AND THAT IT IS A COMPONENT OF THE STRUCTURE BEING REDUCED.
!
!     CHECK FOR COMPONENT
!
                     DO i = 1 , nwds , 2
                        ii = i - 1
                        IF ( Z(ks1+ii)==Z(ks2+nwbs) .AND. Z(ks1+ii+1)==Z(ks2+nwbs+1) ) GOTO 2
                     ENDDO
!
!     NOT A COMPONENT
!
                     WRITE (Outt,99008) Ufm , Z(ks2+nwbs) , Z(ks2+nwbs+1)
99008                FORMAT (A23,' 6604, A BOUNDARY SET HAS BEEN SPECIFIED FOR ',2A4,', BUT IT IS NOT A COMPONENT OF THE',/31X,     &
                            &'PSEUDOSTRUC','TURE BEING REDUCED. THE BOUNDARY SET WILL BE IGNORED.')
                     bad = .TRUE.
!
 2                   IF ( .NOT.(bad) ) nwbs = nwbs + 3
                  ENDIF
               ENDDO
            ELSE
               SPAG_Loop_2_2: DO
                  CALL read(*240,*260,geom4,aray,3,0,nnn)
                  IF ( aray(3)==-1 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         SPAG_Loop_1_3: DO
            CALL page1
            WRITE (Outt,99009) bset
99009       FORMAT (//44X,'SUMMARY OF COMBINED BOUNDARY SET NUMBER',I9,//55X,'BASIC',11X,'BOUNDARY',/52X,'SUBSTRUCTURE',8X,'SET ID',&
                  & /56X,'NAME',12X,'NUMBER',/)
            Line = Line + 7
            DO
               Line = Line + 1
               IF ( Line>Nlpp ) CYCLE SPAG_Loop_1_3
               WRITE (Outt,99010) Z(ks2+ii) , Z(ks2+ii+1) , Z(ks2+ii+2)
99010          FORMAT (54X,2A4,9X,I8)
               ii = ii + 3
               IF ( ii>nwbs-3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
!WKBR 8/94 ALPHA-VMS  490 WRITE (OUTT,493) IFM,BSET
 20      WRITE (Outt,99011) Ufm , bset
99011    FORMAT (A23,' 6606, BOUNDARY SET ,I8,61H SPECIFIED IN CASE ','CONTROL HAS NOT BEEN DEFINED BY BULK DATA.')
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     IV. READ BDYS BULK DATA PROCESSING ONLY THE SET ID S REFERENCED ON
!     THE BDYC CARD.  IF DATA DOES NOT EXIST, GO TO BDYS1 PROCESSING SEC
!     ******************************************************************
!
         j = 0
         ierr = 0
         CALL locate(*40,Z(buf1),bdys,flag)
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_4: DO
            CALL read(*240,*60,geom4,idhid,1,0,nnn)
!
!     CHECK REQUESTED ID
!
            DO i = ks2 , kf2 , 3
               IF ( idhid==Z(i+2) ) EXIT SPAG_Loop_1_4
            ENDDO
            DO
               CALL read(*240,*260,geom4,aray,2,0,nnn)
               IF ( aray(1)==-1 .OR. aray(2)==-1 ) CYCLE SPAG_Loop_1_4
            ENDDO
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
         DO
            CALL read(*240,*260,geom4,aray,2,0,nnn)
            IF ( aray(1)==-1 .AND. aray(2)==-1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Z(score+j) = idhid
            Z(score+j+1) = aray(1)
            Z(score+j+2) = aray(2)
            j = j + 3
         ENDDO
 40      ierr = ierr + 1
!
!     V. READ BDYS1 BULK DATA AND MERGE WITH BDYS IN OPEN CORE.
!     *********************************************************
!
 60      CALL locate(*80,Z(buf1),bdys1,flag)
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_5: DO
            CALL read(*240,*100,geom4,aray(1),2,0,nnn)
!
!     CHECK ID
!
            DO i = ks2 , kf2 , 3
               IF ( aray(1)==Z(i+2) ) EXIT SPAG_Loop_1_5
            ENDDO
            DO
               CALL read(*240,*260,geom4,aray(3),1,0,nnn)
               IF ( aray(3)==-1 ) CYCLE SPAG_Loop_1_5
            ENDDO
            EXIT SPAG_Loop_1_5
         ENDDO SPAG_Loop_1_5
         DO
            CALL read(*240,*260,geom4,aray(3),1,0,nnn)
            IF ( aray(3)==-1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Z(score+j) = aray(1)
            Z(score+j+1) = aray(3)
            Z(score+j+2) = aray(2)
            j = j + 3
         ENDDO
 80      ierr = ierr + 1
 100     CALL close(geom4,1)
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
            IF ( andf(rshift(prtopt,2),1)/=1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iinc = 0
         ELSE
            WRITE (Outt,99012) Ufm , bset
99012       FORMAT (A23,' 6607, NO BDYS OR BDYS1 BULK DATA HAS BEEN INPUT TO',' DEFINE BOUNDARY SET',I8)
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_6: DO
            CALL page1
            WRITE (Outt,99013)
99013       FORMAT (1H0,46X,44HTABLE OF GRID POINTS COMPOSING BOUNDARY SETS,//52X,8HBOUNDARY,/52X,                                  &
                   &34H SET ID      GRID POINT       DOF ,/52X,34H NUMBER      ID  NUMBER       CODE,/)
            Line = Line + 7
            SPAG_Loop_2_7: DO
               Line = Line + 1
               IF ( Line>Nlpp ) CYCLE SPAG_Loop_1_6
               icode = Z(score+iinc+2)
               CALL bitpat(icode,ibits)
               WRITE (Outt,99014) Z(score+iinc) , Z(score+iinc+1) , ibits(1) , ibits(2)
99014          FORMAT (52X,I8,6X,I8,7X,A4,A2)
               iinc = iinc + 3
               IF ( iinc>j-3 ) EXIT SPAG_Loop_2_7
            ENDDO SPAG_Loop_2_7
            EXIT SPAG_Loop_1_6
         ENDDO SPAG_Loop_1_6
         spag_nextblock_1 = 6
      CASE (6)
!
!     WRITE BOUNDARY SET DATA ON TO FILE SCR1, ONE LOGICAL RECORD FOR EA
!     SET ID.
!
         CALL open(*220,scr1,Z(buf2),1)
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
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  ii = i - 1
                  DO j = 1 , nsid
                     IF ( isid(j)==Z(score+ii) ) THEN
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDDO
                  CYCLE
               CASE (2)
                  Z(score+ii) = 0
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         ibad = 0
         DO i = 1 , nrsid
            ii = i - 1
            IF ( Z(score+ii)/=0 ) THEN
               index = (i-1)*3
               WRITE (Outt,99015) Ufm , Z(ks2+index+2) , Z(ks2+index) , Z(ks2+index+1)
99015          FORMAT (A23,' 6608, THE REQUEST FOR BOUNDARY SET ',I8,' SUBSTRUCTURE ',2A4,' WAS NOT DEFINED.')
               ibad = 1
            ENDIF
         ENDDO
         IF ( ibad==1 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     VI. PROCESS THE EQSS FROM THE SOF FOR EACH COMPONENT SUBSTRUCTURE.
!     ******************************************************************
!
         CALL open(*220,scr1,Z(buf3),0)
         CALL open(*220,scr2,Z(buf2),1)
         CALL sfetch(namold,nheqss,1,itest)
         ngrp = 1
         CALL sjump(ngrp)
!
!     READ AND PROCESS EQSS
!
         bad = .FALSE.
         DO i = 1 , ncsub
            spag_nextblock_4 = 1
            SPAG_DispatchLoop_4: DO
               SELECT CASE (spag_nextblock_4)
               CASE (1)
                  ii = 2*(i-1)
                  CALL suread(Z(score),-1,nout,itest)
                  IF ( andf(rshift(prtopt,3),1)==1 ) CALL cmiwrt(1,namold,Z(ks1+ii),score,nout,Z,Z)
!
!     FIND A BOUNDARY SET FOR THE COMPONENT
!
                  inxt = 1
                  fset = .FALSE.
                  spag_nextblock_4 = 2
               CASE (2)
                  DO j = inxt , nwbs , 3
                     jj = j - 1
                     IF ( Z(ks2+jj)==Z(ks1+ii) .AND. Z(ks2+jj+1)==Z(ks1+ii+1) ) THEN
                        spag_nextblock_4 = 3
                        CYCLE SPAG_DispatchLoop_4
                     ENDIF
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
99016                FORMAT (A29,' 6609, NO BOUNDARY SET HAS BEEN SPECIFIED FOR ','COMPONENT ',2A4,' OF PSEUDOSTRUCTURE ',2A4,/35X, &
                            &'ALL DEGREES OF FREEDOM WILL BE REDUCED.')
                     CALL write(scr2,aray(1),0,1)
                  ENDIF
                  CYCLE
               CASE (3)
!
!     COMPONENT HAS A BOUNDARY SET, CALL EQSCOD TO ACCOUNT FOR POSSIBLE
!     MULTIPLE IP NUMBERS.
!
                  IF ( .NOT.(fset) ) THEN
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
                  SPAG_Loop_2_8: DO j = 1 , nsid
                     IF ( nset==isid(j) ) EXIT SPAG_Loop_2_8
                  ENDDO SPAG_Loop_2_8
                  nrec = j - 1
                  IF ( nrec/=0 ) THEN
                     DO jj = 1 , nrec
                        CALL fwdrec(*240,scr1)
                     ENDDO
                  ENDIF
                  spag_nextblock_4 = 4
               CASE (4)
!
!     READ BOUNDARY DATA AND UPDATE CB
!
                  CALL read(*240,*102,scr1,aray,2,0,nnn)
!
!     LOCATE GRID ID IN EQSS AND SETS OF VALUES IF THE GRID IS MULTIPLY
!
                  IF ( nout/=0 ) THEN
                     CALL gridip(aray(1),score,nout,ipset,cset,no,Z,loc)
                     IF ( Iiierr/=1 ) THEN
                        spag_nextblock_4 = 5
                        CYCLE SPAG_DispatchLoop_4
                     ENDIF
                  ENDIF
                  bad = .TRUE.
                  WRITE (Outt,99017) Ufm , aray(1) , nset , Z(ks1+ii) , Z(ks1+ii+1)
99017             FORMAT (A23,' 6611, GRID POINT',I9,' SPECIFIED IN BOUNDARY SET',I9,' FOR SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
                  spag_nextblock_4 = 5
               CASE (5)
                  iadd = loc
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
99018                FORMAT (A25,' 6610, DEGREES OF FREEDOM AT GRID POINT',I9,' COMPONENT SUBSTRUCTURE ',2A4,/31X,'INCLUDED IN A ', &
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
                  spag_nextblock_4 = 4
                  CYCLE SPAG_DispatchLoop_4
!
!     BOUNDARY SET COMPLETE, IS THERE ANOTHER
!
 102              CALL rewind(scr1)
                  spag_nextblock_4 = 2
                  CYCLE SPAG_DispatchLoop_4
               END SELECT
            ENDDO SPAG_DispatchLoop_4
         ENDDO
         CALL close(scr1,1)
         CALL close(scr2,1)
         IF ( bad ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     VII. PROCESS MASTER SIL LIST AND ALLOCATE SPACE FOR CNEW
!     ********************************************************
!
         j = 0
         SPAG_Loop_1_9: DO
            CALL suread(Z(score+j),2,nout,itest)
            IF ( itest==3 ) THEN
               nw = j - 3
               DO i = 1 , nw , 3
                  jj = i - 1
                  Z(score+jj+2) = 0
               ENDDO
               CALL open(*220,scr2,Z(buf2),0)
               EXIT SPAG_Loop_1_9
            ELSE
               j = j + 3
            ENDIF
         ENDDO SPAG_Loop_1_9
 120     DO
            CALL read(*140,*120,scr2,aray,2,0,nnn)
            iloc = 3*aray(1) - 3
!
!     READ NEXT COMPONENT
!
            Z(score+iloc+2) = orf(Z(score+iloc+2),aray(2))
         ENDDO
!
!     PROCESSING COMPLETE
!
 140     CALL close(scr2,1)
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
               rz(score+jjj+j-1) = 0.0
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
                  spag_nextblock_5 = 1
                  SPAG_DispatchLoop_5: DO
                     SELECT CASE (spag_nextblock_5)
                     CASE (1)
                        DO jj = 1 , Nrow
                           IF ( listn(j)==listo(jj) ) THEN
                              spag_nextblock_5 = 2
                              CYCLE SPAG_DispatchLoop_5
                           ENDIF
                        ENDDO
                        CYCLE
                     CASE (2)
                        ijk(j) = jj
                        EXIT SPAG_DispatchLoop_5
                     END SELECT
                  ENDDO SPAG_DispatchLoop_5
               ENDDO
               DO j = 1 , nnew
                  ik = ijk(j)
                  rz(score+jjj+ik-1) = 1.0
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
         CALL pack(rz(score),pvx,mcb)
         CALL wrttrl(mcb)
         CALL close(pvx,1)
         IF ( lonly ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS USX USET EQUIVALENT
!
         CALL open(*220,usx,Z(buf2),1)
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
            IF ( rz(score+jj)/=0.0 ) THEN
               IF ( rz(score+jj)==1.0 ) Z(score+jj) = ibf + iba
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
         CALL open(*220,scr1,Z(buf1),1)
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
         IF ( Dry==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     WRITE FIRST GROUP OF EQSS
!
         CALL open(*220,scr1,Z(buf1),0)
         CALL setlvl(namnew,1,namold,itest,28)
         IF ( itest==8 ) THEN
!
            WRITE (Outt,99019) Ufm
99019       FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ','USED IN A PREVIOUS COMBINE OR REDUCE.')
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
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
         spag_nextblock_1 = 7
      CASE (7)
         CALL read(*180,*160,scr1,Z(score),nz,0,nnn)
         imsg = -8
         CALL mesage(imsg,ifile,modnam)
         RETURN
 160     CALL suwrt(Z(score),nnn,2)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 180     itest = 3
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
            IF ( itest==3 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 200     SPAG_Loop_1_10: DO
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
               EXIT SPAG_Loop_1_10
            ELSEIF ( Z(score+j)/=0 ) THEN
               kid = Z(score+j)
               CALL bisloc(*200,kid,Z(buf3),1,ncsred,jp)
               j = j + 14
            ENDIF
         ENDDO SPAG_Loop_1_10
         spag_nextblock_1 = 8
      CASE (8)
!
!     PROCESS LODS ITEM
!
         CALL sfetch(namold,litm,1,itest)
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
         IF ( lonly ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     X. GENERATE THE INX OUTPUT DATA BLOCK
!     *************************************
!
         CALL gopen(inx,Z(buf2),1)
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
         spag_nextblock_1 = 10
      CASE (10)
         CALL sofcls
         RETURN
      CASE (11)
!
         WRITE (Outt,99020) Ufm
99020    FORMAT (A23,' 6536, MODULE REDUCE TERMINATING DUE TO ABOVE ','ERRORS IN BULK DATA.')
         CALL close(geom4,1)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
!
         WRITE (Outt,99021) Ufm
99021    FORMAT (A23,' 6537, MODULE REDUCE TERMINATING DUE TO ABOVE ','ERRORS.')
         spag_nextblock_1 = 13
      CASE (13)
         Dry = -2
         CALL sofcls
         RETURN
 220     imsg = -1
         CALL mesage(imsg,ifile,modnam)
         RETURN
 240     imsg = -2
         CALL mesage(imsg,ifile,modnam)
         RETURN
 260     imsg = -3
         CALL mesage(imsg,ifile,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE reduce
