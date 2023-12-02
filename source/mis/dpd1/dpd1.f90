!*==dpd1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd1
!
!     DPD1 GENERATES THE GRID POINT LIST-DYNAMICS (GPLD),
!     USET-DYNAMICS (USETD), AND THE SCALAR INDEX LIST-DYNAMICS(SILD).
!
   USE c_bitpos
   USE c_blank
   USE c_dpdcom
   USE c_names
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: falg , file , flag , i , idseq1 , idseq2 , idseq3 , iep , ifail , igpl , ii , imax , irmndr , isil , j , jj , k , kk ,&
            & ksw , m , maxz , mskua , mskud , mskue , mskuf , mskufe , mskun , mskune , mskup , mult , musetd , n , n1 , n2 , nep ,&
            & ngpl , noep , nsil , nwds
   LOGICAL :: first , nodyn
   EXTERNAL andf , close , fname , fwdrec , locate , mesage , open , orf , preloc , rdtrl , read , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
   !>>>>EQUIVALENCE (Msg(2),Ngrid)
!
!
!     SET NODYN FLAG TO TRUE IF NO DYNAMIC
!
         nodyn = .FALSE.
         buf(1) = dpool
         CALL rdtrl(buf)
         IF ( buf(1)/=dpool ) nodyn = .TRUE.
!
!     COMPUTE MAXIMUM EPOINT SIZE ALLOWED BY A COMPUTER WORD
!
         first = .TRUE.
         IF ( .NOT.(nodyn) ) THEN
            imax = 100000000
            IF ( nbpw==32 ) imax = 2147493
            IF ( nbpw==36 ) imax = 34359738
!         2147493=2**31/1000   34359738=2**35/1000
            maxz = imax
            mult = 1000
         ENDIF
!
!     READ SECOND RECORD OF THE GPL INTO CORE. CREATE TABLE OF TRIPLES -
!     EXTERNAL GRID NO., SEQ. NO., AND INTERNAL GRID NO.
!
!     SEQ.NO.= EXTERNAL GIRD NO. * MULT, OR RESEQUENCED GRID PT. NO.
!     (A MULTIFICATION FACTOR WAS SAVED IN GPL HEADER RECORD BY GP1,
!      MULT = 10,100,OR 1000,  AND BY SGEN, MULT = 1000)
!
         file = gpl
         IF ( luset==0 ) GOTO 40
         CALL open(*40,gpl,z(buf1),rdrew)
         CALL read(*160,*140,gpl,z(1),3,1,falg)
         CALL fwdrec(*160,gpl)
         i = 3
         mult = z(i)
         imax = (imax/mult)*1000
         maxz = imax
         igpl = 1
         j = 1
         i = igpl
         DO
            CALL read(*160,*20,gpl,z(i),2,0,flag)
            z(i+2) = j
            i = i + 3
            j = j + 1
         ENDDO
 20      ngpl = i - 3
         CALL close(gpl,clsrew)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     INITIALIZE FOR CASE WHERE NO GRID OR SCALAR PTS EXIST.
!
 40      i = 1
         igpl = 1
         luset = 0
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ EXTRA POINTS (IF ANY). ADD TO TABLE IN CORE.
!     SET INTERNAL GRID NO. OF EXTRA PTS = 0.
!
         IF ( nodyn ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = dpool
         CALL preloc(*140,z(buf1),dpool)
         iep = i
         noep = 0
         CALL locate(*100,z(buf1),epoint,flag)
         noep = 1
         DO
            CALL read(*160,*60,dpool,z(i),1,0,flag)
            IF ( z(i)>maxz ) maxz = z(i)
            z(i+1) = mult*z(i)
            z(i+2) = 0
            i = i + 3
         ENDDO
 60      nep = i - 3
         ngpl = nep
!
!     ONE OR MORE EPOINT WITH VERY LARGE EXTERNAL ID
!     FATAL IF MULTIPLIER IS 10
!     IF MULT IS 1000 OR 100, TRY TO SHRINK THE GRID POINT SEQ. NO. BY
!     10 OR 100 IF POSSIBLE, AND RESET MULT.
!     IF IT IS NOT POSSIBLE, WE HAVE A FATAL CONDITION 2140C
!
         IF ( maxz/=imax ) THEN
            j = 0
            IF ( mult/=10 ) THEN
               mult = 100
               IF ( maxz>10*imax ) mult = 10
               imax = (imax/mult)*1000
               j = 1000/mult
               DO i = igpl , nep , 3
                  IF ( mod(z(i+1),j)/=0 ) GOTO 70
                  z(i+1) = z(i+1)/j
               ENDDO
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 70         WRITE (iout,99001) ufm
99001       FORMAT (A23,' 2140C, ONE OR MORE EPOINTS WITH  EXTERNAL ID TOO ','LARGE.')
            IF ( j/=0 ) WRITE (iout,99002)
99002       FORMAT (/5X,'SUGGESTION - RE-RUN NASTRAN JOB WITH ALL THE EPOINT',                                                      &
                   &' EXTERNAL ID''S SMALLER THAN THE LARGEST GRID POINT ID',/5X,                                                   &
                   &'OR, REDUCE THE SEQID LEVEL IF SEQGP CARDS WERE USED','.  I.E. FROM XXX.X.X TO XXX.X OR XXX')
            CALL close(dpool,clsrew)
            CALL mesage(-37,0,nam)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     IF EXTRA POINTS PRESENT, READ SEQEP DATA (IF ANY).
!     REPLACE OLD SEQ NO WITH NEW SEQ NO.
!
         CALL locate(*100,z(buf1),seqep,flag)
         n1 = i
         n2 = n1 + 1
         ifail = 0
         DO
            CALL read(*160,*80,dpool,z(n2),buf1-1,1,flag)
            ifail = ifail + 1
         ENDDO
 80      IF ( ifail/=0 ) THEN
            nwds = (ifail-1)*(buf1-1) + flag
            WRITE (iout,99003) ufm , nwds
99003       FORMAT (A23,' 3139, UNABLE TO PROCESS SEQEP DATA IN SUBROUTINE ','DPD1 DUE TO INSUFFICIENT CORE.',//5X,                 &
                   &'ADDITIONAL CORE REQUIRED =',I10,7H  WORDS)
            CALL mesage(-61,0,0)
         ENDIF
!
!     CHECK FOR MULTIPLE REFERENCES TO EXTRA POINT ID NOS. AND
!     SEQUENCE ID NOS. ON SEQEP CARDS
!
         k = n2
         kk = n2 + flag - 1
         jj = kk - 2
         SPAG_Loop_1_2: DO
            DO i = k , jj , 2
               IF ( z(i)>=0 .AND. i<kk ) THEN
                  ii = i + 2
                  ifail = 0
                  DO j = ii , kk , 2
                     IF ( z(i)==z(j) ) THEN
                        IF ( ifail==0 ) THEN
                           ifail = 1
                           nogo = 1
                           IF ( k/=n2 ) THEN
                              idseq1 = z(i)/1000
                              irmndr = z(i) - 1000*idseq1
                              IF ( irmndr/=0 .AND. mult>=10 ) THEN
                                 idseq2 = irmndr/100
                                 irmndr = irmndr - 100*idseq2
                                 IF ( irmndr/=0 .AND. mult>=100 ) THEN
                                    idseq3 = irmndr/10
                                    irmndr = irmndr - 10*idseq3
                                    IF ( irmndr/=0 ) THEN
                                       WRITE (iout,99004) ufm , idseq1 , idseq2 , idseq3 , irmndr
99004                                  FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,1H.,I1,         &
                                         &'  ON SEQEP CARDS.')
                                    ELSE
                                       WRITE (iout,99005) ufm , idseq1 , idseq2 , idseq3
99005                                  FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,4X,             &
                                         &'ON SEQEP CARDS.')
                                    ENDIF
                                 ELSE
                                    WRITE (iout,99006) ufm , idseq1 , idseq2
99006                               FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,6X,'ON SEQEP CARDS.')
                                 ENDIF
                              ELSE
                                 WRITE (iout,99007) ufm , idseq1
99007                            FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,6X,'ON SEQEP CARDS.')
                              ENDIF
                           ELSE
                              WRITE (iout,99008) ufm , z(i)
99008                         FORMAT (A23,' 3140, MULTIPLE REFERENCES TO EXTRA POINT ID NO.',I9,' ON SEQEP CARDS.')
                           ENDIF
                        ENDIF
                        z(j) = -z(j)
                     ENDIF
                  ENDDO
               ENDIF
!
               IF ( jj>=kk .AND. mult/=1 .AND. mult/=1000 ) THEN
                  l = z(i)
                  IF ( mult==10 ) THEN
                     IF ( mod(l,100)==0 ) THEN
                        z(i) = l/100
                        CYCLE
                     ENDIF
                  ELSEIF ( mod(l,10)==0 ) THEN
                     z(i) = l/10
                     CYCLE
                  ENDIF
                  IF ( first ) THEN
                     first = .FALSE.
                     nogo = 1
                     WRITE (iout,99009) ufm
99009                FORMAT (A23,' 2140B, ILLEGAL DATA IN SEQEP CARD, POSSIBLY CAUSED',' BY LARGE GRID OR SCALAR POINTS')
                  ENDIF
               ENDIF
            ENDDO
!
            IF ( k/=n2 ) THEN
!
               DO i = n2 , kk , 2
                  IF ( z(i)<0 ) z(i) = -z(i)
               ENDDO
               IF ( nogo/=1 ) THEN
!
!     CHECK TO SEE IF ANY SEQUENCE ID NO. ON SEQEP CARDS IS THE SAME
!     AS AN EXTRA POINT ID NO. THAT HAS NOT BEEN RESEQUENCED
!
                  SPAG_Loop_2_1: DO i = k , kk , 2
                     IF ( z(i)>=0 ) THEN
                        idseq1 = z(i)/mult
                        irmndr = z(i) - mult*idseq1
                        IF ( irmndr==0 ) THEN
                           DO j = n2 , kk , 2
                              IF ( idseq1==z(j) ) CYCLE SPAG_Loop_2_1
                           ENDDO
                           DO j = 1 , n1 , 3
                              IF ( idseq1==z(j) ) GOTO 82
                           ENDDO
                        ENDIF
                        CYCLE
 82                     nogo = 1
                        WRITE (iout,99010) ufm , idseq1
99010                   FORMAT (A23,' 3142, SEQUENCE ID NO.',I6,'  ON SEQEP CARDS IS THE SAME AS AN ',/5X,                          &
                               &'EXTRA POINT ID NO. THAT HAS NOT BEEN RESEQUENCED.')
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
               i = -1
               EXIT SPAG_Loop_1_2
            ELSE
               jj = kk
               k = k + 1
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 4
      CASE (4)
         i = i + 2
         IF ( i>flag ) GOTO 100
         buf(1) = z(n2+i-1)
         buf(2) = z(n2+i)
         DO j = iep , nep , 3
            IF ( z(j)==buf(1) ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         buf(2) = 0
         CALL mesage(30,64,buf)
         nogo = 1
         spag_nextblock_1 = 4
      CASE (6)
         IF ( z(j+2)/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         z(j+1) = buf(2)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     CALL close(dpool,clsrew)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( luset+noep==0 ) THEN
            n = -30
            file = 109
            CALL mesage(n,file,nam)
            RETURN
         ELSE
!
!     IF EXTRA POINTS PRESENT, SORT THE GPL ON SEQ NO.
!     REPLACE SEQ NO WITH INTERNAL GRID NO FOR DYNAMICS.
!
            n = ngpl + 2
            IF ( noep/=0 ) CALL sort(0,0,3,2,z,n)
            i = 2
            z(i) = 1
            IF ( ngpl/=1 ) THEN
               DO i = 4 , ngpl , 3
                  z(i+1) = z(i-2) + 1
               ENDDO
            ENDIF
!
!     WRITE THE GPLD.
!
            file = gpld
            CALL open(*140,gpld,z(buf1),wrtrew)
            CALL fname(gpld,buf)
            CALL write(gpld,buf,2,1)
            DO i = igpl , ngpl , 3
               CALL write(gpld,z(i),1,0)
            ENDDO
            CALL write(gpld,0,0,1)
            CALL close(gpld,clsrew)
            mcb(1) = gpld
            mcb(2) = n/3
            CALL wrttrl(mcb)
            kn = mcb(2)
!
!     OPEN SILD AND USETD. WRITE HEADER RECORDS.
!     OPEN SIL  AND USET.  SKIP  HEADER RECORD.
!     READ SIL INTO CORE.
!
            file = sild
            CALL open(*140,sild,z(buf1),wrtrew)
            CALL fname(sild,buf)
            CALL write(sild,buf,2,1)
            IF ( luset==0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            file = sil
            CALL open(*140,sil,z(buf2),rdrew)
            CALL fwdrec(*160,sil)
            isil = ngpl + 3
            CALL read(*160,*120,sil,z(isil),buf3-isil,1,n)
            CALL mesage(-8,0,nam)
         ENDIF
 120     CALL close(sil,clsrew)
         nsil = isil + n
         z(nsil) = luset + 1
         spag_nextblock_1 = 8
      CASE (8)
         file = usetd
         CALL open(*140,usetd,z(buf3),wrtrew)
         CALL fname(usetd,buf)
         CALL write(usetd,buf,2,1)
         IF ( luset/=0 ) THEN
            file = uset
            CALL open(*140,uset,z(buf2),rdrew)
            CALL fwdrec(*160,uset)
         ENDIF
!
!     INITIALIZE DISPLACEMENT SET BIT MASKS.
!
         i = igpl
         j = isil - 1
         nbrep = 0
         buf(10) = 1
         DO k = 2 , 7
            mcb(k) = 0
         ENDDO
         mskua = two(ua)
         mskun = two(un)
         mskuf = two(uf)
         mskue = two(ue)
         mskup = two(up)
         mskud = two(ud)
         mskune = two(une)
         mskufe = two(ufe)
         musetd = orf(mskue,orf(mskune,orf(mskufe,orf(mskud,mskup))))
         DO
!
!     TEST FOR CURRENT POINT IN G-SET OR IN P-SET (EXTRA POINT).
!
            IF ( z(i+2)==0 ) THEN
!
!     POINT IS AN EXTRA POINT - WRITE MASK ON USETD.
!
               CALL write(usetd,musetd,1,0)
               mcb(5) = orf(mcb(5),musetd)
               m = 1
            ELSE
!
!     POINT IS IN G-SET - READ USET MASKS BELONGING TO POINT.
!     TURN ON APPROPRIATE BITS FOR P-SET. WRITE MASKS ON USETD.
!
               j = j + 1
               m = z(j+1) - z(j)
               CALL read(*160,*180,uset,buf,m,0,flag)
               DO k = 1 , m
                  ksw = orf(buf(k),mskup)
                  IF ( andf(ksw,mskua)/=0 ) ksw = orf(ksw,mskud)
                  IF ( andf(ksw,mskun)/=0 ) ksw = orf(ksw,mskune)
                  IF ( andf(ksw,mskuf)/=0 ) ksw = orf(ksw,mskufe)
                  mcb(5) = orf(mcb(5),ksw)
                  buf(k) = ksw
               ENDDO
               CALL write(usetd,buf,m,0)
            ENDIF
!
!     REPLACE INTERNAL DYNAMICS NO. WITH SILD NO. WRITE SILD ENTRY.
!     REPLACE INTERNAL STATICS NO. WITH SIL NO.
!
            z(i+1) = buf(10)
            CALL write(sild,z(i+1),1,0)
            IF ( z(i+2)==0 ) THEN
               nbrep = nbrep + 1
            ELSE
               z(i+2) = z(j)
            ENDIF
!
!     TEST FOR COMPLETION.
!
            buf(10) = buf(10) + m
            i = i + 3
            IF ( i>ngpl ) THEN
!
!     WRITE SECOND RECORD OF SILD (PAIRS OF SIL NO., SILD NO.)
!
               CALL write(sild,0,0,1)
               CALL write(usetd,0,0,1)
               DO i = igpl , ngpl , 3
                  IF ( z(i+2)/=0 ) THEN
                     buf(1) = z(i+2)
                     buf(2) = z(i+1)
                     CALL write(sild,buf,2,0)
                  ENDIF
               ENDDO
!
!     CLOSE FILES AND WRITE TRAILERS.
!
               CALL close(sild,clsrew)
               CALL close(usetd,clsrew)
               mcb(1) = sild
               lusetd = luset + nbrep
               mcb(2) = lusetd
               mcb(3) = nbrep
               CALL wrttrl(mcb)
               mcb(1) = usetd
               CALL wrttrl(mcb)
               mcb(5) = 0
               CALL close(uset,clsrew)
!
!     REPLACE SIL NO. IN TABLE WITH CODED SILD NO.
!     THEN SORT TABLE ON EXTERNAL GRID NO.
!
               z(ngpl+4) = lusetd + 1
               DO i = igpl , ngpl , 3
                  j = 1
                  IF ( z(i+4)-z(i+1)==1 ) THEN
                     j = 2
                     IF ( z(i+2)==0 ) j = 3
                  ENDIF
                  z(i+2) = 10*z(i+1) + j
               ENDDO
               CALL sort(0,0,3,1,z(igpl),ngpl-igpl+3)
!
!     WRITE EQDYN DATA BLOCK. FIRST RECORD IS PAIRS OF EXTERNAL GRID NO,
!     SILD NO. SECOND RECORD IS PAIRS OF EXTERNAL GRID NO., CODED SILD
!     NO.
!
               file = eqdyn
               CALL open(*140,eqdyn,z(buf1),wrtrew)
               CALL fname(eqdyn,buf)
               CALL write(eqdyn,buf,2,1)
               DO i = igpl , ngpl , 3
                  CALL write(eqdyn,z(i),2,0)
               ENDDO
               CALL write(eqdyn,0,0,1)
               DO i = igpl , ngpl , 3
                  buf(1) = z(i)
                  buf(2) = z(i+2)
                  CALL write(eqdyn,buf,2,0)
               ENDDO
               CALL write(eqdyn,0,0,1)
               CALL close(eqdyn,clsrew)
               mcb(1) = eqdyn
               mcb(2) = kn
               CALL wrttrl(mcb)
               neqdyn = 2*kn - 1
               IF ( nbrep==0 ) nbrep = -1
               RETURN
            ENDIF
         ENDDO
!
!     FATAL FILE ERRORS
!
 140     n = -1
         CALL mesage(n,file,nam)
         RETURN
 160     n = -2
         CALL mesage(n,file,nam)
         RETURN
 180     n = -3
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd1
