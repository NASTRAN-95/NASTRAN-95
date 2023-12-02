!*==dpd1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd1
!
!     DPD1 GENERATES THE GRID POINT LIST-DYNAMICS (GPLD),
!     USET-DYNAMICS (USETD), AND THE SCALAR INDEX LIST-DYNAMICS(SILD).
!
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_DPDCOM
   USE C_NAMES
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
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
         Buf(1) = Dpool
         CALL rdtrl(Buf)
         IF ( Buf(1)/=Dpool ) nodyn = .TRUE.
!
!     COMPUTE MAXIMUM EPOINT SIZE ALLOWED BY A COMPUTER WORD
!
         first = .TRUE.
         IF ( .NOT.(nodyn) ) THEN
            imax = 100000000
            IF ( Nbpw==32 ) imax = 2147493
            IF ( Nbpw==36 ) imax = 34359738
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
         file = Gpl
         IF ( Luset==0 ) GOTO 40
         CALL open(*40,Gpl,Z(Buf1),Rdrew)
         CALL read(*160,*140,Gpl,Z(1),3,1,falg)
         CALL fwdrec(*160,Gpl)
         i = 3
         mult = Z(i)
         imax = (imax/mult)*1000
         maxz = imax
         igpl = 1
         j = 1
         i = igpl
         DO
            CALL read(*160,*20,Gpl,Z(i),2,0,flag)
            Z(i+2) = j
            i = i + 3
            j = j + 1
         ENDDO
 20      ngpl = i - 3
         CALL close(Gpl,Clsrew)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     INITIALIZE FOR CASE WHERE NO GRID OR SCALAR PTS EXIST.
!
 40      i = 1
         igpl = 1
         Luset = 0
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
         file = Dpool
         CALL preloc(*140,Z(Buf1),Dpool)
         iep = i
         noep = 0
         CALL locate(*100,Z(Buf1),Epoint,flag)
         noep = 1
         DO
            CALL read(*160,*60,Dpool,Z(i),1,0,flag)
            IF ( Z(i)>maxz ) maxz = Z(i)
            Z(i+1) = mult*Z(i)
            Z(i+2) = 0
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
                  IF ( mod(Z(i+1),j)/=0 ) GOTO 70
                  Z(i+1) = Z(i+1)/j
               ENDDO
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 70         WRITE (Iout,99001) Ufm
99001       FORMAT (A23,' 2140C, ONE OR MORE EPOINTS WITH  EXTERNAL ID TOO ','LARGE.')
            IF ( j/=0 ) WRITE (Iout,99002)
99002       FORMAT (/5X,'SUGGESTION - RE-RUN NASTRAN JOB WITH ALL THE EPOINT',                                                      &
                   &' EXTERNAL ID''S SMALLER THAN THE LARGEST GRID POINT ID',/5X,                                                   &
                   &'OR, REDUCE THE SEQID LEVEL IF SEQGP CARDS WERE USED','.  I.E. FROM XXX.X.X TO XXX.X OR XXX')
            CALL close(Dpool,Clsrew)
            CALL mesage(-37,0,Nam)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     IF EXTRA POINTS PRESENT, READ SEQEP DATA (IF ANY).
!     REPLACE OLD SEQ NO WITH NEW SEQ NO.
!
         CALL locate(*100,Z(Buf1),Seqep,flag)
         n1 = i
         n2 = n1 + 1
         ifail = 0
         DO
            CALL read(*160,*80,Dpool,Z(n2),Buf1-1,1,flag)
            ifail = ifail + 1
         ENDDO
 80      IF ( ifail/=0 ) THEN
            nwds = (ifail-1)*(Buf1-1) + flag
            WRITE (Iout,99003) Ufm , nwds
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
               IF ( Z(i)>=0 .AND. i<kk ) THEN
                  ii = i + 2
                  ifail = 0
                  DO j = ii , kk , 2
                     IF ( Z(i)==Z(j) ) THEN
                        IF ( ifail==0 ) THEN
                           ifail = 1
                           Nogo = 1
                           IF ( k/=n2 ) THEN
                              idseq1 = Z(i)/1000
                              irmndr = Z(i) - 1000*idseq1
                              IF ( irmndr/=0 .AND. mult>=10 ) THEN
                                 idseq2 = irmndr/100
                                 irmndr = irmndr - 100*idseq2
                                 IF ( irmndr/=0 .AND. mult>=100 ) THEN
                                    idseq3 = irmndr/10
                                    irmndr = irmndr - 10*idseq3
                                    IF ( irmndr/=0 ) THEN
                                       WRITE (Iout,99004) Ufm , idseq1 , idseq2 , idseq3 , irmndr
99004                                  FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,1H.,I1,         &
                                         &'  ON SEQEP CARDS.')
                                    ELSE
                                       WRITE (Iout,99005) Ufm , idseq1 , idseq2 , idseq3
99005                                  FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,4X,             &
                                         &'ON SEQEP CARDS.')
                                    ENDIF
                                 ELSE
                                    WRITE (Iout,99006) Ufm , idseq1 , idseq2
99006                               FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,6X,'ON SEQEP CARDS.')
                                 ENDIF
                              ELSE
                                 WRITE (Iout,99007) Ufm , idseq1
99007                            FORMAT (A23,' 3141, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,6X,'ON SEQEP CARDS.')
                              ENDIF
                           ELSE
                              WRITE (Iout,99008) Ufm , Z(i)
99008                         FORMAT (A23,' 3140, MULTIPLE REFERENCES TO EXTRA POINT ID NO.',I9,' ON SEQEP CARDS.')
                           ENDIF
                        ENDIF
                        Z(j) = -Z(j)
                     ENDIF
                  ENDDO
               ENDIF
!
               IF ( jj>=kk .AND. mult/=1 .AND. mult/=1000 ) THEN
                  L = Z(i)
                  IF ( mult==10 ) THEN
                     IF ( mod(L,100)==0 ) THEN
                        Z(i) = L/100
                        CYCLE
                     ENDIF
                  ELSEIF ( mod(L,10)==0 ) THEN
                     Z(i) = L/10
                     CYCLE
                  ENDIF
                  IF ( first ) THEN
                     first = .FALSE.
                     Nogo = 1
                     WRITE (Iout,99009) Ufm
99009                FORMAT (A23,' 2140B, ILLEGAL DATA IN SEQEP CARD, POSSIBLY CAUSED',' BY LARGE GRID OR SCALAR POINTS')
                  ENDIF
               ENDIF
            ENDDO
!
            IF ( k/=n2 ) THEN
!
               DO i = n2 , kk , 2
                  IF ( Z(i)<0 ) Z(i) = -Z(i)
               ENDDO
               IF ( Nogo/=1 ) THEN
!
!     CHECK TO SEE IF ANY SEQUENCE ID NO. ON SEQEP CARDS IS THE SAME
!     AS AN EXTRA POINT ID NO. THAT HAS NOT BEEN RESEQUENCED
!
                  SPAG_Loop_2_1: DO i = k , kk , 2
                     IF ( Z(i)>=0 ) THEN
                        idseq1 = Z(i)/mult
                        irmndr = Z(i) - mult*idseq1
                        IF ( irmndr==0 ) THEN
                           DO j = n2 , kk , 2
                              IF ( idseq1==Z(j) ) CYCLE SPAG_Loop_2_1
                           ENDDO
                           DO j = 1 , n1 , 3
                              IF ( idseq1==Z(j) ) GOTO 82
                           ENDDO
                        ENDIF
                        CYCLE
 82                     Nogo = 1
                        WRITE (Iout,99010) Ufm , idseq1
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
         Buf(1) = Z(n2+i-1)
         Buf(2) = Z(n2+i)
         DO j = iep , nep , 3
            IF ( Z(j)==Buf(1) ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         Buf(2) = 0
         CALL mesage(30,64,Buf)
         Nogo = 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         IF ( Z(j+2)/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Z(j+1) = Buf(2)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     CALL close(Dpool,Clsrew)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( Luset+noep==0 ) THEN
            n = -30
            file = 109
            CALL mesage(n,file,Nam)
            RETURN
         ELSE
!
!     IF EXTRA POINTS PRESENT, SORT THE GPL ON SEQ NO.
!     REPLACE SEQ NO WITH INTERNAL GRID NO FOR DYNAMICS.
!
            n = ngpl + 2
            IF ( noep/=0 ) CALL sort(0,0,3,2,Z,n)
            i = 2
            Z(i) = 1
            IF ( ngpl/=1 ) THEN
               DO i = 4 , ngpl , 3
                  Z(i+1) = Z(i-2) + 1
               ENDDO
            ENDIF
!
!     WRITE THE GPLD.
!
            file = Gpld
            CALL open(*140,Gpld,Z(Buf1),Wrtrew)
            CALL fname(Gpld,Buf)
            CALL write(Gpld,Buf,2,1)
            DO i = igpl , ngpl , 3
               CALL write(Gpld,Z(i),1,0)
            ENDDO
            CALL write(Gpld,0,0,1)
            CALL close(Gpld,Clsrew)
            Mcb(1) = Gpld
            Mcb(2) = n/3
            CALL wrttrl(Mcb)
            Kn = Mcb(2)
!
!     OPEN SILD AND USETD. WRITE HEADER RECORDS.
!     OPEN SIL  AND USET.  SKIP  HEADER RECORD.
!     READ SIL INTO CORE.
!
            file = Sild
            CALL open(*140,Sild,Z(Buf1),Wrtrew)
            CALL fname(Sild,Buf)
            CALL write(Sild,Buf,2,1)
            IF ( Luset==0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            file = Sil
            CALL open(*140,Sil,Z(Buf2),Rdrew)
            CALL fwdrec(*160,Sil)
            isil = ngpl + 3
            CALL read(*160,*120,Sil,Z(isil),Buf3-isil,1,n)
            CALL mesage(-8,0,Nam)
         ENDIF
 120     CALL close(Sil,Clsrew)
         nsil = isil + n
         Z(nsil) = Luset + 1
         spag_nextblock_1 = 8
      CASE (8)
         file = Usetd
         CALL open(*140,Usetd,Z(Buf3),Wrtrew)
         CALL fname(Usetd,Buf)
         CALL write(Usetd,Buf,2,1)
         IF ( Luset/=0 ) THEN
            file = Uset
            CALL open(*140,Uset,Z(Buf2),Rdrew)
            CALL fwdrec(*160,Uset)
         ENDIF
!
!     INITIALIZE DISPLACEMENT SET BIT MASKS.
!
         i = igpl
         j = isil - 1
         Nbrep = 0
         Buf(10) = 1
         DO k = 2 , 7
            Mcb(k) = 0
         ENDDO
         mskua = Two(Ua)
         mskun = Two(Un)
         mskuf = Two(Uf)
         mskue = Two(Ue)
         mskup = Two(Up)
         mskud = Two(Ud)
         mskune = Two(Une)
         mskufe = Two(Ufe)
         musetd = orf(mskue,orf(mskune,orf(mskufe,orf(mskud,mskup))))
         DO
!
!     TEST FOR CURRENT POINT IN G-SET OR IN P-SET (EXTRA POINT).
!
            IF ( Z(i+2)==0 ) THEN
!
!     POINT IS AN EXTRA POINT - WRITE MASK ON USETD.
!
               CALL write(Usetd,musetd,1,0)
               Mcb(5) = orf(Mcb(5),musetd)
               m = 1
            ELSE
!
!     POINT IS IN G-SET - READ USET MASKS BELONGING TO POINT.
!     TURN ON APPROPRIATE BITS FOR P-SET. WRITE MASKS ON USETD.
!
               j = j + 1
               m = Z(j+1) - Z(j)
               CALL read(*160,*180,Uset,Buf,m,0,flag)
               DO k = 1 , m
                  ksw = orf(Buf(k),mskup)
                  IF ( andf(ksw,mskua)/=0 ) ksw = orf(ksw,mskud)
                  IF ( andf(ksw,mskun)/=0 ) ksw = orf(ksw,mskune)
                  IF ( andf(ksw,mskuf)/=0 ) ksw = orf(ksw,mskufe)
                  Mcb(5) = orf(Mcb(5),ksw)
                  Buf(k) = ksw
               ENDDO
               CALL write(Usetd,Buf,m,0)
            ENDIF
!
!     REPLACE INTERNAL DYNAMICS NO. WITH SILD NO. WRITE SILD ENTRY.
!     REPLACE INTERNAL STATICS NO. WITH SIL NO.
!
            Z(i+1) = Buf(10)
            CALL write(Sild,Z(i+1),1,0)
            IF ( Z(i+2)==0 ) THEN
               Nbrep = Nbrep + 1
            ELSE
               Z(i+2) = Z(j)
            ENDIF
!
!     TEST FOR COMPLETION.
!
            Buf(10) = Buf(10) + m
            i = i + 3
            IF ( i>ngpl ) THEN
!
!     WRITE SECOND RECORD OF SILD (PAIRS OF SIL NO., SILD NO.)
!
               CALL write(Sild,0,0,1)
               CALL write(Usetd,0,0,1)
               DO i = igpl , ngpl , 3
                  IF ( Z(i+2)/=0 ) THEN
                     Buf(1) = Z(i+2)
                     Buf(2) = Z(i+1)
                     CALL write(Sild,Buf,2,0)
                  ENDIF
               ENDDO
!
!     CLOSE FILES AND WRITE TRAILERS.
!
               CALL close(Sild,Clsrew)
               CALL close(Usetd,Clsrew)
               Mcb(1) = Sild
               Lusetd = Luset + Nbrep
               Mcb(2) = Lusetd
               Mcb(3) = Nbrep
               CALL wrttrl(Mcb)
               Mcb(1) = Usetd
               CALL wrttrl(Mcb)
               Mcb(5) = 0
               CALL close(Uset,Clsrew)
!
!     REPLACE SIL NO. IN TABLE WITH CODED SILD NO.
!     THEN SORT TABLE ON EXTERNAL GRID NO.
!
               Z(ngpl+4) = Lusetd + 1
               DO i = igpl , ngpl , 3
                  j = 1
                  IF ( Z(i+4)-Z(i+1)==1 ) THEN
                     j = 2
                     IF ( Z(i+2)==0 ) j = 3
                  ENDIF
                  Z(i+2) = 10*Z(i+1) + j
               ENDDO
               CALL sort(0,0,3,1,Z(igpl),ngpl-igpl+3)
!
!     WRITE EQDYN DATA BLOCK. FIRST RECORD IS PAIRS OF EXTERNAL GRID NO,
!     SILD NO. SECOND RECORD IS PAIRS OF EXTERNAL GRID NO., CODED SILD
!     NO.
!
               file = Eqdyn
               CALL open(*140,Eqdyn,Z(Buf1),Wrtrew)
               CALL fname(Eqdyn,Buf)
               CALL write(Eqdyn,Buf,2,1)
               DO i = igpl , ngpl , 3
                  CALL write(Eqdyn,Z(i),2,0)
               ENDDO
               CALL write(Eqdyn,0,0,1)
               DO i = igpl , ngpl , 3
                  Buf(1) = Z(i)
                  Buf(2) = Z(i+2)
                  CALL write(Eqdyn,Buf,2,0)
               ENDDO
               CALL write(Eqdyn,0,0,1)
               CALL close(Eqdyn,Clsrew)
               Mcb(1) = Eqdyn
               Mcb(2) = Kn
               CALL wrttrl(Mcb)
               Neqdyn = 2*Kn - 1
               IF ( Nbrep==0 ) Nbrep = -1
               RETURN
            ENDIF
         ENDDO
!
!     FATAL FILE ERRORS
!
 140     n = -1
         CALL mesage(n,file,Nam)
         RETURN
 160     n = -2
         CALL mesage(n,file,Nam)
         RETURN
 180     n = -3
         CALL mesage(n,file,Nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd1
