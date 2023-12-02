!*==tabprt.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tabprt(Iname1)
USE C_BLANK
USE C_MACHIN
USE C_OUTPUT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iname1
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: alphc , alphc1 , blank , cont , cparen , d , ebb , ec , ec1 , ec2 , oparen , tabl , uned
   INTEGER , SAVE :: bcd , fpn , int , intgc , nsp , pure
   INTEGER , DIMENSION(20) :: block
   CHARACTER(1) , DIMENSION(2000) :: core1
   REAL(REAL64) , DIMENSION(1) :: dcore , xnd
   LOGICAL :: dec
   INTEGER :: flag , format , i , iflag , iform , iname , inum , iout , ircd , irec , ix , ixxx , j , jj , jpoint , jv , llen , n , &
            & np , npoint , nred , ns , nterms , nz , recf , row , strnbr , type
   INTEGER , DIMENSION(2) , SAVE :: forms , name
   INTEGER , DIMENSION(133) :: icore
   REAL , DIMENSION(3) , SAVE :: sp
   INTEGER , DIMENSION(4) , SAVE :: types
   REAL , DIMENSION(1) :: xns
   EXTERNAL close , endget , fname , getstr , korsz , mesage , numtyp , open , page , page2 , rdtrl , read , rectyp , wrtfmt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     WILL PRINT TABLE - USING 1P,E13.6, I13, OR (9X,A4) FORMAT
!
!     ALL REAL NUMBERS ARE ASSUMED TO BE SINGLE PRECISION.
!
!     REVISED  3/91 BY G.CHAN/UNISYS
!     THREE PARAMETERS ARE ADDED - OP CODE (OP), RECORDD NO. (IRC), AND
!     WORD NO. (IWD)
!     THE DEFAULTS OF THESE PARAMETERS ARE - BLANK, 3, AND 3
!     OP CODE OPTIONS ARE 'PUREBCD', 'PUREFPN', AND 'PUREINT'
!
!     LAST REVISED, 12/92, BY G.CHAN/UNISYS, TO INCLUDE 3 SPECIAL TABLES
!     - KELM, MELM, BELM  - WHICH CONTAIN D.P. DATA WORDS IN 32- AND 36-
!     BIT WORD MACHINES.
!
!     IF OP CODE IS 'PUREBCD', RECORDS IRC AND THEREAFTER, AND BEGINNING
!     FROM WORD IWD OF EACH RECORD TO THE END OF THAT RECORD, ARE ALL
!     BCD  WORDS.
!     SIMILARILY FOR 'PUREINT' FOR INTEGER WORDS, AND 'PUREFPN' FOR
!     FLOATING POINT NUMBERS
!
!     THESE PARAMETER OPTIONS ARE NECESSARY BECAUSE IF THE PRINTED DATA
!     IS NOT OF STRING TYPE, SUBROUTINE NUMTYP IS CALLED TO FIND OUT
!     WHAT TYPE OF DATA IN EACH DATA WORD.  HOWEVER NUMTYP IS NOT 100
!     PERCENT FOOL-PROOF. ONCE IN A FEW THOUSANDS NUMTYP CAN NOT
!     DISTINGUISH A REAL NUMBER FROM A BCD WORD
!
!  $MIXED_FORMATS
!
!WKBI
   !>>>>EQUIVALENCE (Xnd(1),Core(1))
   !>>>>EQUIVALENCE (Xns(1),Xnd(1)) , (Icore(1),Core(1),Dcore(1)) , (block(2),type) , (block(3),format) , (block(4),row) ,               &
!>>>>    & (block(5),jpoint) , (block(6),nterms) , (block(8),flag)
!WKBI
   !>>>>EQUIVALENCE (Core,Core1)
   DATA oparen , cparen , ec , ec1 , ec2 , intgc , alphc , alphc1 , cont , uned/4H(1X  , 4H)    , 4H,1P, , 4HE13. , 2H6  , 4H,I13 , &
       &4H,9X, , 4HA4   , 4HCONT , 4HINUE/d/2HD / , name/4HTABP , 4HRT  /
   DATA blank , tabl , ebb/1H  , 4HTABL , 1HE/
   DATA types/3HRSP , 3HRDP , 3HCSP , 3HCDP/ , forms/3HYES , 2HNO/
   DATA pure , bcd , fpn , int/4HPURE , 4HBCD  , 4HFPN  , 4HINT /
   DATA nsp , sp/3 , 4HKELM , 4HMELM , 4HBELM/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nz = korsz(Core) - Sysbuf
         IF ( nz<=0 ) CALL mesage(-8,-nz,name)
         dec = Mach==5 .OR. Mach==6 .OR. Mach==10 .OR. Mach==21
         iname = Iname1
         CALL open(*140,iname,Core(nz+1),0)
         DO i = 1 , 96
            Head2(i) = blank
         ENDDO
         Head2(1) = tabl
         Head2(2) = ebb
         CALL fname(iname,Head2(3))
         CALL page
         Head2(6) = cont
         Head2(7) = uned
         Head2(8) = d
         IF ( Iprc/=1 .AND. iname==101 ) THEN
            CALL page2(-2)
            WRITE (Otpe,99001) Uwm
99001       FORMAT (A25,', TABPRT MODULE ASSUMES ALL REAL DATA ARE IN S.P.,',' D.P. DATA THEREFORE MAY BE PRINTED ERRONEOUSLY')
         ENDIF
         inum = nz/2 - 1
         inum = max0(inum,133)
         ns = inum + 1
         llen = 0
         Core(1) = oparen
         irec = 0
         ircd = 999999999
         ixxx = 999999999
         IF ( Op(1)==pure .AND. Op(2)/=blank ) THEN
            IF ( Op(2)==int ) jj = 2
            IF ( Op(2)==fpn ) jj = 3
            IF ( Op(2)==bcd ) jj = 4
            IF ( Irc>0 ) ircd = Irc
            IF ( Iwd>0 ) ixxx = Iwd + inum
            IF ( Irc<=0 ) ircd = 3
            IF ( Iwd<=0 ) ixxx = 3 + inum
         ENDIF
 20      CALL page2(-2)
         IF ( dec .AND. irec==0 ) WRITE (Otpe,99002)
99002    FORMAT (4X,'(ALL INTEGERS EXCEEDING 16000 ARE PRINTED AS REAL ','NUMBERS. ALL REAL NUMBERS OUTSIDE E-27 OR E+27 RANGE ',   &
                &'ARE PRINTED AS INTEGERS)')
         WRITE (Otpe,99014) irec
         irec = irec + 1
         DO i = 1 , nsp
            IF ( Head2(3)==sp(i) ) THEN
               icore(1) = iname
               CALL rdtrl(icore)
               IF ( icore(2)==2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         ix = inum
         nred = 0
         np = inum - 1
         block(1) = Iname1
         CALL rectyp(block,recf)
         IF ( recf/=0 ) THEN
!
!
!     HERE IF STRING FORMATTED RECORD
!
            flag = -1
            strnbr = 1
            CALL getstr(*160,block)
            iform = format + 1
            DO
               CALL page2(-2)
               WRITE (Otpe,99003) strnbr , row , types(type) , forms(iform) , nterms
99003          FORMAT ('0STRING NO.',I5,'   ROW POSITION=',I5,'   STRING TYPE=',A3,'   STRING TRAILERS=',A3,'   NUMBER OF TERMS=',  &
                     & I5)
               strnbr = strnbr + 1
               IF ( type==2 ) THEN
!
!     PRINT STRING IN REAL DOUBLE PRECISION
!
                  npoint = jpoint + nterms - 1
                  j = jpoint
                  SPAG_Loop_2_1: DO
                     n = min0(j+7,npoint)
                     CALL page2(-1)
                     WRITE (Otpe,99004) (xnd(i),i=j,n)
99004                FORMAT (1X,8(1P,D15.7))
                     IF ( n==npoint ) THEN
                        CALL endget(block)
                        CALL getstr(*20,block)
                        EXIT SPAG_Loop_2_1
                     ELSE
                        j = n + 1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ELSEIF ( type==3 ) THEN
!
!     PRINT STRING IN COMPLEX SINGLE PRECISION
!
                  npoint = jpoint + 2*nterms - 1
                  j = jpoint
                  SPAG_Loop_2_2: DO
                     n = min0(j+7,npoint)
                     CALL page2(-1)
                     WRITE (Otpe,99005) (xns(i),i=j,n)
99005                FORMAT (1X,4(1P,E14.7,1P,E15.7,2H//))
                     IF ( n==npoint ) THEN
                        CALL endget(block)
                        CALL getstr(*20,block)
                        EXIT SPAG_Loop_2_2
                     ELSE
                        j = n + 1
                     ENDIF
                  ENDDO SPAG_Loop_2_2
               ELSEIF ( type==4 ) THEN
!
!     PRINT STRING IN COMPLEX DOUBLE PRECISION
!
                  npoint = jpoint + 2*nterms - 1
                  j = jpoint
                  SPAG_Loop_2_3: DO
                     n = min0(j+7,npoint)
                     CALL page2(-1)
                     WRITE (Otpe,99006) (xnd(i),i=j,n)
99006                FORMAT (1X,4(1P,D14.7,1P,D15.7,2H//))
                     IF ( n==npoint ) THEN
                        CALL endget(block)
                        CALL getstr(*20,block)
                        EXIT SPAG_Loop_2_3
                     ELSE
                        j = n + 1
                     ENDIF
                  ENDDO SPAG_Loop_2_3
               ELSE
!
!     PRINT REAL SINGLE PRECISION STRING
!
                  npoint = jpoint + nterms - 1
                  j = jpoint
                  SPAG_Loop_2_4: DO
                     n = min0(j+7,npoint)
                     CALL page2(-1)
                     WRITE (Otpe,99007) (xns(i),i=j,n)
99007                FORMAT (1X,8(1P,E15.7))
                     IF ( n==npoint ) THEN
                        CALL endget(block)
                        CALL getstr(*20,block)
                        EXIT SPAG_Loop_2_4
                     ELSE
                        j = n + 1
                     ENDIF
                  ENDDO SPAG_Loop_2_4
               ENDIF
            ENDDO
         ELSE
            jv = 4
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         ix = ix + 1
         iout = 4
         nred = nred + 1
         np = np + 1
         CALL read(*120,*80,iname,Core(ix),1,0,iflag)
!
         IF ( irec<=ircd .AND. ix<=ixxx ) THEN
            jj = numtyp(icore(ix)) + 1
            IF ( jj==1 .AND. jv/=4 ) jj = jv
            jv = jj
         ENDIF
         IF ( jj==1 .OR. jj==2 ) THEN
!
!     INTEGER  (3)
!
            iout = 3
            IF ( llen+13>132 ) GOTO 80
            icore(nred+1) = intgc
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( jj==3 ) THEN
!
!     REAL NUMBER  (1)
!
            iout = 1
            IF ( llen+13>132 ) GOTO 80
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( jj==4 ) THEN
!
!     ALPHA  (2)
!
            iout = 2
            IF ( llen+6>132 ) GOTO 80
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     TABLES KELM, MELM, AND BELM - D.P. DATA ONLY
!
         CALL read(*120,*120,iname,Core(1),2,1,iflag)
         WRITE (Otpe,99008) icore(1) , icore(2)
99008    FORMAT (10X,2A4)
 40      WRITE (Otpe,99014) irec
         CALL read(*120,*60,iname,Core(1),nz,1,iflag)
         CALL mesage(-8,0,name)
 60      np = iflag/2
         jj = (np+9)/10
         CALL page2(-jj)
         irec = irec + 1
         WRITE (Otpe,99009,ERR=40) (dcore(i),i=1,np)
99009    FORMAT (1X,1P,10D13.6)
         GOTO 40
      CASE (4)
         Core(nred+1) = ec
         Core(nred+2) = ec1
         Core(nred+3) = ec2
         nred = nred + 2
         spag_nextblock_1 = 5
      CASE (5)
         llen = llen + 13
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         Core(nred+1) = alphc
         Core(nred+2) = alphc1
         nred = nred + 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     BUFFER FULL- END RECORD AND PRINT THE LINE
!
!     PREVIOUSLY, THE FORMAT IS IN CORE, WHICH IS DIMENSIONED TO 1.
!     THIS MAY NOT WORK IN SOME MACHINES. THE FORMAT IS NOW SPECIFIED IN
!     ICORE, WHICH IS DIMENSIONED TO 133.
!     (CORE AND ICORE ARE EQUIVALENT)
!
 80      Core(nred+1) = cparen
         IF ( nred>=133 ) CALL mesage(-37,0,name)
         CALL page2(-1)
         IF ( nred==1 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Mach/=2 .AND. Mach/=5 ) THEN
            CALL wrtfmt(icore(ns),np-ns+1,core1)
         ELSE
            WRITE (Otpe,icore,ERR=100) (Core(i),i=ns,np)
         ENDIF
 100     llen = 0
         nred = 1
         np = inum
!
!     FINISH SEMI-PROCESSED WORD.
!
         Core(inum+1) = Core(ix)
         ix = inum + 1
         IF ( iout==1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iout==2 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iout==3 ) THEN
            icore(nred+1) = intgc
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( iout==4 ) THEN
            GOTO 20
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
         WRITE (Otpe,99010)
99010    FORMAT (' THIS RECORD IS NULL.')
!
!     GO TO 161 IS LOGICALLY UNSOUND. CHANG TO 164. (G.CHAN/UNISYS 1/93)
!     GO TO 161
!WKBR GO TO 164
         CALL wrtfmt(icore(ns),np-ns+1,core1)
         GOTO 100
!
 120     CALL close(iname,1)
         CALL page2(-2)
         WRITE (Otpe,99011)
99011    FORMAT (//,' END OF FILE')
!
!     PRINT TRAILER FOR FILE
!
 140     icore(1) = iname
         CALL rdtrl(icore)
         CALL page2(-2)
         WRITE (Otpe,99012) (icore(i),i=2,7)
99012    FORMAT ('0TRAILER WORD1 =',I8,' WORD2 =',I8,' WORD3 =',I8,' WORD4 =',I8,' WORD5 =',I8,' WORD6 =',I8)
         RETURN
!
!     PRINT NULL COLUMN
!
 160     CALL page2(-1)
         WRITE (Otpe,99013)
99013    FORMAT (5X,'NULL COLUMN')
         GOTO 20
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99014 FORMAT (/,' RECORD NO.',I6)
!
END SUBROUTINE tabprt
