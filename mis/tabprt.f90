
SUBROUTINE tabprt(Iname1)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1) , Dum(42) , Head1(96) , Head2(96) , Xns(1)
   CHARACTER*1 Core1(2000)
   DOUBLE PRECISION Dcore(1) , Xnd(1)
   INTEGER Icore(133) , Inx(6) , Inx1(2) , Iprc , Irc , Iwd , Line , Mach , Nlpp , Op(2) , Otpe , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Op , Irc , Iwd
   COMMON /machin/ Mach
   COMMON /output/ Head1 , Head2
   COMMON /system/ Sysbuf , Otpe , Inx , Nlpp , Inx1 , Line , Dum , Iprc
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Iname1
!
! Local variable declarations
!
   REAL alphc , alphc1 , blank , cont , cparen , d , ebb , ec , ec1 , ec2 , oparen , sp(3) , tabl , uned
   INTEGER bcd , block(20) , flag , format , forms(2) , fpn , i , iflag , iform , iname , int , intgc , inum , iout , ircd , irec , &
         & ix , ixxx , j , jj , jpoint , jv , llen , n , name(2) , np , npoint , nred , ns , nsp , nterms , nz , pure , recf , row ,&
         & strnbr , type , types(4)
   LOGICAL dec
   INTEGER korsz , numtyp
!
! End of declarations
!
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
   EQUIVALENCE (Xnd(1),Core(1))
   EQUIVALENCE (Xns(1),Xnd(1)) , (Icore(1),Core(1),Dcore(1)) , (block(2),type) , (block(3),format) , (block(4),row) ,               &
    & (block(5),jpoint) , (block(6),nterms) , (block(8),flag)
!WKBI
   EQUIVALENCE (Core,Core1)
   DATA oparen , cparen , ec , ec1 , ec2 , intgc , alphc , alphc1 , cont , uned/4H(1X  , 4H)    , 4H,1P, , 4HE13. , 2H6  , 4H,I13 , &
       &4H,9X, , 4HA4   , 4HCONT , 4HINUE/d/2HD / , name/4HTABP , 4HRT  /
   DATA blank , tabl , ebb/1H  , 4HTABL , 1HE/
   DATA types/3HRSP , 3HRDP , 3HCSP , 3HCDP/ , forms/3HYES , 2HNO/
   DATA pure , bcd , fpn , int/4HPURE , 4HBCD  , 4HFPN  , 4HINT /
   DATA nsp , sp/3 , 4HKELM , 4HMELM , 4HBELM/
!
   nz = korsz(Core) - Sysbuf
   IF ( nz<=0 ) CALL mesage(-8,-nz,name)
   dec = Mach==5 .OR. Mach==6 .OR. Mach==10 .OR. Mach==21
   iname = Iname1
   CALL open(*1300,iname,Core(nz+1),0)
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
99001 FORMAT (A25,', TABPRT MODULE ASSUMES ALL REAL DATA ARE IN S.P.,',' D.P. DATA THEREFORE MAY BE PRINTED ERRONEOUSLY')
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
 100  CALL page2(-2)
   IF ( dec .AND. irec==0 ) WRITE (Otpe,99002)
99002 FORMAT (4X,'(ALL INTEGERS EXCEEDING 16000 ARE PRINTED AS REAL ','NUMBERS. ALL REAL NUMBERS OUTSIDE E-27 OR E+27 RANGE ',      &
             &'ARE PRINTED AS INTEGERS)')
   WRITE (Otpe,99014) irec
   irec = irec + 1
   DO i = 1 , nsp
      IF ( Head2(3)==sp(i) ) THEN
         Icore(1) = iname
         CALL rdtrl(Icore)
         IF ( Icore(2)==2 ) GOTO 300
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
      CALL getstr(*1400,block)
      iform = format + 1
      DO
         CALL page2(-2)
         WRITE (Otpe,99003) strnbr , row , types(type) , forms(iform) , nterms
99003    FORMAT ('0STRING NO.',I5,'   ROW POSITION=',I5,'   STRING TYPE=',A3,'   STRING TRAILERS=',A3,'   NUMBER OF TERMS=',I5)
         strnbr = strnbr + 1
         IF ( type==2 ) THEN
!
!     PRINT STRING IN REAL DOUBLE PRECISION
!
            npoint = jpoint + nterms - 1
            j = jpoint
            DO
               n = min0(j+7,npoint)
               CALL page2(-1)
               WRITE (Otpe,99004) (Xnd(i),i=j,n)
99004          FORMAT (1X,8(1P,D15.7))
               IF ( n==npoint ) THEN
                  CALL endget(block)
                  CALL getstr(*100,block)
                  EXIT
               ELSE
                  j = n + 1
               ENDIF
            ENDDO
         ELSEIF ( type==3 ) THEN
!
!     PRINT STRING IN COMPLEX SINGLE PRECISION
!
            npoint = jpoint + 2*nterms - 1
            j = jpoint
            DO
               n = min0(j+7,npoint)
               CALL page2(-1)
               WRITE (Otpe,99005) (Xns(i),i=j,n)
99005          FORMAT (1X,4(1P,E14.7,1P,E15.7,2H//))
               IF ( n==npoint ) THEN
                  CALL endget(block)
                  CALL getstr(*100,block)
                  EXIT
               ELSE
                  j = n + 1
               ENDIF
            ENDDO
         ELSEIF ( type==4 ) THEN
!
!     PRINT STRING IN COMPLEX DOUBLE PRECISION
!
            npoint = jpoint + 2*nterms - 1
            j = jpoint
            DO
               n = min0(j+7,npoint)
               CALL page2(-1)
               WRITE (Otpe,99006) (Xnd(i),i=j,n)
99006          FORMAT (1X,4(1P,D14.7,1P,D15.7,2H//))
               IF ( n==npoint ) THEN
                  CALL endget(block)
                  CALL getstr(*100,block)
                  EXIT
               ELSE
                  j = n + 1
               ENDIF
            ENDDO
         ELSE
!
!     PRINT REAL SINGLE PRECISION STRING
!
            npoint = jpoint + nterms - 1
            j = jpoint
            DO
               n = min0(j+7,npoint)
               CALL page2(-1)
               WRITE (Otpe,99007) (Xns(i),i=j,n)
99007          FORMAT (1X,8(1P,E15.7))
               IF ( n==npoint ) THEN
                  CALL endget(block)
                  CALL getstr(*100,block)
                  EXIT
               ELSE
                  j = n + 1
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ELSE
      jv = 4
   ENDIF
 200  ix = ix + 1
   iout = 4
   nred = nred + 1
   np = np + 1
   CALL read(*1200,*900,iname,Core(ix),1,0,iflag)
!
   IF ( irec<=ircd .AND. ix<=ixxx ) THEN
      jj = numtyp(Icore(ix)) + 1
      IF ( jj==1 .AND. jv/=4 ) jj = jv
      jv = jj
   ENDIF
   IF ( jj==1 .OR. jj==2 ) THEN
!
!     INTEGER  (3)
!
      iout = 3
      IF ( llen+13>132 ) GOTO 900
      Icore(nred+1) = intgc
      GOTO 700
   ELSEIF ( jj==3 ) THEN
!
!     REAL NUMBER  (1)
!
      iout = 1
      IF ( llen+13<=132 ) GOTO 600
      GOTO 900
   ELSEIF ( jj==4 ) THEN
!
!     ALPHA  (2)
!
      iout = 2
      IF ( llen+6<=132 ) GOTO 800
      GOTO 900
   ENDIF
!
!     TABLES KELM, MELM, AND BELM - D.P. DATA ONLY
!
 300  CALL read(*1200,*1200,iname,Core(1),2,1,iflag)
   WRITE (Otpe,99008) Icore(1) , Icore(2)
99008 FORMAT (10X,2A4)
 400  WRITE (Otpe,99014) irec
   CALL read(*1200,*500,iname,Core(1),nz,1,iflag)
   CALL mesage(-8,0,name)
 500  np = iflag/2
   jj = (np+9)/10
   CALL page2(-jj)
   irec = irec + 1
   WRITE (Otpe,99009,ERR=400) (Dcore(i),i=1,np)
99009 FORMAT (1X,1P,10D13.6)
   GOTO 400
 600  Core(nred+1) = ec
   Core(nred+2) = ec1
   Core(nred+3) = ec2
   nred = nred + 2
 700  llen = llen + 13
   GOTO 200
 800  Core(nred+1) = alphc
   Core(nred+2) = alphc1
   nred = nred + 1
   GOTO 700
!
!     BUFFER FULL- END RECORD AND PRINT THE LINE
!
!     PREVIOUSLY, THE FORMAT IS IN CORE, WHICH IS DIMENSIONED TO 1.
!     THIS MAY NOT WORK IN SOME MACHINES. THE FORMAT IS NOW SPECIFIED IN
!     ICORE, WHICH IS DIMENSIONED TO 133.
!     (CORE AND ICORE ARE EQUIVALENT)
!
 900  Core(nred+1) = cparen
   IF ( nred>=133 ) CALL mesage(-37,0,name)
   CALL page2(-1)
   IF ( nred==1 ) GOTO 1100
   IF ( Mach/=2 .AND. Mach/=5 ) THEN
      CALL wrtfmt(Icore(ns),np-ns+1,Core1)
   ELSE
      WRITE (Otpe,Icore,ERR=1000) (Core(i),i=ns,np)
   ENDIF
 1000 llen = 0
   nred = 1
   np = inum
!
!     FINISH SEMI-PROCESSED WORD.
!
   Core(inum+1) = Core(ix)
   ix = inum + 1
   IF ( iout==1 ) GOTO 600
   IF ( iout==2 ) GOTO 800
   IF ( iout==3 ) THEN
      Icore(nred+1) = intgc
      GOTO 700
   ELSEIF ( iout==4 ) THEN
      GOTO 100
   ENDIF
!
 1100 WRITE (Otpe,99010)
99010 FORMAT (' THIS RECORD IS NULL.')
!
!     GO TO 161 IS LOGICALLY UNSOUND. CHANG TO 164. (G.CHAN/UNISYS 1/93)
!     GO TO 161
!WKBR GO TO 164
   CALL wrtfmt(Icore(ns),np-ns+1,Core1)
   GOTO 1000
!
 1200 CALL close(iname,1)
   CALL page2(-2)
   WRITE (Otpe,99011)
99011 FORMAT (//,' END OF FILE')
!
!     PRINT TRAILER FOR FILE
!
 1300 Icore(1) = iname
   CALL rdtrl(Icore)
   CALL page2(-2)
   WRITE (Otpe,99012) (Icore(i),i=2,7)
99012 FORMAT ('0TRAILER WORD1 =',I8,' WORD2 =',I8,' WORD3 =',I8,' WORD4 =',I8,' WORD5 =',I8,' WORD6 =',I8)
   RETURN
!
!     PRINT NULL COLUMN
!
 1400 CALL page2(-1)
   WRITE (Otpe,99013)
99013 FORMAT (5X,'NULL COLUMN')
   GOTO 100
99014 FORMAT (/,' RECORD NO.',I6)
!
END SUBROUTINE tabprt
