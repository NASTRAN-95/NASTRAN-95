
SUBROUTINE tabpch
   IMPLICIT NONE
   DOUBLE PRECISION Dz(1)
   INTEGER Iz(10) , Ksystm(88) , Lpch , Mach , N1(2,5) , Out , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   REAL Z(1)
   COMMON /blank / N1
   COMMON /machin/ Mach
   COMMON /system/ Sysbuf , Out , Ksystm , Lpch
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER blank , dti(2) , dtis(2) , endrec(2) , file , form(30,2) , forms(30,2) , i , ibcd(2) , ibcdd(2) , ibuf , ichr , icrq ,   &
         & id , idata(20) , idf , idt , if , ifnm(5) , iform(20) , ifrs , ift , ilen , int(2) , intd(2) , io , ip1 , iplus ,        &
         & ircnm1 , ireal(2) , irecno , is , istar , it , j , jb , je , jv , k , kmb , ll(4) , m , mcb(7) , name(2) , nf , nlist ,  &
         & nread , ns , nsp , nz , pform(30) , sp(3) , tabnm(2)
   INTEGER korsz , numtyp
   REAL rdata(20)
!
!     THE TABPCH MODULE WILL PUNCH UP TO 5 TABLES INTO DTI CARDS
!
!     DMAP CALL IS
!
!     TABPCH  IN1,IN2,IN3,IN4,IN5//P1,P2,P3,P4,P5
!
!     SINGLE FIELD CARDS WILL BE MADE UNLESS REAL NUMBERS ARE TO BE MADE
!     ALL REAL NUMBERS ARE ASSUMED TO BE SINGLE PRECISION.
!
!     LAST REVISED, 3/93, BY G.CHAN/UNISYS
!     PUNCH KELM, MELM AND BELM IN D.P. IF THESE DATA BLOCKS ARE IN D.P.
!
!  $MIXED_FORMATS
!
   EQUIVALENCE (Z(1),Iz(1),Dz(1)) , (idata(1),rdata(1))
   DATA blank/1H /
   DATA dti/4HDTI  , 1H /
   DATA dtis/4HDTI* , 1H /
   DATA endrec/4HENDR , 4HEC  /
   DATA forms/4H(2A4 , 26*2H   , 4H,1H+ , 4HA2,I , 4H5)   , 4H(A1, , 4HA2,I , 4H5    , 24*2H   , 4H,1H+ , 4HA2,I , 4H5)  /
   DATA ibcd/4H,2A4 , 1H /
   DATA ibcdd/4H,2A4 , 4H,8X /
   DATA ifnm/101 , 102 , 103 , 104 , 105/
   DATA int/4H,I8  , 1H /
   DATA intd/4H,I16 , 1H /
   DATA iplus/1H+/
   DATA ireal/4H,E16 , 4H.9  /
   DATA istar/1H*/
   DATA name/4HTABP , 4HCH  /
   DATA ll/1 , 1 , 3 , 2/
   DATA nsp , sp/3 , 4HKELM , 4HMELM , 4HBELM/
!
   nz = korsz(Z)
   ibuf = nz - Sysbuf + 1
   nz = ibuf - 1
   icrq = 10 - nz
   IF ( nz<=10 ) GOTO 1300
   nread = nz/2 - 2
   nlist = nread + 3
   DO j = 1 , 2
      DO i = 1 , 30
         form(i,j) = forms(i,j)
      ENDDO
   ENDDO
!
!     FOR EACH  TABLE DEFINED
!
   ns = -1
   DO i = 1 , 5
      mcb(1) = ifnm(i)
      CALL rdtrl(mcb)
      IF ( mcb(1)<=0 ) CYCLE
!
!     TABLE EXISTS SET IT UP
!
      file = ifnm(i)
      CALL open(*1000,file,Iz(ibuf),0)
      CALL fname(file,tabnm)
      io = 0
      kmb = 4
      IF ( mcb(5)/=1 .AND. mcb(5)/=3 ) THEN
         DO j = 1 , nsp
            IF ( kmb/=1 .AND. tabnm(1)==sp(j) ) THEN
               kmb = 1
               io = 1
               nread = nz - 1
            ENDIF
         ENDDO
         IF ( ns==-1 ) THEN
            ns = 1
            CALL page1
            WRITE (Out,99001) Uwm
99001       FORMAT (A25,', MODULE TABPCH ASSUMES ALL REAL DATA ARE IN S.P..','  D.P. DATA THEREFORE MAY BE PUNCHED ERRONEOUSLY')
            IF ( Mach==5 .OR. Mach==6 .OR. Mach==10 .OR. Mach==21 ) WRITE (Out,99002)
99002       FORMAT (4X,'(ALL INTEGERS EXCEEDING 16000 ARE PUNCHED AS REAL ','NUMBERS. ALL REAL NUMBERS OUTSIDE E-27 OR E+27 RANGE ',&
                   &'ARE PUNCHED AS INTEGERS)')
         ENDIF
      ENDIF
!
      CALL read(*1100,*1200,file,Iz(1),-2,0,ilen)
      irecno = 0
      ichr = N1(1,i)
      Iz(3) = 0
!
!     SET UP FIRST RECORD
!
      Iz(1) = tabnm(1)
      Iz(2) = tabnm(2)
      Iz(4) = mcb(2)
      Iz(5) = mcb(3)
      Iz(6) = mcb(4)
      Iz(7) = mcb(5)
      Iz(8) = mcb(6)
      Iz(9) = mcb(7)
      CALL read(*900,*50,file,Iz(10),nread,0,ilen)
      icrq = nread
      GOTO 1300
 50   ilen = ilen + 11
 100  Iz(ilen-1) = endrec(1)
      Iz(ilen) = endrec(2)
!
!     BUILD FORMAT VECTOR  1= INTEGER, 2 =BCD, 3=REAL
!
      jv = 3
      DO k = 1 , ilen
         m = nlist + k - 1
         j = numtyp(Iz(k))
         IF ( j==0 .AND. jv/=3 ) j = jv
         Iz(m) = ll(j+1)
         jv = j
      ENDDO
!
!     MOVE DATA/FORMAT TO DATA AREA 8 FIELDS AT A TIME--SET D.F. FLAG
!
      id = 1
      if = nlist
      ifrs = 1
      GOTO 250
!
!     BRING IN NEXT RECORD
!
 150  CALL read(*900,*200,file,Iz(kmb),nread,io,ilen)
      icrq = nread
      GOTO 1300
 200  IF ( kmb==1 ) THEN
!
!     PUNCH KELM, MELM AND BELM IN D.P.
!
         IF ( ilen==0 ) GOTO 150
         ilen = ilen/2
         je = 0
         DO
            jb = je + 1
            je = je + 4
            ircnm1 = irecno
            irecno = irecno + 1
            IF ( je>=ilen ) THEN
               je = ilen
               WRITE (Lpch,99003,ERR=1400) ichr , ircnm1 , (Dz(j),j=jb,je)
99003          FORMAT (1H*,A2,I5,1P,4D16.9)
               GOTO 150
            ELSE
               WRITE (Lpch,99004,ERR=1400) ichr , ircnm1 , (Dz(j),j=jb,je) , ichr , irecno
99004          FORMAT (1H*,A2,I5,1P,4D16.9,1X,A2,I5)
            ENDIF
         ENDDO
      ELSE
         Iz(3) = Iz(3) + 1
         IF ( ilen==0 ) GOTO 150
         ilen = ilen + 5
         GOTO 100
      ENDIF
!
!     HERE FOR EIGHT MORE WORDS
!
 250  idf = 0
      idt = 1
      ift = 1
      nf = 1
!
!     HERE  FOR EACH FIELD
!
 300  idata(idt) = Iz(id)
      iform(ift) = Iz(if)
      IF ( iform(ift)==3 ) idf = 1
      IF ( iform(ift)/=2 ) GOTO 400
!
!     BCD IS TWO WORDS
!
      idata(idt+1) = Iz(id+1)
!
!     MAY BE FALSE BCD, CHECK FORMAT OF SECOND WORD ALSO
!     (SOME REAL NUMBER BIT PATTERNS LOOK LIKE BCD).
!
      IF ( Iz(if+1)/=2 ) THEN
!
!     SECOND WORD IS NOT BCD, ASSUME FIRST WORD IS REAL.
!
         idf = 1
         iform(ift) = 3
         GOTO 400
      ENDIF
 350  idt = idt + 2
      ift = ift + 1
      id = id + 2
      if = if + 2
      GOTO 450
!
!     REAL OR INTEGER
!
 400  idt = idt + 1
      ift = ift + 1
      id = id + 1
      if = if + 1
!
!     BUMP FIELD COUNTER
!
 450  nf = nf + 1
      IF ( nf>8 ) THEN
!
!     PUNCH OUT 8 FIELDS OF DATA
!
         idt = 0
         IF ( idf/=0 ) THEN
!
!     DOUBLE FIELD CARDS
!
            nf = 1
            is = 1
            it = 4
            idt = 0
            m = 2
            GOTO 650
         ELSE
!
!     SINGLE FIELD CARD
!
            nf = 1
         ENDIF
      ELSE
         IF ( id<ilen ) GOTO 300
!
!     FILL  WITH BLANKS
!
         idata(idt) = blank
         idata(idt+1) = blank
         iform(ift) = 2
         GOTO 350
      ENDIF
      DO
         m = 2*nf + 2
         IF ( iform(nf)<2 ) THEN
!
!     INTEGER
!
            form(m,ifrs) = int(1)
            form(m+1,ifrs) = int(2)
!
!     GET NEXT ITEM
!
            idt = idt + 1
         ELSEIF ( iform(nf)==2 ) THEN
!
!     BCD
!
            form(m,ifrs) = ibcd(1)
            form(m+1,ifrs) = ibcd(2)
            idt = idt + 2
         ELSE
!
!     REAL NOT LEGAL
!
            ip1 = -37
            GOTO 1500
         ENDIF
         nf = nf + 1
         IF ( nf>8 ) THEN
!
!     PUNCH OUT SINGLE CARD
!
            IF ( ifrs/=1 ) THEN
!
!     CONTINUATION CARD
!
               ircnm1 = irecno - 1
               DO j = 1 , 30
                  pform(j) = form(j,2)
               ENDDO
               WRITE (Lpch,pform,ERR=600) iplus , ichr , ircnm1 , (rdata(m),m=1,idt) , ichr , irecno
               GOTO 600
            ELSE
               DO j = 1 , 30
                  pform(j) = form(j,1)
               ENDDO
               WRITE (Lpch,pform,ERR=500) dti , (rdata(m),m=1,idt) , ichr , irecno
               EXIT
            ENDIF
         ENDIF
      ENDDO
 500  irecno = irecno + 1
      ifrs = 2
      DO j = 1 , 30
         form(j,1) = forms(j,1)
      ENDDO
 550  IF ( id<ilen ) GOTO 250
      GOTO 150
 600  irecno = irecno + 1
      DO j = 1 , 30
         form(j,2) = forms(j,2)
      ENDDO
      GOTO 550
 650  m = m + 2
      IF ( iform(nf)<2 ) THEN
!
!     INTEGER
!
         form(m,ifrs) = intd(1)
         form(m+1,ifrs) = intd(2)
      ELSEIF ( iform(nf)==2 ) THEN
!
!     BCD
!
         form(m,ifrs) = ibcdd(1)
         form(m+1,ifrs) = ibcdd(2)
         idt = idt + 2
         GOTO 700
      ELSE
!
!     REAL
!
         form(m,ifrs) = ireal(1)
         form(m+1,ifrs) = ireal(2)
      ENDIF
      idt = idt + 1
 700  nf = nf + 1
      IF ( m<=8 ) GOTO 650
!
!     PUNCH OUT DOUBLE FIELD CARD
!
      IF ( ifrs/=1 ) THEN
!
!     CONTINUATION CARD
!
         ircnm1 = irecno - 1
         DO j = 1 , 30
            pform(j) = form(j,2)
         ENDDO
         WRITE (Lpch,pform,ERR=850) istar , ichr , ircnm1 , (rdata(m),m=is,idt) , ichr , irecno
         GOTO 850
      ELSE
         DO j = 1 , 30
            pform(j) = form(j,1)
         ENDDO
         WRITE (Lpch,pform,ERR=750) dtis , (rdata(m),m=is,idt) , ichr , irecno
      ENDIF
 750  irecno = irecno + 1
      DO j = 1 , 30
         form(j,1) = forms(j,1)
      ENDDO
      ifrs = 2
 800  it = 8
      m = 2
      is = idt + 1
      GOTO 650
 850  irecno = irecno + 1
      DO j = 1 , 30
         form(j,2) = forms(j,2)
      ENDDO
      IF ( it/=4 ) GOTO 550
      GOTO 800
!
!     CLOSE OFF FILES
!
 900  CALL close(file,1)
      CALL page2(2)
      WRITE (Out,99005) Uim , tabnm , irecno
99005 FORMAT (A29,' 4015, TABLE ',2A4,' WAS PUNCHED OUT,',I8,' CARDS.')
   ENDDO
   WRITE (Lpch,99006)
99006 FORMAT (1H ,/,1H ,/,1H )
   RETURN
!
!     ERROR MESAGES
!
 1000 ip1 = -1
   GOTO 1500
 1100 ip1 = -2
   GOTO 1500
 1200 ip1 = -3
   GOTO 1500
 1300 ip1 = -8
   file = icrq
   GOTO 1500
 1400 ip1 = -37
!
 1500 CALL mesage(ip1,file,name)
END SUBROUTINE tabpch
