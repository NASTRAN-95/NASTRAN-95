
SUBROUTINE algap(Ifname,Ifnm)
   IMPLICIT NONE
   INTEGER Iz(10) , Ksystm(100) , Lpunch , Out , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   REAL Z(1)
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER Ifname , Ifnm
   INTEGER blank , dti(2) , dtis(2) , endrec(2) , file , form(30,2) , forms(30,2) , i , ibcd(2) , ibcdd(2) , ibuf , ichr , id ,     &
         & idata(20) , idf , idt , if , iform(20) , ifrs , ift , ilen , int(2) , intd(2) , ip1 , iplus , ircnm1 , ireal(2) ,        &
         & irecno , is , istar , it , j , k , ll(4) , m , mcb(7) , n1 , name(2) , nf , nlist , nread , nz , pform(30) , tabnm(2)
   INTEGER korsz , numtyp
   REAL rdata(20)
!
!     THIS ROUTINE IS A MODIFIED VERSION OF SUBROUTINE TABPCH. IT WILL
!     ONLY PUNCH ONE TABLE INTO DTI CHARDS.
!
!     CONTINUATION CARD CHARACTERS ARE - AL.
!
!     SINGLE FIELD CARDS WILL BE MADE UNLESS REAL NUMBERS ARE TO BE MADE
!     ALL REAL NUMBERS ARE ASSUMED TO BE SINGLE PRECISION.
!
!  $MIXED_FORMATS
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Out) , (Ksystm(91),Lpunch) , (Iz(1),Z(1)) , (idata(1),rdata(1))
   DATA blank/1H /
   DATA dti/4HDTI  , 1H /
   DATA dtis/4HDTI* , 1H /
   DATA endrec/4HENDR , 4HEC  /
   DATA forms/4H(2A4 , 26*4H     , 4H,1H+ , 4HA2,I , 4H5)   , 4H(A1, , 4HA2,I , 4H5    , 24*4H     , 4H,1H+ , 4HA2,I , 4H5)  /
   DATA ibcd/4H,2A4 , 1H /
   DATA ibcdd/4H,2A4 , 4H,8X /
   DATA int/4H,I8  , 1H /
   DATA intd/4H,I16 , 1H /
   DATA iplus/1H+/
   DATA ireal/4H,E16 , 4H.9  /
   DATA istar/1H*/
   DATA name/4HALGA , 4HP   /
   DATA n1/2HAL/
   DATA ll/3 , 1 , 3 , 2/
!
   nz = korsz(Z)
   ibuf = nz - Sysbuf + 1
   nz = ibuf - 1
   IF ( nz<=10 ) CALL mesage(-8,0,name)
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
   mcb(1) = Ifnm
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) GOTO 2000
!
!     TABLE EXISTS SET IT UP
!
   file = Ifnm
   CALL open(*2100,file,Iz(ibuf),0)
   CALL read(*2300,*2400,file,Iz(1),-2,0,ilen)
   CALL fname(Ifname,tabnm)
   irecno = 0
   ichr = n1
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
   CALL read(*1900,*100,file,Iz(10),nread,0,ilen)
   CALL mesage(-8,0,name)
 100  ilen = ilen + 11
 200  Iz(ilen-1) = endrec(1)
   Iz(ilen) = endrec(2)
!
!     BUILD FORMAT VECTOR  1= INTEGER, 2 =BCD, 3=REAL
!
   DO k = 1 , ilen
      m = nlist + k - 1
      j = numtyp(Iz(k))
      Iz(m) = ll(j+1)
   ENDDO
!
!     MOVE DATA/FORMAT TO DATA AREA 8 FIELDS AT A TIME--SET D.F. FLAG
!
   id = 1
   if = nlist
   ifrs = 1
   GOTO 500
!
!     BRING IN NEXT RECORD
!
 300  CALL read(*1900,*400,file,Iz(4),nread,0,ilen)
   CALL mesage(-8,0,name)
 400  Iz(3) = Iz(3) + 1
   IF ( ilen==0 ) GOTO 300
   ilen = ilen + 5
   GOTO 200
!
!     HERE FOR EIGHT MORE WORDS
!
 500  idf = 0
   idt = 1
   ift = 1
   nf = 1
!
!     HERE  FOR EACH FIELD
!
 600  idata(idt) = Iz(id)
   iform(ift) = Iz(if)
   IF ( iform(ift)==3 ) idf = 1
   IF ( iform(ift)/=2 ) GOTO 800
!
!     BCD IS TWO WORDS
!
   idata(idt+1) = Iz(id+1)
!
!     MAY BE FALSE BCD, CHECK FORMAT OF SECOND WORD ALSO
!     ( SOME REAL NUMBER BIT PATTERNS LOOK LIKE BCD ).
!
   IF ( Iz(if+1)/=2 ) THEN
!
!     SECOND WORD IS NOT BCD, ASSUME FIRST WORD IS REAL.
!
      idf = 1
      iform(ift) = 3
      GOTO 800
   ENDIF
 700  idt = idt + 2
   ift = ift + 1
   id = id + 2
   if = if + 2
   GOTO 900
!
!     REAL OR INTEGER
!
 800  idt = idt + 1
   ift = ift + 1
   id = id + 1
   if = if + 1
!
!     BUMP FIELD COUNTER
!
 900  nf = nf + 1
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
         GOTO 1400
      ELSE
!
!     SINGLE FIELD CARD
!
         nf = 1
      ENDIF
   ELSE
      IF ( id<ilen ) GOTO 600
!
!     FILL  WITH BLANKS
!
      idata(idt) = blank
      idata(idt+1) = blank
      iform(ift) = 2
      GOTO 700
   ENDIF
 1000 m = 2*nf + 2
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
      CALL mesage(-61,0,name)
      RETURN
   ENDIF
   nf = nf + 1
   IF ( nf<=8 ) GOTO 1000
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
      WRITE (Lpunch,pform,ERR=1300) iplus , ichr , ircnm1 , (rdata(m),m=1,idt) , ichr , irecno
      GOTO 1300
   ELSE
      DO j = 1 , 30
         pform(j) = form(j,1)
      ENDDO
      WRITE (Lpunch,pform,ERR=1100) dti , (rdata(m),m=1,idt) , ichr , irecno
   ENDIF
 1100 irecno = irecno + 1
   ifrs = 2
   DO j = 1 , 30
      form(j,1) = forms(j,1)
   ENDDO
 1200 IF ( id<ilen ) GOTO 500
   GOTO 300
 1300 irecno = irecno + 1
   DO j = 1 , 30
      form(j,2) = forms(j,2)
   ENDDO
   GOTO 1200
 1400 m = m + 2
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
      GOTO 1500
   ELSE
!
!     REAL
!
      form(m,ifrs) = ireal(1)
      form(m+1,ifrs) = ireal(2)
   ENDIF
   idt = idt + 1
 1500 nf = nf + 1
   IF ( m<=8 ) GOTO 1400
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
      WRITE (Lpunch,pform,ERR=1800) istar , ichr , ircnm1 , (rdata(m),m=is,idt) , ichr , irecno
      GOTO 1800
   ELSE
      DO j = 1 , 30
         pform(j) = form(j,1)
      ENDDO
      WRITE (Lpunch,pform,ERR=1600) dtis , (rdata(m),m=is,idt) , ichr , irecno
   ENDIF
 1600 irecno = irecno + 1
   DO j = 1 , 30
      form(j,1) = forms(j,1)
   ENDDO
   ifrs = 2
 1700 it = 8
   m = 2
   is = idt + 1
   GOTO 1400
 1800 irecno = irecno + 1
   DO j = 1 , 30
      form(j,2) = forms(j,2)
   ENDDO
   IF ( it/=4 ) GOTO 1200
   GOTO 1700
!
!     CLOSE OFF FILES
!
 1900 CALL close(file,1)
   WRITE (Out,99001) Uim , tabnm , irecno
99001 FORMAT (A29,' 4015.',/5X,'TABLE NAMED ',2A4,' PUNCHED ONTO',I9,' CARDS.')
 2000 WRITE (Lpunch,99002)
99002 FORMAT (1H ,/,1H ,/,1H )
   RETURN
!
!     ERROR MESAGES
!
 2100 ip1 = -1
 2200 CALL mesage(ip1,file,name)
   CALL mesage(-61,0,name)
 2300 ip1 = -2
   GOTO 2200
 2400 ip1 = -3
   GOTO 2200
END SUBROUTINE algap
