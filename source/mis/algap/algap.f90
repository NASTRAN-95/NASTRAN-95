!*==algap.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algap(Ifname,Ifnm)
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ifname
   INTEGER :: Ifnm
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , iplus , istar , n1
   INTEGER , DIMENSION(2) , SAVE :: dti , dtis , endrec , ibcd , ibcdd , int , intd , ireal , name
   INTEGER :: file , i , ibuf , ichr , id , idf , idt , if , ifrs , ift , ilen , ip1 , ircnm1 , irecno , is , it , j , k , lpunch , &
            & m , nf , nlist , nread , nz , out , sysbuf
   INTEGER , DIMENSION(30,2) :: form
   INTEGER , DIMENSION(30,2) , SAVE :: forms
   INTEGER , DIMENSION(20) :: idata , iform
   INTEGER , DIMENSION(10) :: iz
   INTEGER , DIMENSION(4) , SAVE :: ll
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(30) :: pform
   REAL , DIMENSION(20) :: rdata
   INTEGER , DIMENSION(2) :: tabnm
   EXTERNAL close , fname , korsz , mesage , numtyp , open , rdtrl , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Out) , (Ksystm(91),Lpunch) , (Iz(1),Z(1)) , (idata(1),rdata(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nz = korsz(z)
         ibuf = nz - sysbuf + 1
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
         IF ( mcb(1)<=0 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TABLE EXISTS SET IT UP
!
         file = Ifnm
         CALL open(*160,file,iz(ibuf),0)
         CALL read(*180,*200,file,iz(1),-2,0,ilen)
         CALL fname(Ifname,tabnm)
         irecno = 0
         ichr = n1
         iz(3) = 0
!
!     SET UP FIRST RECORD
!
         iz(1) = tabnm(1)
         iz(2) = tabnm(2)
         iz(4) = mcb(2)
         iz(5) = mcb(3)
         iz(6) = mcb(4)
         iz(7) = mcb(5)
         iz(8) = mcb(6)
         iz(9) = mcb(7)
         CALL read(*140,*20,file,iz(10),nread,0,ilen)
         CALL mesage(-8,0,name)
 20      ilen = ilen + 11
         spag_nextblock_1 = 2
      CASE (2)
         iz(ilen-1) = endrec(1)
         iz(ilen) = endrec(2)
!
!     BUILD FORMAT VECTOR  1= INTEGER, 2 =BCD, 3=REAL
!
         DO k = 1 , ilen
            m = nlist + k - 1
            j = numtyp(iz(k))
            iz(m) = ll(j+1)
         ENDDO
!
!     MOVE DATA/FORMAT TO DATA AREA 8 FIELDS AT A TIME--SET D.F. FLAG
!
         id = 1
         if = nlist
         ifrs = 1
         spag_nextblock_1 = 4
      CASE (3)
!
!     BRING IN NEXT RECORD
!
         CALL read(*140,*40,file,iz(4),nread,0,ilen)
         CALL mesage(-8,0,name)
 40      iz(3) = iz(3) + 1
         IF ( ilen==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ilen = ilen + 5
         spag_nextblock_1 = 2
      CASE (4)
!
!     HERE FOR EIGHT MORE WORDS
!
         idf = 0
         idt = 1
         ift = 1
         nf = 1
         spag_nextblock_1 = 5
      CASE (5)
!
!     HERE  FOR EACH FIELD
!
         idata(idt) = iz(id)
         iform(ift) = iz(if)
         IF ( iform(ift)==3 ) idf = 1
         IF ( iform(ift)/=2 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BCD IS TWO WORDS
!
         idata(idt+1) = iz(id+1)
!
!     MAY BE FALSE BCD, CHECK FORMAT OF SECOND WORD ALSO
!     ( SOME REAL NUMBER BIT PATTERNS LOOK LIKE BCD ).
!
         IF ( iz(if+1)/=2 ) THEN
!
!     SECOND WORD IS NOT BCD, ASSUME FIRST WORD IS REAL.
!
            idf = 1
            iform(ift) = 3
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         idt = idt + 2
         ift = ift + 1
         id = id + 2
         if = if + 2
         spag_nextblock_1 = 8
      CASE (7)
!
!     REAL OR INTEGER
!
         idt = idt + 1
         ift = ift + 1
         id = id + 1
         if = if + 1
         spag_nextblock_1 = 8
      CASE (8)
!
!     BUMP FIELD COUNTER
!
         nf = nf + 1
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
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     SINGLE FIELD CARD
!
               nf = 1
            ENDIF
         ELSE
            IF ( id<ilen ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     FILL  WITH BLANKS
!
            idata(idt) = blank
            idata(idt+1) = blank
            iform(ift) = 2
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_1: DO
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
               CALL mesage(-61,0,name)
               RETURN
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
                  WRITE (lpunch,pform,ERR=80) iplus , ichr , ircnm1 , (rdata(m),m=1,idt) , ichr , irecno
                  GOTO 80
               ELSE
                  DO j = 1 , 30
                     pform(j) = form(j,1)
                  ENDDO
                  WRITE (lpunch,pform,ERR=60) dti , (rdata(m),m=1,idt) , ichr , irecno
               ENDIF
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
 60      irecno = irecno + 1
         ifrs = 2
         DO j = 1 , 30
            form(j,1) = forms(j,1)
         ENDDO
         spag_nextblock_1 = 9
      CASE (9)
         IF ( id>=ilen ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      irecno = irecno + 1
         DO j = 1 , 30
            form(j,2) = forms(j,2)
         ENDDO
         spag_nextblock_1 = 9
      CASE (10)
         m = m + 2
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
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     REAL
!
            form(m,ifrs) = ireal(1)
            form(m+1,ifrs) = ireal(2)
         ENDIF
         idt = idt + 1
         spag_nextblock_1 = 11
      CASE (11)
         nf = nf + 1
         IF ( m<=8 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
            WRITE (lpunch,pform,ERR=120) istar , ichr , ircnm1 , (rdata(m),m=is,idt) , ichr , irecno
            GOTO 120
         ELSE
            DO j = 1 , 30
               pform(j) = form(j,1)
            ENDDO
            WRITE (lpunch,pform,ERR=100) dtis , (rdata(m),m=is,idt) , ichr , irecno
         ENDIF
 100     irecno = irecno + 1
         DO j = 1 , 30
            form(j,1) = forms(j,1)
         ENDDO
         ifrs = 2
         spag_nextblock_1 = 12
      CASE (12)
         it = 8
         m = 2
         is = idt + 1
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 120     irecno = irecno + 1
         DO j = 1 , 30
            form(j,2) = forms(j,2)
         ENDDO
         IF ( it==4 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE OFF FILES
!
 140     CALL close(file,1)
         WRITE (out,99001) uim , tabnm , irecno
99001    FORMAT (A29,' 4015.',/5X,'TABLE NAMED ',2A4,' PUNCHED ONTO',I9,' CARDS.')
         spag_nextblock_1 = 13
      CASE (13)
         WRITE (lpunch,99002)
99002    FORMAT (1H ,/,1H ,/,1H )
         RETURN
!
!     ERROR MESAGES
!
 160     ip1 = -1
         spag_nextblock_1 = 14
      CASE (14)
         CALL mesage(ip1,file,name)
         CALL mesage(-61,0,name)
 180     ip1 = -2
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 200     ip1 = -3
         spag_nextblock_1 = 14
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE algap
