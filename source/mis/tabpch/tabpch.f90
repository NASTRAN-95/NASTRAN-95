!*==tabpch.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tabpch
USE C_BLANK
USE C_MACHIN
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , iplus , istar , nsp
   INTEGER , DIMENSION(2) , SAVE :: dti , dtis , endrec , ibcd , ibcdd , int , intd , ireal , name
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: file , i , ibuf , ichr , icrq , id , idf , idt , if , ifrs , ift , ilen , io , ip1 , ircnm1 , irecno , is , it , j ,  &
            & jb , je , jv , k , kmb , m , nf , nlist , nread , ns , nz
   INTEGER , DIMENSION(30,2) :: form
   INTEGER , DIMENSION(30,2) , SAVE :: forms
   INTEGER , DIMENSION(20) :: idata , iform
   INTEGER , DIMENSION(5) , SAVE :: ifnm
   INTEGER , DIMENSION(10) :: iz
   INTEGER , DIMENSION(4) , SAVE :: ll
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(30) :: pform
   REAL , DIMENSION(20) :: rdata
   INTEGER , DIMENSION(3) , SAVE :: sp
   INTEGER , DIMENSION(2) :: tabnm
   EXTERNAL close , fname , korsz , mesage , numtyp , open , page1 , page2 , rdtrl , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1)) , (idata(1),rdata(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nz = korsz(Z)
         ibuf = nz - Sysbuf + 1
         nz = ibuf - 1
         icrq = 10 - nz
         IF ( nz<=10 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  mcb(1) = ifnm(i)
                  CALL rdtrl(mcb)
                  IF ( mcb(1)<=0 ) CYCLE
!
!     TABLE EXISTS SET IT UP
!
                  file = ifnm(i)
                  CALL open(*20,file,iz(ibuf),0)
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
99001                   FORMAT (A25,', MODULE TABPCH ASSUMES ALL REAL DATA ARE IN S.P..',                                           &
                               &'  D.P. DATA THEREFORE MAY BE PUNCHED ERRONEOUSLY')
                        IF ( Mach==5 .OR. Mach==6 .OR. Mach==10 .OR. Mach==21 ) WRITE (Out,99002)
99002                   FORMAT (4X,'(ALL INTEGERS EXCEEDING 16000 ARE PUNCHED AS REAL ',                                            &
                               &'NUMBERS. ALL REAL NUMBERS OUTSIDE E-27 OR E+27 RANGE ','ARE PUNCHED AS INTEGERS)')
                     ENDIF
                  ENDIF
!
                  CALL read(*40,*60,file,iz(1),-2,0,ilen)
                  irecno = 0
                  ichr = N1(1,i)
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
                  CALL read(*14,*2,file,iz(10),nread,0,ilen)
                  icrq = nread
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 2                ilen = ilen + 11
                  spag_nextblock_2 = 2
               CASE (2)
                  iz(ilen-1) = endrec(1)
                  iz(ilen) = endrec(2)
!
!     BUILD FORMAT VECTOR  1= INTEGER, 2 =BCD, 3=REAL
!
                  jv = 3
                  DO k = 1 , ilen
                     m = nlist + k - 1
                     j = numtyp(iz(k))
                     IF ( j==0 .AND. jv/=3 ) j = jv
                     iz(m) = ll(j+1)
                     jv = j
                  ENDDO
!
!     MOVE DATA/FORMAT TO DATA AREA 8 FIELDS AT A TIME--SET D.F. FLAG
!
                  id = 1
                  if = nlist
                  ifrs = 1
                  spag_nextblock_2 = 4
                  CYCLE SPAG_DispatchLoop_2
               CASE (3)
!
!     BRING IN NEXT RECORD
!
                  CALL read(*14,*4,file,iz(kmb),nread,io,ilen)
                  icrq = nread
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 4                IF ( kmb==1 ) THEN
!
!     PUNCH KELM, MELM AND BELM IN D.P.
!
                     IF ( ilen==0 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     ilen = ilen/2
                     je = 0
                     DO
                        jb = je + 1
                        je = je + 4
                        ircnm1 = irecno
                        irecno = irecno + 1
                        IF ( je>=ilen ) THEN
                           je = ilen
                           WRITE (Lpch,99003,ERR=80) ichr , ircnm1 , (dz(j),j=jb,je)
99003                      FORMAT (1H*,A2,I5,1P,4D16.9)
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ELSE
                           WRITE (Lpch,99004,ERR=80) ichr , ircnm1 , (dz(j),j=jb,je) , ichr , irecno
99004                      FORMAT (1H*,A2,I5,1P,4D16.9,1X,A2,I5)
                        ENDIF
                     ENDDO
                  ELSE
                     iz(3) = iz(3) + 1
                     IF ( ilen==0 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     ilen = ilen + 5
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (4)
!
!     HERE FOR EIGHT MORE WORDS
!
                  idf = 0
                  idt = 1
                  ift = 1
                  nf = 1
                  spag_nextblock_2 = 5
               CASE (5)
!
!     HERE  FOR EACH FIELD
!
                  idata(idt) = iz(id)
                  iform(ift) = iz(if)
                  IF ( iform(ift)==3 ) idf = 1
                  IF ( iform(ift)/=2 ) THEN
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     BCD IS TWO WORDS
!
                  idata(idt+1) = iz(id+1)
!
!     MAY BE FALSE BCD, CHECK FORMAT OF SECOND WORD ALSO
!     (SOME REAL NUMBER BIT PATTERNS LOOK LIKE BCD).
!
                  IF ( iz(if+1)/=2 ) THEN
!
!     SECOND WORD IS NOT BCD, ASSUME FIRST WORD IS REAL.
!
                     idf = 1
                     iform(ift) = 3
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 6
               CASE (6)
                  idt = idt + 2
                  ift = ift + 1
                  id = id + 2
                  if = if + 2
                  spag_nextblock_2 = 8
                  CYCLE SPAG_DispatchLoop_2
               CASE (7)
!
!     REAL OR INTEGER
!
                  idt = idt + 1
                  ift = ift + 1
                  id = id + 1
                  if = if + 1
                  spag_nextblock_2 = 8
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
                        spag_nextblock_2 = 10
                        CYCLE SPAG_DispatchLoop_2
                     ELSE
!
!     SINGLE FIELD CARD
!
                        nf = 1
                     ENDIF
                  ELSE
                     IF ( id<ilen ) THEN
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
!
!     FILL  WITH BLANKS
!
                     idata(idt) = blank
                     idata(idt+1) = blank
                     iform(ift) = 2
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  SPAG_Loop_2_1: DO
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
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
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
                           WRITE (Lpch,pform,ERR=8) iplus , ichr , ircnm1 , (rdata(m),m=1,idt) , ichr , irecno
                           GOTO 8
                        ELSE
                           DO j = 1 , 30
                              pform(j) = form(j,1)
                           ENDDO
                           WRITE (Lpch,pform,ERR=6) dti , (rdata(m),m=1,idt) , ichr , irecno
                           EXIT SPAG_Loop_2_1
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_2_1
 6                irecno = irecno + 1
                  ifrs = 2
                  DO j = 1 , 30
                     form(j,1) = forms(j,1)
                  ENDDO
                  spag_nextblock_2 = 9
               CASE (9)
                  IF ( id>=ilen ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 4
                  CYCLE SPAG_DispatchLoop_2
 8                irecno = irecno + 1
                  DO j = 1 , 30
                     form(j,2) = forms(j,2)
                  ENDDO
                  spag_nextblock_2 = 9
                  CYCLE SPAG_DispatchLoop_2
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
                     spag_nextblock_2 = 11
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
!
!     REAL
!
                     form(m,ifrs) = ireal(1)
                     form(m+1,ifrs) = ireal(2)
                  ENDIF
                  idt = idt + 1
                  spag_nextblock_2 = 11
               CASE (11)
                  nf = nf + 1
                  IF ( m<=8 ) THEN
                     spag_nextblock_2 = 10
                     CYCLE SPAG_DispatchLoop_2
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
                     WRITE (Lpch,pform,ERR=12) istar , ichr , ircnm1 , (rdata(m),m=is,idt) , ichr , irecno
                     GOTO 12
                  ELSE
                     DO j = 1 , 30
                        pform(j) = form(j,1)
                     ENDDO
                     WRITE (Lpch,pform,ERR=10) dtis , (rdata(m),m=is,idt) , ichr , irecno
                  ENDIF
 10               irecno = irecno + 1
                  DO j = 1 , 30
                     form(j,1) = forms(j,1)
                  ENDDO
                  ifrs = 2
                  spag_nextblock_2 = 12
               CASE (12)
                  it = 8
                  m = 2
                  is = idt + 1
                  spag_nextblock_2 = 10
                  CYCLE SPAG_DispatchLoop_2
 12               irecno = irecno + 1
                  DO j = 1 , 30
                     form(j,2) = forms(j,2)
                  ENDDO
                  IF ( it==4 ) THEN
                     spag_nextblock_2 = 12
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 9
                  CYCLE SPAG_DispatchLoop_2
!
!     CLOSE OFF FILES
!
 14               CALL close(file,1)
                  CALL page2(2)
                  WRITE (Out,99005) Uim , tabnm , irecno
99005             FORMAT (A29,' 4015, TABLE ',2A4,' WAS PUNCHED OUT,',I8,' CARDS.')
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         WRITE (Lpch,99006)
99006    FORMAT (1H ,/,1H ,/,1H )
         RETURN
!
!     ERROR MESAGES
!
 20      ip1 = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      ip1 = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      ip1 = -3
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         ip1 = -8
         file = icrq
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      ip1 = -37
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL mesage(ip1,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE tabpch
