!*==gencos.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gencos
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: all , rec
   INTEGER , SAVE :: bgpdt , cstm , dircos
   INTEGER :: buf1 , file , i , ip , isys , iwords , j , jsub , lcore , n , ncount , ncstm , ndir , npts
   REAL , DIMENSION(4) :: coord
   INTEGER , DIMENSION(4) :: icoord
   INTEGER , DIMENSION(3) :: idir , isub
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL , DIMENSION(9) :: tfinal , tpoint , tshock
   EXTERNAL close , fread , fwdrec , gmmats , gopen , korsz , mesage , open , pack , pretrs , rdtrl , read , transs , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GENCOS  GENERATES DIRECTION COSINE MATRIX, UP TO NX3, FOR DDAM.
!     THE SHOCK DIRECTIONS ARE GIVEN BY A COORDINATE SYSTEM (PROBABLY
!     RECTANGULAR, BUT NOT NECESSARILY) DEFINED ON A CORDIJ CARD.
!     THE ID OF THAT SYSTEM MUST BE SPECIFIED BY PARAM SHOCK ID.
!     THE DIRECTIONS OF INTEREST MUST BE SPECIFIED ON A PARAM DIRECT DIR
!     CARD WHERE DIR=1,2,3,12,13,23,OR 123 GIVING THE SHOCK DIRECTIONS
!     DESIRED IN THE SHOCK COORDINATE SYSTEM.  (DEFAULT IS 123)  WE WILL
!     BE CONVERTING A ROW VECTOR IN THE GLOBAL SYSTEM TO A ROW VECTOR IN
!     THE SHOCK SYSTEM.  TO CONVERT A COLUMN VECTOR FROM GLOBAL TO SHOCK
!     FIRST CONVERT TO BASIC.  THEN TRANSFORM FROM BASIC TO SHOCK, I.E.
!     (VECTOR-SHOCK) = (TRANSPOSE(T-SHOCK TO BASIC))*
!                      (T-GLOBAL TO BASIC)*(VECTOR-GLOBAL)
!     BUT BECAUSE WE ARE TRANSFORMING ROW VECTORS, THE EQUATION IS
!     TRANSPOSED . NSCALE =1 MEANS THERE ARE SCALAR POINTS,=0 MEANS NO
!
!     GENCOS    BGPDT,CSTM/DIRCOS/C,Y,SHOCK=0/C,Y,DIRECT=123/
!               V,N,LUSET/V,N,NSCALE $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (coord(1),icoord(1))
   DATA bgpdt , cstm , dircos/101 , 102 , 201/
   DATA nam/4HGENC , 4HOS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN CORE AND BUFFERS
!
         lcore = korsz(Z)
         buf1 = lcore - Ibuf + 1
         lcore = buf1 - 1
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHECK FOR SCALAR POINTS AND SET NSCALE
!
         mcb(1) = bgpdt
         CALL rdtrl(mcb)
         npts = mcb(2)
         CALL gopen(bgpdt,Z(buf1),0)
         DO i = 1 , npts
            CALL fread(bgpdt,coord,4,0)
            IF ( icoord(1)==-1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         Nscale = 0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         Nscale = 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(bgpdt,1)
!
         IF ( Direct<1 .OR. Direct>3 ) THEN
            IF ( Direct/=12 .AND. Direct/=13 .AND. Direct/=23 .AND. Direct/=123 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Shock<0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ncstm = 0
         ncount = 0
         all = .FALSE.
         rec = .FALSE.
         ndir = 2
         IF ( Direct<=3 ) ndir = 1
         IF ( Direct==123 ) ndir = 3
         IF ( Luset*ndir<=lcore ) THEN
!
            IF ( ndir==2 ) THEN
!
               IF ( Direct==23 ) THEN
                  idir(1) = 2
                  idir(2) = 3
               ELSE
                  idir(1) = 1
                  idir(2) = 2
                  IF ( Direct==13 ) idir(2) = 3
               ENDIF
            ELSEIF ( ndir==3 ) THEN
!
               idir(1) = 1
               idir(2) = 2
               idir(3) = 3
            ELSE
!
               idir(1) = Direct
            ENDIF
!
!
!     READ CSTM FOR FETCHING TRANSFORMATION MATRICES
!
            CALL open(*20,cstm,Z(buf1),0)
!
            file = cstm
            CALL fwdrec(*80,cstm)
            CALL read(*80,*40,cstm,Z,lcore,0,ncstm)
         ENDIF
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
!
!     CSTM IS PURGED.  SO, GLOBAL SYSTEM IS BASIC AND SHOCK SYSTEM MUST
!     BE ALSO.  IF SHOCK SYSTEM IS NOT 0, FATAL MESSAGE.  IF IT IS 0,
!     THEN NEED ONLY IDENTITIES.
!
 20      IF ( Shock/=0 ) THEN
            WRITE (Otpe,99001) Ufm
99001       FORMAT (A23,', IN GENCOS, CSTM IS PURGED AND SHOCK COORDINATE ','SYSTEM IS NOT BASIC')
            CALL mesage(-61,0,0)
         ENDIF
!
!     EVERYTHING IS BASIC - CHECK FOR SCALAR POINTS - IF THEY EXIST,
!     WE MUST READ BGPDT
!
         IF ( Nscale==1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         all = .TRUE.
         isys = 0
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(cstm,1)
!
!     CHECK FOR ENOUGH OPEN CORE
!
         IF ( ncstm+Luset*ndir>lcore ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL pretrs(Z(1),ncstm)
!
!     IF SHOCK COORDINATE SYSTEM IS RECTANGULAR, LET'S GET THE TRANS-
!     FORMATION MATRIX ONCE SINCE IT WILL NOT BE POINT-DEPENDENT.
!
         IF ( Shock/=0 ) THEN
            DO i = 1 , ncstm , 14
               IF ( Shock==iz(i) ) THEN
                  IF ( iz(i+1)==1 ) THEN
!
!     RECTANGULAR
!
                     rec = .TRUE.
                     DO j = 1 , 9
                        tshock(j) = Z(i+j+4)
                     ENDDO
                  ENDIF
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     CAN'T FIND SHOCK COORDINATE SYSTEM
!
            CALL mesage(-30,25,Shock)
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     SHOCK IS BASIC
!
         rec = .TRUE.
         DO i = 1 , 9
            tshock(i) = 0.
         ENDDO
         tshock(1) = 1.
         tshock(5) = 1.
         tshock(9) = 1.
         spag_nextblock_1 = 5
      CASE (5)
!
!     OPEN BGPDT TO GET GRID POINT OUTPUT COORDINATE SYSTEMS AND
!     BASIC COORDINATES
!
         CALL gopen(bgpdt,Z(buf1),0)
         file = bgpdt
         spag_nextblock_1 = 6
      CASE (6)
         CALL read(*80,*60,bgpdt,coord,4,0,iwords)
         isys = icoord(1)
         IF ( icoord(1)==-1 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( icoord(1)/=0 ) THEN
!
!     FETCH GLOBAL-TO-BASIC MATRIX FOR THIS POINT
!
            CALL transs(coord,tpoint)
         ELSE
!
!     IDENTITY - BASIC SYSTEM
!
            DO i = 1 , 9
               tpoint(i) = 0.
            ENDDO
            tpoint(1) = 1.
            tpoint(5) = 1.
            tpoint(9) = 1.
         ENDIF
!
!     IF SHOCK IS NOT RECTANGULAR, FETCH SHOCK-TO-BASIC FOR THIS POINT
!
         IF ( .NOT.(rec) ) THEN
            icoord(1) = Shock
            CALL transs(coord,tshock)
         ENDIF
!
!     THE MATRIX WE NEED IS (TRANSPOSE(TPOINT))*(TSHOCK)
!
         IF ( Shock/=0 ) THEN
            IF ( isys==0 ) THEN
!
!     TPOINT IS IDENTITY, BUT TSHOCK IS NOT
!
               DO i = 1 , 9
                  tfinal(i) = tshock(i)
               ENDDO
            ELSE
!
!     NEITHER MATRIX IS NECESSARILY IDENTITY
!
               CALL gmmats(tpoint,3,3,1,tshock,3,3,0,tfinal)
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
!
!     TSHOCK IS IDENTITY
!
         ELSEIF ( isys/=0 ) THEN
!
!     BUT TPOINT IS NOT
!
            tfinal(1) = tpoint(1)
            tfinal(2) = tpoint(4)
            tfinal(3) = tpoint(7)
            tfinal(4) = tpoint(2)
            tfinal(5) = tpoint(5)
            tfinal(6) = tpoint(8)
            tfinal(7) = tpoint(3)
            tfinal(8) = tpoint(6)
            tfinal(9) = tpoint(9)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     BOTH ARE IDENTITY
!
         DO i = 1 , 9
            tfinal(i) = 0.
         ENDDO
         tfinal(1) = 1.
         tfinal(5) = 1.
         tfinal(9) = 1.
         spag_nextblock_1 = 8
      CASE (8)
         SPAG_Loop_1_1: DO
!
!     STORE TFINAL BY INTERNAL ORDERING AND DIRECTIONS REQUESTED START-
!     ING AT Z(NCSTM+1) - MAKE UP TO 3 COLUMNS OF LUSET EACH
!
            isub(1) = ncstm + ncount
            isub(2) = isub(1) + Luset
            isub(3) = isub(2) + Luset
!
            DO i = 1 , ndir
               ip = idir(i)
               jsub = isub(i)
               IF ( isys==-1 ) THEN
!
!     SCALAR
!
                  Z(jsub+1) = 1.
               ELSE
                  Z(jsub+1) = tfinal(ip)
                  Z(jsub+2) = tfinal(ip+3)
                  Z(jsub+3) = tfinal(ip+6)
                  Z(jsub+4) = 0.
                  Z(jsub+5) = 0.
                  Z(jsub+6) = 0.
               ENDIF
            ENDDO
!
!     GO BACK FOR ANOTHER POINT
!
            ncount = ncount + 6
            IF ( isys==-1 ) ncount = ncount - 5
            IF ( .NOT.all ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ncount==Luset ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     DONE WITH ALL POINTS - PACK RESULTS
!
 60      IF ( .NOT.all ) CALL close(bgpdt,1)
         CALL gopen(dircos,Z(buf1),1)
         In = 1
         Iout = 1
         Ii = 1
         Nn = Luset
         Incr = 1
         mcb(1) = dircos
         mcb(2) = 0
         mcb(3) = Luset
         mcb(4) = 2
         mcb(5) = 1
         mcb(6) = 0
         mcb(7) = 0
         DO i = 1 , ndir
            jsub = ncstm + Luset*(i-1)
            CALL pack(Z(jsub+1),dircos,mcb)
         ENDDO
!
         CALL close(dircos,1)
         CALL wrttrl(mcb)
         RETURN
      CASE (9)
!
         WRITE (Otpe,99002) Ufm , Shock , Direct
99002    FORMAT (A23,', SHOCK AND DIRECT ARE',2I10,/10X,'RESPECTIVELY. ','SHOCK MUST BE NONNEGATIVE AND DIRECT MUST BE EITHER 1,2', &
                &',3,12,13,23, OR 123')
         CALL mesage(-61,0,0)
!
 80      n = -2
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         n = -8
         file = 0
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gencos
