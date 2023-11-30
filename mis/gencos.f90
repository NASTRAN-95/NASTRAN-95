
SUBROUTINE gencos
   IMPLICIT NONE
   INTEGER Direct , Ibuf , Ii , In , Incr , Iout , Iz(1) , Luset , Nn , Nscale , Otpe , Shock
   CHARACTER*23 Ufm
   REAL Z(1)
   COMMON /blank / Shock , Direct , Luset , Nscale
   COMMON /packx / In , Iout , Ii , Nn , Incr
   COMMON /system/ Ibuf , Otpe
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   LOGICAL all , rec
   INTEGER bgpdt , buf1 , cstm , dircos , file , i , icoord(4) , idir(3) , ip , isub(3) , isys , iwords , j , jsub , lcore , mcb(7) &
         & , n , nam(2) , ncount , ncstm , ndir , npts
   REAL coord(4) , tfinal(9) , tpoint(9) , tshock(9)
   INTEGER korsz
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
   EQUIVALENCE (Z(1),Iz(1)) , (coord(1),icoord(1))
   DATA bgpdt , cstm , dircos/101 , 102 , 201/
   DATA nam/4HGENC , 4HOS  /
!
!     OPEN CORE AND BUFFERS
!
   lcore = korsz(Z)
   buf1 = lcore - Ibuf + 1
   lcore = buf1 - 1
   IF ( lcore<=0 ) GOTO 1300
!
!     CHECK FOR SCALAR POINTS AND SET NSCALE
!
   mcb(1) = bgpdt
   CALL rdtrl(mcb)
   npts = mcb(2)
   CALL gopen(bgpdt,Z(buf1),0)
   DO i = 1 , npts
      CALL fread(bgpdt,coord,4,0)
      IF ( icoord(1)==-1 ) GOTO 100
   ENDDO
   Nscale = 0
   GOTO 200
 100  Nscale = 1
 200  CALL close(bgpdt,1)
!
   IF ( Direct<1 .OR. Direct>3 ) THEN
      IF ( Direct/=12 .AND. Direct/=13 .AND. Direct/=23 .AND. Direct/=123 ) GOTO 1100
   ENDIF
   IF ( Shock<0 ) GOTO 1100
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
      CALL open(*300,cstm,Z(buf1),0)
!
      file = cstm
      CALL fwdrec(*1200,cstm)
      CALL read(*1200,*400,cstm,Z,lcore,0,ncstm)
   ENDIF
   GOTO 1300
!
!     CSTM IS PURGED.  SO, GLOBAL SYSTEM IS BASIC AND SHOCK SYSTEM MUST
!     BE ALSO.  IF SHOCK SYSTEM IS NOT 0, FATAL MESSAGE.  IF IT IS 0,
!     THEN NEED ONLY IDENTITIES.
!
 300  IF ( Shock/=0 ) THEN
      WRITE (Otpe,99001) Ufm
99001 FORMAT (A23,', IN GENCOS, CSTM IS PURGED AND SHOCK COORDINATE ','SYSTEM IS NOT BASIC')
      CALL mesage(-61,0,0)
   ENDIF
!
!     EVERYTHING IS BASIC - CHECK FOR SCALAR POINTS - IF THEY EXIST,
!     WE MUST READ BGPDT
!
   IF ( Nscale==1 ) GOTO 500
   all = .TRUE.
   isys = 0
   GOTO 800
 400  CALL close(cstm,1)
!
!     CHECK FOR ENOUGH OPEN CORE
!
   IF ( ncstm+Luset*ndir>lcore ) GOTO 1300
   CALL pretrs(Z(1),ncstm)
!
!     IF SHOCK COORDINATE SYSTEM IS RECTANGULAR, LET'S GET THE TRANS-
!     FORMATION MATRIX ONCE SINCE IT WILL NOT BE POINT-DEPENDENT.
!
   IF ( Shock/=0 ) THEN
      DO i = 1 , ncstm , 14
         IF ( Shock==Iz(i) ) THEN
            IF ( Iz(i+1)==1 ) THEN
!
!     RECTANGULAR
!
               rec = .TRUE.
               DO j = 1 , 9
                  tshock(j) = Z(i+j+4)
               ENDDO
            ENDIF
            GOTO 600
         ENDIF
      ENDDO
!
!     CAN'T FIND SHOCK COORDINATE SYSTEM
!
      CALL mesage(-30,25,Shock)
   ENDIF
!
!     SHOCK IS BASIC
!
 500  rec = .TRUE.
   DO i = 1 , 9
      tshock(i) = 0.
   ENDDO
   tshock(1) = 1.
   tshock(5) = 1.
   tshock(9) = 1.
!
!     OPEN BGPDT TO GET GRID POINT OUTPUT COORDINATE SYSTEMS AND
!     BASIC COORDINATES
!
 600  CALL gopen(bgpdt,Z(buf1),0)
   file = bgpdt
 700  CALL read(*1200,*1000,bgpdt,coord,4,0,iwords)
   isys = icoord(1)
   IF ( icoord(1)==-1 ) GOTO 900
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
      GOTO 900
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
      GOTO 900
   ENDIF
!
!     BOTH ARE IDENTITY
!
 800  DO i = 1 , 9
      tfinal(i) = 0.
   ENDDO
   tfinal(1) = 1.
   tfinal(5) = 1.
   tfinal(9) = 1.
 900  DO
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
      IF ( .NOT.all ) GOTO 700
      IF ( ncount==Luset ) EXIT
   ENDDO
!
!     DONE WITH ALL POINTS - PACK RESULTS
!
 1000 IF ( .NOT.all ) CALL close(bgpdt,1)
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
!
 1100 WRITE (Otpe,99002) Ufm , Shock , Direct
99002 FORMAT (A23,', SHOCK AND DIRECT ARE',2I10,/10X,'RESPECTIVELY. ','SHOCK MUST BE NONNEGATIVE AND DIRECT MUST BE EITHER 1,2',    &
             &',3,12,13,23, OR 123')
   CALL mesage(-61,0,0)
!
 1200 n = -2
   GOTO 1400
 1300 n = -8
   file = 0
 1400 CALL mesage(n,file,nam)
END SUBROUTINE gencos
