!*==ifp3b.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ifp3b
!
!        CARDS           TYPE         REC.ID-BIT  CARDS-FILE, CARDS-FILE
!    === =======         ===========  ==========  ==========  ==========
!     1  AXIC     -----  AX.SY.SHELL     515- 5
!     2  CCONEAX  -----  AX.SY.SHELL    8515-85  CCONE-GEOM2,
!     3  FORCEAX  -----  AX.SY.SHELL    2115-21  FORCE-GEOM3,
!     4  FORCE    -----  STANDARD       4201-42  FORCE-GEOM3,
!     5  GRAV     -----  STANDARD       4401-44   GRAV-GEOM3,
!     6  LOAD     -----  STANDARD       4551-61   LOAD-GEOM3,
!     7  MOMAX    -----  AX.SY.SHELL    3815-38  MOMNT-GEOM3,
!     8  MOMENT   -----  STANDARD       4801-48  MOMNT-GEOM3,
!     9  MPCADD   -----  STANDARD       4891-60 MPCADD-GEOM4,
!    10  MPCAX    -----  AX.SY.SHELL    4015-40    MPC-GEOM4,
!    11  OMITAX   -----  AX.SY.SHELL    4315-43   OMIT-GEOM4,
!    12  POINTAX  -----  AX.SY.SHELL    4915-49    MPC-GEOM4, GRID-GEOM1
!    13  PRESAX   -----  AX.SY.SHELL    5215-52  PLOAD-GEOM3,
!    13+ RFORCE   -----  STANDARD       5509-55 RFORCE-GEOM3,
!    14  RINGAX   -----  AX.SY.SHELL    5615-56    SPC-GEOM4, GRID-GEOM1
!    15  SECTAX   -----  AX.SY.SHELL    6315-63    MPC-GEOM4, GRID-GEOM1
!    16  SEQGP    -----  STANDARD       5301-53  SEQGP-GEOM1,
!    17  SPCADD   -----  STANDARD       5491-59 SPCADD-GEOM4,
!    18  SPCAX    -----  AX.SY.SHELL    6215-62    SPC-GEOM4,
!    19  SUPAX    -----  AX.SY.SHELL    6415-64 SUPORT-GEOM4,
!    20  TEMPAX   -----  AX.SY.SHELL    6815-68   TEMP-GEOM3,
!    21  TEMPD    -----  STANDARD       5641-65  TEMPD-GEOM3,
!
   IMPLICIT NONE
   USE c_blank
   USE c_condas
   USE c_ifp3cm
   USE c_ifp3lv
   USE c_ifpdta
   USE c_machin
   USE c_output
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: axic , clorwd , eor , i , icard , ilast , imsg , inprwd , irec , j , l2 , noeor , outrwd , scrtch
   INTEGER , DIMENSION(4) :: geom
   INTEGER , SAVE :: i3 , i4 , i5 , ifist
   INTEGER , DIMENSION(175) :: isystm
   REAL :: raddeg , t3
   INTEGER , DIMENSION(13) :: z
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Consts(4),Raddeg) , (Z(1),Rz(1)) , (Geom(1),File(1)) , (Scrtch,File(5)) , (Axic,File(6)) , (Noeor,Inprwd,Zero) ,    &
!>>>>    & (Eor,Clorwd,Outrwd,One)
   !>>>>EQUIVALENCE (Ibufsz,Isystm(1))
   DATA ifist/4HFIST/ , i3 , i4 , i5/3 , 4 , 5/
!
!
!     GEOM4 PROCESSING
!     ================
!
!     OPEN GEOM4
!
   ifile = geom(4)
   i = 4
   op = outrwd
   buff = ibuff2
   ASSIGN 100 TO iretrn
   GOTO 8600
!
!     SPCADD OR MPCADD CARDS
!     ======================
!
 100  ASSIGN 800 TO icont
   rec(1) = mpcadd(1)
   rec(2) = mpcadd(2)
   rec(3) = mpcadd(3)
   rec1(1) = mpcax(1)
   rec1(2) = mpcax(2)
   rec1(3) = mpcax(3)
 200  ASSIGN 300 TO iheadr
   GOTO 9200
!
!     MANDATORY SPCADD AND MPCADD CARDS.
!
 300  z(1) = 100000101
   z(2) = 101
   z(3) = -1
   z(4) = 200000102
   z(5) = 102
   z(6) = -1
   z(7) = 100000000
   z(8) = 101
   z(9) = -1
   z(10) = 200000000
   z(11) = 102
   z(12) = -1
   IF ( .NOT.(nogo) ) CALL write(geom(4),z(1),12,noeor)
   CALL locate(*400,z(ibuff1),rec(1),flag)
!
!     READ AN OPEN ENDED SPCADD OR MPCADD CARD INTO CORE.
!
   i = 1
   DO
      CALL read(*9400,*400,axic,z(i),1,noeor,iamt)
      IF ( z(i)<0 ) THEN
!
!     COMPLETE CARD IS AT HAND
!
         z(i) = 101
         i = i + 1
         z(i) = -1
         z(1) = z(1) + 100000000
         IF ( nogo ) GOTO icont
         DO
            CALL write(geom(4),z(1),i,noeor)
            IF ( z(i-1)==102 ) GOTO 400
            z(i-1) = 102
            z(1) = z(1) + 100000000
         ENDDO
      ELSE
         i = i + 1
         IF ( (i+1)>icore ) THEN
!
            CALL page2(3)
            imsg = 363
            WRITE (nout,99012) ufm , imsg
            WRITE (nout,99001)
99001       FORMAT (5X,'INSUFFICIENT CORE TO PROCESS AXIC DATA IN SUBROUTINE','IFP3B')
            nogo = .TRUE.
            GOTO 9300
         ENDIF
      ENDIF
   ENDDO
!
!     ALL SPCADD OR MPCADD CARDS COMPLETE.
!     NOW CREATE SPCADD OR MPCADD FROM SPCAX OR MPCAX
!     CARDS RESPECTIVELY.
!
 400  irec = rec(1)
   rec(1) = rec1(1)
   rec(2) = rec1(2)
   rec(3) = rec1(3)
   CALL locate(*600,z(ibuff1),rec(1),flag)
!
!     OK SPCAX OR MPCAX RECORD EXISTS.
!
   ilast = -1
 500  z(4) = -1
   CALL read(*9400,*600,axic,z(2),1,noeor,iamt)
!
!     MPCAX CARDS ARE OPEN ENDED
!     SPCAX CARDS ARE 5 WORDS LONG.
!
   IF ( z(2)/=ilast ) THEN
      ilast = z(2)
!
!     CREATE TWO SPCADD OR MPCADD CARDS.
!
      z(3) = 101
      z(1) = z(2) + 100000000
      IF ( nogo ) GOTO icont
      DO
         CALL write(geom(4),z(1),4,noeor)
         IF ( z(3)==102 ) EXIT
         z(3) = 102
         z(1) = z(1) + 100000000
      ENDDO
   ENDIF
   DO
!
!     READ UP TO NEXT CARD
!
      CALL read(*9400,*600,axic,z(1),4,noeor,iamt)
      IF ( rec(1)==spcax(1) .OR. z(1)==(-1) ) GOTO 500
   ENDDO
!
!     ALL CARDS COMPLETE.
!     WRITE EOR AND PUT BITS IN TRAILER.
!
 600  iamt = 0
   ASSIGN 700 TO iretrn
   IF ( irec==spcadd(1) ) THEN
      rec(1) = spcadd(1)
      rec(2) = spcadd(2)
      rec(3) = spcadd(3)
   ELSE
      rec(1) = mpcadd(1)
      rec(2) = mpcadd(2)
      rec(3) = mpcadd(3)
   ENDIF
   GOTO 8400
 700  GOTO icont
!
!     MPCAX CARD
!     ==========
!
 800  mpcon = 0
   rec(1) = mpc(1)
   rec(2) = mpc(2)
   rec(3) = mpc(3)
   recoff = .FALSE.
   last = -1
   ncard = 10
   nwords = 0
   CALL locate(*1600,z(ibuff1),mpcax(1),flag)
!
!     WRITE RECORD HEADER
!
   recoff = .TRUE.
   ASSIGN 900 TO iheadr
   GOTO 9200
!
 900  mpcon = 1
   last = 0
!
!     READ SET ID
!
 1000 CALL read(*9400,*1500,axic,setid,1,noeor,iamt)
   IF ( setid>100 ) GOTO 1600
   nwords = nwords + 1
   IF ( .NOT.(nogo) ) CALL write(geom(4),setid,1,noeor)
!
!     READ 4-WORDS SETS UNTIL -1,-1,-1,-1 ENCOUNTERED...
!
 1100 CALL read(*9400,*1400,axic,z(1),4,noeor,iamt)
   nwords = nwords + 4
   IF ( z(4)==-1 ) THEN
!
!     END OF EQUATION
!
      IF ( .NOT.(nogo) ) CALL write(geom(4),neg111(1),3,noeor)
      GOTO 1000
   ELSE
!
!     CHECK HARMONIC NUMBER
!
      nnn = z(2)
      ASSIGN 1200 TO ierrtn
      GOTO 8900
   ENDIF
!
!     CHECK RING ID
!
 1200 nnn = z(1)
   ASSIGN 1300 TO ierrtn
   GOTO 9000
!
 1300 z(2) = z(1) + (z(2)+1)*1000000
   IF ( .NOT.(nogo) ) CALL write(geom(4),z(2),3,noeor)
   GOTO 1100
 1400 CALL page2(3)
   imsg = 1063
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99002) sfm , imsg
99002 FORMAT (5X,50HEOR ON AXIC FILE WHILE READING MPCAX CARD RECORDS.)
   nogo = .TRUE.
   GOTO 9300
 1500 last = 1
!
!     FIRST NWORDS HAVE BEEN PROCESSED OF MPCAX CARDS UNLESS
!     LAST = 1, IN WHICH CASE ALL MPCAX CARDS ARE COMPLETE.
!     GO NOW TO THE S-SET MPC CARD-GENERATION FOR POINTAX CARDS
!     IF LAST = -1, THERE ARE NO MPCAX CARDS.
!
!
!     S-SET MPC-S FROM POINTAX CARDS
!     ==============================
!
 1600 rec(1) = pointx(1)
   rec(2) = pointx(2)
   rec(3) = pointx(3)
   ncard = 12
   n3or5 = 3
   k3or6 = 6
   sorc = 101
   ASSIGN 2400 TO icont
!     TURN NOPONT OR NOSECT ON IF POINTAX OR SECTAX CARDS EXIST RESPECT.
!
   ibit = pointx(2)
   ASSIGN 1700 TO ibitr
   GOTO 9100
 1700 nopont = non
   ibit = sectax(2)
   ASSIGN 1800 TO ibitr
   GOTO 9100
 1800 nosect = non
!
   IF ( nopont==0 ) GOTO 2300
!
 1900 CALL locate(*2300,z(ibuff1),rec(1),flag)
   mpcon = 1
   IF ( .NOT.(recoff) ) THEN
!
!     WRITE RECORD HEADER
!
      recoff = .TRUE.
      rec(1) = mpc(1)
      rec(2) = mpc(2)
      rec(3) = mpc(3)
      ASSIGN 2000 TO iheadr
      GOTO 9200
   ENDIF
!
 2000 CALL read(*9400,*2300,axic,z(1),n3or5,noeor,iamt)
!
!     CHECK RING ID FOR S-SET PASS ONLY FOR POINTAX AND SECTAX CARDS.
!     NO CHECK WILL BE MADE IN THE GRID CARD GENERATION AREA.
!
!     IF (SORC .EQ. 102) GO TO 785
   nnn = z(2)
   ASSIGN 2100 TO ierrtn
   GOTO 9000
!
 2100 iat = n3or5 + 1
   DO i = 1 , k3or6
      z(iat) = sorc
      z(iat+1) = z(1)
      z(iat+2) = i
      rz(iat+3) = -1.0
      IF ( .NOT.(nogo) ) CALL write(geom(4),z(iat),4,noeor)
      DO j = 1 , nplus1
!
!     COMPUTE COEFFICIENT.
!
         ni = j - 1
         IF ( n3or5==5 ) THEN
!
!     SECTAX CARD COEFFICIENTS
!
            t1 = ni*rz(i4)*raddeg
            t2 = ni*rz(i5)*raddeg
            IF ( i>=4 ) THEN
               IF ( andf(i,1)<=0 ) GOTO 2140
            ELSEIF ( andf(i,1)>0 ) THEN
               GOTO 2140
            ENDIF
!
!     EVEN I
!
            IF ( sorc==101 ) GOTO 2160
         ELSE
!
!     POINTAX CARD COEFFICIENTS
!
            t1 = ni*rz(i3)*raddeg
            IF ( andf(i,1)<=0 ) THEN
!
!     EVEN I
!
               IF ( sorc>101 ) THEN
!
                  coef = sin(t1)
                  GOTO 2200
               ENDIF
!
!     ODD I
!
            ELSEIF ( sorc<=101 ) THEN
               coef = sin(t1)
               GOTO 2200
            ENDIF
!
            coef = cos(t1)
            IF ( sorc==101 ) coef = -coef
            IF ( ni==0.0 .AND. sorc==101 ) coef = 1.0
            GOTO 2200
         ENDIF
 2120    IF ( ni/=0 ) THEN
            t3 = t2
            t2 = cos(t1)
            t1 = cos(t3)
            GOTO 2180
         ELSE
            coef = 0.0
            GOTO 2200
         ENDIF
!
!     ODD I
!
 2140    IF ( sorc==101 ) GOTO 2120
 2160    IF ( ni/=0 ) THEN
            t1 = sin(t1)
            t2 = sin(t2)
         ELSE
            coef = rz(i3)*(rz(i5)-rz(i4))*raddeg
            GOTO 2200
         ENDIF
 2180    coef = rz(i3)*(t2-t1)/ni
         IF ( sorc==101 .AND. (i==2 .OR. i==5) ) coef = -coef
!
 2200    z(iat) = z(2) + j*1000000
         z(iat+1) = i
         rz(iat+2) = coef
         IF ( .NOT.(nogo) ) CALL write(geom(4),z(iat),3,noeor)
      ENDDO
      IF ( .NOT.(nogo) ) CALL write(geom(4),neg111(1),3,noeor)
   ENDDO
   GOTO 2000
!
 2300 GOTO icont
!
!     S-SET MPC-S FROM SECTAX CARDS
!     =============================
!
!     DO SECTAX CARDS FOR S-SET.
!
 2400 rec(1) = sectax(1)
   rec(2) = sectax(2)
   rec(3) = sectax(3)
   n3or5 = 5
   k3or6 = 6
   sorc = 101
   ncard = 15
   ASSIGN 2500 TO icont
   IF ( nosect/=0 ) GOTO 1900
!
!     C-SET MPC-S FROM POINTAX CARDS
!     ==============================
!
!
 2500 rec(1) = pointx(1)
   rec(2) = pointx(2)
   rec(3) = pointx(3)
   n3or5 = 3
   k3or6 = 6
   sorc = 102
   ASSIGN 2600 TO icont
   IF ( nopont/=0 ) GOTO 1900
!
!     C-SET MPC-S FROM SECTAX CARDS
!     =============================
!
 2600 rec(1) = sectax(1)
   rec(2) = sectax(2)
   rec(3) = sectax(3)
   n3or5 = 5
   k3or6 = 6
   sorc = 102
   ASSIGN 2700 TO icont
   IF ( nosect/=0 ) GOTO 1900
!
!     BALANCE OF MPCAX CARDS
!
 2700 IF ( last/=0 ) GOTO 3300
   CALL locate(*3300,z(ibuff1),mpcax(1),flag)
   ncard = 10
   IF ( nwords/=0 ) THEN
      DO i = 1 , nwords
         CALL read(*9400,*3100,axic,z(1),1,noeor,iamt)
      ENDDO
   ENDIF
!
!     NOW POSITIONED AT POINT LEFT OFF AT ABOVE.
!
 2800 CALL read(*9400,*3300,axic,setid,1,noeor,iamt)
   IF ( setid<101 ) GOTO 3100
   IF ( setid<=102 ) THEN
      nogo = .TRUE.
      CALL page2(3)
      imsg = 366
      WRITE (nout,99012) ufm , imsg
      WRITE (nout,99013)
   ENDIF
   IF ( .NOT.(nogo) ) CALL write(geom(4),setid,1,noeor)
 2900 CALL read(*9400,*1400,axic,z(1),4,noeor,iamt)
   IF ( z(4)==(-1) ) THEN
!
!     END OF EQUATION
!
      IF ( .NOT.(nogo) ) CALL write(geom(4),neg111(1),3,noeor)
      GOTO 2800
   ELSE
!
!     CHECK HARMONIC NUMBER
!
      nnn = z(2)
      ASSIGN 3000 TO ierrtn
      GOTO 8900
   ENDIF
!
!     CHECK RING ID
!
 3000 nnn = z(1)
   ASSIGN 3200 TO ierrtn
   GOTO 9000
 3100 CALL page2(3)
   imsg = 1063
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99014) cdtype(19) , cdtype(20)
   nogo = .TRUE.
   GOTO 9300
!
 3200 z(2) = z(1) + (z(2)+1)*1000000
   IF ( .NOT.(nogo) ) CALL write(geom(4),z(2),3,noeor)
   GOTO 2900
!
!     AT 713(?) WRITE EOR AND PUT BITS IN TRAILER.
!
 3300 IF ( mpcon/=0 ) THEN
      iamt = 0
      rec(1) = mpc(1)
      rec(2) = mpc(2)
      rec(3) = mpc(3)
      ASSIGN 3400 TO iretrn
      GOTO 8400
   ENDIF
!
!     OMITAX CARDS
!
 3400 rec(1) = omitax(1)
   rec(2) = omitax(2)
   rec(3) = omitax(3)
   ncard = 11
   rec1(1) = omit(1)
   rec1(2) = omit(2)
   rec1(3) = omit(3)
   ASSIGN 4100 TO icont
 3500 CALL locate(*4000,z(ibuff1),rec(1),flag)
   IF ( .NOT.(nogo) ) CALL write(geom(4),rec1(1),3,noeor)
 3600 CALL read(*9400,*3900,axic,z(1),3,noeor,iamt)
!
!     CHECK HARMONIC NUMBER
!
   nnn = z(2)
   ASSIGN 3700 TO ierrtn
   GOTO 8900
!
!     CHECK RING ID
!
 3700 nnn = z(1)
   ASSIGN 3800 TO ierrtn
   GOTO 9000
!
 3800 z(2) = z(1) + (z(2)+1)*1000000
   IF ( ifpdco(z(3)) ) THEN
      nogo = .TRUE.
      CALL page2(3)
      imsg = 367
      WRITE (nout,99012) ufm , imsg
      WRITE (nout,99003) z(3) , cdtype(2*ncard-1) , cdtype(2*ncard)
99003 FORMAT (5X,'COMPONENT SPECIFICATION',I8,4H ON ,2A4,' CARD IS INCORRECT')
   ELSE
      DO l2 = 1 , 6
         IF ( ll(l2)/=0 ) THEN
            z(3) = ll(l2)
            IF ( nogo ) EXIT
            CALL write(geom(4),z(2),2,noeor)
         ENDIF
      ENDDO
   ENDIF
   GOTO 3600
!
!     WRITE EOR AND PUT BITS IN TRAILER
!
 3900 iamt = 0
   rec(1) = rec1(1)
   rec(2) = rec1(2)
   rec(3) = rec1(3)
   ASSIGN 4000 TO iretrn
   GOTO 8400
 4000 GOTO icont
!
!     SPCADD CARD
!     ===========
!
 4100 rec(1) = spcadd(1)
   rec(2) = spcadd(2)
   rec(3) = spcadd(3)
   rec1(1) = spcax(1)
   rec1(2) = spcax(2)
   rec1(3) = spcax(3)
   ASSIGN 4200 TO icont
   GOTO 200
!
!     SPCAX CARD
!     ==========
!
 4200 rec(1) = spc(1)
   rec(2) = spc(2)
   rec(3) = spc(3)
!
!     RECORD HEADER FOR SPC-S
!
   ASSIGN 4300 TO iheadr
   GOTO 9200
!
 4300 last = -1
   ncard = 18
   CALL locate(*4800,z(ibuff1),spcax(1),flag)
   last = 0
   nwords = 0
 4400 CALL read(*9400,*4700,axic,z(1),5,noeor,iamt)
   IF ( z(1)>100 ) GOTO 4800
   nwords = nwords + 5
!
!     ALTER CARD JUST READ AND OUTPUT
!
!     CHECK HARMONIC NUMBER
!
   nnn = z(3)
   ASSIGN 4500 TO ierrtn
   GOTO 8900
!
!     CHECK RING ID
!
 4500 nnn = z(2)
   ASSIGN 4600 TO ierrtn
   GOTO 9000
!
 4600 z(2) = z(2) + (z(3)+1)*1000000
   z(3) = z(4)
   z(4) = z(5)
!
   IF ( .NOT.(nogo) ) CALL write(geom(4),z(1),4,noeor)
   GOTO 4400
 4700 last = 1
!
!     FIRST NWORDS HAVE BEEN PROCESSED OF SPCAX CARDS
!     UNLESS LAST = 1, IN WHICH CASE ALL SPCAX CARDS ARE COMPLETE.
!     IF LAST = -1, THERE ARE NO SPCAX CARDS
!
!     S-SET AND C-SET SPC-S FROM RINGAX CARDS
!     =======================================
!
 4800 sorc = 101
   ncard = 14
   compon = 135
   IF ( iconso==1 ) compon = 13
   ASSIGN 5400 TO icont
 4900 CALL locate(*5500,z(ibuff1),ringax(1),flag)
 5000 CALL read(*9400,*5300,axic,z(1),4,noeor,iamt)
!
   IF ( sorc==102 ) GOTO 5200
!
!     GIVE RING CARD A CHECK FOR MINIMUM DATA.
!
!     CHECK RING ID
!
   nnn = z(1)
   ASSIGN 5100 TO ierrtn
   GOTO 9000
!
!     CHECK FOR NON-ZERO RADIUS
!
 5100 IF ( rz(i3-1)==0 ) THEN
      CALL page2(3)
      imsg = 368
      WRITE (nout,99012) ufm , imsg
      WRITE (nout,99004) z(1)
99004 FORMAT (5X,'RINGAX CARD WITH RING ID =',I10,' HAS A ZERO RADIUS',' SPECIFIED.')
      nogo = .TRUE.
   ENDIF
 5200 z(4) = 0
   z(3) = compon
   z(2) = z(1) + 1000000
   z(1) = sorc
   IF ( .NOT.(nogo) ) CALL write(geom(4),z(1),4,noeor)
   GOTO 5000
!
 5300 GOTO icont
 5400 sorc = 102
   compon = 246
   IF ( iconso==1 ) compon = 2
!
!     KEEP DOF 4 FOR PIEZOELECTRIC PROBLEM
!
   IF ( ipiez==1 ) compon = 26
   ASSIGN 5600 TO icont
   GOTO 4900
!
!     MISSING REQUIRED CARD
!
 5500 ASSIGN 5600 TO ierrtn
!
!     MISSING REQUIRED CARD
!
   CALL page2(3)
   imsg = 362
   WRITE (nout,99012) ufm , imsg
   WRITE (nout,99005) cdtype(2*ncard-1) , cdtype(2*ncard)
99005 FORMAT (5X,'MINIMUM PROBLEM REQUIRES ',2A4,' CARD.  NONE FOUND.')
   nogo = .TRUE.
   GOTO ierrtn
!
!     BALANCE OF SPCAX CARDS
!
 5600 IF ( last/=0 ) GOTO 6000
   CALL locate(*6000,z(ibuff1),spcax(1),flag)
   ncard = 18
   IF ( nwords/=0 ) THEN
      DO i = 1 , nwords , 5
         CALL read(*9400,*6100,axic,z(1),5,noeor,iamt)
      ENDDO
   ENDIF
!
!     NOW POSITIONED AT POINT LEFT OFF AT ABOVE...
!
 5700 CALL read(*9400,*6000,axic,z(1),5,noeor,iamt)
   IF ( z(1)<101 ) GOTO 6100
   IF ( z(1)<=102 ) THEN
      nogo = .TRUE.
      CALL page2(3)
      imsg = 366
      WRITE (nout,99012) ufm , imsg
      WRITE (nout,99013)
   ENDIF
!
!     CHECK HARMONIC NUMBER
!
   nnn = z(3)
   ASSIGN 5800 TO ierrtn
   GOTO 8900
!
!     RING ID CHECK
!
 5800 nnn = z(2)
   ASSIGN 5900 TO ierrtn
   GOTO 9000
!
 5900 z(2) = z(2) + (z(3)+1)*1000000
   z(3) = z(4)
   z(4) = z(5)
   IF ( .NOT.(nogo) ) CALL write(geom(4),z(1),4,noeor)
   GOTO 5700
!
!     WRITE EOR AND PUT BITS IN THE TRAILER
!
 6000 iamt = 0
   ASSIGN 6200 TO iretrn
   GOTO 8400
 6100 CALL page2(3)
   imsg = 1063
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99014) cdtype(35) , cdtype(36)
   nogo = .TRUE.
   GOTO 9300
!
!     SUPAX CARDS
!     ===========
!
 6200 rec(1) = supax(1)
   rec(2) = supax(2)
   rec(3) = supax(3)
   ncard = 19
   rec1(1) = suport(1)
   rec1(2) = suport(2)
   rec1(3) = suport(3)
   ASSIGN 6300 TO icont
   GOTO 3500
!
!     CLOSE GEOM4
!
 6300 i = 4
   ASSIGN 6400 TO iretrn
   GOTO 8800
!
!
!     GEOM1 PROCESSING
!     ================
!
!     OPEN GEOM1
!
 6400 ifile = geom(1)
   i = 1
   op = outrwd
   buff = ibuff2
   ASSIGN 6500 TO iretrn
   GOTO 8600
!
!     GRID CARDS FROM POINTAX AND SECTAX CARDS
!
!     NOPONT = 0 OR 1, DEPENDING ON THE PRESSENCE OF POINTAX CARDS
!     NOSECT = 0 OR 1, DEPENDING ON THE PRESSENCE OF SECTAX  CARDS
!
!     RECORD HEADER FOR GRID CARDS
!
 6500 rec(1) = grid(1)
   rec(2) = grid(2)
   rec(3) = grid(3)
   ASSIGN 6600 TO iheadr
   GOTO 9200
!
 6600 IF ( nosect==0 ) THEN
      IF ( nopont==0 ) GOTO 7700
      GOTO 6900
   ELSEIF ( nopont/=0 ) THEN
!
!     LOCATE SECTAX CARDS, READ SECTAX, CONVERT TO GRID, PUT ON NFILE
!
      nfile = scrtch
!
!     OPEN SCRTCH FILE
!
      i = 5
      op = outrwd
      buff = ibuff3
      ASSIGN 6700 TO iretrn
      GOTO 8600
   ELSE
!
      nfile = geom(1)
   ENDIF
!
 6700 icard = 15
   CALL locate(*7600,z(ibuff1),sectax(1),flag)
   DO
      CALL read(*9400,*6800,axic,z(1),5,noeor,iamt)
      z(2) = 0
      z(6) = csid
      z(7) = 0
      z(8) = 0
      IF ( .NOT.(nogo) ) CALL write(nfile,z(1),8,noeor)
   ENDDO
 6800 IF ( nopont==0 ) GOTO 7700
 6900 icard = 12
   CALL locate(*7600,z(ibuff1),pointx(1),flag)
!
!     READ POINT CARD CONVERT TO GRID CARD AND PUT OUT ON GEOM(1)
!     MERGING GRID CARDS FROM SCRTCH IF NOSECT IS NON-ZERO
!
   IF ( nosect/=0 ) THEN
      IF ( nogo ) GOTO 7700
      CALL close(scrtch,clorwd)
      CALL open(*9500,scrtch,z(ibuff3),inprwd)
      CALL read(*7300,*7300,scrtch,z(9),8,noeor,iamt)
   ENDIF
 7000 CALL read(*9400,*7500,axic,z(1),3,noeor,iamt)
!
!     CONVERT POINTAX CARD
!
   z(2) = 0
   rz(i4) = 0.0
   rz(i5) = 0.0
   z(6) = csid
   z(7) = 0
   z(8) = 0
   IF ( nosect==0 ) THEN
      zpt = 1
      GOTO 7200
   ENDIF
 7100 IF ( z(1)>=z(9) ) THEN
      zpt = 9
   ELSE
      zpt = 1
   ENDIF
 7200 DO WHILE ( .NOT.(nogo) )
      CALL write(geom(1),z(zpt),8,noeor)
      IF ( zpt==1 ) GOTO 7000
      CALL read(*7300,*7300,scrtch,z(9),8,noeor,iamt)
      IF ( nopont/=0 ) GOTO 7100
   ENDDO
   GOTO 7700
 7300 nosect = 0
!
!     CLOSE SCRTCH
!
   i = 5
   ASSIGN 7400 TO iretrn
   GOTO 8800
 7400 IF ( nopont==0 ) GOTO 7700
   zpt = 1
   GOTO 7200
!
 7500 IF ( nosect==0 ) GOTO 7700
   zpt = 9
   nopont = 0
   GOTO 7200
!
 7600 CALL page2(3)
   imsg = 1064
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99006) cdtype(2*icard-1) , cdtype(2*icard)
99006 FORMAT (5X,2A4,' CARD COULD NOT BE LOCATED ON AXIC FILE AS ','EXPECTED.')
   nogo = .TRUE.
!
!     GRID CARDS FROM RING CARDS
!
!     COPY RINGAX CARDS INTO CORE AND TO SCRTCH IF CORE IS EXCEEDED.
!
 7700 CALL locate(*8200,z(ibuff1),ringax(1),flag)
   nwords = (icore/4)*4 - 12
   ibegin = 13
   iscrat = 0
   CALL read(*9400,*7900,axic,z(13),nwords,noeor,iamt)
!
!     FALL HERE IMPLIES CORE IS FULL.. SPILL BALANCE TO SCRTCH FILE.
!
   ion = 0
   iscrat = 0
   IF ( nogo ) GOTO 8200
   CALL open(*9500,scrtch,z(ibuff3),outrwd)
   DO
      CALL read(*9400,*7800,axic,z(1),8,noeor,iamt)
      ion = 1
      CALL write(scrtch,z(1),8,noeor)
   ENDDO
 7800 IF ( (iamt/4)*4/=iamt ) GOTO 8100
   IF ( ion==0 .AND. iamt==0 ) GOTO 8000
   iscrat = 1
   IF ( nogo ) GOTO 8200
   CALL write(scrtch,z(1),iamt,eor)
   CALL close(scrtch,clorwd)
   GOTO 8000
!
 7900 IF ( (iamt/4)*4/=iamt ) GOTO 8100
   nwords = iamt
!
!     NWORDS-WORDS ARE IN CORE AND IF ISCRAT = 1 THERE IS
!     A RECORD OF RINGAX CARDS ON SCRTCH FILE ALSO
!
!     NOW MAKE N+1 PASSES THROUGH THE RING CARDS
!
 8000 IF ( iscrat/=0 ) THEN
      IF ( nogo ) GOTO 8200
      CALL open(*9500,scrtch,z(ibuff3),inprwd)
   ENDIF
   z(2) = 0
   z(5) = 0
   z(6) = csid
   z(8) = 0
   ncards = nwords/4
!
!     27TH WORD OF SYSTEM IS PACKED AND HOLDS NUMBER OF RINGS AND HARMS
!
   mn = nplus1
   isystm(161) = ncards
   nadd = 0
   DO i = 1 , nplus1
      nadd = nadd + 1000000
      ipt = ibegin - 4
!
!     PASS THROUGH THE INCORE CARDS
!
      DO j = 1 , ncards
         ipt = ipt + 4
         z(1) = z(ipt) + nadd
         z(3) = z(ipt+1)
         z(4) = z(ipt+2)
         z(7) = z(ipt+3)
         IF ( .NOT.(nogo) ) CALL write(geom(1),z(1),8,noeor)
      ENDDO
!
!     PASS THROUGH SCRTCH CARDS IF ANY
!
      IF ( nogo ) CYCLE
      IF ( iscrat==0 ) CYCLE
      DO
         CALL read(*9400,*8050,scrtch,z(9),4,noeor,iamt)
         z(1) = z(9) + nadd
         z(3) = z(10)
         z(4) = z(11)
         z(7) = z(12)
         CALL write(geom(1),z(1),8,noeor)
      ENDDO
!
 8050 CALL rewind(scrtch)
   ENDDO
!
!     PUT BITS IN TRAILER AND WRITE EOR FOR GRID CARDS
!
   iamt = 0
   rec(1) = grid(1)
   rec(2) = grid(2)
   rec(3) = grid(3)
   ASSIGN 8200 TO iretrn
   GOTO 8400
 8100 ncard = 14
   ASSIGN 8200 TO ierrtn
!
!     END-OF-RECORD ON AXIC FILE.
!
   CALL page2(3)
   imsg = 1063
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99014) cdtype(2*ncard-1) , cdtype(2*ncard)
   nogo = .TRUE.
   GOTO ierrtn
!
!     SEQGP CARD
!     ==========
!
 8200 rec(1) = seqgp(1)
   rec(2) = seqgp(2)
   rec(3) = seqgp(3)
   ASSIGN 8300 TO iretrn
!
!
!     UTILITY SECTION FOR IFP3
!     AXIS-SYMETRIC-CONICAL-SHELL DATA GENERATOR.
!     ==========================================
!
!     COMMON CODE FOR TRANSFER OF RECORD FROM AXIC FILE TO SOME
!     OTHER FILE
!
   CALL locate(*8500,z(ibuff1),rec(1),flag)
   IF ( nogo ) GOTO 8500
   CALL write(ifile,rec(1),3,noeor)
   DO
      CALL read(*9400,*8400,axic,z(1),icore,noeor,iamt)
      iamt = icore
      CALL write(ifile,z(1),iamt,noeor)
   ENDDO
!
!     CLOSE GEOM1
!
 8300 i = 1
   ASSIGN 9300 TO iretrn
   GOTO 8800
 8400 IF ( .NOT.(nogo) ) THEN
      CALL write(ifile,z(1),iamt,eor)
!
!     PUT BITS IN TRAILER
!
      i1 = (rec(2)-1)/16 + 2
      i2 = rec(2) - (i1-2)*16 + 16
      trail(i1) = orf(trail(i1),two(i2))
   ENDIF
!
 8500 GOTO iretrn
!
!     OPEN A FILE AND GET THE TRAILER
!
 8600 IF ( .NOT.(nogo) ) THEN
      CALL open(*8700,file(i),z(buff),op)
      openfl(i) = 1
      IF ( i<=4 ) THEN
!
!     WRITE THE HEADER RECORD
!
         CALL write(file(i),iname(2*i-1),2,eor)
         trail(1) = file(i)
         CALL rdtrl(trail(1))
      ENDIF
   ENDIF
!
   GOTO iretrn
!
 8700 CALL page2(3)
   imsg = 1061
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99007) file(i) , iname(2*i-1) , iname(2*i) , ifist
99007 FORMAT (5X,11HFILE NUMBER,I4,3H ( ,2A4,12H) IS NOT IN ,A4)
   nogo = .TRUE.
   GOTO 9300
!
!     CLOSE A FILE
!
 8800 IF ( openfl(i)/=0 ) THEN
      IF ( i<=4 ) CALL write(file(i),t65535(1),3,eor)
      CALL close(file(i),clorwd)
      openfl(i) = 0
      IF ( i<=4 ) CALL wrttrl(trail(1))
   ENDIF
   GOTO iretrn
!
!     HARMONIC NUMBER,  ON CARD TYPE ..... IS OUT OF RANGE 0 TO 998
!
 8900 IF ( nnn<999 .AND. nnn>=0 ) GOTO ierrtn
   CALL page2(3)
   imsg = 364
   WRITE (nout,99012) ufm , imsg
   WRITE (nout,99008) nnn , cdtype(2*ncard-1) , cdtype(2*ncard)
99008 FORMAT (5X,'HARMONIC NUMBER',I6,4H ON ,2A4,' CARD OUT OF 0 TO ','998 ALLOWABLE RANGE')
   nogo = .TRUE.
   GOTO ierrtn
!
!     RING ID OUT PERMISSABLE RANGE OF 1 TO 999999
!
 9000 IF ( nnn>0 .AND. nnn<=999999 ) GOTO ierrtn
   CALL page2(3)
   imsg = 365
   WRITE (nout,99012) ufm , imsg
   WRITE (nout,99009) nnn , cdtype(2*ncard-1) , cdtype(2*ncard)
99009 FORMAT (5X,'RING ID',I10,4H ON ,2A4,' CARD OUT OF 0 TO 999999',' ALLOWABLE RANGE')
   nogo = .TRUE.
   GOTO ierrtn
!
!     CHECK BIT-IBIT IN TRAILER AND RETURN NON = ZERO OR NON-ZERO
!
 9100 i1 = (ibit-1)/16 + 2
   i2 = ibit - (i1-2)*16 + 16
   non = andf(axtrl(i1),two(i2))
   GOTO ibitr
!
!     WRITE 3 WORD RECORD HEADER
!
 9200 IF ( .NOT.(nogo) ) CALL write(ifile,rec(1),3,noeor)
   GOTO iheadr
!
!     RETURN TO IFP3
!
 9300 RETURN
!
!     EOF ENCOUNTERED READING AXIC FILE
!
 9400 nfile = axic
   CALL page2(3)
   imsg = 3002
   WRITE (nout,99011) sfm , imsg
   WRITE (nout,99010) iname(11) , iname(12) , nfile
99010 FORMAT (5X,'EOF ENCOUNTERED WHILE READING DATA SET ',2A4,' (FILE',I4,') IN SUBROUTINE IFP3B')
   nogo = .TRUE.
   GOTO 9300
!
 9500 i = 5
   GOTO 8700
99011 FORMAT (A25,I5)
99012 FORMAT (A23,I5)
99013 FORMAT (5X,'SPCAX OR MPCAX CARD HAS A SETID = 101 OR 102.  101 ','AND 102 ARE SYSTEM ID-S RESERVED FOR SINE AND COSINE SETS')
99014 FORMAT (5X,'EOR ON AXIC FILE WHILE READING ',2A4,'CARD RECORDS.')
END SUBROUTINE ifp3b
