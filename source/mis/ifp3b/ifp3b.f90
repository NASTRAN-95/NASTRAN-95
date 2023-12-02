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
   USE C_BLANK
   USE C_CONDAS
   USE C_IFP3CM
   USE C_IFP3LV
   USE C_IFPDTA
   USE C_MACHIN
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
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
   Ifile = geom(4)
   i = 4
   Op = outrwd
   Buff = Ibuff2
   ASSIGN 100 TO Iretrn
   GOTO 8600
!
!     SPCADD OR MPCADD CARDS
!     ======================
!
 100  ASSIGN 800 TO Icont
   Rec(1) = Mpcadd(1)
   Rec(2) = Mpcadd(2)
   Rec(3) = Mpcadd(3)
   Rec1(1) = Mpcax(1)
   Rec1(2) = Mpcax(2)
   Rec1(3) = Mpcax(3)
 200  ASSIGN 300 TO Iheadr
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
   IF ( .NOT.(Nogo) ) CALL write(geom(4),z(1),12,noeor)
   CALL locate(*400,z(Ibuff1),Rec(1),Flag)
!
!     READ AN OPEN ENDED SPCADD OR MPCADD CARD INTO CORE.
!
   i = 1
   DO
      CALL read(*9400,*400,axic,z(i),1,noeor,Iamt)
      IF ( z(i)<0 ) THEN
!
!     COMPLETE CARD IS AT HAND
!
         z(i) = 101
         i = i + 1
         z(i) = -1
         z(1) = z(1) + 100000000
         IF ( Nogo ) GOTO Icont
         DO
            CALL write(geom(4),z(1),i,noeor)
            IF ( z(i-1)==102 ) GOTO 400
            z(i-1) = 102
            z(1) = z(1) + 100000000
         ENDDO
      ELSE
         i = i + 1
         IF ( (i+1)>Icore ) THEN
!
            CALL page2(3)
            imsg = 363
            WRITE (Nout,99012) Ufm , imsg
            WRITE (Nout,99001)
99001       FORMAT (5X,'INSUFFICIENT CORE TO PROCESS AXIC DATA IN SUBROUTINE','IFP3B')
            Nogo = .TRUE.
            GOTO 9300
         ENDIF
      ENDIF
   ENDDO
!
!     ALL SPCADD OR MPCADD CARDS COMPLETE.
!     NOW CREATE SPCADD OR MPCADD FROM SPCAX OR MPCAX
!     CARDS RESPECTIVELY.
!
 400  irec = Rec(1)
   Rec(1) = Rec1(1)
   Rec(2) = Rec1(2)
   Rec(3) = Rec1(3)
   CALL locate(*600,z(Ibuff1),Rec(1),Flag)
!
!     OK SPCAX OR MPCAX RECORD EXISTS.
!
   ilast = -1
 500  z(4) = -1
   CALL read(*9400,*600,axic,z(2),1,noeor,Iamt)
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
      IF ( Nogo ) GOTO Icont
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
      CALL read(*9400,*600,axic,z(1),4,noeor,Iamt)
      IF ( Rec(1)==Spcax(1) .OR. z(1)==(-1) ) GOTO 500
   ENDDO
!
!     ALL CARDS COMPLETE.
!     WRITE EOR AND PUT BITS IN TRAILER.
!
 600  Iamt = 0
   ASSIGN 700 TO Iretrn
   IF ( irec==Spcadd(1) ) THEN
      Rec(1) = Spcadd(1)
      Rec(2) = Spcadd(2)
      Rec(3) = Spcadd(3)
   ELSE
      Rec(1) = Mpcadd(1)
      Rec(2) = Mpcadd(2)
      Rec(3) = Mpcadd(3)
   ENDIF
   GOTO 8400
 700  GOTO Icont
!
!     MPCAX CARD
!     ==========
!
 800  Mpcon = 0
   Rec(1) = Mpc(1)
   Rec(2) = Mpc(2)
   Rec(3) = Mpc(3)
   Recoff = .FALSE.
   Last = -1
   Ncard = 10
   Nwords = 0
   CALL locate(*1600,z(Ibuff1),Mpcax(1),Flag)
!
!     WRITE RECORD HEADER
!
   Recoff = .TRUE.
   ASSIGN 900 TO Iheadr
   GOTO 9200
!
 900  Mpcon = 1
   Last = 0
!
!     READ SET ID
!
 1000 CALL read(*9400,*1500,axic,Setid,1,noeor,Iamt)
   IF ( Setid>100 ) GOTO 1600
   Nwords = Nwords + 1
   IF ( .NOT.(Nogo) ) CALL write(geom(4),Setid,1,noeor)
!
!     READ 4-WORDS SETS UNTIL -1,-1,-1,-1 ENCOUNTERED...
!
 1100 CALL read(*9400,*1400,axic,z(1),4,noeor,Iamt)
   Nwords = Nwords + 4
   IF ( z(4)==-1 ) THEN
!
!     END OF EQUATION
!
      IF ( .NOT.(Nogo) ) CALL write(geom(4),Neg111(1),3,noeor)
      GOTO 1000
   ELSE
!
!     CHECK HARMONIC NUMBER
!
      Nnn = z(2)
      ASSIGN 1200 TO Ierrtn
      GOTO 8900
   ENDIF
!
!     CHECK RING ID
!
 1200 Nnn = z(1)
   ASSIGN 1300 TO Ierrtn
   GOTO 9000
!
 1300 z(2) = z(1) + (z(2)+1)*1000000
   IF ( .NOT.(Nogo) ) CALL write(geom(4),z(2),3,noeor)
   GOTO 1100
 1400 CALL page2(3)
   imsg = 1063
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99002) Sfm , imsg
99002 FORMAT (5X,50HEOR ON AXIC FILE WHILE READING MPCAX CARD RECORDS.)
   Nogo = .TRUE.
   GOTO 9300
 1500 Last = 1
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
 1600 Rec(1) = Pointx(1)
   Rec(2) = Pointx(2)
   Rec(3) = Pointx(3)
   Ncard = 12
   N3or5 = 3
   K3or6 = 6
   Sorc = 101
   ASSIGN 2400 TO Icont
!     TURN NOPONT OR NOSECT ON IF POINTAX OR SECTAX CARDS EXIST RESPECT.
!
   Ibit = Pointx(2)
   ASSIGN 1700 TO Ibitr
   GOTO 9100
 1700 Nopont = Non
   Ibit = Sectax(2)
   ASSIGN 1800 TO Ibitr
   GOTO 9100
 1800 Nosect = Non
!
   IF ( Nopont==0 ) GOTO 2300
!
 1900 CALL locate(*2300,z(Ibuff1),Rec(1),Flag)
   Mpcon = 1
   IF ( .NOT.(Recoff) ) THEN
!
!     WRITE RECORD HEADER
!
      Recoff = .TRUE.
      Rec(1) = Mpc(1)
      Rec(2) = Mpc(2)
      Rec(3) = Mpc(3)
      ASSIGN 2000 TO Iheadr
      GOTO 9200
   ENDIF
!
 2000 CALL read(*9400,*2300,axic,z(1),N3or5,noeor,Iamt)
!
!     CHECK RING ID FOR S-SET PASS ONLY FOR POINTAX AND SECTAX CARDS.
!     NO CHECK WILL BE MADE IN THE GRID CARD GENERATION AREA.
!
!     IF (SORC .EQ. 102) GO TO 785
   Nnn = z(2)
   ASSIGN 2100 TO Ierrtn
   GOTO 9000
!
 2100 Iat = N3or5 + 1
   DO i = 1 , K3or6
      z(Iat) = Sorc
      z(Iat+1) = z(1)
      z(Iat+2) = i
      Rz(Iat+3) = -1.0
      IF ( .NOT.(Nogo) ) CALL write(geom(4),z(Iat),4,noeor)
      DO j = 1 , Nplus1
!
!     COMPUTE COEFFICIENT.
!
         Ni = j - 1
         IF ( N3or5==5 ) THEN
!
!     SECTAX CARD COEFFICIENTS
!
            T1 = Ni*Rz(i4)*raddeg
            T2 = Ni*Rz(i5)*raddeg
            IF ( i>=4 ) THEN
               IF ( andf(i,1)<=0 ) GOTO 2140
            ELSEIF ( andf(i,1)>0 ) THEN
               GOTO 2140
            ENDIF
!
!     EVEN I
!
            IF ( Sorc==101 ) GOTO 2160
         ELSE
!
!     POINTAX CARD COEFFICIENTS
!
            T1 = Ni*Rz(i3)*raddeg
            IF ( andf(i,1)<=0 ) THEN
!
!     EVEN I
!
               IF ( Sorc>101 ) THEN
!
                  Coef = sin(T1)
                  GOTO 2200
               ENDIF
!
!     ODD I
!
            ELSEIF ( Sorc<=101 ) THEN
               Coef = sin(T1)
               GOTO 2200
            ENDIF
!
            Coef = cos(T1)
            IF ( Sorc==101 ) Coef = -Coef
            IF ( Ni==0.0 .AND. Sorc==101 ) Coef = 1.0
            GOTO 2200
         ENDIF
 2120    IF ( Ni/=0 ) THEN
            t3 = T2
            T2 = cos(T1)
            T1 = cos(t3)
            GOTO 2180
         ELSE
            Coef = 0.0
            GOTO 2200
         ENDIF
!
!     ODD I
!
 2140    IF ( Sorc==101 ) GOTO 2120
 2160    IF ( Ni/=0 ) THEN
            T1 = sin(T1)
            T2 = sin(T2)
         ELSE
            Coef = Rz(i3)*(Rz(i5)-Rz(i4))*raddeg
            GOTO 2200
         ENDIF
 2180    Coef = Rz(i3)*(T2-T1)/Ni
         IF ( Sorc==101 .AND. (i==2 .OR. i==5) ) Coef = -Coef
!
 2200    z(Iat) = z(2) + j*1000000
         z(Iat+1) = i
         Rz(Iat+2) = Coef
         IF ( .NOT.(Nogo) ) CALL write(geom(4),z(Iat),3,noeor)
      ENDDO
      IF ( .NOT.(Nogo) ) CALL write(geom(4),Neg111(1),3,noeor)
   ENDDO
   GOTO 2000
!
 2300 GOTO Icont
!
!     S-SET MPC-S FROM SECTAX CARDS
!     =============================
!
!     DO SECTAX CARDS FOR S-SET.
!
 2400 Rec(1) = Sectax(1)
   Rec(2) = Sectax(2)
   Rec(3) = Sectax(3)
   N3or5 = 5
   K3or6 = 6
   Sorc = 101
   Ncard = 15
   ASSIGN 2500 TO Icont
   IF ( Nosect/=0 ) GOTO 1900
!
!     C-SET MPC-S FROM POINTAX CARDS
!     ==============================
!
!
 2500 Rec(1) = Pointx(1)
   Rec(2) = Pointx(2)
   Rec(3) = Pointx(3)
   N3or5 = 3
   K3or6 = 6
   Sorc = 102
   ASSIGN 2600 TO Icont
   IF ( Nopont/=0 ) GOTO 1900
!
!     C-SET MPC-S FROM SECTAX CARDS
!     =============================
!
 2600 Rec(1) = Sectax(1)
   Rec(2) = Sectax(2)
   Rec(3) = Sectax(3)
   N3or5 = 5
   K3or6 = 6
   Sorc = 102
   ASSIGN 2700 TO Icont
   IF ( Nosect/=0 ) GOTO 1900
!
!     BALANCE OF MPCAX CARDS
!
 2700 IF ( Last/=0 ) GOTO 3300
   CALL locate(*3300,z(Ibuff1),Mpcax(1),Flag)
   Ncard = 10
   IF ( Nwords/=0 ) THEN
      DO i = 1 , Nwords
         CALL read(*9400,*3100,axic,z(1),1,noeor,Iamt)
      ENDDO
   ENDIF
!
!     NOW POSITIONED AT POINT LEFT OFF AT ABOVE.
!
 2800 CALL read(*9400,*3300,axic,Setid,1,noeor,Iamt)
   IF ( Setid<101 ) GOTO 3100
   IF ( Setid<=102 ) THEN
      Nogo = .TRUE.
      CALL page2(3)
      imsg = 366
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99013)
   ENDIF
   IF ( .NOT.(Nogo) ) CALL write(geom(4),Setid,1,noeor)
 2900 CALL read(*9400,*1400,axic,z(1),4,noeor,Iamt)
   IF ( z(4)==(-1) ) THEN
!
!     END OF EQUATION
!
      IF ( .NOT.(Nogo) ) CALL write(geom(4),Neg111(1),3,noeor)
      GOTO 2800
   ELSE
!
!     CHECK HARMONIC NUMBER
!
      Nnn = z(2)
      ASSIGN 3000 TO Ierrtn
      GOTO 8900
   ENDIF
!
!     CHECK RING ID
!
 3000 Nnn = z(1)
   ASSIGN 3200 TO Ierrtn
   GOTO 9000
 3100 CALL page2(3)
   imsg = 1063
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99014) Cdtype(19) , Cdtype(20)
   Nogo = .TRUE.
   GOTO 9300
!
 3200 z(2) = z(1) + (z(2)+1)*1000000
   IF ( .NOT.(Nogo) ) CALL write(geom(4),z(2),3,noeor)
   GOTO 2900
!
!     AT 713(?) WRITE EOR AND PUT BITS IN TRAILER.
!
 3300 IF ( Mpcon/=0 ) THEN
      Iamt = 0
      Rec(1) = Mpc(1)
      Rec(2) = Mpc(2)
      Rec(3) = Mpc(3)
      ASSIGN 3400 TO Iretrn
      GOTO 8400
   ENDIF
!
!     OMITAX CARDS
!
 3400 Rec(1) = Omitax(1)
   Rec(2) = Omitax(2)
   Rec(3) = Omitax(3)
   Ncard = 11
   Rec1(1) = Omit(1)
   Rec1(2) = Omit(2)
   Rec1(3) = Omit(3)
   ASSIGN 4100 TO Icont
 3500 CALL locate(*4000,z(Ibuff1),Rec(1),Flag)
   IF ( .NOT.(Nogo) ) CALL write(geom(4),Rec1(1),3,noeor)
 3600 CALL read(*9400,*3900,axic,z(1),3,noeor,Iamt)
!
!     CHECK HARMONIC NUMBER
!
   Nnn = z(2)
   ASSIGN 3700 TO Ierrtn
   GOTO 8900
!
!     CHECK RING ID
!
 3700 Nnn = z(1)
   ASSIGN 3800 TO Ierrtn
   GOTO 9000
!
 3800 z(2) = z(1) + (z(2)+1)*1000000
   IF ( ifpdco(z(3)) ) THEN
      Nogo = .TRUE.
      CALL page2(3)
      imsg = 367
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99003) z(3) , Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99003 FORMAT (5X,'COMPONENT SPECIFICATION',I8,4H ON ,2A4,' CARD IS INCORRECT')
   ELSE
      DO l2 = 1 , 6
         IF ( Ll(l2)/=0 ) THEN
            z(3) = Ll(l2)
            IF ( Nogo ) EXIT
            CALL write(geom(4),z(2),2,noeor)
         ENDIF
      ENDDO
   ENDIF
   GOTO 3600
!
!     WRITE EOR AND PUT BITS IN TRAILER
!
 3900 Iamt = 0
   Rec(1) = Rec1(1)
   Rec(2) = Rec1(2)
   Rec(3) = Rec1(3)
   ASSIGN 4000 TO Iretrn
   GOTO 8400
 4000 GOTO Icont
!
!     SPCADD CARD
!     ===========
!
 4100 Rec(1) = Spcadd(1)
   Rec(2) = Spcadd(2)
   Rec(3) = Spcadd(3)
   Rec1(1) = Spcax(1)
   Rec1(2) = Spcax(2)
   Rec1(3) = Spcax(3)
   ASSIGN 4200 TO Icont
   GOTO 200
!
!     SPCAX CARD
!     ==========
!
 4200 Rec(1) = Spc(1)
   Rec(2) = Spc(2)
   Rec(3) = Spc(3)
!
!     RECORD HEADER FOR SPC-S
!
   ASSIGN 4300 TO Iheadr
   GOTO 9200
!
 4300 Last = -1
   Ncard = 18
   CALL locate(*4800,z(Ibuff1),Spcax(1),Flag)
   Last = 0
   Nwords = 0
 4400 CALL read(*9400,*4700,axic,z(1),5,noeor,Iamt)
   IF ( z(1)>100 ) GOTO 4800
   Nwords = Nwords + 5
!
!     ALTER CARD JUST READ AND OUTPUT
!
!     CHECK HARMONIC NUMBER
!
   Nnn = z(3)
   ASSIGN 4500 TO Ierrtn
   GOTO 8900
!
!     CHECK RING ID
!
 4500 Nnn = z(2)
   ASSIGN 4600 TO Ierrtn
   GOTO 9000
!
 4600 z(2) = z(2) + (z(3)+1)*1000000
   z(3) = z(4)
   z(4) = z(5)
!
   IF ( .NOT.(Nogo) ) CALL write(geom(4),z(1),4,noeor)
   GOTO 4400
 4700 Last = 1
!
!     FIRST NWORDS HAVE BEEN PROCESSED OF SPCAX CARDS
!     UNLESS LAST = 1, IN WHICH CASE ALL SPCAX CARDS ARE COMPLETE.
!     IF LAST = -1, THERE ARE NO SPCAX CARDS
!
!     S-SET AND C-SET SPC-S FROM RINGAX CARDS
!     =======================================
!
 4800 Sorc = 101
   Ncard = 14
   Compon = 135
   IF ( Iconso==1 ) Compon = 13
   ASSIGN 5400 TO Icont
 4900 CALL locate(*5500,z(Ibuff1),Ringax(1),Flag)
 5000 CALL read(*9400,*5300,axic,z(1),4,noeor,Iamt)
!
   IF ( Sorc==102 ) GOTO 5200
!
!     GIVE RING CARD A CHECK FOR MINIMUM DATA.
!
!     CHECK RING ID
!
   Nnn = z(1)
   ASSIGN 5100 TO Ierrtn
   GOTO 9000
!
!     CHECK FOR NON-ZERO RADIUS
!
 5100 IF ( Rz(i3-1)==0 ) THEN
      CALL page2(3)
      imsg = 368
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99004) z(1)
99004 FORMAT (5X,'RINGAX CARD WITH RING ID =',I10,' HAS A ZERO RADIUS',' SPECIFIED.')
      Nogo = .TRUE.
   ENDIF
 5200 z(4) = 0
   z(3) = Compon
   z(2) = z(1) + 1000000
   z(1) = Sorc
   IF ( .NOT.(Nogo) ) CALL write(geom(4),z(1),4,noeor)
   GOTO 5000
!
 5300 GOTO Icont
 5400 Sorc = 102
   Compon = 246
   IF ( Iconso==1 ) Compon = 2
!
!     KEEP DOF 4 FOR PIEZOELECTRIC PROBLEM
!
   IF ( Ipiez==1 ) Compon = 26
   ASSIGN 5600 TO Icont
   GOTO 4900
!
!     MISSING REQUIRED CARD
!
 5500 ASSIGN 5600 TO Ierrtn
!
!     MISSING REQUIRED CARD
!
   CALL page2(3)
   imsg = 362
   WRITE (Nout,99012) Ufm , imsg
   WRITE (Nout,99005) Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99005 FORMAT (5X,'MINIMUM PROBLEM REQUIRES ',2A4,' CARD.  NONE FOUND.')
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     BALANCE OF SPCAX CARDS
!
 5600 IF ( Last/=0 ) GOTO 6000
   CALL locate(*6000,z(Ibuff1),Spcax(1),Flag)
   Ncard = 18
   IF ( Nwords/=0 ) THEN
      DO i = 1 , Nwords , 5
         CALL read(*9400,*6100,axic,z(1),5,noeor,Iamt)
      ENDDO
   ENDIF
!
!     NOW POSITIONED AT POINT LEFT OFF AT ABOVE...
!
 5700 CALL read(*9400,*6000,axic,z(1),5,noeor,Iamt)
   IF ( z(1)<101 ) GOTO 6100
   IF ( z(1)<=102 ) THEN
      Nogo = .TRUE.
      CALL page2(3)
      imsg = 366
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99013)
   ENDIF
!
!     CHECK HARMONIC NUMBER
!
   Nnn = z(3)
   ASSIGN 5800 TO Ierrtn
   GOTO 8900
!
!     RING ID CHECK
!
 5800 Nnn = z(2)
   ASSIGN 5900 TO Ierrtn
   GOTO 9000
!
 5900 z(2) = z(2) + (z(3)+1)*1000000
   z(3) = z(4)
   z(4) = z(5)
   IF ( .NOT.(Nogo) ) CALL write(geom(4),z(1),4,noeor)
   GOTO 5700
!
!     WRITE EOR AND PUT BITS IN THE TRAILER
!
 6000 Iamt = 0
   ASSIGN 6200 TO Iretrn
   GOTO 8400
 6100 CALL page2(3)
   imsg = 1063
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99014) Cdtype(35) , Cdtype(36)
   Nogo = .TRUE.
   GOTO 9300
!
!     SUPAX CARDS
!     ===========
!
 6200 Rec(1) = Supax(1)
   Rec(2) = Supax(2)
   Rec(3) = Supax(3)
   Ncard = 19
   Rec1(1) = Suport(1)
   Rec1(2) = Suport(2)
   Rec1(3) = Suport(3)
   ASSIGN 6300 TO Icont
   GOTO 3500
!
!     CLOSE GEOM4
!
 6300 i = 4
   ASSIGN 6400 TO Iretrn
   GOTO 8800
!
!
!     GEOM1 PROCESSING
!     ================
!
!     OPEN GEOM1
!
 6400 Ifile = geom(1)
   i = 1
   Op = outrwd
   Buff = Ibuff2
   ASSIGN 6500 TO Iretrn
   GOTO 8600
!
!     GRID CARDS FROM POINTAX AND SECTAX CARDS
!
!     NOPONT = 0 OR 1, DEPENDING ON THE PRESSENCE OF POINTAX CARDS
!     NOSECT = 0 OR 1, DEPENDING ON THE PRESSENCE OF SECTAX  CARDS
!
!     RECORD HEADER FOR GRID CARDS
!
 6500 Rec(1) = Grid(1)
   Rec(2) = Grid(2)
   Rec(3) = Grid(3)
   ASSIGN 6600 TO Iheadr
   GOTO 9200
!
 6600 IF ( Nosect==0 ) THEN
      IF ( Nopont/=0 ) GOTO 6900
      GOTO 7700
   ELSEIF ( Nopont/=0 ) THEN
!
!     LOCATE SECTAX CARDS, READ SECTAX, CONVERT TO GRID, PUT ON NFILE
!
      Nfile = scrtch
!
!     OPEN SCRTCH FILE
!
      i = 5
      Op = outrwd
      Buff = Ibuff3
      ASSIGN 6700 TO Iretrn
      GOTO 8600
   ELSE
!
      Nfile = geom(1)
   ENDIF
!
 6700 icard = 15
   CALL locate(*7600,z(Ibuff1),Sectax(1),Flag)
   DO
      CALL read(*9400,*6800,axic,z(1),5,noeor,Iamt)
      z(2) = 0
      z(6) = Csid
      z(7) = 0
      z(8) = 0
      IF ( .NOT.(Nogo) ) CALL write(Nfile,z(1),8,noeor)
   ENDDO
 6800 IF ( Nopont==0 ) GOTO 7700
 6900 icard = 12
   CALL locate(*7600,z(Ibuff1),Pointx(1),Flag)
!
!     READ POINT CARD CONVERT TO GRID CARD AND PUT OUT ON GEOM(1)
!     MERGING GRID CARDS FROM SCRTCH IF NOSECT IS NON-ZERO
!
   IF ( Nosect/=0 ) THEN
      IF ( Nogo ) GOTO 7700
      CALL close(scrtch,clorwd)
      CALL open(*9500,scrtch,z(Ibuff3),inprwd)
      CALL read(*7300,*7300,scrtch,z(9),8,noeor,Iamt)
   ENDIF
 7000 CALL read(*9400,*7500,axic,z(1),3,noeor,Iamt)
!
!     CONVERT POINTAX CARD
!
   z(2) = 0
   Rz(i4) = 0.0
   Rz(i5) = 0.0
   z(6) = Csid
   z(7) = 0
   z(8) = 0
   IF ( Nosect==0 ) THEN
      Zpt = 1
      GOTO 7200
   ENDIF
 7100 IF ( z(1)>=z(9) ) THEN
      Zpt = 9
   ELSE
      Zpt = 1
   ENDIF
 7200 DO WHILE ( .NOT.(Nogo) )
      CALL write(geom(1),z(Zpt),8,noeor)
      IF ( Zpt==1 ) GOTO 7000
      CALL read(*7300,*7300,scrtch,z(9),8,noeor,Iamt)
      IF ( Nopont/=0 ) GOTO 7100
   ENDDO
   GOTO 7700
 7300 Nosect = 0
!
!     CLOSE SCRTCH
!
   i = 5
   ASSIGN 7400 TO Iretrn
   GOTO 8800
 7400 IF ( Nopont==0 ) GOTO 7700
   Zpt = 1
   GOTO 7200
!
 7500 IF ( Nosect==0 ) GOTO 7700
   Zpt = 9
   Nopont = 0
   GOTO 7200
!
 7600 CALL page2(3)
   imsg = 1064
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99006) Cdtype(2*icard-1) , Cdtype(2*icard)
99006 FORMAT (5X,2A4,' CARD COULD NOT BE LOCATED ON AXIC FILE AS ','EXPECTED.')
   Nogo = .TRUE.
!
!     GRID CARDS FROM RING CARDS
!
!     COPY RINGAX CARDS INTO CORE AND TO SCRTCH IF CORE IS EXCEEDED.
!
 7700 CALL locate(*8200,z(Ibuff1),Ringax(1),Flag)
   Nwords = (Icore/4)*4 - 12
   Ibegin = 13
   Iscrat = 0
   CALL read(*9400,*7900,axic,z(13),Nwords,noeor,Iamt)
!
!     FALL HERE IMPLIES CORE IS FULL.. SPILL BALANCE TO SCRTCH FILE.
!
   Ion = 0
   Iscrat = 0
   IF ( Nogo ) GOTO 8200
   CALL open(*9500,scrtch,z(Ibuff3),outrwd)
   DO
      CALL read(*9400,*7800,axic,z(1),8,noeor,Iamt)
      Ion = 1
      CALL write(scrtch,z(1),8,noeor)
   ENDDO
 7800 IF ( (Iamt/4)*4/=Iamt ) GOTO 8100
   IF ( Ion==0 .AND. Iamt==0 ) GOTO 8000
   Iscrat = 1
   IF ( Nogo ) GOTO 8200
   CALL write(scrtch,z(1),Iamt,eor)
   CALL close(scrtch,clorwd)
   GOTO 8000
!
 7900 IF ( (Iamt/4)*4/=Iamt ) GOTO 8100
   Nwords = Iamt
!
!     NWORDS-WORDS ARE IN CORE AND IF ISCRAT = 1 THERE IS
!     A RECORD OF RINGAX CARDS ON SCRTCH FILE ALSO
!
!     NOW MAKE N+1 PASSES THROUGH THE RING CARDS
!
 8000 IF ( Iscrat/=0 ) THEN
      IF ( Nogo ) GOTO 8200
      CALL open(*9500,scrtch,z(Ibuff3),inprwd)
   ENDIF
   z(2) = 0
   z(5) = 0
   z(6) = Csid
   z(8) = 0
   Ncards = Nwords/4
!
!     27TH WORD OF SYSTEM IS PACKED AND HOLDS NUMBER OF RINGS AND HARMS
!
   Mn = Nplus1
   isystm(161) = Ncards
   Nadd = 0
   DO i = 1 , Nplus1
      Nadd = Nadd + 1000000
      Ipt = Ibegin - 4
!
!     PASS THROUGH THE INCORE CARDS
!
      DO j = 1 , Ncards
         Ipt = Ipt + 4
         z(1) = z(Ipt) + Nadd
         z(3) = z(Ipt+1)
         z(4) = z(Ipt+2)
         z(7) = z(Ipt+3)
         IF ( .NOT.(Nogo) ) CALL write(geom(1),z(1),8,noeor)
      ENDDO
!
!     PASS THROUGH SCRTCH CARDS IF ANY
!
      IF ( Nogo ) CYCLE
      IF ( Iscrat==0 ) CYCLE
      DO
         CALL read(*9400,*8050,scrtch,z(9),4,noeor,Iamt)
         z(1) = z(9) + Nadd
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
   Iamt = 0
   Rec(1) = Grid(1)
   Rec(2) = Grid(2)
   Rec(3) = Grid(3)
   ASSIGN 8200 TO Iretrn
   GOTO 8400
 8100 Ncard = 14
   ASSIGN 8200 TO Ierrtn
!
!     END-OF-RECORD ON AXIC FILE.
!
   CALL page2(3)
   imsg = 1063
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99014) Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     SEQGP CARD
!     ==========
!
 8200 Rec(1) = Seqgp(1)
   Rec(2) = Seqgp(2)
   Rec(3) = Seqgp(3)
   ASSIGN 8300 TO Iretrn
!
!
!     UTILITY SECTION FOR IFP3
!     AXIS-SYMETRIC-CONICAL-SHELL DATA GENERATOR.
!     ==========================================
!
!     COMMON CODE FOR TRANSFER OF RECORD FROM AXIC FILE TO SOME
!     OTHER FILE
!
   CALL locate(*8500,z(Ibuff1),Rec(1),Flag)
   IF ( Nogo ) GOTO 8500
   CALL write(Ifile,Rec(1),3,noeor)
   DO
      CALL read(*9400,*8400,axic,z(1),Icore,noeor,Iamt)
      Iamt = Icore
      CALL write(Ifile,z(1),Iamt,noeor)
   ENDDO
!
!     CLOSE GEOM1
!
 8300 i = 1
   ASSIGN 9300 TO Iretrn
   GOTO 8800
 8400 IF ( .NOT.(Nogo) ) THEN
      CALL write(Ifile,z(1),Iamt,eor)
!
!     PUT BITS IN TRAILER
!
      I1 = (Rec(2)-1)/16 + 2
      I2 = Rec(2) - (I1-2)*16 + 16
      Trail(I1) = orf(Trail(I1),Two(I2))
   ENDIF
!
 8500 GOTO Iretrn
!
!     OPEN A FILE AND GET THE TRAILER
!
 8600 IF ( .NOT.(Nogo) ) THEN
      CALL open(*8700,File(i),z(Buff),Op)
      Openfl(i) = 1
      IF ( i<=4 ) THEN
!
!     WRITE THE HEADER RECORD
!
         CALL write(File(i),Iname(2*i-1),2,eor)
         Trail(1) = File(i)
         CALL rdtrl(Trail(1))
      ENDIF
   ENDIF
!
   GOTO Iretrn
!
 8700 CALL page2(3)
   imsg = 1061
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99007) File(i) , Iname(2*i-1) , Iname(2*i) , ifist
99007 FORMAT (5X,11HFILE NUMBER,I4,3H ( ,2A4,12H) IS NOT IN ,A4)
   Nogo = .TRUE.
   GOTO 9300
!
!     CLOSE A FILE
!
 8800 IF ( Openfl(i)/=0 ) THEN
      IF ( i<=4 ) CALL write(File(i),T65535(1),3,eor)
      CALL close(File(i),clorwd)
      Openfl(i) = 0
      IF ( i<=4 ) CALL wrttrl(Trail(1))
   ENDIF
   GOTO Iretrn
!
!     HARMONIC NUMBER,  ON CARD TYPE ..... IS OUT OF RANGE 0 TO 998
!
 8900 IF ( Nnn<999 .AND. Nnn>=0 ) GOTO Ierrtn
   CALL page2(3)
   imsg = 364
   WRITE (Nout,99012) Ufm , imsg
   WRITE (Nout,99008) Nnn , Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99008 FORMAT (5X,'HARMONIC NUMBER',I6,4H ON ,2A4,' CARD OUT OF 0 TO ','998 ALLOWABLE RANGE')
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     RING ID OUT PERMISSABLE RANGE OF 1 TO 999999
!
 9000 IF ( Nnn>0 .AND. Nnn<=999999 ) GOTO Ierrtn
   CALL page2(3)
   imsg = 365
   WRITE (Nout,99012) Ufm , imsg
   WRITE (Nout,99009) Nnn , Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99009 FORMAT (5X,'RING ID',I10,4H ON ,2A4,' CARD OUT OF 0 TO 999999',' ALLOWABLE RANGE')
   Nogo = .TRUE.
   GOTO Ierrtn
!
!     CHECK BIT-IBIT IN TRAILER AND RETURN NON = ZERO OR NON-ZERO
!
 9100 I1 = (Ibit-1)/16 + 2
   I2 = Ibit - (I1-2)*16 + 16
   Non = andf(Axtrl(I1),Two(I2))
   GOTO Ibitr
!
!     WRITE 3 WORD RECORD HEADER
!
 9200 IF ( .NOT.(Nogo) ) CALL write(Ifile,Rec(1),3,noeor)
   GOTO Iheadr
!
!     RETURN TO IFP3
!
 9300 RETURN
!
!     EOF ENCOUNTERED READING AXIC FILE
!
 9400 Nfile = axic
   CALL page2(3)
   imsg = 3002
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99010) Iname(11) , Iname(12) , Nfile
99010 FORMAT (5X,'EOF ENCOUNTERED WHILE READING DATA SET ',2A4,' (FILE',I4,') IN SUBROUTINE IFP3B')
   Nogo = .TRUE.
   GOTO 9300
!
 9500 i = 5
   GOTO 8700
99011 FORMAT (A25,I5)
99012 FORMAT (A23,I5)
99013 FORMAT (5X,'SPCAX OR MPCAX CARD HAS A SETID = 101 OR 102.  101 ','AND 102 ARE SYSTEM ID-S RESERVED FOR SINE AND COSINE SETS')
99014 FORMAT (5X,'EOR ON AXIC FILE WHILE READING ',2A4,'CARD RECORDS.')
END SUBROUTINE ifp3b
