
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
   REAL A1 , A2 , A3 , A4 , Angle , Coef , Consts(5) , Gc(7) , Ni , Nisq , Nphi , Nphi1 , Raddeg , Rz(1) , Sum , T1 , T2
   INTEGER Axic , Axic1(3) , Axtrl(7) , Bottom , Buff , Cconex(3) , Cdtype(50) , Clorwd , Compon , Csid , Csset , Ctrapa(3) ,       &
         & Ctriaa(3) , Ddd(14) , Dum(521) , Dum50(50) , Dumdum(8) , Dummy(96) , Eor , File(6) , Flag , Force(3) , Forcex(3) ,       &
         & Geom(4) , Grav(3) , Grid(3) , I1 , I2 , Iamt , Iat , Ibegin , Ibit , Ibitr , Ibuff1 , Ibuff2 , Ibuff3 , Ibufsz , Iconso ,&
         & Icont , Icore , Icore1 , Iend , Ierrtn , Ifile , Ihalf , Ihead(96) , Iheadb(96) , Iheadr , Iname(12) , Inprwd , Ion ,    &
         & Iphi , Ipiez , Ipt , Iretrn , Iscrat , Istart , Isystm(175) , It , K3or6 , Last , Ll(6) , Load(3) , Mach , Mn , Momax(3) &
         & , Moment(3) , Mpc(3) , Mpcadd(3) , Mpcax(3) , Mpcon , N , N3or5 , Nadd , Ncard , Ncards , Neg111(3) , Nfile , Nlines ,   &
         & Nmove
   INTEGER Nnn , Noaxic , Noeor , Noflag , Non , Nopont , Noreg , Nosect , Nout , Nplus1 , Nwords , Omit(3) , Omitax(3) , One , Op ,&
         & Openfl(6) , Outbuf , Outrwd , Pload(3) , Pointx(3) , Presax(3) , Rec(3) , Rec1(3) , Recid(3) , Recid1(3) , Recidx(3) ,   &
         & Ringax(3) , Ringid , Scrtch , Sectax(3) , Seqgp(3) , Setid , Sorc , Spc(3) , Spcadd(3) , Spcax(3) , Supax(3) , Suport(3) &
         & , T65535(3) , Temp(3) , Tempax(3) , Tempd(3) , Trail(7) , Two(32) , Veor , Z(13) , Zero , Zpt
   LOGICAL Nogo , Recoff
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Bottom
   COMMON /condas/ Consts
   COMMON /ifp3cm/ File , Iname , Cdtype , Axic1 , Cconex , Forcex , Force , Grav , Load , Momax , Moment , Mpcadd , Mpcax ,        &
                 & Omitax , Pointx , Presax , Ringax , Sectax , Seqgp , Spcax , Supax , Tempax , Tempd , Pload , Mpc , Spc , Grid , &
                 & Suport , Neg111 , T65535 , Temp , Omit , Spcadd , One , Zero , Iheadb , Ctriaa , Ctrapa , Iconso
   COMMON /ifp3lv/ Recid , Recid1 , Recidx , Iend , Rec , Rec1 , Trail , It , Axtrl , Openfl , N , A1 , Csid , Ni , Nisq , A2 ,     &
                 & Ibuff1 , Ibuff2 , Ibuff3 , A3 , Buff , Nogo , Op , A4 , Iheadr , Ibitr , Ifile , Noreg , Last , Ierrtn , Icont , &
                 & Noaxic , Ringid , Outbuf , Veor , Istart , Iretrn , Flag , Iamt , Sum , Ibit , Setid , Sorc , Ibegin , Mpcon ,   &
                 & Nwords , Nnn , Angle , K3or6 , Nphi1 , Zpt , Nmove , Csset , Nopont , Non , Iphi , Recoff , Nphi , N3or5 , Ion , &
                 & Nplus1 , Nosect , Coef , Ipt , Compon , Icore , Iscrat , Icore1 , Ncards , I1 , Iat , I2 , T1 , T2 , Nfile ,     &
                 & Nadd , Ncard
   COMMON /ifpdta/ Dum , Gc , Ll
   COMMON /machin/ Mach , Ihalf
   COMMON /output/ Dummy , Ihead
   COMMON /system/ Ibufsz , Nout , Noflag , Dumdum , Nlines , Ddd , Mn , Dum50 , Ipiez
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Rz
   INTEGER andf , lshift , orf
   INTEGER i , i3 , i4 , i5 , icard , ifist , ilast , imsg , irec , j , l2
   LOGICAL ifpdco
   REAL t3
   EXTERNAL andf , lshift , orf
   EQUIVALENCE (Consts(4),Raddeg) , (Z(1),Rz(1)) , (Geom(1),File(1)) , (Scrtch,File(5)) , (Axic,File(6)) , (Noeor,Inprwd,Zero) ,    &
    & (Eor,Clorwd,Outrwd,One)
   EQUIVALENCE (Ibufsz,Isystm(1))
   DATA ifist/4HFIST/ , i3 , i4 , i5/3 , 4 , 5/
!
!
!     GEOM4 PROCESSING
!     ================
!
!     OPEN GEOM4
!
   Ifile = Geom(4)
   i = 4
   Op = Outrwd
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
 300  Z(1) = 100000101
   Z(2) = 101
   Z(3) = -1
   Z(4) = 200000102
   Z(5) = 102
   Z(6) = -1
   Z(7) = 100000000
   Z(8) = 101
   Z(9) = -1
   Z(10) = 200000000
   Z(11) = 102
   Z(12) = -1
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(1),12,Noeor)
   CALL locate(*400,Z(Ibuff1),Rec(1),Flag)
!
!     READ AN OPEN ENDED SPCADD OR MPCADD CARD INTO CORE.
!
   i = 1
   DO
      CALL read(*9400,*400,Axic,Z(i),1,Noeor,Iamt)
      IF ( Z(i)<0 ) THEN
!
!     COMPLETE CARD IS AT HAND
!
         Z(i) = 101
         i = i + 1
         Z(i) = -1
         Z(1) = Z(1) + 100000000
         IF ( Nogo ) GOTO Icont
         DO
            CALL write(Geom(4),Z(1),i,Noeor)
            IF ( Z(i-1)==102 ) GOTO 400
            Z(i-1) = 102
            Z(1) = Z(1) + 100000000
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
   CALL locate(*600,Z(Ibuff1),Rec(1),Flag)
!
!     OK SPCAX OR MPCAX RECORD EXISTS.
!
   ilast = -1
 500  Z(4) = -1
   CALL read(*9400,*600,Axic,Z(2),1,Noeor,Iamt)
!
!     MPCAX CARDS ARE OPEN ENDED
!     SPCAX CARDS ARE 5 WORDS LONG.
!
   IF ( Z(2)/=ilast ) THEN
      ilast = Z(2)
!
!     CREATE TWO SPCADD OR MPCADD CARDS.
!
      Z(3) = 101
      Z(1) = Z(2) + 100000000
      IF ( Nogo ) GOTO Icont
      DO
         CALL write(Geom(4),Z(1),4,Noeor)
         IF ( Z(3)==102 ) EXIT
         Z(3) = 102
         Z(1) = Z(1) + 100000000
      ENDDO
   ENDIF
   DO
!
!     READ UP TO NEXT CARD
!
      CALL read(*9400,*600,Axic,Z(1),4,Noeor,Iamt)
      IF ( Rec(1)==Spcax(1) .OR. Z(1)==(-1) ) GOTO 500
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
   CALL locate(*1600,Z(Ibuff1),Mpcax(1),Flag)
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
 1000 CALL read(*9400,*1500,Axic,Setid,1,Noeor,Iamt)
   IF ( Setid>100 ) GOTO 1600
   Nwords = Nwords + 1
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Setid,1,Noeor)
!
!     READ 4-WORDS SETS UNTIL -1,-1,-1,-1 ENCOUNTERED...
!
 1100 CALL read(*9400,*1400,Axic,Z(1),4,Noeor,Iamt)
   Nwords = Nwords + 4
   IF ( Z(4)==-1 ) THEN
!
!     END OF EQUATION
!
      IF ( .NOT.(Nogo) ) CALL write(Geom(4),Neg111(1),3,Noeor)
      GOTO 1000
   ELSE
!
!     CHECK HARMONIC NUMBER
!
      Nnn = Z(2)
      ASSIGN 1200 TO Ierrtn
      GOTO 8900
   ENDIF
!
!     CHECK RING ID
!
 1200 Nnn = Z(1)
   ASSIGN 1300 TO Ierrtn
   GOTO 9000
!
 1300 Z(2) = Z(1) + (Z(2)+1)*1000000
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(2),3,Noeor)
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
 1900 CALL locate(*2300,Z(Ibuff1),Rec(1),Flag)
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
 2000 CALL read(*9400,*2300,Axic,Z(1),N3or5,Noeor,Iamt)
!
!     CHECK RING ID FOR S-SET PASS ONLY FOR POINTAX AND SECTAX CARDS.
!     NO CHECK WILL BE MADE IN THE GRID CARD GENERATION AREA.
!
!     IF (SORC .EQ. 102) GO TO 785
   Nnn = Z(2)
   ASSIGN 2100 TO Ierrtn
   GOTO 9000
!
 2100 Iat = N3or5 + 1
   DO i = 1 , K3or6
      Z(Iat) = Sorc
      Z(Iat+1) = Z(1)
      Z(Iat+2) = i
      Rz(Iat+3) = -1.0
      IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(Iat),4,Noeor)
      DO j = 1 , Nplus1
!
!     COMPUTE COEFFICIENT.
!
         Ni = j - 1
         IF ( N3or5==5 ) THEN
!
!     SECTAX CARD COEFFICIENTS
!
            T1 = Ni*Rz(i4)*Raddeg
            T2 = Ni*Rz(i5)*Raddeg
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
            T1 = Ni*Rz(i3)*Raddeg
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
            Coef = Rz(i3)*(Rz(i5)-Rz(i4))*Raddeg
            GOTO 2200
         ENDIF
 2180    Coef = Rz(i3)*(T2-T1)/Ni
         IF ( Sorc==101 .AND. (i==2 .OR. i==5) ) Coef = -Coef
!
 2200    Z(Iat) = Z(2) + j*1000000
         Z(Iat+1) = i
         Rz(Iat+2) = Coef
         IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(Iat),3,Noeor)
      ENDDO
      IF ( .NOT.(Nogo) ) CALL write(Geom(4),Neg111(1),3,Noeor)
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
   CALL locate(*3300,Z(Ibuff1),Mpcax(1),Flag)
   Ncard = 10
   IF ( Nwords/=0 ) THEN
      DO i = 1 , Nwords
         CALL read(*9400,*3100,Axic,Z(1),1,Noeor,Iamt)
      ENDDO
   ENDIF
!
!     NOW POSITIONED AT POINT LEFT OFF AT ABOVE.
!
 2800 CALL read(*9400,*3300,Axic,Setid,1,Noeor,Iamt)
   IF ( Setid<101 ) GOTO 3100
   IF ( Setid<=102 ) THEN
      Nogo = .TRUE.
      CALL page2(3)
      imsg = 366
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99013)
   ENDIF
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Setid,1,Noeor)
 2900 CALL read(*9400,*1400,Axic,Z(1),4,Noeor,Iamt)
   IF ( Z(4)==(-1) ) THEN
!
!     END OF EQUATION
!
      IF ( .NOT.(Nogo) ) CALL write(Geom(4),Neg111(1),3,Noeor)
      GOTO 2800
   ELSE
!
!     CHECK HARMONIC NUMBER
!
      Nnn = Z(2)
      ASSIGN 3000 TO Ierrtn
      GOTO 8900
   ENDIF
!
!     CHECK RING ID
!
 3000 Nnn = Z(1)
   ASSIGN 3200 TO Ierrtn
   GOTO 9000
 3100 CALL page2(3)
   imsg = 1063
   WRITE (Nout,99011) Sfm , imsg
   WRITE (Nout,99014) Cdtype(19) , Cdtype(20)
   Nogo = .TRUE.
   GOTO 9300
!
 3200 Z(2) = Z(1) + (Z(2)+1)*1000000
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(2),3,Noeor)
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
 3500 CALL locate(*4000,Z(Ibuff1),Rec(1),Flag)
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Rec1(1),3,Noeor)
 3600 CALL read(*9400,*3900,Axic,Z(1),3,Noeor,Iamt)
!
!     CHECK HARMONIC NUMBER
!
   Nnn = Z(2)
   ASSIGN 3700 TO Ierrtn
   GOTO 8900
!
!     CHECK RING ID
!
 3700 Nnn = Z(1)
   ASSIGN 3800 TO Ierrtn
   GOTO 9000
!
 3800 Z(2) = Z(1) + (Z(2)+1)*1000000
   IF ( ifpdco(Z(3)) ) THEN
      Nogo = .TRUE.
      CALL page2(3)
      imsg = 367
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99003) Z(3) , Cdtype(2*Ncard-1) , Cdtype(2*Ncard)
99003 FORMAT (5X,'COMPONENT SPECIFICATION',I8,4H ON ,2A4,' CARD IS INCORRECT')
   ELSE
      DO l2 = 1 , 6
         IF ( Ll(l2)/=0 ) THEN
            Z(3) = Ll(l2)
            IF ( Nogo ) EXIT
            CALL write(Geom(4),Z(2),2,Noeor)
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
   CALL locate(*4800,Z(Ibuff1),Spcax(1),Flag)
   Last = 0
   Nwords = 0
 4400 CALL read(*9400,*4700,Axic,Z(1),5,Noeor,Iamt)
   IF ( Z(1)>100 ) GOTO 4800
   Nwords = Nwords + 5
!
!     ALTER CARD JUST READ AND OUTPUT
!
!     CHECK HARMONIC NUMBER
!
   Nnn = Z(3)
   ASSIGN 4500 TO Ierrtn
   GOTO 8900
!
!     CHECK RING ID
!
 4500 Nnn = Z(2)
   ASSIGN 4600 TO Ierrtn
   GOTO 9000
!
 4600 Z(2) = Z(2) + (Z(3)+1)*1000000
   Z(3) = Z(4)
   Z(4) = Z(5)
!
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(1),4,Noeor)
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
 4900 CALL locate(*5500,Z(Ibuff1),Ringax(1),Flag)
 5000 CALL read(*9400,*5300,Axic,Z(1),4,Noeor,Iamt)
!
   IF ( Sorc==102 ) GOTO 5200
!
!     GIVE RING CARD A CHECK FOR MINIMUM DATA.
!
!     CHECK RING ID
!
   Nnn = Z(1)
   ASSIGN 5100 TO Ierrtn
   GOTO 9000
!
!     CHECK FOR NON-ZERO RADIUS
!
 5100 IF ( Rz(i3-1)==0 ) THEN
      CALL page2(3)
      imsg = 368
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99004) Z(1)
99004 FORMAT (5X,'RINGAX CARD WITH RING ID =',I10,' HAS A ZERO RADIUS',' SPECIFIED.')
      Nogo = .TRUE.
   ENDIF
 5200 Z(4) = 0
   Z(3) = Compon
   Z(2) = Z(1) + 1000000
   Z(1) = Sorc
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(1),4,Noeor)
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
   CALL locate(*6000,Z(Ibuff1),Spcax(1),Flag)
   Ncard = 18
   IF ( Nwords/=0 ) THEN
      DO i = 1 , Nwords , 5
         CALL read(*9400,*6100,Axic,Z(1),5,Noeor,Iamt)
      ENDDO
   ENDIF
!
!     NOW POSITIONED AT POINT LEFT OFF AT ABOVE...
!
 5700 CALL read(*9400,*6000,Axic,Z(1),5,Noeor,Iamt)
   IF ( Z(1)<101 ) GOTO 6100
   IF ( Z(1)<=102 ) THEN
      Nogo = .TRUE.
      CALL page2(3)
      imsg = 366
      WRITE (Nout,99012) Ufm , imsg
      WRITE (Nout,99013)
   ENDIF
!
!     CHECK HARMONIC NUMBER
!
   Nnn = Z(3)
   ASSIGN 5800 TO Ierrtn
   GOTO 8900
!
!     RING ID CHECK
!
 5800 Nnn = Z(2)
   ASSIGN 5900 TO Ierrtn
   GOTO 9000
!
 5900 Z(2) = Z(2) + (Z(3)+1)*1000000
   Z(3) = Z(4)
   Z(4) = Z(5)
   IF ( .NOT.(Nogo) ) CALL write(Geom(4),Z(1),4,Noeor)
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
 6400 Ifile = Geom(1)
   i = 1
   Op = Outrwd
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
      IF ( Nopont==0 ) GOTO 7700
      GOTO 6900
   ELSEIF ( Nopont/=0 ) THEN
!
!     LOCATE SECTAX CARDS, READ SECTAX, CONVERT TO GRID, PUT ON NFILE
!
      Nfile = Scrtch
!
!     OPEN SCRTCH FILE
!
      i = 5
      Op = Outrwd
      Buff = Ibuff3
      ASSIGN 6700 TO Iretrn
      GOTO 8600
   ELSE
!
      Nfile = Geom(1)
   ENDIF
!
 6700 icard = 15
   CALL locate(*7600,Z(Ibuff1),Sectax(1),Flag)
   DO
      CALL read(*9400,*6800,Axic,Z(1),5,Noeor,Iamt)
      Z(2) = 0
      Z(6) = Csid
      Z(7) = 0
      Z(8) = 0
      IF ( .NOT.(Nogo) ) CALL write(Nfile,Z(1),8,Noeor)
   ENDDO
 6800 IF ( Nopont==0 ) GOTO 7700
 6900 icard = 12
   CALL locate(*7600,Z(Ibuff1),Pointx(1),Flag)
!
!     READ POINT CARD CONVERT TO GRID CARD AND PUT OUT ON GEOM(1)
!     MERGING GRID CARDS FROM SCRTCH IF NOSECT IS NON-ZERO
!
   IF ( Nosect/=0 ) THEN
      IF ( Nogo ) GOTO 7700
      CALL close(Scrtch,Clorwd)
      CALL open(*9500,Scrtch,Z(Ibuff3),Inprwd)
      CALL read(*7300,*7300,Scrtch,Z(9),8,Noeor,Iamt)
   ENDIF
 7000 CALL read(*9400,*7500,Axic,Z(1),3,Noeor,Iamt)
!
!     CONVERT POINTAX CARD
!
   Z(2) = 0
   Rz(i4) = 0.0
   Rz(i5) = 0.0
   Z(6) = Csid
   Z(7) = 0
   Z(8) = 0
   IF ( Nosect==0 ) THEN
      Zpt = 1
      GOTO 7200
   ENDIF
 7100 IF ( Z(1)>=Z(9) ) THEN
      Zpt = 9
   ELSE
      Zpt = 1
   ENDIF
 7200 DO WHILE ( .NOT.(Nogo) )
      CALL write(Geom(1),Z(Zpt),8,Noeor)
      IF ( Zpt==1 ) GOTO 7000
      CALL read(*7300,*7300,Scrtch,Z(9),8,Noeor,Iamt)
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
 7700 CALL locate(*8200,Z(Ibuff1),Ringax(1),Flag)
   Nwords = (Icore/4)*4 - 12
   Ibegin = 13
   Iscrat = 0
   CALL read(*9400,*7900,Axic,Z(13),Nwords,Noeor,Iamt)
!
!     FALL HERE IMPLIES CORE IS FULL.. SPILL BALANCE TO SCRTCH FILE.
!
   Ion = 0
   Iscrat = 0
   IF ( Nogo ) GOTO 8200
   CALL open(*9500,Scrtch,Z(Ibuff3),Outrwd)
   DO
      CALL read(*9400,*7800,Axic,Z(1),8,Noeor,Iamt)
      Ion = 1
      CALL write(Scrtch,Z(1),8,Noeor)
   ENDDO
 7800 IF ( (Iamt/4)*4/=Iamt ) GOTO 8100
   IF ( Ion==0 .AND. Iamt==0 ) GOTO 8000
   Iscrat = 1
   IF ( Nogo ) GOTO 8200
   CALL write(Scrtch,Z(1),Iamt,Eor)
   CALL close(Scrtch,Clorwd)
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
      CALL open(*9500,Scrtch,Z(Ibuff3),Inprwd)
   ENDIF
   Z(2) = 0
   Z(5) = 0
   Z(6) = Csid
   Z(8) = 0
   Ncards = Nwords/4
!
!     27TH WORD OF SYSTEM IS PACKED AND HOLDS NUMBER OF RINGS AND HARMS
!
   Mn = Nplus1
   Isystm(161) = Ncards
   Nadd = 0
   DO i = 1 , Nplus1
      Nadd = Nadd + 1000000
      Ipt = Ibegin - 4
!
!     PASS THROUGH THE INCORE CARDS
!
      DO j = 1 , Ncards
         Ipt = Ipt + 4
         Z(1) = Z(Ipt) + Nadd
         Z(3) = Z(Ipt+1)
         Z(4) = Z(Ipt+2)
         Z(7) = Z(Ipt+3)
         IF ( .NOT.(Nogo) ) CALL write(Geom(1),Z(1),8,Noeor)
      ENDDO
!
!     PASS THROUGH SCRTCH CARDS IF ANY
!
      IF ( Nogo ) CYCLE
      IF ( Iscrat==0 ) CYCLE
      DO
         CALL read(*9400,*8050,Scrtch,Z(9),4,Noeor,Iamt)
         Z(1) = Z(9) + Nadd
         Z(3) = Z(10)
         Z(4) = Z(11)
         Z(7) = Z(12)
         CALL write(Geom(1),Z(1),8,Noeor)
      ENDDO
!
 8050 CALL rewind(Scrtch)
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
   CALL locate(*8500,Z(Ibuff1),Rec(1),Flag)
   IF ( Nogo ) GOTO 8500
   CALL write(Ifile,Rec(1),3,Noeor)
   DO
      CALL read(*9400,*8400,Axic,Z(1),Icore,Noeor,Iamt)
      Iamt = Icore
      CALL write(Ifile,Z(1),Iamt,Noeor)
   ENDDO
!
!     CLOSE GEOM1
!
 8300 i = 1
   ASSIGN 9300 TO Iretrn
   GOTO 8800
 8400 IF ( .NOT.(Nogo) ) THEN
      CALL write(Ifile,Z(1),Iamt,Eor)
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
      CALL open(*8700,File(i),Z(Buff),Op)
      Openfl(i) = 1
      IF ( i<=4 ) THEN
!
!     WRITE THE HEADER RECORD
!
         CALL write(File(i),Iname(2*i-1),2,Eor)
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
      IF ( i<=4 ) CALL write(File(i),T65535(1),3,Eor)
      CALL close(File(i),Clorwd)
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
 9200 IF ( .NOT.(Nogo) ) CALL write(Ifile,Rec(1),3,Noeor)
   GOTO Iheadr
!
!     RETURN TO IFP3
!
 9300 RETURN
!
!     EOF ENCOUNTERED READING AXIC FILE
!
 9400 Nfile = Axic
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
